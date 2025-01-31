package com.protreino.services.usecase;

import java.io.BufferedReader;
import java.io.IOException;
import java.lang.reflect.Type;
import java.net.ConnectException;
import java.net.SocketTimeoutException;
import java.net.UnknownHostException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Objects;

import javax.swing.SwingWorker;

import org.apache.commons.codec.binary.Base64;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.reflect.TypeToken;
import com.protreino.services.client.SmartAcessoFotoServiceClient;
import com.protreino.services.constants.Configurations;
import com.protreino.services.devices.Device;
import com.protreino.services.devices.TopDataDevice;
import com.protreino.services.entity.BiometricEntity;
import com.protreino.services.entity.DocumentoEntity;
import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.entity.PedestreRegraEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.entity.PedestrianEquipamentEntity;
import com.protreino.services.entity.PedestrianMessagesEntity;
import com.protreino.services.entity.UserEntity;
import com.protreino.services.enumeration.BroadcastMessageType;
import com.protreino.services.main.Main;
import com.protreino.services.repository.BiometricRepository;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.repository.PedestrianAccessRepository;
import com.protreino.services.to.BroadcastMessageTO;
import com.protreino.services.to.PedestrianAccessTO;
import com.protreino.services.utils.HttpConnection;
import com.protreino.services.utils.Utils;

public class SyncPedestrianAccessListUseCase {

	private static boolean updatingPedestrianAccessList = false;
	private static Long lastSync = 0L;
	private static final String MAQUINA_TEM_SERVER_MESSAGE = "Sincronizacao desabilitada: Maquina possui servidor";
	private static final SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss:sss");
	private static Gson gson  = new GsonBuilder().registerTypeAdapter(Date.class, new JsonDeserializer<Date>() {
        public Date deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context) throws JsonParseException {
            try {
                return Utils.convertDataJson(json);
            } catch (Exception e) {
            }

            return null;
        }
    }).create();
	
	private final SmartAcessoFotoServiceClient smartAcessoFotoServiceClient = new SmartAcessoFotoServiceClient();
	
	public void syncPedestrianAccessList() {
        if (getUpdatingPedestrianAccessList()) {
            return;
        }

        if (Main.temServidor()) {
            System.out.println(sdf.format(new Date()) + " " + MAQUINA_TEM_SERVER_MESSAGE);
            return;
        }

        setUpdatingPedestrianAccessList(true);
        SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {

            @Override
            public Void doInBackground() {
                try {
                    while (Main.isCadastrandoBiometria()) {
                        Thread.sleep(2000);
                    }
                    while (Main.getUpdatingUsersAccessList()) {
                        Thread.sleep(500);
                    }

                    while (Main.loggedUser == null) {
                        Thread.sleep(500);
                    }

                    if (Main.mainScreen != null && Main.mainScreen.isVisible()) {
                    	Main.mainScreen.getListaAcessoPanel().getSyncButton().setText("Atualizando lista com o servidor...");
                    	Main.mainScreen.getListaAcessoPanel().getSyncButton().setEnabled(false);
                    	Main.mainScreen.getListaAcessoPanel().getSyncButton().revalidate();
                    }

                    if (Main.timerSyncAthleteAccessList.isRunning()) {
                        Main.timerSyncAthleteAccessList.stop();
                    }

                    Main.timerSyncAthleteAccessList.setInitialDelay(Integer.valueOf(Utils.getPreference("timeAccessList")) * 60000);
                    Main.getTrayIcon().setImage(Main.getTrayIconImageLoading());

                    //Sincroniza os logs de acesso antes de sincronizar os pedestres
                    Main.syncLogAthleteAccess();

                    while (Main.getUpdatingLogAccessList()) {
                        Thread.sleep(500);
                    }

                    HttpConnection con = new HttpConnection(Main.urlApplication + "/restful-services/login/action");
                    int responseCode = con.getResponseCode();

                    if (responseCode != 200) {
                        return null;
                    }

                    Long backUpLastSync = lastSync != null ? lastSync.longValue() : 0L;

                    List<PedestrianAccessEntity> visitantesLocais = enviaPedestresCadastradosOuEditadosDesktop();

                    enviaBiometriasColetadasLocalmente();
                    recebePedestresEBiometriasDaWeb();
                    buscaFotosDosPedestres(backUpLastSync);

                    if (visitantesLocais != null && !visitantesLocais.isEmpty()) {
                        apagaDadosNovos(visitantesLocais);
                    }

                    atualizaListadeAcessoCatracaOffline();

                } catch (UnknownHostException | SocketTimeoutException | ConnectException ce) {
                    System.out.println(ce.getMessage());
                    ce.printStackTrace();
                } catch (Exception e) {
                    e.printStackTrace();
                    System.out.println(sdf.format(new Date()) + "  ERRO NA SINCRONIZACAO: Exception: " + e.getMessage());

                } finally {
                    if (Main.loggedUser != null) {
                        Main.timerSyncAthleteAccessList.start();
                    }

                    setUpdatingPedestrianAccessList(false);
                    Main.getTrayIcon().setImage(Main.getTrayIconImage());

                    if (Main.mainScreen != null && Main.mainScreen.isVisible()) {
                    	Main.mainScreen.getListaAcessoPanel().getSyncButton().setText("Atualizar lista com o servidor");
                    	Main.mainScreen.getListaAcessoPanel().getSyncButton().setEnabled(true);
                    	Main.mainScreen.getListaAcessoPanel().getSyncButton().revalidate();
                    	Main.mainScreen.getListaAcessoPanel().updateDateLastSync();
                    }
                }

                //limpa lixos
                new Thread() {
                    public void run() {
                        Runtime.getRuntime().gc();
                    }

                }.start();

                return null;
            }

            private void atualizaListadeAcessoCatracaOffline() throws Exception {
                if (Objects.isNull(Main.devicesList) || Main.devicesList.isEmpty()) {
                    return;
                }

                for (Device device : Main.devicesList) {
                    if (device instanceof TopDataDevice) {
                        TopDataDevice topDataDevice = (TopDataDevice) device;
                        if (!topDataDevice.isConnected()) {
                            topDataDevice.enviaCartaoCatracaOffline();
                        }
                    }
                }
            }

            @SuppressWarnings("unchecked")
            private List<PedestrianAccessEntity> enviaPedestresCadastradosOuEditadosDesktop() throws IOException {
                Main.verificaValidandoAcesso();
                final BiometricRepository biometricRepository = new BiometricRepository();
                List<PedestrianAccessEntity> visitantesLocais = (List<PedestrianAccessEntity>) HibernateAccessDataFacade
                        .getResultListLimited(PedestrianAccessEntity.class, "PedestrianAccessEntity.findAllCadastradosOuEditadosDesktop", 100L);

                if (visitantesLocais == null || visitantesLocais.isEmpty()) {
                    System.out.println(sdf.format(new Date()) + "  PEDESTRES/VISITANTES LOCAIS: sem registros para enviar");
                    return null;
                }

                System.out.println("Iniciando sincronismo de pedestres para web");

                JsonArray responseArray = new JsonArray();
                for (PedestrianAccessEntity visitante : visitantesLocais) {
                    // se foi criado, envia os dados para o servidor
                    // porque está sem o ID
                    if (Boolean.TRUE.equals(visitante.getCadastradoNoDesktop())) {
                        visitante.setListaAcessosTransient(buscaAcessosVisitante(visitante.getId()));

                        if (visitante.getListaAcessosTransient() != null
                                && !visitante.getListaAcessosTransient().isEmpty()) {
                            int countAtivos = 0;
                            for (LogPedestrianAccessEntity l : visitante.getListaAcessosTransient()) {
                                if (!"Regras ignoradas".equals(l.getReason())
                                        && l.getStatus() != null
                                        && "ATIVO".equalsIgnoreCase(l.getStatus())) {
                                    countAtivos++;
                                }
                            }
                            if (countAtivos > 0) {
                                visitante.setQtdAcessoAntesSinc(countAtivos);
                            }
                        }

                        visitante.setListaBiometriasTransient(biometricRepository.buscaBiometriasVisitante(visitante.getId()));
                    }

                    JsonObject responseObj = getNewVisitanteResponseObj(visitante);
                    responseArray.add(responseObj);
                }

                System.out.println("Enviando request com visitantes: " + responseArray.size());

                HttpConnection con = new HttpConnection(Main.urlApplication + "/restful-services/access/uploadVisitantes");
                int responseCode = con.sendResponse(responseArray.toString());

                if (responseCode != 200) {
                    System.out.println(sdf.format(new Date())
                            + "  ERRO AO ENVIAR VISITANTES LOCAIS: Response code: " + responseCode);
                    System.out.println(sdf.format(new Date())
                            + "  ERRO AO ENVIAR VISITANTES LOCAIS: Error String: " + con.getErrorString());
                    return null;
                }

                atualizaDadosAlterados(visitantesLocais);

                return visitantesLocais;
            }

            private void atualizaDadosAlterados(List<PedestrianAccessEntity> visitantesLocais) {
                if (visitantesLocais == null || visitantesLocais.isEmpty()) {
                    return;
                }

                //antes de alterar dados, verifica se existe validacao de acesso em andamento
                Main.verificaValidandoAcesso();

                for (PedestrianAccessEntity visitante : visitantesLocais) {
                    if (!Boolean.TRUE.equals(visitante.getCadastradoNoDesktop())) {

                        if (visitante.getMensagens() != null && !visitante.getMensagens().isEmpty()) {
                            for (PedestrianMessagesEntity m : visitante.getMensagens()) {
                            	HibernateAccessDataFacade.remove(m);
                            }
                        }

                        if (visitante.getDocumentos() != null && !visitante.getDocumentos().isEmpty()) {
                            for (DocumentoEntity d : visitante.getDocumentos()) {
                            	HibernateAccessDataFacade.remove(d);
                            }
                        }

                        if (visitante.getPedestreRegra() != null && !visitante.getPedestreRegra().isEmpty()) {
                            for (PedestreRegraEntity pr : visitante.getPedestreRegra()) {
                            	HibernateAccessDataFacade.remove(pr);
                            }
                        }

                        if (visitante.getEquipamentos() != null && !visitante.getEquipamentos().isEmpty()) {
                            for (PedestrianEquipamentEntity pe : visitante.getEquipamentos()) {
                            	HibernateAccessDataFacade.remove(pe);
                            }
                        }

                        visitante.setDocumentos(new ArrayList<>());
                        visitante.setPedestreRegra(new ArrayList<>());
                        visitante.setEquipamentos(new ArrayList<>());
                        visitante.setMensagens(new ArrayList<>());

                        visitante.setEditadoNoDesktop(false);
                        HibernateAccessDataFacade.update(PedestrianAccessEntity.class, visitante);
                    }
                }
            }

            private void apagaDadosNovos(List<PedestrianAccessEntity> visitantesLocais) {
                //antes de alterar dados, verifica se existe validacao de acesso em andamento
                Main.verificaValidandoAcesso();

                final PedestrianAccessRepository pedestrianAccessRepository = new PedestrianAccessRepository();
                for (PedestrianAccessEntity visitante : visitantesLocais) {
                    if (!Boolean.TRUE.equals(visitante.getCadastradoNoDesktop())) {
                    	continue;
                    }
                    
                    if (visitante.getMensagens() != null && !visitante.getMensagens().isEmpty()) {
                    	for (PedestrianMessagesEntity m : visitante.getMensagens()) {
                    		HibernateAccessDataFacade.remove(m);
                    	}
                    }

                    if (visitante.getDocumentos() != null && !visitante.getDocumentos().isEmpty()) {
                    	for (DocumentoEntity d : visitante.getDocumentos()) {
                    		HibernateAccessDataFacade.remove(d);
                    	}
                    }

                    if (visitante.getPedestreRegra() != null && !visitante.getPedestreRegra().isEmpty()) {
                    	for (PedestreRegraEntity pr : visitante.getPedestreRegra()) {
                    		HibernateAccessDataFacade.remove(pr);
                    	}
                    }

                    if (visitante.getEquipamentos() != null && !visitante.getEquipamentos().isEmpty()) {
                    	for (PedestrianEquipamentEntity pe : visitante.getEquipamentos()) {
                    		HibernateAccessDataFacade.remove(pe);
                    	}
                    }

                    if (visitante.getListaAcessosTransient() != null
                            && !visitante.getListaAcessosTransient().isEmpty()) {
                    	for (LogPedestrianAccessEntity acesso : visitante.getListaAcessosTransient()) {
                    		HibernateAccessDataFacade.remove(acesso);
                    	}
                    }

                    List<LogPedestrianAccessEntity> novosLogs = buscaAcessosVisitante(visitante.getId());

                    if (novosLogs != null && !novosLogs.isEmpty()) {
                        PedestrianAccessEntity novoPedestre = pedestrianAccessRepository.buscaPedestrePorIdTemp(visitante.getId());
                        System.out.println("id do pedestre antigo" + visitante.getId());
                        for (LogPedestrianAccessEntity log : novosLogs) {
                            log.setIdPedestrian(novoPedestre.getId());
                            log.setPedestre(novoPedestre);

                            HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, log);
                        }
                        System.out.println("id do pedestre novo" + novoPedestre.getId());
                        visitante.setVersao(visitante.getVersao() + 1);
                    }

                    if (visitante.getListaBiometriasTransient() != null
                            && !visitante.getListaBiometriasTransient().isEmpty()) {
                        for (BiometricEntity biometria : visitante.getListaBiometriasTransient()) {
                            biometria = (BiometricEntity) HibernateAccessDataFacade
                                    .getSingleResultById(BiometricEntity.class, biometria.getId());

                            if (biometria != null) {
                                try {
                                	HibernateAccessDataFacade.remove(biometria);
                                } catch (Exception e) {
                                    System.out.println("Sem digital pra remover");
                                }
                            }
                        }
                    }

                    try {
                    	HibernateAccessDataFacade.remove(visitante);

                    } catch (Exception e) {
                        visitante = (PedestrianAccessEntity) HibernateAccessDataFacade
                                .getSingleResultById(PedestrianAccessEntity.class, visitante.getId());
                        visitante.setInvisivel(true);

                        HibernateAccessDataFacade.update(PedestrianAccessEntity.class, visitante);
                    }
                
                }
            }

            private void recebePedestresEBiometriasDaWeb() throws IOException {
                HttpConnection con = new HttpConnection(Main.urlApplication + "/restful-services/access/request"
                        + "?client=" + Main.loggedUser.getIdClient()
                        + "&lastsync=" + lastSync
                        + "&version=" + Configurations.VERSION);
                con.setTimeout(0); // tempo ilimitado

                Integer responseCode = con.getResponseCode();
                

                if (responseCode != 200 && responseCode != 404) {
                    System.out.println(sdf.format(new Date()) + "  ERRO NA SINCRONIZACAO: Error String: " + con.getErrorString());
                    return;
                }

                if (responseCode == 404) {
                    System.out.println(sdf.format(new Date()) + "  SEM REGISTROS PARA RECEBER: Error String: " + con.getErrorString());
                    return;
                }

                BufferedReader bufferedReader = con.getResponseReader();
                Type type = new TypeToken<List<PedestrianAccessTO>>() {
                }.getType();
                List<PedestrianAccessTO> athleteAccessTOList = gson.fromJson(bufferedReader, type);

                if (athleteAccessTOList == null || athleteAccessTOList.isEmpty()) {
                    System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: sem registros para receber");
                    return;
                }

                if ("true".equals(Utils.getPreference("printLog"))) {
                	System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: Response string: " + gson.toJson(athleteAccessTOList, type));                	
                }

                boolean atualizaDigitais = false;
                for (PedestrianAccessTO athleteAccessTO : athleteAccessTOList) {
                    if (Main.loggedUser == null) { // usuario deslogou durante a sincronizacao
                    	break;
                    }

                    // TODO : criar novo m�todo para pegar pedestre removido ou nao
                    //        isso pode resolver v�rios bugs
                    // TODO : verificar onde o luxand ID e removido para nao fazer mais. Pode ser
                    //		  aqui ou ne

                    PedestrianAccessEntity existentAthleteAccess = (PedestrianAccessEntity) HibernateAccessDataFacade.
                            getAllPedestresById(athleteAccessTO.getId());

                    if (existentAthleteAccess != null) {

                        // verifica se houve alteracao nos campos principais e se recebeu templates
                        if (existentAthleteAccess.toString().equals(athleteAccessTO.toString())) {
                            continue;
                        }

                        // Procedimento de atualizacao de usuarios nas catracas RWTech
                        if (Boolean.TRUE.equals(existentAthleteAccess.getCadastradoNaCatracaRWTech())
                                && !Boolean.TRUE.equals(existentAthleteAccess.getDesatualizadoNaCatracaRWTech())) {
                            existentAthleteAccess.setDesatualizadoNaCatracaRWTech(true);
                        }

                        //verifica se usuario foi apagado e se tem facial para apagar tamb�m no servidor facial
                        String idFacial = null;
                        if ((Boolean.TRUE.equals(athleteAccessTO.getRemovido())
                                || !"ATIVO".equals(athleteAccessTO.getStatus()))
                                && existentAthleteAccess.getLuxandIdentifier() != null) {
                            idFacial = existentAthleteAccess.getLuxandIdentifier();
                        }

                        //apaga dados do facial
//						if(idFacial != null && !"".equals(idFacial) && LuxandService.getInstance() != null) {
//							System.out.println("Estou deletando automaticamente, face: " + idFacial);
//							LuxandService.getInstance().clearName(Long.valueOf(idFacial));
//						}
                        
                        final String oldStatus = existentAthleteAccess.getStatus();

                        existentAthleteAccess.update(athleteAccessTO);

                        if((existentAthleteAccess.isRemovido() 
                        			|| !Objects.equals(oldStatus, existentAthleteAccess.getStatus()))
                        		&& Objects.nonNull(athleteAccessTO.getDataCadastroFotoNaHikivision()) 
                        		&& Utils.isHikivisionConfigValid() ) {
                        	
                        	final HikivisionUseCases hikivisionUseCases = new HikivisionUseCases();
                        	try {
                        		hikivisionUseCases.syncronizarUsuarioInDevices(existentAthleteAccess);

                        	} catch (Exception e) {
                        		System.out.println(e.getMessage());
							}
                        }

                        if (!atualizaDigitais && Boolean.TRUE.equals(existentAthleteAccess.getNovasDigitais())) {
                            atualizaDigitais = true;
                        }
                        
                        final boolean apagaFotoDePedestresInativcos = Utils.getPreferenceAsBoolean("deletePhotoFromInactivePedestrian");
                        
                        if(apagaFotoDePedestresInativcos 
                        		&& (existentAthleteAccess.isRemovido() || existentAthleteAccess.isInativo())
                        		&& Objects.nonNull(existentAthleteAccess.getFoto())) {
                        	existentAthleteAccess.setFoto(null);
                        }
                        
                        HibernateAccessDataFacade.update(PedestrianAccessEntity.class, existentAthleteAccess);

                    } else {
                        PedestrianAccessEntity newAthleteAccess = new PedestrianAccessEntity(athleteAccessTO);
                        if (!atualizaDigitais && Boolean.TRUE.equals(newAthleteAccess.getNovasDigitais())) {
                            atualizaDigitais = true;
                        }
                        HibernateAccessDataFacade.save(PedestrianAccessEntity.class, newAthleteAccess);
                    }
                }

                if (Main.loggedUser == null) {
                    return;
                }

                while (Main.isCadastrandoBiometria()) {
                    try {
                        Thread.sleep(2000);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }

                if (atualizaDigitais) {
                    for (Device device : Main.devicesList) {
                        try {
                            //reinicializa digitais na catraca topdata
                            if (device instanceof TopDataDevice && device.isConnected()) {
                                System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: atualizando templates " + device.getName());
                                TopDataDevice topData = (TopDataDevice) device;

                                if (topData.modeloLC) {
                                	topData.verificaCadastroNoInner(true, false, lastSync != null ? new Date(lastSync) : null);
                                
                                } else if (topData.getIndexSearchEngine() != null) {
                                	topData.restartIndexSearchEngine();
                                
                                } else {
                                	topData.atualizaDigitaisLFD(true, false, lastSync != null ? new Date(lastSync) : null);
                                }
                            }
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    }
                } else {
                    System.out.println(sdf.format(new Date()) + "SINCRONIZACAO: Sem alteracoes de templates para catracas");
                }

                Utils.sleep(1000);
                lastSync = Calendar.getInstance(new Locale("pt", "BR")).getTimeInMillis();
                Main.loggedUser.setLastSync(new Date(lastSync));
                Main.loggedUser = (UserEntity) HibernateAccessDataFacade.updateUser(UserEntity.class, Main.loggedUser)[0];

                if (Main.broadcastServer != null) {
                    Main.broadcastServer.sendMessage(new BroadcastMessageTO(BroadcastMessageType.REFRESH_TEMPLATES));
                }
            }

			@SuppressWarnings("unchecked")
            private void enviaBiometriasColetadasLocalmente() throws IOException {
                // Enviando as biometrias coletadas localmente
                List<BiometricEntity> biometriasLocais = (List<BiometricEntity>) HibernateAccessDataFacade.getResultList(BiometricEntity.class, 
                		"BiometricEntity.findAll");

                if (biometriasLocais == null || biometriasLocais.isEmpty()) {
                    System.out.println(sdf.format(new Date()) + "  BIOMETRIAS LOCAIS: sem registros para enviar");
                    return;
                }

                System.out.println("\r\n" + sdf.format(new Date()) + "  BIOMETRIAS LOCAIS: " + biometriasLocais.size() + " registros para enviar");
                JsonArray responseArray = new JsonArray();

                for (BiometricEntity biometria : biometriasLocais) {
                    JsonObject responseObj = new JsonObject();
                    responseObj.addProperty("idUser", biometria.getUser());
                    responseObj.addProperty("finger", biometria.getFinger().toString());
                    responseObj.addProperty("template", Base64.encodeBase64String(biometria.getTemplate()));
                    responseObj.addProperty("sample", biometria.getSample() != null ? Base64.encodeBase64String(biometria.getSample()) : null);
                    responseArray.add(responseObj);
                }

                HttpConnection con = new HttpConnection(Main.urlApplication + "/restful-services/access/saveBiometry");

                int responseCode = con.sendResponse(responseArray.toString());

                if (responseCode == 200) { // OK
                    System.out.println(sdf.format(new Date()) + "  BIOMETRIAS LOCAIS: dados enviados!");
                    // Biometrias enviadas com sucesso, podem ser apagadas localmente porque serao recebidas novamente atraves do AthleteAccess
                    for (BiometricEntity biometria : biometriasLocais) {
                    	HibernateAccessDataFacade.remove(biometria);
                        Utils.sleep(10);
                    }

                } else {
                    System.out.println(sdf.format(new Date()) + "  ERRO AO ENVIAR BIOMETRIAS LOCAIS: Response code: " + responseCode);
                    System.out.println(sdf.format(new Date()) + "  ERRO AO ENVIAR BIOMETRIAS LOCAIS: Error String: " + con.getErrorString());
                }
            }

            private void buscaFotosDosPedestres(Long backUpLastSync) throws IOException {
                List<String> ids = smartAcessoFotoServiceClient.buscaIdsDePedestreComFotoAlterada(backUpLastSync);
                
                if(Objects.isNull(ids) || ids.isEmpty()) {
                	return;
                }
                
                System.out.println(sdf.format(new Date()) + "  BUSCANDO FOTOS: " + ids.size());
                
                final Integer imageSize = Utils.getPreferenceAsInteger("imageSizeRequestServer");
                final Integer targetWidth = Utils.getPreferenceAsInteger("imageTargetWidthRequestServer");
                final Integer targetHeight = Utils.getPreferenceAsInteger("imageTargetHeightRequestServer");
                final boolean resize = Utils.getPreferenceAsBoolean("shouldMakeImageResize");
                
                ids.forEach(id -> {
                	final String idsParameter = id + ";";
                	
                    List<PedestrianAccessTO> athleteAccessTOList = smartAcessoFotoServiceClient
                    		.buscaFotoDePedestres(idsParameter, imageSize, resize, targetWidth, targetHeight);

                    if (athleteAccessTOList != null && !athleteAccessTOList.isEmpty()) {
                        for (PedestrianAccessTO athleteAccessTO : athleteAccessTOList) {
                            if (Main.loggedUser == null) {
                                break;
                            }

                            PedestrianAccessEntity existentAthleteAccess = (PedestrianAccessEntity) HibernateAccessDataFacade
                                    .getSingleResultById(PedestrianAccessEntity.class, athleteAccessTO.getId());

                            if (existentAthleteAccess != null) {
                                existentAthleteAccess.setFoto(Base64.decodeBase64(athleteAccessTO.getFotoBase64()));
                                HibernateAccessDataFacade.update(PedestrianAccessEntity.class, existentAthleteAccess);
                            }
                        }
                    }
                });
            }
        };
        worker.execute();
    }
	
	@SuppressWarnings("unchecked")
    private List<LogPedestrianAccessEntity> buscaAcessosVisitante(Long idVisitante) {
        HashMap<String, Object> args = new HashMap<String, Object>();
        args.put("ID_PEDESTRE", idVisitante);

        List<LogPedestrianAccessEntity> acessosVisitantesLocais = (List<LogPedestrianAccessEntity>)
        		HibernateAccessDataFacade.getResultListWithParams(LogPedestrianAccessEntity.class,
                        "LogPedestrianAccessEntity.findAllByPedestre", args);

        if (acessosVisitantesLocais != null
                && acessosVisitantesLocais.size() == 1
                && acessosVisitantesLocais.get(0) != null
                && "INDEFINIDO".equals(acessosVisitantesLocais.get(0).getStatus())) {
            Utils.sleep(2000);
            acessosVisitantesLocais = (List<LogPedestrianAccessEntity>)
            		HibernateAccessDataFacade.getResultListWithParams(LogPedestrianAccessEntity.class,
                            "LogPedestrianAccessEntity.findAllByPedestre", args);
        }

        return acessosVisitantesLocais;
    }
	
	private JsonObject getNewVisitanteResponseObj(PedestrianAccessEntity visitante) {
        JsonObject responseObj = new JsonObject();

        if (visitante.getEditadoNoDesktop() && !visitante.getCadastradoNoDesktop()) {
            responseObj.addProperty("id", visitante.getId().toString());
        } else {
            responseObj.addProperty("id", "");
        }

        responseObj.addProperty("idTemp", visitante.getIdTemp() != null ? visitante.getIdTemp().toString() : "");
        responseObj.addProperty("idCliente", Main.loggedUser.getIdClient());
        responseObj.addProperty("idUsuario", visitante.getIdUsuario() != null ? visitante.getIdUsuario().toString() : "");

        //Dados basicos
        responseObj.addProperty("nome", visitante.getName() != null ? visitante.getName() : "");
        try {
            responseObj.addProperty("dataNascimento", sdf.format(visitante.getDataNascimento()));
        } catch (Exception e) {
            responseObj.addProperty("dataNascimento", "");
        }
        responseObj.addProperty("email", visitante.getEmail() != null ? visitante.getEmail() : "");
        responseObj.addProperty("cpf", visitante.getCpf() != null ? visitante.getCpf() : "");
        responseObj.addProperty("genero", visitante.getGenero() != null ? visitante.getGenero() : "");
        responseObj.addProperty("rg", visitante.getRg() != null ? visitante.getRg() : "");
        responseObj.addProperty("telefone", visitante.getTelefone() != null ? visitante.getTelefone() : "");
        responseObj.addProperty("celular", visitante.getCelular() != null ? visitante.getCelular() : "");
        responseObj.addProperty("responsavel", visitante.getResponsavel() != null ? visitante.getResponsavel() : "");
        responseObj.addProperty("observacoes", visitante.getObservacoes() != null ? visitante.getObservacoes() : "");

        //Dados empresa
        responseObj.addProperty("idEmpresa", visitante.getIdEmpresa() != null ? visitante.getIdEmpresa().toString() : "");
        responseObj.addProperty("idDepartamento", visitante.getIdDepartamento() != null ? visitante.getIdDepartamento().toString() : "");
        responseObj.addProperty("idCentroCusto", visitante.getIdCentroCusto() != null ? visitante.getIdCentroCusto().toString() : "");
        responseObj.addProperty("idCargo", visitante.getIdCargo() != null ? visitante.getIdCargo().toString() : "");

        //Dados aba lateral
        responseObj.addProperty("foto", visitante.getFoto() != null ? Base64.encodeBase64String(visitante.getFoto()) : "");
        responseObj.addProperty("tipo", visitante.getTipo() != null ? visitante.getTipo() : "");
        responseObj.addProperty("status", visitante.getStatus() != null ? visitante.getStatus() : "INATIVO");
        responseObj.addProperty("matricula", visitante.getMatricula() != null ? visitante.getMatricula() : "");
        responseObj.addProperty("numeroCartao", visitante.getCardNumber() != null ? visitante.getCardNumber() : "");
        responseObj.addProperty("sempreLiberado", visitante.getSempreLiberado() != null ? visitante.getSempreLiberado().toString() : "false");
        responseObj.addProperty("habilitarTeclado", visitante.getHabilitarTeclado() != null
                ? visitante.getHabilitarTeclado().toString() : "false");
        responseObj.addProperty("enviaSmsAoPassarNaCatraca", visitante.getEnviaSmsAoPassarNaCatraca() != null
                ? visitante.getEnviaSmsAoPassarNaCatraca().toString() : "false");

        try {
            responseObj.addProperty("dataCadastroFotoNaHikivision", sdf.format(visitante.getDataCadastroFotoNaHikivision()));
        } catch (Exception e) {
            responseObj.addProperty("dataCadastroFotoNaHikivision", "");
        }
        
        //Dados endereco
        responseObj.addProperty("cep", visitante.getCep() != null ? visitante.getCep() : "");
        responseObj.addProperty("logradouro", visitante.getLogradouro() != null ? visitante.getLogradouro() : "");
        responseObj.addProperty("numero", visitante.getNumero() != null ? visitante.getNumero() : "");
        responseObj.addProperty("complemento", visitante.getComplemento() != null ? visitante.getComplemento() : "");
        responseObj.addProperty("bairro", visitante.getBairro() != null ? visitante.getBairro() : "");
        responseObj.addProperty("cidade", visitante.getCidade() != null ? visitante.getCidade() : "");
        responseObj.addProperty("estado", visitante.getEstado() != null ? visitante.getEstado() : "");
        responseObj.addProperty("qtdeCreditos", visitante.getQuantidadeCreditos() != null ?
                visitante.getQuantidadeCreditos().toString() : "");

        responseObj.addProperty("luxandIdentifier", visitante.getLuxandIdentifier() != null ?
                visitante.getLuxandIdentifier() : "");
        responseObj.addProperty("idRegra", visitante.getIdRegra() != null ? visitante.getIdRegra().toString() : "");

        responseObj.addProperty("login", visitante.getLogin() != null ? visitante.getLogin() : "");
        responseObj.addProperty("senha", visitante.getSenha() != null ? visitante.getSenha() : "");
        responseObj.addProperty("tipoAcesso", visitante.getTipoAcesso() != null ? visitante.getTipoAcesso() : "");
        responseObj.addProperty("tipoQRCode", visitante.getTipoQRCode() != null ? visitante.getTipoQRCode() : "");
        responseObj.addProperty("qrCodeParaAcesso", visitante.getQrCodeParaAcesso() != null ? visitante.getQrCodeParaAcesso() : "");
        responseObj.addProperty("qtdeAcessosAntesSinc", visitante.getQtdAcessoAntesSinc() != null
                ? visitante.getQtdAcessoAntesSinc().toString() : "");

        adicionaListaDeRegras(responseObj, visitante.getPedestreRegra());

        adicionaListaDeDocumentos(responseObj, visitante.getDocumentos());

        adicionaListaDeEquipamentos(responseObj, visitante.getEquipamentos());

        adicionaListaDeMensagens(responseObj, visitante.getMensagens());

        adicionaListaDeAcessosTransiente(responseObj, visitante.getListaAcessosTransient());

        adicionaListaDeBiometriasTransiente(responseObj, visitante.getListaBiometriasTransient());

        return responseObj;
    }

    private void adicionaListaDeRegras(JsonObject responseObj, List<PedestreRegraEntity> pedestresRegras) {
        if (pedestresRegras == null || pedestresRegras.isEmpty()) {
            responseObj.add("pedestresRegras", new JsonArray());
            return;
        }

        JsonArray pedestresRegrasArray = new JsonArray();

        for (PedestreRegraEntity pedestreRegra : pedestresRegras) {
            if (!pedestreRegra.getCadastradoNoDesktop()
                    && !pedestreRegra.getRemovidoNoDesktop())
                continue;

            JsonObject pedestreRegraObj = new JsonObject();
            pedestreRegraObj.addProperty("idRegraPR", pedestreRegra.getRegra() != null ? pedestreRegra.getRegra().getId().toString() : "0");
            try {
                pedestreRegraObj.addProperty("validadeRegraPR", sdf.format(pedestreRegra.getValidade()));

            } catch (Exception e) {
                pedestreRegraObj.addProperty("validadeRegraPR", "");
            }
            pedestreRegraObj.addProperty("qtdeDeCreditosPR", pedestreRegra.getQtdeDeCreditos() != null
                    ? pedestreRegra.getQtdeDeCreditos().toString() : "");
            pedestreRegraObj.addProperty("qtdeTotalDeCreditosPR", pedestreRegra.getQtdeTotalDeCreditos() != null
                    ? pedestreRegra.getQtdeTotalDeCreditos().toString() : "");
            pedestreRegraObj.addProperty("diasValidadeCreditoPR", pedestreRegra.getDiasValidadeCredito() != null
                    ? pedestreRegra.getDiasValidadeCredito().toString() : "");
            try {
                pedestreRegraObj.addProperty("dataInicioPeriodoPR", sdf.format(pedestreRegra.getDataInicioPeriodo()));
            } catch (Exception e) {
                pedestreRegraObj.addProperty("dataInicioPeriodoPR", "");
            }

            try {
                pedestreRegraObj.addProperty("dataFimPeriodo", sdf.format(pedestreRegra.getDataFimPeriodo()));

            } catch (Exception e) {
                pedestreRegraObj.addProperty("dataFimPeriodo", "");
            }
            pedestreRegraObj.addProperty("removido", pedestreRegra.getRemovidoNoDesktop().toString());
            pedestreRegraObj.addProperty("idPedestreRegra", pedestreRegra.getId().toString());

            pedestresRegrasArray.add(pedestreRegraObj);
        }

        responseObj.add("pedestresRegras", pedestresRegrasArray);
    }

    private void adicionaListaDeDocumentos(JsonObject responseObj, List<DocumentoEntity> documentos) {
        if (documentos == null || documentos.isEmpty()) {
            responseObj.add("documentos", new JsonArray());
            return;
        }

        JsonArray documentosArray = new JsonArray();

        for (DocumentoEntity documento : documentos) {
            if (!documento.getCadastradoNoDesktop()
                    && !documento.getRemovidoNoDesktop())
                continue;

            JsonObject documentoObj = new JsonObject();
            documentoObj.addProperty("nomeDoc", documento.getNome() != null ? documento.getNome() : "");
            documentoObj.addProperty("arquivoDoc", documento.getArquivo() != null ? Base64.encodeBase64String(documento.getArquivo()) : "");
            try {
                documentoObj.addProperty("validadeDoc", sdf.format(documento.getValidade()));

            } catch (Exception e) {
                documentoObj.addProperty("validadeDoc", "");
            }
            documentoObj.addProperty("removido", documento.getRemovidoNoDesktop().toString());
            documentoObj.addProperty("idDocumento", documento.getId().toString());

            documentosArray.add(documentoObj);
        }

        responseObj.add("documentos", documentosArray);
    }

    private void adicionaListaDeEquipamentos(JsonObject responseObj, List<PedestrianEquipamentEntity> equipamentos) {
        if (equipamentos == null || equipamentos.isEmpty()) {
            responseObj.add("equipamentos", new JsonArray());
            return;
        }

        JsonArray equipamentosArray = new JsonArray();

        for (PedestrianEquipamentEntity equipamento : equipamentos) {
            if (!equipamento.getCadastradoNoDesktop()
                    && !equipamento.getRemovidoNoDesktop())
                continue;

            JsonObject equipamentoObj = new JsonObject();
            equipamentoObj.addProperty("idEquipamento", equipamento.getIdEquipamento() != null ? equipamento.getIdEquipamento() : "");
            try {
                equipamentoObj.addProperty("validadeEquipamento", sdf.format(equipamento.getValidadeEquipamento()));

            } catch (Exception e) {
                equipamentoObj.addProperty("validadeEquipamento", "");
            }
            equipamentoObj.addProperty("nomeEquipamento", equipamento.getNomeEquipamento() != null ? equipamento.getNomeEquipamento() : "");
            equipamentoObj.addProperty("removido", equipamento.getRemovidoNoDesktop().toString());
            equipamentoObj.addProperty("id", equipamento.getId().toString());

            equipamentosArray.add(equipamentoObj);
        }

        responseObj.add("equipamentos", equipamentosArray);
    }

    private void adicionaListaDeMensagens(JsonObject responseObj, List<PedestrianMessagesEntity> mensagens) {
        if (mensagens == null || mensagens.isEmpty()) {
            responseObj.add("mensagens", new JsonArray());
            return;
        }

        JsonArray mensagensArray = new JsonArray();

        for (PedestrianMessagesEntity mensagem : mensagens) {
            if (!mensagem.getCadastradoNoDesktop()
                    && !mensagem.getRemovidoNoDesktop())
                continue;

            JsonObject mensagemObj = new JsonObject();
            mensagemObj.addProperty("nomeMsg", mensagem.getNome() != null ? mensagem.getNome() : "");
            mensagemObj.addProperty("statusMsg", mensagem.getStatus() != null ? mensagem.getStatus().toString() : "");
            mensagemObj.addProperty("mensagemMsg", mensagem.getMensagem() != null ? mensagem.getMensagem() : "");
            mensagemObj.addProperty("quantidadeMsg", mensagem.getQuantidade() != null ? mensagem.getQuantidade().toString() : "0");
            try {
                mensagemObj.addProperty("validadeMsg", sdf.format(mensagem.getValidade()));
            } catch (Exception e) {
                mensagemObj.addProperty("validadeMsg", "");
            }
            mensagemObj.addProperty("removido", mensagem.getRemovidoNoDesktop().toString());
            mensagemObj.addProperty("idMensagem", mensagem.getId().toString());

            mensagensArray.add(mensagemObj);
        }

        responseObj.add("mensagens", mensagensArray);
    }

    private void adicionaListaDeBiometriasTransiente(JsonObject responseObj, List<BiometricEntity> biometrias) {
        if (biometrias == null || biometrias.isEmpty()) {
            responseObj.add("biometrias", new JsonArray());
            return;
        }

        JsonArray biometriasArray = new JsonArray();

        for (BiometricEntity biometria : biometrias) {
            JsonObject biometriaObj = new JsonObject();

            biometriaObj.addProperty("idBiometria", biometria.getId() != null ? biometria.getId().toString() : "");
            biometriaObj.addProperty("idUserBiometria", biometria.getUser() != null ? biometria.getUser().toString() : "");
            biometriaObj.addProperty("userNameBiometria", biometria.getUserName() != null ? biometria.getUserName() : "");
            biometriaObj.addProperty("finger", biometria.getFinger() != null ? biometria.getFinger().toString() : "");
            biometriaObj.addProperty("template", biometria.getTemplate() != null
                    ? Base64.encodeBase64String(biometria.getTemplate()) : "");
            biometriaObj.addProperty("sample", biometria.getSample() != null ? Base64.encodeBase64String(biometria.getSample()) : null);

            biometriasArray.add(biometriaObj);
        }
        responseObj.add("biometrias", biometriasArray);
    }

    private void adicionaListaDeAcessosTransiente(JsonObject responseObj, List<LogPedestrianAccessEntity> acessos) {
        if (acessos == null || acessos.isEmpty()) {
            responseObj.add("acessos", new JsonArray());
            return;
        }

        JsonArray acessosArray = new JsonArray();

        for (LogPedestrianAccessEntity acesso : acessos) {
            JsonObject acessoObj = new JsonObject();

            acessoObj.addProperty("idAcesso", acesso.getId() != null ? acesso.getId().toString() : "");
            try {
                acessoObj.addProperty("dataAcesso", sdf.format(acesso.getAccessDate()));
            } catch (Exception e) {
                acessoObj.addProperty("dataAcesso", "");
            }
            acessoObj.addProperty("statusAcesso", acesso.getStatus() != null ? acesso.getStatus() : "");
            acessoObj.addProperty("localizacao", acesso.getLocation() != null ? acesso.getLocation() : "");
            acessoObj.addProperty("razao", acesso.getReason() != null ? acesso.getReason() : "");
            acessoObj.addProperty("direcao", acesso.getDirection() != null ? acesso.getDirection() : "");
            acessoObj.addProperty("equipamento", acesso.getEquipament() != null ? acesso.getEquipament() : "");

            acessosArray.add(acessoObj);
        }
        responseObj.add("acessos", acessosArray);
    }
	
	public static synchronized Long getLastSync() {
    	return lastSync;
    }
    
    public static synchronized void setLastSync(final Long value) {
    	lastSync = value;
    }
    
    public static synchronized void setUpdatingPedestrianAccessList(final boolean status) {
    	updatingPedestrianAccessList = status;
    }
    
    public static synchronized boolean getUpdatingPedestrianAccessList() {
    	return updatingPedestrianAccessList;
    }
}
