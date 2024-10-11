package com.protreino.services.devices;

import static com.protreino.services.constants.TopDataDeviceConstants.BLOQUEAR_SAIDA;
import static com.protreino.services.constants.TopDataDeviceConstants.COLETA_CARTOES_OFFLINE;
import static com.protreino.services.constants.TopDataDeviceConstants.DOIS_LEITORES;
import static com.protreino.services.constants.TopDataDeviceConstants.ECOAR_ASTERISCOS;
import static com.protreino.services.constants.TopDataDeviceConstants.ENVIA_DIGITAIS_PARA_CATRACA;
import static com.protreino.services.constants.TopDataDeviceConstants.HABILITAR_TECLADO;
import static com.protreino.services.constants.TopDataDeviceConstants.IDENTIFICACAO_BIOMETRICA;
import static com.protreino.services.constants.TopDataDeviceConstants.IGNORAR_REGRAS_DE_ACESSO;
import static com.protreino.services.constants.TopDataDeviceConstants.IS_DEVICE_RESTRITO;
import static com.protreino.services.constants.TopDataDeviceConstants.LEITOR_1;
import static com.protreino.services.constants.TopDataDeviceConstants.LEITOR_2;
import static com.protreino.services.constants.TopDataDeviceConstants.LOGICA_DE_CATRACA_COM_URNA;
import static com.protreino.services.constants.TopDataDeviceConstants.MENSAGEM_ONLINE;
import static com.protreino.services.constants.TopDataDeviceConstants.MODELO_BIOMETRICO;
import static com.protreino.services.constants.TopDataDeviceConstants.MODO_DE_TRABALHO;
import static com.protreino.services.constants.TopDataDeviceConstants.NIVEL_RECONHECIMENTO;
import static com.protreino.services.constants.TopDataDeviceConstants.PADRAO_DE_CARTAO;
import static com.protreino.services.constants.TopDataDeviceConstants.QUANTIDADE_DIGITOS_CARTAO;
import static com.protreino.services.constants.TopDataDeviceConstants.SENTIDO_DA_CATRACA;
import static com.protreino.services.constants.TopDataDeviceConstants.TEMPO_DE_LIBERADO;
import static com.protreino.services.constants.TopDataDeviceConstants.TEMPO_DE_MENSAGEM_NEGADO;
import static com.protreino.services.constants.TopDataDeviceConstants.TEMPO_DE_PING;
import static com.protreino.services.constants.TopDataDeviceConstants.TEMPO_ESPERA_PARA_CONECTAR;
import static com.protreino.services.constants.TopDataDeviceConstants.TEMPO_MUDANCA_ONLINE_OFFLINE;
import static com.protreino.services.constants.TopDataDeviceConstants.TEMPO_TECLADO;
import static com.protreino.services.constants.TopDataDeviceConstants.TIPO_BIOMETRICO;
import static com.protreino.services.constants.TopDataDeviceConstants.TIPO_LEITOR;
import static com.protreino.services.constants.TopDataDeviceConstants.VERIFICACAO_BIOMETRICA;
import static com.protreino.services.constants.TopDataDeviceConstants.ONLY_ENABLED_MODE;


import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import javax.swing.SwingWorker;
import javax.swing.SwingWorker.StateValue;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;
import com.nitgen.SDK.BSP.NBioBSPJNI;
import com.nitgen.SDK.BSP.NBioBSPJNI.EXPORT_MINCONV_TYPE;
import com.nitgen.SDK.BSP.NBioBSPJNI.FIR_HANDLE;
import com.nitgen.SDK.BSP.NBioBSPJNI.INPUT_FIR;
import com.nitgen.SDK.BSP.NBioBSPJNI.IndexSearch;
import com.protreino.services.client.SmartAcessoClient;
import com.protreino.services.constants.Origens;
import com.protreino.services.constants.Tipo;
import com.protreino.services.entity.BiometricEntity;
import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.entity.PedestrianMessagesEntity;
import com.protreino.services.entity.TemplateEntity;
import com.protreino.services.entity.UserEntity;
import com.protreino.services.enumeration.BroadcastMessageType;
import com.protreino.services.enumeration.DeviceMode;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.enumeration.FieldType;
import com.protreino.services.enumeration.Finger;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.enumeration.MessageType;
import com.protreino.services.enumeration.NotificationType;
import com.protreino.services.enumeration.VerificationResult;
import com.protreino.services.main.Main;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.repository.LogPedestrianAccessRepository;
import com.protreino.services.repository.PedestrianAccessRepository;
import com.protreino.services.to.AttachedTO;
import com.protreino.services.to.BroadcastMessageTO;
import com.protreino.services.to.ConfigurationGroupTO;
import com.protreino.services.to.ConfigurationTO;
import com.protreino.services.usecase.EnviaSmsDeRegistroUseCase;
import com.protreino.services.usecase.HikivisionUseCases;
import com.protreino.services.usecase.ProcessAccessRequestUseCase;
import com.protreino.services.usecase.ReleaseAccessUseCase;
import com.protreino.services.utils.Utils;
import com.topdata.EasyInner;
import com.topdata.easyInner.entity.Inner;
import com.topdata.easyInner.enumeradores.Enumeradores;
import com.topdata.easyInner.enumeradores.Enumeradores.EstadosInner;

@SuppressWarnings("serial")
public class TopDataDevice extends Device {
	
	private UserEntity user = Utils.userLogado();
	public static boolean portaAberta = false;
	protected EasyInner easyInner;
	protected Inner inner;
	protected Integer innerNumber;
	private int tempoEspera = 100;
	private NBioBSPJNI bsp;
	private NBioBSPJNI.Export export;
	private NBioBSPJNI.FIR_HANDLE storedFIRHandle;
	private NBioBSPJNI.INPUT_FIR storedInputFIR;
	private byte[] templateTemp;
	private byte[] template;

	private List<byte[]> templates;
	
	private boolean coletaCompleta = false;
	private int samplesCollected = 0;
	//private int exportType = NBioBSPJNI.EXPORT_MINCONV_TYPE.FIM01_HV; // FIM01_HV = 7 ; ANSI = 35
	private String mensagemEntradaOffLine       = "Entrada liberada";
	private String mensagemSaidaOffLine         = " Saida liberada ";
	private String mensagemPadraoOffLine        =  user.getName();
	private String mensagemPadraoMudancaOffLine =  user.getName();
	private String mensagemPadraoMudancaOnLine  = "  Modo Online   ";
	protected boolean sendingConfiguration = false;
	private IndexSearch indexSearchEngine;
	protected boolean validandoAcesso = false;
	protected String messagePersonalizedInDevice;
	protected String mensagemPermitido;
	
	public Boolean modeloLC = false;
	public int countAccess = 0;
	
	private Long idCadastro;
	
	private Integer nivelSeguranca;
	
	private final static StringBuffer CARTAO_MASTER = new StringBuffer(Utils.getPreference("cardMaster"));
	
	private SwingWorker<Void, Void> onlyEnabledPingWorker;
	private boolean isOnlyEnabledPingWorkerExecuting;

	protected String tipo = Tipo.ENTRADA;
	private final HikivisionUseCases hikivisionUseCases = new HikivisionUseCases();
	private final PedestrianAccessRepository pedestrianAccessRepository = new PedestrianAccessRepository();
	private final LogPedestrianAccessRepository logPedestrianAccessRepository = new LogPedestrianAccessRepository();

	private final EnviaSmsDeRegistroUseCase enviaSmsDeRegistroUseCase = new EnviaSmsDeRegistroUseCase();
	private final ProcessAccessRequestUseCase processAccessRequestUseCase = new ProcessAccessRequestUseCase();
	final Boolean removeVisitanteCamera = Utils.getPreferenceAsBoolean("removeVisitanteCameraSaida");
	
	public TopDataDevice(DeviceEntity deviceEntity){
		this(deviceEntity.getIdentifier(), deviceEntity.getConfigurationGroupsTO());
		this.deviceEntity = deviceEntity;
		this.name = deviceEntity.getName();
		this.location = deviceEntity.getLocation();
		this.desiredStatus = deviceEntity.getDesiredStatus();
		this.defaultDevice = deviceEntity.getDefaultDevice();
		this.athleteScreenConfig = deviceEntity.getAthleteScreenConfig();
		
		Gson gson = new GsonBuilder().create();
		List<AttachedTO> attachedDevices = gson.fromJson(deviceEntity.getAttachedDevices(), new TypeToken<List<AttachedTO>>() {}.getType());
		List<AttachedTO> attachedHikivisionCameras = gson.fromJson(deviceEntity.getAttachedHikivisionCameras(), new TypeToken<List<AttachedTO>>() {}.getType());

		this.setAttachedDevices(attachedDevices);
		this.setAttachedHikivisionCameras(attachedHikivisionCameras);
	}
	
	public TopDataDevice(String identifier){
		this(identifier, null);
	}
	
	public TopDataDevice(String identifier, List<ConfigurationGroupTO> configurationGroups){
		this.manufacturer = Manufacturer.TOP_DATA;
		this.identifier = identifier;
		String partes[] = identifier.split(";");
		this.innerNumber = Integer.valueOf(partes[0]);
		this.port = Integer.valueOf(partes[1]);
		if(partes.length > 2) {
			this.tipo = partes[2];
		}

		this.name = "TopData Catraca Inner " + innerNumber;
		if (configurationGroups != null) {
			this.configurationGroups = configurationGroups;
		} else {
			createDefaultConfiguration();
		}
		
		createConfigurationMap();

		this.inner = new Inner();
		configureInner();
	}
	
	@Override
	public void connect(String... args) throws Exception {
		easyInner = new EasyInner();
		int ret = 0;
		
		if(!portaAberta) {
			EasyInner.DefinirTipoConexao(2);
	        ret = EasyInner.AbrirPortaComunicacao(port);
	        EasyInner.LigarLedVerde(1);
	        if (ret != Enumeradores.RET_COMANDO_OK 
	        		&& ret != Enumeradores.RET_PORTA_JAABERTA) {
	        	throw new Exception("Erro ao abrir a porta de comunica��o: " + ret);
	        }
	        
	        portaAberta = true;
		}
        
		ret = Enumeradores.Limpar;
        Long inicio = System.currentTimeMillis();
        Long tempoDeEspera = getConfigurationValueAsLong(TEMPO_ESPERA_PARA_CONECTAR) * 1000;
        
        if(Objects.nonNull(args) && args.length > 0 && "NOT_WAIT_TIME".equals(args[0])) {
        	tempoDeEspera = 1000l;
        }

        
        boolean onlyEnabledMode = getConfigurationValueAsBoolean(ONLY_ENABLED_MODE);  	
       // !(this instanceof TopDataAcessoDevice)&& 
        if(
        		!(this instanceof TopDataExpedidoraDevice)
        		&& Boolean.TRUE.equals(onlyEnabledMode)) {
        	
        	disconnect();
        	
        	onlyEnabledPingWorker = getOnlyEnabledPingWorker();
        	
        	if(!isOnlyEnabledPingWorkerExecuting) {
        		isOnlyEnabledPingWorkerExecuting = true;
        		onlyEnabledPingWorker.execute();
        	}
        	
        	ret = pingOffline();

        	if (ret == Enumeradores.RET_COMANDO_OK) {
        		setStatus(DeviceStatus.ONLY_ENABLED);
        		
        		return;
        	
        	} else {
        		setStatus(DeviceStatus.DISCONNECTED);
        		throw new Exception("Catraca " + deviceEntity.getName() +  " n�o pode ser conectada");
        	}

        }
        
        while (ret != Enumeradores.RET_COMANDO_OK && (System.currentTimeMillis() - inicio) < tempoDeEspera) {
        	ret = testarConexaoInner(inner.Numero);
        	Utils.sleep(50);
        }
        
        if (ret != Enumeradores.RET_COMANDO_OK) {
        	throw new Exception("N�o foi poss�vel conectar na catraca " + deviceEntity.getName() + ", motivo: " + ret);
        }
        
        bsp = new NBioBSPJNI();
		export = bsp.new Export();
		
		nivelSeguranca = getConfigurationValueAsInteger(NIVEL_RECONHECIMENTO);
		if (nivelSeguranca == 0) {
			nivelSeguranca = 6;
		}
        
		startIndexSearch();
        sendConfiguration();
        
		setStatus(DeviceStatus.CONNECTED);
        watchDogEnabled = true;
		workerEnabled = true;
		
		worker = getDeviceProccessWorker();
		worker.execute();
		
        watchDog = getPingWorker();
		watchDog.execute();
	}
	
	private void startIndexSearch() {
		modeloLC = getConfigurationValueAsString(TIPO_BIOMETRICO).equals("lc");
		String modo = getConfigurationValueAsString(MODO_DE_TRABALHO);
		
		if("noServidor".equals(modo)) {
			if(modeloLC) {
				verificaCadastroNoInner(false, false, null);				
			} else {
				startIndexSearchEngine();				
			}
		}
	}
	
	private SwingWorker<Void, Void> getDeviceProccessWorker() {
		return new SwingWorker<Void, Void>() {

			@Override
			protected Void doInBackground() throws Exception {
				while (workerEnabled) {
					try {
						while (sendingConfiguration) {
							Utils.sleep(50);
						}
						
						if (DeviceMode.VERIFICATION.equals(mode)) {
							StringBuffer Cartao = new StringBuffer();
							Cartao.delete(0, Cartao.length());
							int[] iArrBCartaoRb = new int[8];
							
							int ret = -1;
							
							if (inner.TipoLeitor == Enumeradores.QRCODE 
											|| inner.TipoLeitor ==  Enumeradores.BARRAS_PROX_QRCODE) {
								ret = EasyInner.ReceberDadosOnLineComLetras(inner.Numero, iArrBCartaoRb, Cartao);

								if(iArrBCartaoRb[0] == 18) {
									Cartao = new StringBuffer("0000000000");
								}

								if(ret == Enumeradores.RET_COMANDO_OK && !Cartao.toString().contains("_")) {
									if((Cartao != null && Cartao.length() != 0 || iArrBCartaoRb[0] == 2 || iArrBCartaoRb[0] == 3)) {
										Cartao = new StringBuffer(Utils.toHEX(Cartao.toString().replaceAll("[^a-zA-Z0-9]+","")));
									}
								}
								
							} else {
								ret = EasyInner.ReceberDadosOnLine(inner.Numero, iArrBCartaoRb, Cartao);
							}
							
							if (ret == Enumeradores.RET_COMANDO_OK) {
								System.out.println("Origem: " + iArrBCartaoRb[0]);
								System.out.println(" o Pedestre/Visitante passou na catraca: " +inner.Numero);
								
								try {
									if(CARTAO_MASTER.equals(Cartao)) {
										System.out.printf("Habilitou o cart�o Master, com o n�mero: ", CARTAO_MASTER);
										EasyInner.DefinirNumeroCartaoMaster(CARTAO_MASTER + "");
										EasyInner.LiberarCatracaDoisSentidos(inner.Numero);
									}
									
									Main.validandoAcesso = true;
									if(Main.temServidor()) {
										HibernateAccessDataFacade.enviaInicioVerificandoAcesso();
									}
									
									inner.CountTentativasEnvioComando = 0;
									if(iArrBCartaoRb[0] == Enumeradores.GIRO_DA_CATRACA_TOPDATA) {
										registraGiro(iArrBCartaoRb[1], null);
										
										enviarMensagemPadrao();
										configurarEntradasOnline();
									
									} else if(iArrBCartaoRb[0] == Enumeradores.ORIGEM_URNA) {
										inner.BilheteInner.Origem = iArrBCartaoRb[0];
										allowAccess();
										
									} else if (iArrBCartaoRb[0] == Enumeradores.FIM_TEMPO_ACIONAMENTO
											|| iArrBCartaoRb[0] == Enumeradores.TECLA_FUNCAO
											|| iArrBCartaoRb[0] == Enumeradores.TECLA_ANULA
											|| ((Cartao.length() == 0)
													&& !(inner.EstadoTeclado == Enumeradores.EstadosTeclado.AGUARDANDO_TECLADO))) {
										enviarMensagemPadrao();
										configurarEntradasOnline();
	
									} else {
										inner.BilheteInner.Origem = iArrBCartaoRb[0];
										inner.BilheteInner.Complemento = iArrBCartaoRb[1];
										inner.BilheteInner.Cartao.setLength(0);
										inner.BilheteInner.Cartao = new StringBuilder(Cartao.toString());
										if (inner.BilheteInner.Origem != 13) {
											validarAcesso();
										}
									}
								
								} catch (Throwable e) {
									e.printStackTrace();
								} finally {
									Main.validandoAcesso = false;
									if(Main.temServidor()) {
										HibernateAccessDataFacade.enviaFimVerificandoAcesso();										
									}
								}
								
							}
							
						} else if (DeviceMode.ENROLLMENT.equals(mode)) {
							if(modeloLC) {
								templateTemp = new byte[502];
								
								//solicita template
								int ret = EasyInner.RequisitarReceberTemplateLeitorInnerBio(inner.Numero, 1);
								
								do {
									ret = EasyInner.RespostaReceberTemplateLeitorInnerBio(inner.Numero, new int[] {502});
									Thread.sleep(500);
								} while (ret != Enumeradores.RET_COMANDO_OK
										&& DeviceMode.ENROLLMENT.equals(mode));
								
								if(ret == Enumeradores.RET_COMANDO_OK) {
									Long inicio = System.currentTimeMillis();
									do {
										ret = EasyInner.ReceberTemplateLeitorInnerBio(inner.Numero,templateTemp, 502);
										Thread.sleep(500);
									} while (ret != Enumeradores.RET_COMANDO_OK 
											&& templateTemp != null
											&& (System.currentTimeMillis() - inicio) < 5000 
											&& DeviceMode.ENROLLMENT.equals(mode));
									
									if (ret == Enumeradores.RET_COMANDO_OK) {
										processSampleForEnrollmentLC(null);
									}
								}
								
							} else {
						
								templateTemp = new byte[404];
								int ret = EasyInner.SolicitarTemplateLeitor(inner.Numero);
								Long inicio = System.currentTimeMillis();
								
								do {
									ret = EasyInner.ReceberTemplateLeitor(inner.Numero, 1, templateTemp);
									Thread.sleep(50);
								
								} while (ret == Enumeradores.RET_BIO_PROCESSANDO 
											&& (System.currentTimeMillis() - inicio) < 5000 
												&& DeviceMode.ENROLLMENT.equals(mode));
								
								if (ret == Enumeradores.RET_BIO_OK) {
									processSampleForEnrollment(null);
								}
							}
						}
	                } catch (Exception e) {
	                    e.printStackTrace();
	                
	                } finally {
						Utils.sleep(50l);
					}
				}
				return null;
			}
			
		};
	}
	
	private SwingWorker<Void, Void> getPingWorker() {
		return new SwingWorker<Void, Void>() {
			@Override
			protected Void doInBackground() throws Exception {
				while (watchDogEnabled) {
					Long sleepTime = null;
					try {
						int ret = 0; 
						if (!busy) {
							ret = ping();
							
							if (ret == easyInner.RET_COMANDO_OK) {
								inner.TempoInicialPingOnLine = System.currentTimeMillis();
								if(!coletandoDadosOffLine) {
									setStatus(DeviceStatus.CONNECTED);
								}
								inner.CountRepeatPingOnline = 0;
							
							} else {
								System.out.println("a catraca caiu " + inner.Numero);
								setStatus(DeviceStatus.DISCONNECTED);
							}
						}

						sleepTime = getConfigurationValueAsLong(TEMPO_DE_PING) * 1000;
						
						if(DeviceStatus.DISCONNECTED.equals(getStatus())) {
							System.out.println(Main.sdf.format(new Date()) + " Catraca desconectada, resposta do ping: " + ret);
						}
						
						if(DeviceStatus.DISCONNECTED.equals(lastStatus)
								&& DeviceStatus.CONNECTED.equals(getStatus())) {
							System.out.println(Main.sdf.format(new Date()) + " Catraca reconectada, resposta do ping: " + ret);
							System.out.println(Main.sdf.format(new Date()) + " Lançado processo de reconexão");
							
//							new Thread() {
//								public void run() {
							try {
								System.out.println(Main.sdf.format(new Date()) + " Desconectando catraca, pode levar at� 5 segundos");
								disconnect();
								Utils.sleep(5000);
								System.out.println(Main.sdf.format(new Date()) + " Tentando reconectar catraca...");
								connect();
							} catch (Exception e) {
								System.out.println(Main.sdf.format(new Date()) + " Erro ao reconectar a catraca, veja abaixo: ");
								e.printStackTrace();
								if(watchDog != null && !watchDogEnabled) {
									System.out.println(Main.sdf.format(new Date()) + " Reiniciando processo de reconexão autom�tica... ");
									watchDogEnabled = true;
									watchDog.execute();
								}
							}
//								}
//							}.start();
							
						}
					} catch (Exception e) {
						e.printStackTrace();
						setStatus(DeviceStatus.DISCONNECTED);
					} finally {
						Utils.sleep(sleepTime != null ? sleepTime : 5000);
					}
				}
				return null;
			}
		};
	}
	
	private SwingWorker<Void, Void> getOnlyEnabledPingWorker() {
		return new SwingWorker<Void, Void>() {
			@Override
			protected Void doInBackground() throws Exception {
				while (isOnlyEnabledPingWorkerExecuting) {
					Long sleepTime = null;
					try {
						int ret = 0; 
						if (!busy) {
							ret = pingOffline();
							
							if (ret == easyInner.RET_COMANDO_OK) {
								inner.TempoInicialPingOnLine = System.currentTimeMillis();
								setStatus(DeviceStatus.ONLY_ENABLED);
								inner.CountRepeatPingOnline = 0;
							
							} else {
								System.out.println("a catraca caiu " + inner.Numero);
								setStatus(DeviceStatus.DISCONNECTED);
							}
						}

						sleepTime = getConfigurationValueAsLong(TEMPO_DE_PING) * 1000;
						
						if(DeviceStatus.DISCONNECTED.equals(getStatus())) {
							System.out.println(Main.sdf.format(new Date()) + " Catraca desconectada, resposta do ping: " + ret);
						}
						
					} catch (Exception e) {
						e.printStackTrace();
						setStatus(DeviceStatus.DISCONNECTED);
					} finally {
						Utils.sleep(sleepTime != null ? sleepTime : 5000);
					}
				}

				return null;
			}
		};
	}
	
	public void atualizaDigitaisLFD(boolean online, boolean todas, Date data) {
		if(!isSyncUsers()) {
			return;
		}
		
		if(!portaAberta) {
			EasyInner.DefinirTipoConexao(2);
	        int ret = EasyInner.AbrirPortaComunicacao(port);
	        if (ret != Enumeradores.RET_COMANDO_OK 
	        		&& ret != Enumeradores.RET_PORTA_JAABERTA) {
	        	System.out.println("Porta j� aberta");
	        }
	        portaAberta = true;
		}
		
		enviarDigitaisLFD(online, todas, data);
	}
	
	public boolean isDeviceRestrito() {
		return getConfigurationValueAsBoolean(IS_DEVICE_RESTRITO);
	}
	


	@SuppressWarnings("unchecked")
	private void enviarDigitaisLFD(boolean online, boolean todas, Date data) {
		System.out.println("Enviando digitais para a catraca LFD: " + inner.Numero + " Data: " + data);
		deviceCard.setMensagem("Sincronizando digitais...", MessageType.NORMAL);
		try {
			Date dataAlteracao = data != null ? data : deviceEntity.getUltimaAtualizacao();
		
			List<PedestrianAccessEntity> pedestres = null;
			if(!todas && dataAlteracao != null) {
				System.out.println("Sincroniza por data " + new SimpleDateFormat("dd/MM/yyyy HH:mm:ss").format(deviceEntity.getUltimaAtualizacao()));
				
				HashMap<String, Object> args = new HashMap<String, Object>();
				args.put("ULTIMA_SINC", dataAlteracao);
				pedestres = (List<PedestrianAccessEntity>) 
						HibernateAccessDataFacade.getResultListWithParams(PedestrianAccessEntity.class, "PedestrianAccessEntity.findAllPedestreAlterados", args);
				
			} else {
				System.out.println("Sincroniza total");
				pedestres = (List<PedestrianAccessEntity>) 
						HibernateAccessDataFacade.getResultList(PedestrianAccessEntity.class, "PedestrianAccessEntity.findAllPedestre");
			}
			
			System.out.println("Qtd encontradas: " + (Objects.isNull(pedestres) ? 0 : pedestres.size()));
			if(Objects.isNull(pedestres) || pedestres.isEmpty()) {
				System.out.println("Nenhuma altera��o em pedestres para envio.");
				return;
			}
			
			coletandoDadosOffLine = true;
			
			for (PedestrianAccessEntity pedestre : pedestres) {
				if(Objects.isNull(pedestre.getTemplates()) || pedestre.getTemplates().isEmpty()) {
					removeDigitalLFD(online, pedestre);
					continue;
				}

				boolean alteraTemplate = false;
				List<TemplateEntity> templates = pedestre.getTemplates();
				
				if(todas) {
					alteraTemplate = true;
				}
				
				if(!alteraTemplate 
						&& dataAlteracao != null
						&& templates.get(0).getDataCriacao().getTime() >= dataAlteracao.getTime()) {
					alteraTemplate = true;
				}
				
				if(!alteraTemplate 
						&& dataAlteracao != null
						&& templates.size() >= 2
						&& templates.get(1).getDataCriacao().getTime() >= dataAlteracao.getTime()) {
					alteraTemplate = true;
				}
				
				if(alteraTemplate) {
					System.out.println("Enviar " + pedestre.getName());
					TemplateEntity envio = new TemplateEntity();
					envio.setPedestrianAccess(pedestre);	
					envio.setTemplate(templates.get(0).getTemplate());
					
					if(templates.size() >= 2) {
						envio.setSample(templates.get(1).getTemplate());
					}
					manutencaoDigitalCatraca(online, envio);
				
				} else{
					System.out.println("Biometrias N�o alteradas " + pedestre.getName());
				}
			
			}
			System.out.println("Digitais sincronizadas.");
			
			deviceEntity.setUltimaAtualizacao(new Date());
			deviceEntity = (DeviceEntity) HibernateAccessDataFacade.save(DeviceEntity.class, deviceEntity)[0];
				
		} finally {
			coletandoDadosOffLine = false;
			if(isConnected()) {
				deviceCard.setMensagem("Conectado", MessageType.NORMAL);				
			} else {
				deviceCard.setMensagem(" ", MessageType.NORMAL);				
			}
		}
	}
	
	public void insereDigitalInner(boolean online, PedestrianAccessEntity pTemplate) {
		if (!isSyncUsers()) {
			return;
		}

		if (!portaAberta) {
			EasyInner.DefinirTipoConexao(2);
			int ret = EasyInner.AbrirPortaComunicacao(port);
			if (ret != Enumeradores.RET_COMANDO_OK && ret != Enumeradores.RET_PORTA_JAABERTA)
				System.out.println("Porta j� aberta");
			portaAberta = true;
		}

		PedestrianAccessEntity p = (PedestrianAccessEntity) HibernateAccessDataFacade
				.getSingleResultById(PedestrianAccessEntity.class, pTemplate.getId());
		if (p.getTemplates() != null && !p.getTemplates().isEmpty()) {
			List<TemplateEntity> templates = p.getTemplates();
			
			// ajusta ate duas digitais no mesmo template
			TemplateEntity envio = new TemplateEntity();
			envio.setPedestrianAccess(p);
			envio.setTemplate(templates.get(0).getTemplate());

			if (templates.size() >= 2) {
				envio.setSample(templates.get(1).getTemplate());
			}

			manutencaoDigitalCatraca(online, envio);
		} else {
			removeDigitalInner(online, p);
		}
	}

	private void manutencaoDigitalCatraca(boolean online, TemplateEntity t) {
		if(Boolean.TRUE.equals(t.getPedestrianAccess().getRemovido())) {
			removeDigitalLFD(online, t.getPedestrianAccess());
			return;
		}
		
		boolean excluidoSeExiste = removeDigitalLFD(online, t.getPedestrianAccess());
		
		if(!excluidoSeExiste) {
			System.out.println("usu�rio N�o foi removido !! id: " + t.getIdPedestreianAccess());
			return;
		}
		
		byte[] template = montaBytesCatraca(t);
		int ret = EasyInner.EnviarUsuarioBio(inner.Numero, template);
		
		Utils.sleep(50);
		
		if(ret == Enumeradores.RET_COMANDO_OK) {
			int tentativas = 0;
			do {
				ret = EasyInner.UsuarioFoiEnviado(inner.Numero, online ? 1 : 0);
				Utils.sleep(50);
				tentativas++;
			} while(ret != 0 && ret != 1 && tentativas <= 5);
			
			if(ret == Enumeradores.RET_COMANDO_OK || ret == 1) {
				System.out.println("Usu�rio enviado catraca ID: " + t.getPedestrianAccess().getId());				
			} else {
				System.out.println("Usu�rio N�o enviado catraca, motivo: " + ret);				
			}

		} else {
			System.out.println("Usu�rio N�o enviado catraca, motivo: " + ret);
		}
	}
	
	private byte[] montaBytesCatraca(TemplateEntity t) {
		byte[] template = new byte[844];
		//Bytes Descrição
		
		//0 Master (0 = N�o / 1 = sim) sempre utilizar 0
		template[0] = 0;
		
		//1 - 10 Número do usu�rio (em ASC)
		int p = 1;
		String id = StringUtils.leftPad(t.getPedestrianAccess().getId().toString(), 10, '0');
        for (int j = 0; j < id.length(); j++) {
            template[p] = (byte) (Long.parseLong(id.substring(j, j + 1)) + 48);
            p++;
        }
		
		//11 - 27 Reservado, preencher com 0
		for (int i = 0 ; i < 17 ; i++) {
			template[p] = 0;
			p++;
		}
		
		//28 - 431 Template(Digital) 1(tamanho 404 bytes)
		for (byte b : t.getTemplate()) {
			template[p] = b;
			p++;
		}
		
		//432 - 835 Template(Digital) 2 (tamanho 404 bytes)
		for (byte b : (t.getSample() != null 
				? t.getSample() : t.getTemplate())) {
			template[p] = b;
			p++;
		}
		
		//836 - 843 Data/hora do cadastro (tamanho 8 bytes)
		HashMap<String, Object> dataCad = dataCadastro();
		template[p] = (byte) Integer.parseInt(dataCad.get("yyyy").toString().substring(0, 2));p++;
		template[p] = (byte) ((int) dataCad.get("yyyy") % 100);p++;
		template[p] = (byte) (int) dataCad.get("MM");p++;
		template[p] = (byte) (int) dataCad.get("dd");p++;
		template[p] = (byte) (int) dataCad.get("HH");p++;
		template[p] = (byte) (int) dataCad.get("mm");p++;
		template[p] = (byte) (int) dataCad.get("ss");p++;
		template[p] = 0;
		
		return template;
	}
	
	/**
     * Monta um objeto com a data
     *
     * @return
     */
    private HashMap<String, Object> dataCadastro() {
        HashMap<String, Object> hashMapData = new HashMap<>();
        Date Data = new Date();
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy");
        hashMapData.put("yyyy", Integer.parseInt(formatter.format(Data)));

        formatter = new SimpleDateFormat("MM");
        hashMapData.put("MM", Integer.parseInt(formatter.format(Data)));

        formatter = new SimpleDateFormat("dd");
        hashMapData.put("dd", Integer.parseInt(formatter.format(Data)));

        formatter = new SimpleDateFormat("HH");
        hashMapData.put("HH", Integer.parseInt(formatter.format(Data)));

        formatter = new SimpleDateFormat("mm");
        hashMapData.put("mm", Integer.parseInt(formatter.format(Data)));

        formatter = new SimpleDateFormat("ss");
        hashMapData.put("ss", Integer.parseInt(formatter.format(Data)));

        return hashMapData;
    }
    
    public void removeDigitalInner(boolean online, PedestrianAccessEntity p) {
		if(isSyncUsers()) {
			
			if(!portaAberta) {
				EasyInner.DefinirTipoConexao(2);
		        int ret = EasyInner.AbrirPortaComunicacao(port);
		        if (ret != Enumeradores.RET_COMANDO_OK 
		        		&& ret != Enumeradores.RET_PORTA_JAABERTA) 
		        	System.out.println("Porta j� aberta");
		        portaAberta = true;
			}
			
			removeDigitalLFD(online, p);
		}
    }
    
	private boolean removeDigitalLFD(boolean online, PedestrianAccessEntity p) {
		if(Main.desenvolvimento) {
			System.out.println("Removendo digital existente ID: " + p.getId());
		}
		
		int ret = EasyInner.SolicitarExclusaoUsuario(inner.Numero, p.getId().toString());
		
		Utils.sleep(50);
		
		if(ret == Enumeradores.RET_COMANDO_OK) {
			int tentativas = 0;
			do {
				ret = EasyInner.UsuarioFoiExcluido(inner.Numero, online ? 1 : 0);
				Utils.sleep(50);
				tentativas++;
			} while(ret != 0 && ret != 1 &&  ret != 132 && tentativas <= 10);
			
			if(ret != Enumeradores.RET_COMANDO_OK
					&& ret != 1
					&& ret != Enumeradores.RET_BIO_USR_NAO_CADASTRADO) {
				System.out.println("N�o encontrado na catraca (verifica removido): " + ret);
				return false;
			}
			
			if(Main.desenvolvimento) {
				System.out.println("Removido");
			}

		} else {
			if(Main.desenvolvimento) {
				System.out.println("N�o encontrado na catraca (solicita removido): "  + ret);
			}
			return false;
		}
		
		return true;
	}

	
	protected void registraGiro(int sentido, Date data) {
		String query = "";

		HashMap<String, Object> args = new HashMap<String, Object>();
		args.put("EQUIPAMENTO", getFullIdentifier());

		if(Objects.nonNull(inner.BilheteInner.Cartao)) {
			final String cartaoStr = inner.BilheteInner.Cartao.toString();
			
			if(!cartaoStr.isEmpty()
					&& !"".equals(cartaoStr.replace("0",""))) {
				System.out.println("Registra giro com cart�o: " + cartaoStr);
				
				args.put("NUMERO_CARTAO_RECEBIDO", cartaoStr);
				query = "LogPedestrianAccessEntity.findByEquipamentSemDirectionAndComCartaoRecebido";
			
			} else if(cartaoStr.isEmpty()
					|| "".equals(cartaoStr.replace("0",""))) {
				query = "LogPedestrianAccessEntity.findByEquipamentSemDirectionAndSemCartaoRecebido";
			}
		
		} else {
			if(this.matchedFacialId != null) {
				String codigoFacial = this.matchedFacialId.toString();
				args.put("NUMERO_CARTAO_RECEBIDO", codigoFacial);
				query = "LogPedestrianAccessEntity.findByEquipamentSemDirectionAndComCartaoRecebido";
				
				this.matchedFacialId = null;
			} else {
				System.out.println("Registra giro da liberado manual");
				inner.BilheteInner.Cartao = new StringBuilder();
				return;
			}
		}
		
		inner.BilheteInner.Cartao = new StringBuilder();
		
		 LogPedestrianAccessEntity ultimoAcesso = (LogPedestrianAccessEntity) HibernateAccessDataFacade
		                        .getUniqueResultWithParams(LogPedestrianAccessEntity.class, query, args);
		
		
		if(ultimoAcesso == null) {
			return;
		}
		
		// sentido = 1 / horario
		// sentido = 0 / antihorario
		String direction = Tipo.ENTRADA;
		String sentidoCatraca = getConfigurationValue(SENTIDO_DA_CATRACA);
		
		boolean bloquearSaida = getConfigurationValueAsBoolean(BLOQUEAR_SAIDA);
		
		if(sentido == 1) {
			direction = "anticlockwise".equals(sentidoCatraca) ? Tipo.SAIDA : Tipo.ENTRADA;			
		} else if(sentido == 0) {
			direction = !"anticlockwise".equals(sentidoCatraca) ? Tipo.SAIDA : Tipo.ENTRADA ;
		}
		
		ultimoAcesso.setDirection(direction);
		ultimoAcesso.setStatus("ATIVO");
		ultimoAcesso.setBloquearSaida(bloquearSaida);
		
		if(data != null) {
			ultimoAcesso.setAccessDate(data);
			ultimoAcesso.setOffline(true);
		} else {
			ultimoAcesso.setAccessDate(new Date());
		}
		
		ultimoAcesso.setDataCriacao(new Date());
		
		HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, ultimoAcesso);
		
		PedestrianAccessEntity pedestre = (PedestrianAccessEntity) HibernateAccessDataFacade
				.getSingleResultById(PedestrianAccessEntity.class, ultimoAcesso.getIdPedestrian());
		
		if(pedestre == null) {
			pedestre = (PedestrianAccessEntity) HibernateAccessDataFacade
					.getSingleResultByIdTemp(PedestrianAccessEntity.class, ultimoAcesso.getIdPedestrian());			
		}

		if(pedestre == null) {
			return;
		}
		
		if(Objects.nonNull(pedestre.getDataCadastroFotoNaHikivision())) {
			pedestre.setLastAccessHikiVision(new Date());
		}
		
		boolean ignoraRegras = getConfigurationValueAsBoolean(IGNORAR_REGRAS_DE_ACESSO);
		if(!ignoraRegras) {
			if(pedestre.temMensagens()) {
				pedestre.decrementaMensagens();
			}
			
			if(ultimoAcesso.isSaida() || !bloquearSaida) {
				 pedestre.decrementaCreditos();
			}
			
			if(!pedestre.temCreditos() && pedestre.isVisitante()) {
				if(Utils.isHikivisionConfigValid() 
						&& Objects.nonNull(pedestre.getFoto()) 
						&& Objects.nonNull(pedestre.getDataCadastroFotoNaHikivision())) {					
					if(removeVisitanteCamera) {						
						hikivisionUseCases.removerUsuarioFromDevices(pedestre);
					}

				} else {
					pedestre.apagarCartao();				
				}
				
			}
			
			if(pedestre.isQrCodeUsoDinamico()) {
				pedestre.decrementaQRCodeUso();
			}

			HibernateAccessDataFacade.save(PedestrianAccessEntity.class, pedestre);
		}

		if(ultimoAcesso.isSaida()) {
			enviaSmsDeRegistroUseCase.execute(pedestre);
		}
		
		System.out.println("Registrou giro no equipamento: " + inner.Numero);
	}
	
	@Override
	public void sendConfiguration() throws Exception {
		sendingConfiguration = true;
		configureInner();
		
		if(modeloLC) {
			System.out.println("Configura para envio do template");
			//easyInner.ConfigurarBio(inner.Numero, 1, 1);
			//easyInner.SetarBioVariavel(1);
		} else {
			EasyInner.ConfigurarComportamentoIndexSearch("naCatraca".equals(getConfigurationValue(MODO_DE_TRABALHO)) ? 0 : 1); // 0=match na catraca  1=match no computador
		}
		
//		enviarConfiguracoesOffline();
		enviarMensagensOffline();
		enviarConfiguracaoMudancaOnlineOffline();
		
		if(Boolean.TRUE.equals(getConfigurationValueAsBoolean(COLETA_CARTOES_OFFLINE))) {
			coletarBilhetesOffLine();			
		}
		
		if(isSyncUsers()) {
			enviarDigitaisLFD(false, false, null);
		}
		
		enviarConfiguracoesOnline();
		enviarDataHora();
		enviarMensagemPadrao();
		configurarEntradasOnline();
		limparInner();
		sendingConfiguration = false;
	}
	
	@Override
	public boolean isSyncUsers() {
		String modo = getConfigurationValueAsString(MODO_DE_TRABALHO);
		Boolean enviaDigitais = getConfigurationValueAsBoolean(ENVIA_DIGITAIS_PARA_CATRACA);

		if("naCatraca".equals(modo) && Boolean.TRUE.equals(enviaDigitais)) {
			return true;
		}
			
		return false;
	}
	
	
	protected void coletarBilhetesOffLine() {
        try {
            final int[] bilhetes = { 0 };
            int Ret = EasyInner.ReceberQuantidadeBilhetes(this.inner.Numero, bilhetes);
            if (Ret == 0) {
                if (bilhetes != null && bilhetes[0] > 0) {
                	coletandoDadosOffLine = true;
                	
                	deviceCard.setMensagem("Recuperando dados...", MessageType.NORMAL);
                    System.out.println("---- " + bilhetes[0] + " passaram com catraca desconectada.");
                    int qtdPermitido = 0;
                    int qtdNaoPermitido = 0;
                    for (int i = 0; i < bilhetes[0]; ++i) {
                        try {
                            Thread.sleep(500L);
                            final StringBuffer valores = new StringBuffer();
                            final int[] bilhete = new int[8];
                            Ret = EasyInner.ColetarBilhete(this.inner.Numero, bilhete, valores);
                            if (Ret == 0) {
                                System.out.println(valores.toString());
                                if (!"".equals(valores.toString())) {
                                    final Calendar c = Calendar.getInstance();
                                    c.set(Calendar.DAY_OF_MONTH, bilhete[1]);
                                    c.set(Calendar.MONTH, bilhete[2]-1);
                                    c.set(Calendar.YEAR, 2000 + bilhete[3]);
                                    c.set(Calendar.HOUR_OF_DAY, bilhete[4]);
                                    c.set(Calendar.MINUTE, bilhete[5]);
                                    c.set(Calendar.SECOND, bilhete[6]);
                                    
                                    processAccessRequest(valores.toString().trim(), c.getTime());
                                    if (VerificationResult.ALLOWED.equals(this.verificationResult)) {
                                    	//prepara dados de origem
                                    	inner.BilheteInner.Cartao = new StringBuilder(valores.toString().trim());
                                    	int sentido = 0;
                                    	if(this instanceof TopDataExpedidoraDevice
                                    			&& this.tipo.equals(Tipo.ENTRADA)) {
                                    		//para expedidora tratar
                                    		 sentido = bilhete[0];
                                    	} else {
	                                		 if(bilhete[0] == 10 || bilhete[0] == 11) {
	                                         	//cart�o
	                                        	sentido = bilhete[0] == 10 ? 0 : 1;
	                                         	inner.BilheteInner.Origem = 2;
	                                         } else {
	                                         	//teclado
	                                         	sentido = bilhete[0] == 110 ? 0 : 1;
	                                         	inner.BilheteInner.Origem = 1;
	                                         }
                                    	}
                                        
                                        System.out.println( "Data: " + new SimpleDateFormat("dd/MM/yyyy HH:mm:ss").format(c.getTime()));
                                        
                                    	registraGiro(sentido, c.getTime());
                                        ++qtdPermitido;
                                    } else {
                                    	System.out.println("N�o permitido!");
                                        ++qtdNaoPermitido;
                                    }
                                
                                } else{
                                	System.out.println("cart�o vazio!");
                                }
                            } else {
                                System.out.println("N�o foi poss�vel coletar os bilhetes na catraca: " + Ret);
                            }
                        } catch (Throwable e) {
                            e.printStackTrace();
                        }
                    }
                    if (qtdPermitido > 0 || qtdNaoPermitido > 0) {
                        Utils.createNotification("Dados recuperados da catraca: " + (qtdNaoPermitido + qtdPermitido), NotificationType.GOOD);
                    }
                }
            }
        
        } catch (Exception ex) {
            ex.printStackTrace();
            
            if(this instanceof TopDataExpedidoraDevice) {
            	//N�o mexe aqui
            } else {
            	this.inner.EstadoAtual = Enumeradores.EstadosInner.ESTADO_CONECTAR;
            }
        } finally {
        	coletandoDadosOffLine = false;
		}
    }
	
	@Override
	public void disconnect(String... args) throws Exception {
		super.disconnect();
		
		if(onlyEnabledPingWorker != null) {
			onlyEnabledPingWorker.cancel(true);
		}
		
		isOnlyEnabledPingWorkerExecuting = false;

		if (easyInner != null) {
			encerrarConexao(args != null && args.length > 0 && "SAIR".equals(args[0]));
		}

		if (indexSearchEngine != null)  {
			indexSearchEngine.dispose();
			indexSearchEngine = null;
        }

		enviaCartaoCatracaOffline();

		setStatus(DeviceStatus.DISCONNECTED);
	}
	
	protected void procuraSeExisteMensagemParaPedestre() {
		if(matchedAthleteAccess != null && matchedAthleteAccess.getMensagens() != null
				&& !matchedAthleteAccess.getMensagens().isEmpty()) {
			
			for(PedestrianMessagesEntity m : matchedAthleteAccess.getMensagens()) {
				if(m.getQuantidade() > 0) {
					messagePersonalizedInDevice = m.getMensagem();
					if(messagePersonalizedInDevice.length() > 16) {
						messagePersonalizedInDevice = messagePersonalizedInDevice.substring(0,16) 
								+ ";" + messagePersonalizedInDevice.substring(16, messagePersonalizedInDevice.length());
					}
					break;
				}
			}
		}
		
		if(messagePersonalizedInDevice != null && !messagePersonalizedInDevice.isEmpty()) {
			mensagemPermitido = formatMessage(messagePersonalizedInDevice);
		}
	}
	
	private void definiMensagemExibidaNoDisplay() {
		procuraSeExisteMensagemParaPedestre();
		
		if (VerificationResult.ALLOWED.equals(verificationResult) 
						&& (messagePersonalizedInDevice == null 
									|| messagePersonalizedInDevice.isEmpty())) {
			
			mensagemPermitido = formatMessage(verificationResult.getMessage() + ";" + allowedUserName);
			
		} else if (VerificationResult.TOLERANCE_PERIOD.equals(verificationResult))
			mensagemPermitido = formatMessage(verificationResult.getMessage());
	}
	
	@Override
	public void allowAccess() {
		try {
			definiMensagemExibidaNoDisplay();
			String sentidoCatraca = getConfigurationValue(SENTIDO_DA_CATRACA);
			boolean bloquearSaida = getConfigurationValueAsBoolean(BLOQUEAR_SAIDA);
			int ret = 0;

			ret = decideLadoEntrada(sentidoCatraca, bloquearSaida);
			
			EasyInner.EnviarMensagemPadraoOnLine(inner.Numero, 0, mensagemPermitido);
			
			int countTentativasEnvioComando = 0;
			while (ret != Enumeradores.RET_COMANDO_OK && countTentativasEnvioComando < 3) {
				Utils.sleep(tempoEspera);
				ret = decideLadoEntrada(sentidoCatraca, bloquearSaida);
				countTentativasEnvioComando++;
			}
			
			if (ret != Enumeradores.RET_COMANDO_OK) {
				Main.mainScreen.addEvento(name + ": N�o foi poss�vel enviar liberar a catraca");
				setStatus(DeviceStatus.DISCONNECTED);
			}
			
		} catch (Exception e) {
			e.printStackTrace();
		
		} finally{
			allowedUserName = "";
			messagePersonalizedInDevice = "";
			matchedAthleteAccess = null;
			mensagemPermitido = null;
		}
	}

	private int decideLadoEntrada(String sentidoCatraca, boolean bloquearSaida) {
		int ret = 0;
		final Boolean doisSentidosLiberado = Utils.getPreferenceAsBoolean("doisSentidos");
		final String entrar = Utils.getPreference("messageEntryAllowed");
		final String sair = Utils.getPreference("messageExitAllowed");

		final String espacoEntrar = entrar.length() < 14 ? " " : "";
		final String espacoSair = sair.length() < 14 ? " " : "";
		
		if(bloquearSaida && matchedAthleteAccess != null) {
			if(inner.BilheteInner.Origem == Enumeradores.ORIGEM_URNA || inner.BilheteInner.Origem == Origens.ORIGEM_LEITOR_2) {
							
				if(doisSentidosLiberado) {
					EasyInner.LiberarCatracaDoisSentidos(inner.Numero);
					EasyInner.AcionarBipCurto(inner.Numero);
				
				} else {
					ret = !"anticlockwise".equals(sentidoCatraca) 
							? EasyInner.LiberarCatracaEntrada(inner.Numero) 
							: EasyInner.LiberarCatracaEntradaInvertida(inner.Numero);			
					
				}
		

				if(messagePersonalizedInDevice == null || messagePersonalizedInDevice.isEmpty()) {
					mensagemPermitido = "anticlockwise".equals(sentidoCatraca) 
											? formatMessage(sair + espacoSair + "->" + ";" /*+ allowedUserName*/)
											: formatMessage("<-" + espacoSair + sair + ";" /*+ allowedUserName*/);
				}
				
				return ret;
			}

			LogPedestrianAccessEntity lastAccess = logPedestrianAccessRepository.buscaUltimoAcesso(matchedAthleteAccess.getId(),
					matchedAthleteAccess.getQtdAcessoAntesSinc());
			if(lastAccess == null || Tipo.SAIDA.equals(lastAccess.getDirection()) || lastAccess.getDirection() == null) {
				
				if(doisSentidosLiberado) {
					EasyInner.LiberarCatracaDoisSentidos(inner.Numero);
					EasyInner.AcionarBipCurto(inner.Numero);	
				} else {
					ret = "anticlockwise".equals(sentidoCatraca) 
							? EasyInner.LiberarCatracaEntrada(inner.Numero) 
							: EasyInner.LiberarCatracaEntradaInvertida(inner.Numero);	
				}
		
				
				if(messagePersonalizedInDevice == null || messagePersonalizedInDevice.isEmpty()) {
					mensagemPermitido = defineMensagemPermitido(sentidoCatraca, espacoEntrar, entrar);					
				}

			} else {
				if(doisSentidosLiberado) {
					EasyInner.LiberarCatracaDoisSentidos(inner.Numero);
					EasyInner.AcionarBipCurto(inner.Numero);
				
				} else {
					ret = !"anticlockwise".equals(sentidoCatraca) 
							? EasyInner.LiberarCatracaEntrada(inner.Numero) 
							: EasyInner.LiberarCatracaEntradaInvertida(inner.Numero);
				}
		
				
				if(messagePersonalizedInDevice == null || messagePersonalizedInDevice.isEmpty()) {
					mensagemPermitido = "anticlockwise".equals(sentidoCatraca) 
											? formatMessage(sair + espacoSair + "->" + ";" /*+ allowedUserName*/)
											: formatMessage("<-" + espacoSair + sair + ";" /*+ allowedUserName*/);
				}
			}
			
		} else {
			if(ReleaseAccessUseCase.getApertouF10()) {
				mensagemPermitido = !"anticlockwise".equals(sentidoCatraca) 
						? formatMessage("<-" + espacoEntrar + sair + ";" + "") 
						: formatMessage(sair + espacoEntrar + "->" + ";" + "");
				

				if(doisSentidosLiberado) {	
					EasyInner.AcionarBipCurto(inner.Numero);
					return EasyInner.LiberarCatracaDoisSentidos(inner.Numero);		
				
				} else {
					return !"anticlockwise".equals(sentidoCatraca) 
							? EasyInner.LiberarCatracaEntrada(inner.Numero) 
							: EasyInner.LiberarCatracaEntradaInvertida(inner.Numero);
				}
		
			}

			if(messagePersonalizedInDevice == null || messagePersonalizedInDevice.isEmpty()) {
				mensagemPermitido = "anticlockwise".equals(sentidoCatraca) 
										? formatMessage("<-" + espacoEntrar + entrar + ";" + "") 
										: formatMessage(entrar + espacoEntrar + "->" + ";" + "");
			}

			if(doisSentidosLiberado) {
				EasyInner.LiberarCatracaDoisSentidos(inner.Numero);
				EasyInner.AcionarBipCurto(inner.Numero);
			
			} else {
				ret = "anticlockwise".equals(sentidoCatraca) 
						? EasyInner.LiberarCatracaEntrada(inner.Numero) 
						: EasyInner.LiberarCatracaEntradaInvertida(inner.Numero);
			}
	
		}
		
		return ret;
	}
	
	@Override
	public void denyAccess() {
		try {
			int countTentativasEnvioComando = 0;
			String mensagemAcessoNegado = formatMessage(verificationResult.getMessage());

			int ret = EasyInner.EnviarMensagemPadraoOnLine(inner.Numero, 0, mensagemAcessoNegado);

			while (ret != Enumeradores.RET_COMANDO_OK && countTentativasEnvioComando < 3) {
				Utils.sleep(tempoEspera);
				ret = EasyInner.EnviarMensagemPadraoOnLine(inner.Numero, 0, mensagemAcessoNegado);
				countTentativasEnvioComando++;
			}

			if (ret == Enumeradores.RET_COMANDO_OK) {
				EasyInner.AcionarBipLongo(inner.Numero);
				EasyInner.LigarLedVermelho(inner.Numero);
				
				Long tempoAguardo = getConfigurationValueAsLong(TEMPO_DE_MENSAGEM_NEGADO);
				if(tempoAguardo != null && tempoAguardo > 0) {
					Utils.sleep(tempoAguardo * 1000);					
				} else {
					Utils.sleep(5000);
				}
				
				EasyInner.DesligarLedVermelho(inner.Numero);
				enviarMensagemPadrao();
				configurarEntradasOnline();

			} else {
				Main.mainScreen.addEvento(name + ": N�o foi poss�vel enviar mensagem acesso negado");
				setStatus(DeviceStatus.DISCONNECTED);
				throw new Exception(name + ": N�o foi poss�vel enviar mensagem acesso negado");
			}
		} catch (Exception ex) {
			ex.printStackTrace();

		} finally {
			allowedUserName = "";
		}
	}
	
	public void enviaCartaoCatracaOffline() throws Exception {
		if (!Utils.getPreferenceAsBoolean("enableOfflineCard")) {
			EasyInner.DefinirTipoListaAcesso(2);
			EasyInner.DefinirPadraoCartao(1);
			EasyInner.DefinirQuantidadeDigitosCartao(8);
			EasyInner.InserirUsuarioListaAcesso("0", 102);
			return;
		}
		EasyInner.DefinirTipoListaAcesso(1);
		EasyInner.DefinirPadraoCartao(1);
		EasyInner.DefinirQuantidadeDigitosCartao(8);
		
		List<PedestrianAccessEntity> pedestresComCartao = HibernateAccessDataFacade.buscaPedestresAtivosComCartao();
		
		if(Objects.isNull(pedestresComCartao) || pedestresComCartao.isEmpty()) {
			return;
		}
				
		for (PedestrianAccessEntity pedestre : pedestresComCartao) {
			String temp = "";
			for (int i = 0; i< pedestre.getCardNumber().length(); i++) {
				if (!pedestre.getCardNumber().substring(i, i + 1).equals("0")) {
					temp += pedestre.getCardNumber().substring(i, pedestre.getCardNumber().length());
					break;
				}
			}
			EasyInner.InserirUsuarioListaAcesso(temp, 101);
			System.out.println("quem est� sendo enviado   " + pedestre.getName());
			EasyInner.InserirUsuarioListaAcesso(pedestre.getId()+"", 101);
			System.out.println("qual cart�o   " + temp);
		}
		
		List<PedestrianAccessEntity> biometriasNaoRemovidas = HibernateAccessDataFacade.buscaPedestresAtivosComBiometria();
		for (PedestrianAccessEntity biometria : biometriasNaoRemovidas) {
			System.out.println("qual usu�rio est� sendo enviado a biometria  " + biometria.getName());
			EasyInner.InserirUsuarioListaAcesso(biometria.getId()+"", 101);
		}	
		
		EasyInner.EnviarListaAcesso(inner.Numero);
	}

	private void processSampleForEnrollmentLC(Object obj) {
		if(templates == null) {
			templates = new ArrayList<byte[]>();			
		}
		templates.add(templateTemp);
		
		if(samplesCollected >= 2) {
			
			int ret = insereUserLC(idCadastro, templates.get(0), templates.get(1));
			if (ret == Enumeradores.RET_COMANDO_OK) {
				EasyInner.EnviarMensagemPadraoOnLine(inner.Numero, 0, formatMessage(Utils.getPreference("messageEnrollmentFinished")));
				template = ArrayUtils.addAll(templates.get(0), templates.get(1));
				System.out.println(sdf.format(new Date()) + "  Template size: " + template.length);
				Utils.sleep(1000);
				samplesCollected = 0;
				setMode(DeviceMode.VERIFICATION);
				coletaCompleta = true;
				idCadastro = null;
			}
			return;
	
		} else {
			EasyInner.EnviarMensagemPadraoOnLine(inner.Numero, 0, formatMessage((Utils.getPreference("messageEnrollment") + " " 
	    			+ String.valueOf((samplesCollected + 1)) + "/2")));
		}
		
		samplesCollected++;
	}

	public int insereUserLC(Long idCadastro, byte [] t1, byte [] t2) {
		int ret;
		ret = EasyInner.EnviarDigitalUsuarioBio(inner.Numero, 1, idCadastro+"", t1, t2 != null ? t2 : null);
		
		Utils.sleep(1000);
		
		Long inicio = System.currentTimeMillis();
		do {
			ret = EasyInner.RespostaEnviarDigitalUsuarioBio(inner.Numero);
			Utils.sleep(50);
		} while (ret != Enumeradores.RET_COMANDO_OK 
				&& (System.currentTimeMillis() - inicio) < 5000 
				&& DeviceMode.ENROLLMENT.equals(mode));
		return ret;
	}
	
	@Override
	public void processSampleForEnrollment(Object obj) {
		try {
			int exportType = NBioBSPJNI.EXPORT_MINCONV_TYPE.FIM01_HV;
			
	    	if (samplesCollected == 0) {
	    		storedFIRHandle = bsp.new FIR_HANDLE();
	            storedInputFIR = bsp.new INPUT_FIR();
	            int ret = export.ImportFIR(templateTemp, 404, exportType, NBioBSPJNI.FIR_PURPOSE.ENROLL, storedFIRHandle);
	            if (ret != Enumeradores.RET_COMANDO_OK) {
	            	Utils.createNotification("Ocorreu um erro ao processar amostra. Retorno: " + ret, NotificationType.BAD);
	            	return;
	            } 
        		storedInputFIR.SetFIRHandle(storedFIRHandle);
        		samplesCollected++;
	    	
	    	} else {
	            NBioBSPJNI.FIR_HANDLE capturedFIRHandle = bsp.new FIR_HANDLE();
	            NBioBSPJNI.INPUT_FIR capturedInputFIR = bsp.new INPUT_FIR();
	            int ret = export.ImportFIR(templateTemp, 404, exportType, NBioBSPJNI.FIR_PURPOSE. ENROLL, capturedFIRHandle);
	            if (ret != Enumeradores.RET_COMANDO_OK) {
	            	Utils.createNotification("Ocorreu um erro ao processar amostra. Retorno: " + ret, NotificationType.BAD);
        			return;
	            }
                capturedInputFIR.SetFIRHandle(capturedFIRHandle);
        		ret = bsp. CreateTemplate(capturedInputFIR, storedInputFIR, storedFIRHandle, null);
        		if (ret != Enumeradores.RET_COMANDO_OK) {
        			Utils.createNotification("Ocorreu um erro ao criar o template. Retorno: " + ret, NotificationType.BAD);
        			return;
        		}
        		storedInputFIR.SetFIRHandle(storedFIRHandle);
    			samplesCollected++;
	    	}
	    	
	    	if (samplesCollected < 2) {
	    		EasyInner.EnviarMensagemPadraoOnLine(inner.Numero, 0, formatMessage((Utils.getPreference("messageEnrollment") + " " 
	    			+ String.valueOf((samplesCollected + 1)) + "/2")));
	    	
	    	} else {
	    		EasyInner.EnviarMensagemPadraoOnLine(inner.Numero, 0, formatMessage(Utils.getPreference("messageEnrollmentFinished")));
				NBioBSPJNI.Export.DATA exportData = export.new DATA();
				export.ExportFIR(storedInputFIR, exportData, exportType);
				template = exportData.FingerData[0].Template[0].Data;
				System.out.println(sdf.format(new Date()) + "  Template size: " + template.length);
				Utils.sleep(1000);
				setMode(DeviceMode.VERIFICATION);
				coletaCompleta = true;
			}
    	} catch (Exception e){
    		e.printStackTrace();
    		Utils.createNotification("Ocorreu um erro ao processar amostra: " + e.getMessage(), NotificationType.BAD);
    	}
	}
	
	@Override
	public void processAccessRequest(Object obj) {
		try {
			Object[] retorno = processAccessRequestUseCase.processAccessRequest((String) obj, getFullIdentifier(), 
					inner.BilheteInner.Origem, location, getConfigurationValueAsBoolean(LOGICA_DE_CATRACA_COM_URNA), true, 
					getConfigurationValueAsBoolean(IGNORAR_REGRAS_DE_ACESSO));
			verificationResult = (VerificationResult) retorno[0];
			allowedUserName = (String) retorno[1];
			matchedAthleteAccess = (PedestrianAccessEntity) retorno[2];
		
		} catch (Exception e) {
			e.printStackTrace();
			verificationResult = VerificationResult.ERROR;
		}
	}
	
	public void processAccessRequest(Object obj, final Boolean usaUrna) {
		try {
			Object[] retorno = processAccessRequestUseCase.processAccessRequest((String) obj, getFullIdentifier(), 
					inner.BilheteInner.Origem, location, usaUrna, true, 
					getConfigurationValueAsBoolean(IGNORAR_REGRAS_DE_ACESSO));
			verificationResult = (VerificationResult) retorno[0];
			allowedUserName = (String) retorno[1];
			matchedAthleteAccess = (PedestrianAccessEntity) retorno[2];
		
		} catch (Exception e) {
			e.printStackTrace();
			verificationResult = VerificationResult.ERROR;
		}
	}
	
	public void processAccessRequest(Object obj, Date data) {
		try {
			Object[] retorno = processAccessRequestUseCase.processAccessRequest((String) obj, getFullIdentifier(), 
					inner.BilheteInner.Origem, location, getConfigurationValueAsBoolean(LOGICA_DE_CATRACA_COM_URNA), false, data,
					isRegistrationProcessStartedOnDevice(), getConfigurationValueAsBoolean(IGNORAR_REGRAS_DE_ACESSO));
			
			verificationResult = (VerificationResult) retorno[0];
			allowedUserName = (String) retorno[1];
			matchedAthleteAccess = (PedestrianAccessEntity) retorno[2];
		
		} catch (Exception e) {
			e.printStackTrace();
			verificationResult = VerificationResult.ERROR;
		}
	}
	
	@Override
	public Set<Integer> getRegisteredUserList() throws Exception {
		Set<Integer> retorno = new HashSet<Integer>();
		Long inicio = System.currentTimeMillis();
		EasyInner.InicializarColetaListaUsuariosBio();
		while (EasyInner.TemProximoPacote() != Enumeradores.Opcao_NAO) {
			int ret = EasyInner.SolicitarListaUsuariosBio(inner.Numero);
			if (ret != Enumeradores.RET_COMANDO_OK)
				throw new Exception();
			Utils.sleep(500);
			ret = EasyInner.ReceberPacoteListaUsuariosBio(inner.Numero);
			Utils.sleep(500);
			while (EasyInner.TemProximoUsuario() != Enumeradores.Opcao_NAO) {
				StringBuffer usuario = new StringBuffer();
				ret = EasyInner.ReceberUsuarioLista(inner.Numero, usuario);
				System.out.println(sdf.format(new Date()) + "  Recebeu usuario: " + usuario.toString());
				if (ret == Enumeradores.RET_COMANDO_OK) {
					Integer idUsuario = Integer.valueOf(usuario.toString());
					retorno.add(idUsuario);
				}
			}
		}
		System.out.println(sdf.format(new Date()) + "  TEMPO PARA RECEBER LISTA DE USUARIOS: " + (System.currentTimeMillis() - inicio) + " ms");
		return retorno;
	}
	
	/*
	private String receberUsuarioDaCatraca(Long idUsuario) {
		// TODO 
		int ret = EasyInner.SolicitarUsuarioCadastradoBio(inner.Numero, "0000000028");
		if (ret != Enumeradores.RET_COMANDO_OK) 
			return "Erro solicitacao " + ret;
		
		System.out.println(sdf.format(new Date()) + "  Solicitado!");
		Utils.sleep(500);
		
		Long inicio = System.currentTimeMillis();
		byte[] template = new byte[844];
		do {
			ret = EasyInner.ReceberUsuarioCadastradoBio(inner.Numero, 1, template);
			Utils.sleep(200);
		}
		while (ret != Enumeradores.RET_COMANDO_OK && (System.currentTimeMillis()-inicio) < 20000); 
		
		System.out.println(sdf.format(new Date()) + "  Sucesso!");
		
		return "";
	}
	*/
	
	/**
	 * Método usado somente quando as digitais sao armazenadas no servidor. 
	 * Realiza a coleta da digital para criar o template
	 */
	@Override
	public String cadastrateUser(PedestrianAccessEntity athleteAccessEntity) {
		System.out.println(sdf.format(new Date()) + "   Iniciando coleta de digital na catraca...");
		this.coletaCompleta = false;
		this.samplesCollected = 0;
		this.idCadastro = athleteAccessEntity.getId();
		
		// altera o modo da catraca e tenta enviar as novas configuracoes
		String retorno = setMode(DeviceMode.ENROLLMENT);
		if (!"".equals(retorno))
			return retorno;
		
		System.out.println(sdf.format(new Date()) + "   Aguardando coleta...");
		while(!coletaCompleta && !cancelAction)
			Utils.sleep(50);
		
		if (cancelAction) {
			System.out.println(sdf.format(new Date()) + "   Coleta cancelada pelo usuario.");
			EasyInner.EnviarMensagemPadraoOnLine(inner.Numero, 0, formatMessage(Utils.getPreference("messageEnrollmentCancelled")));
			Utils.sleep(1000);
			setMode(DeviceMode.VERIFICATION);
			templates = null;
			return "Coleta cancelada.";
		}
		System.out.println(sdf.format(new Date()) + "   Coleta completa!");
		
		// Cria e salva uma coleta para ser enviada para o servidor
		System.out.println(sdf.format(new Date()) + "   Salvando biometric entity...");
		BiometricEntity biometry = new BiometricEntity();
		biometry.setUser(athleteAccessEntity.getId());
		biometry.setUserName(athleteAccessEntity.getName());
		biometry.setFinger(Finger.RIGHT_INDEX);
		if(modeloLC && !templates.isEmpty()) {
			//usa dois campos de template
			biometry.setTemplate(templates.get(0));
			biometry.setSample(templates.get(1));
//			biometry.setSample(new byte[10]);
		
		} else {
			biometry.setTemplate(template);
		}
		HibernateAccessDataFacade.save(BiometricEntity.class, biometry);
		
		// salva o template
		System.out.println(sdf.format(new Date()) + "   Salvando template entity...");
		TemplateEntity templateEntity = new TemplateEntity();
		templateEntity.setPedestrianAccess(athleteAccessEntity);
		if(modeloLC && !templates.isEmpty()) {
			//usa dois campos de template
			templateEntity.setTemplate(templates.get(0));
			templateEntity.setSample(templates.get(1));
//			templateEntity.setSample(new byte[10]);
		
		} else {
			templateEntity.setTemplate(template);
		}
		templateEntity.setLocal(true);
		templateEntity.setManufacturer(modeloLC ? Manufacturer.LC_DEVICE : Manufacturer.NITGEN);
		templateEntity = (TemplateEntity) HibernateAccessDataFacade.save(TemplateEntity.class, templateEntity)[0];
		
		// envia o template via broadcast
		TemplateEntity temp = new TemplateEntity(templateEntity);
		if (Main.broadcastServer != null) {
			Main.broadcastServer.sendMessage(new BroadcastMessageTO(BroadcastMessageType.NEW_TEMPLATE, temp));
		}
		
		if(modeloLC) {
			//realiza cadastro nas outras catracas
			System.out.println(sdf.format(new Date()) + "   Adicionando template nas outras catracas...");
			for(Device d : Main.devicesList) {
				if(d != null 
						&& d instanceof TopDataDevice
						&& d.identifier != this.identifier) {
					//verifica se pode adicionar
					TopDataDevice topData = (TopDataDevice)d;
					if(topData.modeloLC)
						topData.insereUserLC(idCadastro, templates.get(0), templates.get(1));
				}
			}
			
			templates = null;
			
		} else {
			// adiciona no indexSearch
			System.out.println(sdf.format(new Date()) + "   Adicionando template ao indexSearch...");
			addTemplateToIndexSearch(templateEntity);
			
			//adicionar também nas outras catracas conectadas
			for(Device d : Main.devicesList) {
				if(d != null && d instanceof TopDataDevice) {
					//verifica se pode adicionar
					TopDataDevice topData = (TopDataDevice)d;
					if(this.innerNumber != topData.getInnerNumber() 
							&& topData.getIndexSearchEngine() != null) {
						System.out.println(sdf.format(new Date()) + 
								"   Enviando template para indexSearch do Inner "+topData.getInnerNumber()+"...");
						topData.addTemplateToIndexSearch(templateEntity);
					}
				}
			}
		}
		
		System.out.println(sdf.format(new Date()) + "   Retornando...");
		return "";
	}
	
	/**
	 * Método usado somente quando as digitais sao armazenadas no servidor.
	 * Realiza a coleta da digital para criar o template
	 */
	@Override
	public String removeUser(PedestrianAccessEntity athleteAccessEntity) {
		return removeUser(athleteAccessEntity, true);
	}
	
	public String removeUserLCDevice(PedestrianAccessEntity athleteAccessEntity) {
		if(modeloLC) {
			removeBiometricaLC(athleteAccessEntity, false);
		}
		
		HibernateAccessDataFacade.removeTemplates(athleteAccessEntity.getId());
		SmartAcessoClient.removeTemplatesFromServer(athleteAccessEntity.getId());
		if (Main.broadcastServer != null) {
			Main.broadcastServer.sendMessage(new BroadcastMessageTO(BroadcastMessageType.REMOVE_TEMPLATES, athleteAccessEntity.getId()));
		}
	
		return "";
	}
	
	private String removeUser(PedestrianAccessEntity athleteAccessEntity, boolean isCatraca) {
		if (modeloLC) {
			removeBiometricaLC(athleteAccessEntity, isCatraca);
		}
		HibernateAccessDataFacade.removeTemplates(athleteAccessEntity.getId());
		SmartAcessoClient.removeTemplatesFromServer(athleteAccessEntity.getId());
		if (Main.broadcastServer != null) {
			Main.broadcastServer.sendMessage(
					new BroadcastMessageTO(BroadcastMessageType.REMOVE_TEMPLATES, athleteAccessEntity.getId()));
		}

		return "";
	}

	private void removeBiometricaLC(PedestrianAccessEntity athleteAccessEntity, boolean isCatraca) {
		//remove nessa catraca e nas outras
		int ret = EasyInner.RequisitarExcluirUsuarioBio(inner.Numero, 1, athleteAccessEntity.getId()+"");
		
		Long inicio = System.currentTimeMillis();
		do {
			ret = EasyInner.RespostaExcluirUsuarioBio(inner.Numero);
			Utils.sleep(200);
		}
		while (ret != Enumeradores.RET_COMANDO_OK && (System.currentTimeMillis()-inicio) < 5000); 
		System.out.println("caiu na emoveBiometricaLC");
		//remove usu�rio em outras catracas
		if(isCatraca) {
			System.out.println(sdf.format(new Date()) + "   Remove template nas outras catracas...");
			for(Device d : Main.devicesList) {
				System.out.println("d.identify: " + d.identifier);
				System.out.println("this.indentity: " + this.identifier);
				if(d != null 
						&& d instanceof TopDataDevice 
						&& d.identifier != this.identifier) {
					TopDataDevice topData = (TopDataDevice) d;
					if(topData.modeloLC) {
						topData.removeUser(athleteAccessEntity, false);
					}
				}
			}
		}
	}
	
	
	@Override
	public String setMode(DeviceMode mode){
		DeviceMode previousMode = this.mode;
		this.mode = mode;
		try {
			sendConfiguration();
		
		} catch (Exception e) {
			e.printStackTrace();
			this.mode = previousMode;
			try {
				sendConfiguration();
			
			} catch (Exception e1) {
				e1.printStackTrace();
			}
			return "Erro ao enviar as configura��es. " + e.getMessage();
		}
		return "";
	}
	
	@Override
	public boolean isRegistrationProcessStartedOnDevice(){
		return "naCatraca".equals(getConfigurationValue(MODO_DE_TRABALHO));
	}
	
	@Override
	public void createDefaultConfiguration(){
		List<ConfigurationTO> geralConfigurations = new ArrayList<ConfigurationTO>();
		geralConfigurations.add(new ConfigurationTO(MODO_DE_TRABALHO, "Digitais no servidor_noServidor", FieldType.COMBOBOX, 
				"Digitais na catraca_naCatraca;Digitais no servidor_noServidor"));
		geralConfigurations.add(new ConfigurationTO(ENVIA_DIGITAIS_PARA_CATRACA, "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(SENTIDO_DA_CATRACA, "Hor�rio_clockwise", FieldType.COMBOBOX, 
				"Hor�rio_clockwise;Antihor�rio_anticlockwise"));
		geralConfigurations.add(new ConfigurationTO(TEMPO_DE_LIBERADO, "7", FieldType.NUMERIC_LIST, "5;1;15"));
		geralConfigurations.add(new ConfigurationTO(TEMPO_DE_MENSAGEM_NEGADO, "1", FieldType.NUMERIC_LIST, "1;1;15"));
		geralConfigurations.add(new ConfigurationTO(BLOQUEAR_SAIDA, "true", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(HABILITAR_TECLADO, "true", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(ECOAR_ASTERISCOS, "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(NIVEL_RECONHECIMENTO, "6", FieldType.NUMERIC_LIST, "1;1;9"));
		geralConfigurations.add(new ConfigurationTO(TEMPO_TECLADO, "10", FieldType.NUMERIC_LIST, "5;1;20"));
		geralConfigurations.add(new ConfigurationTO(TEMPO_MUDANCA_ONLINE_OFFLINE, "10", FieldType.NUMERIC_LIST, "6;1;20"));
		geralConfigurations.add(new ConfigurationTO(TEMPO_DE_PING, "5", FieldType.NUMERIC_LIST, "2;1;10"));
		geralConfigurations.add(new ConfigurationTO(TEMPO_ESPERA_PARA_CONECTAR, "10", FieldType.NUMERIC_LIST, "5;1;20"));
		geralConfigurations.add(new ConfigurationTO(TIPO_LEITOR, "Proximidade Wiegand_3", FieldType.COMBOBOX, 
				"Codigo de barras_0;Magn�tico_1;Proximidade AbaTrack2_2;Proximidade Wiegand_3;Proximidade Wiegand FC_33;"
				+ "Proximidade Wiegand FC Sem Separador_6;Proximidade Smart Card_4;QRCode_7;", 240));
		geralConfigurations.add(new ConfigurationTO(QUANTIDADE_DIGITOS_CARTAO, "5", FieldType.NUMERIC_LIST, "4;1;16"));

		geralConfigurations.add(new ConfigurationTO(MODELO_BIOMETRICO, "true", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(TIPO_BIOMETRICO, "LFD_lfd", FieldType.COMBOBOX, "LFD_lfd;LC_lc"));
		geralConfigurations.add(new ConfigurationTO(DOIS_LEITORES, "true", FieldType.CHECKBOX, "(usa para catracas com urna)", true));
		geralConfigurations.add(new ConfigurationTO(LEITOR_1, "Entrada e sa�da_3", FieldType.COMBOBOX, 
				"Desativado_0;Somente entrada_1;Somente sa�da_2;Entrada e sa�da_3;sa�da e entrada_4"));
		geralConfigurations.add(new ConfigurationTO(LEITOR_2, "Entrada e sa�da_3", FieldType.COMBOBOX, 
				"Desativado_0;Somente entrada_1;Somente sa�da_2;Entrada e sa�da_3;sa�da e entrada_4"));
		geralConfigurations.add(new ConfigurationTO(IDENTIFICACAO_BIOMETRICA, "Sim_1", FieldType.COMBOBOX, "Sim_1;N�o_0"));
		geralConfigurations.add(new ConfigurationTO(VERIFICACAO_BIOMETRICA, "N�o_0", FieldType.COMBOBOX, "Sim_1;N�o_0"));
		geralConfigurations.add(new ConfigurationTO(PADRAO_DE_CARTAO, "Padr�o livre_1", FieldType.COMBOBOX, "Padr�o livre_1;Padr�o TopData_0"));
		geralConfigurations.add(new ConfigurationTO(LOGICA_DE_CATRACA_COM_URNA, "true", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(COLETA_CARTOES_OFFLINE, "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(IGNORAR_REGRAS_DE_ACESSO, "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(IS_DEVICE_RESTRITO, "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(ONLY_ENABLED_MODE, "false", FieldType.CHECKBOX));
		
		String nomeAcademia = "SmartPonto;Controle Acesso";
    	if (Main.loggedUser != null) {
    		nomeAcademia = Utils.formatAcademyName(Main.loggedUser.getName());
    	}

    	if (nomeAcademia.length() > 16) {
    		nomeAcademia = nomeAcademia.substring(0, 16).trim() + ";" + nomeAcademia.substring(16, 32).trim();
    	}
    	
		List<ConfigurationTO> customConfigurations = new ArrayList<ConfigurationTO>();
    	customConfigurations.add(new ConfigurationTO(MENSAGEM_ONLINE, nomeAcademia, FieldType.MESSAGE_LINES));

		configurationGroups = new ArrayList<ConfigurationGroupTO>();
		configurationGroups.add(new ConfigurationGroupTO("Geral", geralConfigurations));
		configurationGroups.add(new ConfigurationGroupTO("Personalizado", customConfigurations));
	}
	
	protected void configureInner(){
		this.inner.Numero = innerNumber;
		this.inner.QtdDigitos = getConfigurationValueAsInteger(QUANTIDADE_DIGITOS_CARTAO);
		this.inner.Teclado = getConfigurationValueAsBoolean(HABILITAR_TECLADO);
		this.inner.Lista = false;
		this.inner.ListaBio = false;
		this.inner.TipoLeitor = getConfigurationValueAsInteger(TIPO_LEITOR).equals(33) 
													? 3 : getConfigurationValueAsInteger(TIPO_LEITOR); 
		this.inner.Identificacao = getConfigurationValueAsInteger(IDENTIFICACAO_BIOMETRICA);
		this.inner.Verificacao = getConfigurationValueAsInteger(VERIFICACAO_BIOMETRICA);
		this.inner.DoisLeitores = getConfigurationValueAsBoolean(DOIS_LEITORES);
		this.inner.Catraca = true;
		this.inner.Biometrico = getConfigurationValueAsBoolean(MODELO_BIOMETRICO);
		this.inner.CntDoEvents = 0;
		this.inner.CountPingFail = 0;
		this.inner.CountTentativasEnvioComando = 0;
		this.inner.EstadoAtual = Enumeradores.EstadosInner.ESTADO_CONECTAR;
		this.inner.TempoInicialPingOnLine = (int) System.currentTimeMillis();
		this.inner.EstadoTeclado = Enumeradores.EstadosTeclado.TECLADO_EM_BRANCO;
		this.inner.ValorLeitor1 = getConfigurationValueAsInteger(LEITOR_1);
		this.inner.ValorLeitor2 = getConfigurationValueAsInteger(LEITOR_2);
	}
	
	protected Integer testarConexaoInner(Integer Inner) {
		int[] DataHora = new int[6];
		Integer ret = EasyInner.ReceberRelogio(Inner, DataHora);
		if(ret == 0) {
			
		}
		return ret;
	}
	
	protected int ping() {
		int ret = 0;
		try {
			int countTentativasEnvioComando = 0;
			ret = EasyInner.PingOnLine(inner.Numero);
			while (ret != easyInner.RET_COMANDO_OK && countTentativasEnvioComando < 3) {
				Utils.sleep(300);
				ret = EasyInner.PingOnLine(inner.Numero);
				countTentativasEnvioComando++;
			}
			
		} catch (Exception ex) {
			ex.printStackTrace();
			ret = -1;
		}
		
		return ret;
	}
	
	protected int pingOffline() {
		int ret = 0;
		try {
			int countTentativasEnvioComando = 0;
			ret = EasyInner.Ping(inner.Numero);
			while (ret != easyInner.RET_COMANDO_OK && countTentativasEnvioComando < 3) {
				Utils.sleep(300);
				ret = EasyInner.Ping(inner.Numero);
				countTentativasEnvioComando++;
			}
			
		} catch (Exception ex) {
			ex.printStackTrace();
			ret = -1;
		}
		
		return ret;
	}
	
	@SuppressWarnings("static-access")
	protected void validarAcesso(){
		try {
			validandoAcesso = true;
			System.out.print("\n" + sdf.format(new Date()) + "  VALIDAR ACESSO: ");
			System.out.print(" Origem: " + inner.BilheteInner.Origem);
			System.out.println("   Cartao: " + inner.BilheteInner.Cartao);

			if (inner.BilheteInner.Origem == 1 
					|| inner.BilheteInner.Origem == 2
					|| inner.BilheteInner.Origem == 3
					|| inner.BilheteInner.Origem == 12) {
				processAccessRequest(inner.BilheteInner.Cartao.toString());
				
			} else if (inner.BilheteInner.Origem == 18) { // Leitor biometrico(template)
				templateTemp = new byte[404];
				Integer tamanho = 404;
				
				int ret = easyInner.ReceberTemplateLeitorInnerBio(inner.Numero, templateTemp, tamanho);
				
				if (ret == Enumeradores.RET_COMANDO_OK) {
					try {
						Long idUsuario = searchTemplate();
						System.out.println(sdf.format(new Date()) + "  VALIDAR ACESSO: Usuario identificado: " + idUsuario);
						if (idUsuario != null) {
							processAccessRequest(idUsuario.toString());

						} else {
							verificationResult = VerificationResult.NOT_FOUND;
						}

					} catch (Exception e) {
					} finally {
					}
					
				}
			}
			
			if (athleteScreen != null) {
				athleteScreen.requisicaoPorDigital(null, verificationResult, allowedUserName, matchedAthleteAccess);
			}
			
			if (VerificationResult.ALLOWED.equals(getVerificationResult())
					|| VerificationResult.TOLERANCE_PERIOD.equals(getVerificationResult())) {
				boolean usaUrna = getConfigurationValueAsBoolean(LOGICA_DE_CATRACA_COM_URNA);

				if(usaUrna && matchedAthleteAccess != null 
	            			&& matchedAthleteAccess.getOrigemCatraca() != null 
	            			&& matchedAthleteAccess.getOrigemCatraca() == 3) {
	            	EasyInner.AcionarRele2(inner.Numero);

	            } else {
	            	allowAccess();
	            }
			
			} else {
				denyAccess();
			}
			
		} catch (Exception e) {
			e.printStackTrace();
			inner.EstadoAtual = EstadosInner.ESTADO_CONECTAR;
		}
		validandoAcesso = false;
	}
	
	public void validaAcessoHikivision(final String cardNumber) {
		try {
			while (sendingConfiguration) {
				Utils.sleep(50);
			}
			
			Main.validandoAcesso = true;
			if(Main.temServidor()) {
				HibernateAccessDataFacade.enviaInicioVerificandoAcesso();
			}
			
			validandoAcesso = true;
			System.out.print("\n" + sdf.format(new Date()) + "  VALIDAR ACESSO HIKIVISION: ");
			System.out.print(" Origem: " + Origens.ORIGEM_LEITOR_1);
			System.out.println("\tCartao: " + cardNumber);

			inner.BilheteInner.Origem = Origens.ORIGEM_LEITOR_1;
			inner.BilheteInner.Cartao.setLength(0);
			inner.BilheteInner.Cartao = new StringBuilder(cardNumber);
			
			processAccessRequest(cardNumber, false);
			
			if (athleteScreen != null) {
				athleteScreen.requisicaoPorDigital(null, verificationResult, allowedUserName, matchedAthleteAccess);
			}
			
			if (VerificationResult.ALLOWED.equals(getVerificationResult())
					|| VerificationResult.TOLERANCE_PERIOD.equals(getVerificationResult())) {
	            allowAccess();
			} else {
				denyAccess();
			}
			
		} catch (Exception e) {
			e.printStackTrace();
			inner.EstadoAtual = EstadosInner.ESTADO_CONECTAR;
		} finally {
			validandoAcesso = false;
			Main.validandoAcesso = false;
			if(Main.temServidor()) {
				HibernateAccessDataFacade.enviaFimVerificandoAcesso();										
			}
		}
		
	}
	
	private Long searchTemplate(){
		// Primeiro é feito o processo de match para identificar o usu�rio.
		// Apos a identificaao é verificado se o acesso é permitido.
		try {
			int exportType = EXPORT_MINCONV_TYPE.FIM01_HV;
		   
			EasyInner.EnviarMensagemPadraoOnLine(inner.Numero, 0, formatMessage(Utils.getPreference("messageWait")));
			
			// digital coletada agora
			NBioBSPJNI.FIR_HANDLE capturedFIRHandle = bsp.new FIR_HANDLE();
			int ret = export.ImportFIR(templateTemp, 404, exportType, NBioBSPJNI.FIR_PURPOSE.VERIFY, capturedFIRHandle);
			if (ret != Enumeradores.RET_COMANDO_OK) {
				EasyInner.EnviarMensagemPadraoOnLine(inner.Numero, 0, formatMessage(Utils.getPreference("messageError")));
				return null;
			}
			
			NBioBSPJNI.INPUT_FIR capturedInputFIR = bsp.new INPUT_FIR();
			capturedInputFIR.SetFIRHandle(capturedFIRHandle);
			
			NBioBSPJNI.IndexSearch.FP_INFO fpInfo = indexSearchEngine.new FP_INFO();
			indexSearchEngine.Identify(capturedInputFIR, nivelSeguranca, fpInfo, Origens.nMaxSearchTime);
			
			if (!bsp.IsErrorOccured()) {
				Integer idTemplate = fpInfo.ID;
				TemplateEntity template = (TemplateEntity) HibernateAccessDataFacade.getSingleResultById(TemplateEntity.class, idTemplate.longValue());
				
				if(template != null) {
					Long idPedestrian = template.getIdPedestreianAccess();
					PedestrianAccessEntity pedestre = pedestrianAccessRepository.buscaPedestrePorIdOuIdTemp(idPedestrian);
					
					if(Objects.nonNull(pedestre)) {
						return pedestre.getId();
					}
				}
				
				return null;
			}

			System.out.println(sdf.format(new Date()) + "  Erro no IndexSearch: " + bsp.GetErrorCode());
			 
			if (bsp.GetErrorCode() == NBioBSPJNI.ERROR.NBioAPIERROR_INDEXSEARCH_IDENTIFY_FAIL) {
				EasyInner.EnviarMensagemPadraoOnLine(inner.Numero, 0,
						formatMessage(Utils.getPreference("messageNotFound")));
				return null;
			}

			EasyInner.EnviarMensagemPadraoOnLine(inner.Numero, 0, formatMessage(Utils.getPreference("messageError")));
			
			return null;

		} catch (Exception e) {
			e.printStackTrace();
			EasyInner.EnviarMensagemPadraoOnLine(inner.Numero, 0, formatMessage(Utils.getPreference("messageError")));
		}
        return null;
    }
	
	protected void enviarConfiguracoesOffline() throws Exception {
		int countTentativasEnvioComando = 0;
		montaConfiguracaoInner(Enumeradores.MODO_OFF_LINE);
        int ret = EasyInner.EnviarConfiguracoes(inner.Numero);
        while (ret != easyInner.RET_COMANDO_OK && countTentativasEnvioComando < 3) {
        	System.out.println("retorno de ret  -----------> "+ ret );
        	Utils.sleep(tempoEspera);
        	ret = EasyInner.EnviarConfiguracoes(inner.Numero);
        	countTentativasEnvioComando++;
        }
        if (ret != easyInner.RET_COMANDO_OK) {
        	Main.mainScreen.addEvento(name + ": N�o foi poss�vel enviar configura��es offline");
        	setStatus(DeviceStatus.DISCONNECTED);
        	throw new Exception(name + ": N�o foi poss�vel enviar configura��es offline");
        }
	}
	
	protected void enviarMensagensOffline() throws Exception{
		int countTentativasEnvioComando = 0;
		EasyInner.DefinirMensagemEntradaOffLine(0, mensagemEntradaOffLine);
        EasyInner.DefinirMensagemSaidaOffLine(0, mensagemSaidaOffLine);            
        EasyInner.DefinirMensagemPadraoOffLine(0, mensagemPadraoOffLine);
        int ret = EasyInner.EnviarMensagensOffLine(inner.Numero);
        while (ret != easyInner.RET_COMANDO_OK && countTentativasEnvioComando < 3) {
        	Utils.sleep(tempoEspera);
        	ret = EasyInner.EnviarMensagensOffLine(inner.Numero);
        	countTentativasEnvioComando++;
        }
        if (ret != easyInner.RET_COMANDO_OK) {
        	Main.mainScreen.addEvento(name + ": N�o foi poss�vel enviar mensagens offline. Retorno da catraca " + ret);
        	setStatus(DeviceStatus.DISCONNECTED);
        	throw new Exception(name + ": N�o foi poss�vel enviar mensagens offline. Retorno da catraca " + ret);
        }
	}
	
	protected void enviarConfiguracaoMudancaOnlineOffline() throws Exception{
		int countTentativasEnvioComando = 0;
    	int tempoMudancaOnlineOffline = getConfigurationValueAsInteger(TEMPO_MUDANCA_ONLINE_OFFLINE);
    	int habilitaTeclado = inner.Teclado ? Enumeradores.Opcao_SIM : Enumeradores.Opcao_NAO;
        EasyInner.HabilitarMudancaOnLineOffLine(2, tempoMudancaOnlineOffline);
        
        if (inner.Biometrico) {
        	int leitor1 = inner.ValorLeitor1 != 1 && inner.ValorLeitor1 != 2 ? inner.ValorLeitor1 : 3;
        	int leitor2 = inner.DoisLeitores ? (inner.ValorLeitor1 == 0 ? 0 : 3) : 0;
            EasyInner.DefinirEntradasMudancaOffLineComBiometria(habilitaTeclado, leitor1, leitor2, inner.Verificacao, inner.Identificacao);
        } else
            EasyInner.DefinirEntradasMudancaOffLine(habilitaTeclado, inner.ValorLeitor1, (inner.DoisLeitores ? inner.ValorLeitor2 : 0), 0);
        
        EasyInner.DefinirMensagemPadraoMudancaOffLine(1, mensagemPadraoMudancaOffLine);
        EasyInner.DefinirMensagemPadraoMudancaOnLine(1, mensagemPadraoMudancaOnLine);
        EasyInner.DefinirEntradasMudancaOnLine(configuraEntradasMudancaOnLine());
        EasyInner.DefinirConfiguracaoTecladoOnLine(inner.QtdDigitos, (getConfigurationValueAsBoolean(ECOAR_ASTERISCOS) ? 2 : 1), 6, 17);
        
        int ret = EasyInner.EnviarConfiguracoesMudancaAutomaticaOnLineOffLine(inner.Numero);
        while (ret != easyInner.RET_COMANDO_OK && countTentativasEnvioComando < 3) {
        	Utils.sleep(tempoEspera);
        	ret = EasyInner.EnviarConfiguracoesMudancaAutomaticaOnLineOffLine(inner.Numero);
        	countTentativasEnvioComando++;
        }
       
        if (ret != easyInner.RET_COMANDO_OK) {
        	Main.mainScreen.addEvento(name + ": N�o foi poss�vel enviar configura��es de mudan�a online/offline");
        	setStatus(DeviceStatus.DISCONNECTED);
        	throw new Exception(name + ": N�o foi poss�vel enviar configura��es de mudan�a online/offline");
        }
	}
	
	private void enviarConfiguracoesOnline() throws Exception{
		int countTentativasEnvioComando = 0;
		montaConfiguracaoInner(Enumeradores.MODO_ON_LINE);
        int ret = EasyInner.EnviarConfiguracoes(inner.Numero);
        while (ret != easyInner.RET_COMANDO_OK && countTentativasEnvioComando < 3) {
        	Utils.sleep(tempoEspera);
        	ret = EasyInner.EnviarConfiguracoes(inner.Numero);
        	countTentativasEnvioComando++;
        }
        if (ret != easyInner.RET_COMANDO_OK) {
        	Main.mainScreen.addEvento(name + ": N�o foi poss�vel enviar configura��es online");
        	setStatus(DeviceStatus.DISCONNECTED);
        	throw new Exception(name + ": N�o foi poss�vel enviar configura��es online");
        }
	}
	
	protected void enviarDataHora() throws Exception{
		int countTentativasEnvioComando = 0;
		Date Data = new Date();
        int Ano = Integer.parseInt(new SimpleDateFormat("yy").format(Data));
        int Mes = Integer.parseInt(new SimpleDateFormat("MM").format(Data));
        int Dia = Integer.parseInt(new SimpleDateFormat("dd").format(Data));
        int Hora = Integer.parseInt(new SimpleDateFormat("HH").format(Data));
        int Minuto = Integer.parseInt(new SimpleDateFormat("mm").format(Data));
        int Segundo = Integer.parseInt(new SimpleDateFormat("ss").format(Data));
        int ret = EasyInner.EnviarRelogio(inner.Numero, Dia, Mes, Ano, Hora, Minuto, Segundo);
        while (ret != easyInner.RET_COMANDO_OK && countTentativasEnvioComando < 3) {
        	Utils.sleep(tempoEspera);
        	ret = EasyInner.EnviarRelogio(inner.Numero, Dia, Mes, Ano, Hora, Minuto, Segundo);
        	countTentativasEnvioComando++;
        }
        if (ret != easyInner.RET_COMANDO_OK) {
        	Main.mainScreen.addEvento(name + ": N�o foi poss�vel enviar data e hora");
        	setStatus(DeviceStatus.DISCONNECTED);
        	throw new Exception(name + ": N�o foi poss�vel enviar data e hora");
        }
	}
	
	protected boolean enviarMensagemPadrao() {
		try {
			int countTentativasEnvioComando = 0;
			String mensagemPadraoOnLine = formatMessage(
					DeviceMode.ENROLLMENT.equals(mode) ? (Utils.getPreference("messageEnrollment") + " 1/2")
							: getConfigurationValue(MENSAGEM_ONLINE));
			int ret = EasyInner.EnviarMensagemPadraoOnLine(inner.Numero, 0, mensagemPadraoOnLine);
			while (ret != easyInner.RET_COMANDO_OK && countTentativasEnvioComando < 3) {
				Utils.sleep(tempoEspera);
				ret = EasyInner.EnviarMensagemPadraoOnLine(inner.Numero, 0, mensagemPadraoOnLine);
				countTentativasEnvioComando++;
			}
			
			if (ret == easyInner.RET_COMANDO_OK) {
				return true;
			
			} else {
				Main.mainScreen.addEvento(name + ": N�o foi poss�vel enviar mensagens Padr�o");
				setStatus(DeviceStatus.DISCONNECTED);
			}

		} catch (Exception ex) {
			ex.printStackTrace();
			Main.mainScreen.addEvento(name + ": N�o foi poss�vel enviar mensagens Padr�o. " + ex.getMessage());
			setStatus(DeviceStatus.DISCONNECTED);
		}
		return false;
	}
	
	protected void configurarEntradasOnline() throws Exception {
		int countTentativasEnvioComando = 0;
		int qtdDigitos = inner.QtdDigitos;
    	int ecoAsteriscos = getConfigurationValueAsBoolean(ECOAR_ASTERISCOS) ? 2 : 1;
    	int formaEntrada = configuraEntradasMudancaOnLine();
    	int tempoTeclado = getConfigurationValueAsInteger(TEMPO_TECLADO);
    	int posicaoCursorTeclado = 17;
    	if (DeviceMode.ENROLLMENT.equals(mode)) {
    		qtdDigitos = 0;
    		ecoAsteriscos = 0;
    		tempoTeclado = 0;
    		posicaoCursorTeclado = 32;
    	}
        int ret = EasyInner.EnviarFormasEntradasOnLine(inner.Numero, qtdDigitos, ecoAsteriscos, formaEntrada, 
        												tempoTeclado, posicaoCursorTeclado);
        while (ret != easyInner.RET_COMANDO_OK && countTentativasEnvioComando < 3) {
        	Utils.sleep(tempoEspera);
        	ret = EasyInner.EnviarFormasEntradasOnLine(inner.Numero, qtdDigitos, ecoAsteriscos, formaEntrada, 
        												tempoTeclado, posicaoCursorTeclado);
        	countTentativasEnvioComando++;
        }
        
        if (ret != easyInner.RET_COMANDO_OK) {
        	Main.mainScreen.addEvento(name + ": N�o foi poss�vel enviar configura��es entradas online");
        	setStatus(DeviceStatus.DISCONNECTED);
        	throw new Exception(name + ": N�o foi poss�vel enviar configura��es entradas online");
        }
	}
	
	protected void montaConfiguracaoInner(int modo) {
		try {
			String sentidoCatraca = getConfigurationValue(SENTIDO_DA_CATRACA);
			boolean bloquearSaida = getConfigurationValueAsBoolean(BLOQUEAR_SAIDA);
			int tipoCatraca = bloquearSaida ? Enumeradores.ACIONA_REGISTRO_ENTRADA_OU_SAIDA
					: ("anticlockwise".equals(sentidoCatraca) ? Enumeradores.CATRACA_SAIDA_LIBERADA
							: Enumeradores.CATRACA_ENTRADA_LIBERADA);
			int tempoLiberacao = getConfigurationValueAsInteger(TEMPO_DE_LIBERADO);
			int valorLeitor1 = inner.ValorLeitor1;
			int valorLeitor2 = inner.DoisLeitores ? inner.ValorLeitor2 : Enumeradores.DESATIVADO;
			int habilitaTeclado = inner.Teclado ? Enumeradores.Opcao_SIM : Enumeradores.Opcao_NAO;
			int ecoarAsteriscos = getConfigurationValueAsBoolean(ECOAR_ASTERISCOS) ? Enumeradores.ECOA_ASTERISCO
					: Enumeradores.ECOA_DIGITADO;
			int tipoLeitorProximidade = Enumeradores.REGISTRAR_CONFORME_GIRO;
			int registrarAcessoNegado = 1;
			int quantidadeDigitosCartao = inner.QtdDigitos;

			if (DeviceMode.ENROLLMENT.equals(getMode())) {
				tipoCatraca = Enumeradores.NAO_UTILIZADO;
				tempoLiberacao = 5;
				valorLeitor1 = Enumeradores.DESATIVADO;
				valorLeitor2 = Enumeradores.DESATIVADO;
				habilitaTeclado = Enumeradores.Opcao_NAO;
				ecoarAsteriscos = Enumeradores.Opcao_NAO;
				tipoLeitorProximidade = Enumeradores.DESATIVADO;
				registrarAcessoNegado = 0;
				quantidadeDigitosCartao = 0;
			}

			EasyInner.DefinirPadraoCartao(getConfigurationValueAsInteger(PADRAO_DE_CARTAO));
			if (modo == Enumeradores.MODO_OFF_LINE) {
				EasyInner.ConfigurarInnerOffLine();
			} else {
				EasyInner.ConfigurarInnerOnLine();
			}

			EasyInner.ConfigurarAcionamento1(tipoCatraca, tempoLiberacao);
			EasyInner.ConfigurarAcionamento2(tipoCatraca, tempoLiberacao);
			EasyInner.HabilitarTeclado(habilitaTeclado, ecoarAsteriscos);
			EasyInner.DefinirFuncaoDefaultLeitoresProximidade(tipoLeitorProximidade);
			EasyInner.RegistrarAcessoNegado(registrarAcessoNegado);
//			EasyInner.ConfigurarAcionamento2(Enumeradores.NAO_UTILIZADO, 0);
			EasyInner.ConfigurarTipoLeitor(inner.TipoLeitor);

			if (getConfigurationValueAsInteger(TIPO_LEITOR).equals(7)) { 
				EasyInner.InserirQuantidadeDigitoVariavel(4);
				EasyInner.InserirQuantidadeDigitoVariavel(6);
				EasyInner.InserirQuantidadeDigitoVariavel(8);
				EasyInner.InserirQuantidadeDigitoVariavel(10);
				EasyInner.InserirQuantidadeDigitoVariavel(12);
				EasyInner.InserirQuantidadeDigitoVariavel(14);
				int retDigitos = EasyInner.InserirQuantidadeDigitoVariavel(16);
				System.out.println("Configura digitos variáveis: " + retDigitos);
			} else {
				EasyInner.DefinirQuantidadeDigitosCartao(quantidadeDigitosCartao);
			}

			EasyInner.ConfigurarLeitor1(valorLeitor1);
			EasyInner.ConfigurarLeitor2(valorLeitor2);
			EasyInner.DefinirFuncaoDefaultSensorBiometria(Enumeradores.REGISTRAR_CONFORME_GIRO);
			EasyInner.ReceberDataHoraDadosOnLine(Enumeradores.Opcao_SIM);


		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	private int configuraEntradasMudancaOnLine() {
	    String teclado = inner.Teclado ? "1" : "0";
	    String configuracao = DeviceMode.VERIFICATION.equals(mode) ? teclado : "0";
	    if (!inner.Biometrico) {
	    	String leitor1 = inner.ValorLeitor1 == 0 ? "000" 
        			: (inner.ValorLeitor1 == 1 ? "001" 
        					: (inner.ValorLeitor1 == 2 ? "010" 
        							: (inner.ValorLeitor1 == 3 ? "011" : ("100"))));
	    	String leitor2 = inner.ValorLeitor2 == 0 ? "000" 
        			: (inner.ValorLeitor2 == 1 ? "001" 
        					: (inner.ValorLeitor2 == 2 ? "010" 
        							: (inner.ValorLeitor2 == 3 ? "011" : ("100"))));
	        if (!inner.DoisLeitores)
	        	leitor2 = "000";
	            
	    	configuracao = "1" + leitor2 + leitor1 + configuracao;
	
	        /*
	         --------------------------------------------------------------------------------------------------
	         |       7        |     6      |   5    |   4    |   3    |    2    |      1     |        0       |
	         --------------------------------------------------------------------------------------------------
	         | Seta/Reseta    |  Bit 2     |  Bit 1 |  Bit 0 | Bit 2  |  Bit 1  |   Bit 0    |  Teclado       |
	         |   config.      | Leitor 2   |        |        |        |         |            |                |
	         |   bit-a-bit    |            |        |        |        |         |            |                |
	         --------------------------------------------------------------------------------------------------
	         | 1 ' Habilita   | 000 - Desativa leitor        |  000 - Desativa leitor        | 1 ' Habilita   |
	         | 0 ' Desabilita | 001 - Leitor só entrada      |  001 - Leitor só entrada      | 0 ' Desabilita |
	         |                | 010 - Leitor só sa�da        |  010 - Leitor só sa�da        |                |
	         |                | 011 - Leitor Entrada e saida |  011 - Leitor Entrada e sa�da |                |
	         |                | 100 - Leitor Entrada e sa�da |  100 - Leitor Entrada e       |                |
	         |                |   Invertido                  |   sa�da Invertido             |                |
	         --------------------------------------------------------------------------------------------------
	         */
	    
	    } else { //Com Biometria
	
	        configuracao = "0" + //Bit Fixo
	                "1" + //Habilitado
	                inner.Identificacao + //identifica��o
	                inner.Verificacao + //Verifica��o
	                "0" + //Bit fixo
	                (inner.DoisLeitores ? "11" : "10") + // 11 -> habilita leitor 1 e 2, 10 -> habilita apenas leitor 1
	                configuracao;
	
	        /*
	         ------------------------------------------------------------------------------------------------------------------------
	         |    7     |       6       |       5       |       4       |      3       |       2      |      1       |      0       |
	         ------------------------------------------------------------------------------------------------------------------------
	         | Bit fixo | Seta/Reseta   | identifica��o |  Verifica��o  |   Bit fixo   |   Leitor 1   | Leitor 2     |  Teclado     |
	         |   '0'    |    config.    |      Bio      |      Bio      |     '0'      |              |              |              |
	         |          | bit-a-bit bio |               |               |              |              |              |              |
	         ------------------------------------------------------------------------------------------------------------------------
	         |    0     |  1-Habilita   | 1-Habilita    | 1-Habilita    |      0       | 1-Habilita   | 1-Habilita   | 1-Habilita   |
	         |          |  0-Desabilita | 0-Desabilita  | 0-Desabilita  |              | 0-Desabilita | 0-Desabilita | 0-Desabilita |
	         ------------------------------------------------------------------------------------------------------------------------
	         */ 
	    }
	    return binarioParaDecimal(configuracao);
	}
	
	private int binarioParaDecimal(String valorBinario) {
		int length_bin = 0, aux = 0, retorno = 0, i;
		length_bin = valorBinario.length();
		for (i = 0; i < length_bin; i++) {
			aux = Integer.parseInt(valorBinario.substring(i, i + 1));
			retorno += aux * (int) Math.pow(2, (length_bin - i)) / 2;
		}
		return (retorno);
	}
	
	protected String formatMessage(String message){
		message = Utils.removerAcentos(message);
    	String[] partes = message.split(";");
    	String parte1 = partes.length > 0 ? Utils.formatString16(partes[0]) : "";
    	String parte2 = partes.length > 1 ? Utils.formatString16(partes[1]) : "";
    	return parte1 + parte2;
    }
	
	private void limparInner(){
		StringBuffer Cartao = new StringBuffer();
        int[] iArrBCartaoRb = new int[8];
        int ret = Enumeradores.Limpar;
        do {
        	ret = EasyInner.ReceberDadosOnLine(inner.Numero, iArrBCartaoRb, Cartao);
        }
        while (ret != 128);
    }
	
	private void encerrarConexao(boolean fechaPorta) {
		try {
			montaConfiguracaoInner(Enumeradores.MODO_OFF_LINE);
			EasyInner.EnviarConfiguracoes(inner.Numero);
			EasyInner.DefinirMensagemEntradaOffLine(0, mensagemEntradaOffLine);
			EasyInner.DefinirMensagemSaidaOffLine(0, mensagemSaidaOffLine);
			EasyInner.DefinirMensagemPadraoOffLine(0, mensagemPadraoOffLine);
			EasyInner.EnviarMensagensOffLine(inner.Numero);

			if(fechaPorta) {
				EasyInner.FecharPortaComunicacao();				
			}

		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}
	
	@SuppressWarnings("unchecked")
	private void startIndexSearchEngine() {
		indexSearchEngine = bsp.new IndexSearch();
		List<TemplateEntity> templatesList = (List<TemplateEntity>) HibernateAccessDataFacade.getResultList(TemplateEntity.class, "TemplateEntity.findAllNaoRemovido");
		
		if (templatesList == null || templatesList.isEmpty()) {
			return;
		}
		
		int count = 0;
		System.out.println("tamanho do template list " + templatesList.size());
		for (TemplateEntity templateEntity : templatesList) {
			
			if (templateEntity.getTemplate().length != 404) { // Insere apenas templates Nitgen
				continue;
			}
			
			String template = Base64.encodeBase64String(templateEntity.getTemplate());
			if (!template.contains("AAAAAAAAAAAA")) {
//					addTemplateToIndexSearch(templateEntity);
					// 5310 ID
					// 87279 PEDESTRE
				if(count < 10000) {
					
					addTemplateToIndexSearch(templateEntity);
				} else {
					System.out.println(count);
					System.out.println(template);
				}
				
				count++;
			} else {
				System.out.println("Tem AAAAA");

			}
		}
		System.out.println("Quantas biometrias foram inseridas " + count);
		
	}
	
	public void restartIndexSearchEngine() {
		
		if(modeloLC) {
			return;
		}
		
		//usa L�gica de IndexSearch para Nitgen
		if(indexSearchEngine != null) {

			int i = 0;
			while(validandoAcesso && i < 20) {
				try {
					Thread.sleep(1000);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
				i++;
			}
			
			bsp = new NBioBSPJNI();
			export = bsp.new Export();
			
			startIndexSearchEngine();
		}
	}
	
	@SuppressWarnings("unchecked")
	public void verificaCadastroNoInner(boolean online, boolean todas, Date data) {
		//adiciona todos os templates
		Date dataAlteracao = data != null ? data : deviceEntity.getUltimaAtualizacao();
		
		//busca biometrias alteradas/cadastrada deste a última sincronização
		List<PedestrianAccessEntity> pedestres = null;
		if(!todas && dataAlteracao != null) {
			System.out.println("Sincroniza por data " + new SimpleDateFormat("dd/MM/yyyy HH:mm:ss").format(deviceEntity.getUltimaAtualizacao()));
			HashMap<String, Object> args = new HashMap<String, Object>();
			args.put("ULTIMA_SINC", dataAlteracao);
			pedestres = (List<PedestrianAccessEntity>) 
					HibernateAccessDataFacade.getResultListWithParams(PedestrianAccessEntity.class, "PedestrianAccessEntity.findAllPedestreAlterados", args);
			
		} else {
			System.out.println("Sincroniza total");
			pedestres = (List<PedestrianAccessEntity>) 
					HibernateAccessDataFacade.getResultList(PedestrianAccessEntity.class, "PedestrianAccessEntity.findAll");
			todas = true;
		}
		
		System.out.println("Qtd encontradas: " + pedestres == null ? 0 : pedestres.size());
		
		try {
			if(pedestres != null && !pedestres.isEmpty()) {
				coletandoDadosOffLine = true;
				for (PedestrianAccessEntity pedestre : pedestres) {
					if(Objects.isNull(pedestre.getTemplates()) || pedestre.getTemplates().isEmpty()) {
						continue;
					}
					
					boolean alteraTemplate = false;
					
					if(todas) {
						alteraTemplate = true;
					}
					
					List<TemplateEntity> templates = pedestre.getTemplates();
					
					if(!alteraTemplate 
							&& dataAlteracao != null
							&& templates.get(0).getDataCriacao().getTime() >= dataAlteracao.getTime()) {
						alteraTemplate = true;
					}
					
					if(!alteraTemplate) {
						continue;
					}
					
					TemplateEntity envio = new TemplateEntity();
					envio.setPedestrianAccess(pedestre);
					envio.setTemplate(templates.get(0).getTemplate());
					
					processaAdicaoBiometriaLC(envio);
				}
				
				System.out.println("Digitais sincronizadas.");
				
				deviceEntity.setUltimaAtualizacao(new Date());
				deviceEntity = (DeviceEntity) HibernateAccessDataFacade.save(DeviceEntity.class, deviceEntity)[0];
			}
		
		} finally {
			coletandoDadosOffLine = false;
			if(isConnected()) {
				deviceCard.setMensagem("Conectado", MessageType.NORMAL);
			} else {
				deviceCard.setMensagem(" ", MessageType.NORMAL);
			}
		}
		
	}

	private void processaAdicaoBiometriaLC(TemplateEntity templateEntity) {
		if(templateEntity.getTemplate().length < 502) {
			return;
		}
		
		int ret = EasyInner.RequisitarVerificarCadastroUsuarioBio(inner.Numero, 1, templateEntity.getPedestrianAccess().getId().toString());
		
		if(ret != Enumeradores.RET_COMANDO_OK) {
			return;
		}

		Long inicio = System.currentTimeMillis();
		do {
			ret = EasyInner.RespostaVerificarCadastroUsuarioBio(inner.Numero);
			Utils.sleep(50);

		} while (ret != Enumeradores.RET_COMANDO_OK 
				&& templateTemp != null
				&& (System.currentTimeMillis() - inicio) < 5000 
				&& DeviceMode.ENROLLMENT.equals(mode));
		
		if(ret != Enumeradores.RET_BIO_USR_NAO_CADASTRADO) {
			return;
		}
		
		byte[] template1 = new byte[502];
		byte[] template2 = new byte[502];
		
		//cadastra usu�rio
		if(templateEntity.getTemplate().length > 502) {
			//proveniente do leitor
			LcDevice.extracTopDataTemplate(templateEntity.getTemplate(), template1, template2);
			String tStr = Base64.encodeBase64String(template2);
			//verificar se segunda est� vazia
			if(tStr.startsWith("AAAQAAAAAAAAAAA") ) {
				template2 = null;
			}
			
		} else {
			//proveniente da catraca
			template1 = templateEntity.getTemplate();
			template2 = templateEntity.getSample();
		}
		
		insereUserLC(templateEntity.getPedestrianAccess().getId(), template1, template2);
	}

	public void addTemplateToIndexSearch(TemplateEntity templateEntity){
    	try {
    		int exportType = NBioBSPJNI.EXPORT_MINCONV_TYPE.FIM01_HV;
    		INPUT_FIR storedInputFIR = bsp.new INPUT_FIR();
    		FIR_HANDLE storedFIRHandle = bsp.new FIR_HANDLE();
    		export.ImportFIR(templateEntity.getTemplate(), templateEntity.getTemplate().length, exportType, 
    				NBioBSPJNI.FIR_PURPOSE.IDENTIFY, storedFIRHandle);
            storedInputFIR.SetFIRHandle(storedFIRHandle);
            
            if(indexSearchEngine != null) {
            	indexSearchEngine.AddFIR(storedInputFIR, templateEntity.getId().intValue(), indexSearchEngine.new SAMPLE_INFO());
            }
            
            if (!bsp.IsErrorOccured() && bsp.GetErrorCode()!=0) {
    			System.out.println(sdf.format(new Date()) + " topdata topdata Erro ao adicionar template na IndexSearchEngine. Erro: " + bsp.GetErrorCode());
    		}
		
    	} catch (Exception e) {
			e.printStackTrace();
		}
    }
	
	private String defineMensagemPermitido(String sentidoCatraca, String espacoEntrar, String entrar) {
		String mensagem = "";
		
		if("anticlockwise".equals(sentidoCatraca)) {
			mensagem = formatMessage("<-" + espacoEntrar + entrar + ";" + allowedUserName);
		} else {
			mensagem = formatMessage(entrar + espacoEntrar + "->" + ";" + allowedUserName);
		}
		
		return mensagem;
	}
	
	@Override
	public String getFullIdentifier() {
		return "Inner " + inner.Numero;
	}
	
	public Inner getInner() {
		return inner;
	}

	public IndexSearch getIndexSearchEngine() {
		return indexSearchEngine;
	}

	public Integer getInnerNumber() {
		return innerNumber;
	}

}
