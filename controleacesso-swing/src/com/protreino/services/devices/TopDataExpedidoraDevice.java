package com.protreino.services.devices;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import javax.swing.SwingWorker;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;
import com.nitgen.SDK.BSP.NBioBSPJNI;
import com.nitgen.SDK.BSP.NBioBSPJNI.Export;
import com.protreino.services.entity.CartaoComandaEntity;
import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.entity.LogCartaoComandaEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.enumeration.DeviceMode;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.enumeration.FieldType;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.enumeration.NotificationType;
import com.protreino.services.enumeration.StatusCard;
import com.protreino.services.enumeration.VerificationResult;
import com.protreino.services.main.Main;
import com.protreino.services.to.AttachedTO;
import com.protreino.services.to.ConfigurationGroupTO;
import com.protreino.services.to.ConfigurationTO;
import com.protreino.services.utils.HibernateUtil;
import com.protreino.services.utils.Utils;
import com.topdata.EasyInner;
import com.topdata.easyInner.enumeradores.Enumeradores;
import com.topdata.easyInner.enumeradores.Enumeradores.EstadosInner;

@SuppressWarnings("serial")
public class TopDataExpedidoraDevice extends TopDataDevice {
	
	public static final int EXPEDIDORA_ENTRADA_COM_CARTAO = 10;
    public static final int EXPEDIDORA_TIMEOUT_COM_CARTAO = 12;
    public static final int EXPEDIDORA_CARTAO_SEM_VALIDACAO = 13;
    public static final int EXPEDIDORA_ENTRADA_COM_BOTAO = 0;
    public static final int EXPEDIDORA_TIMEOUT_COM_BOTAO = 2;
    public static final int EXPEDIDORA_MECANISMO_COM_CARTAO = 3;
    public static final int EXPEDIDORA_MECANISMO_POUCO_CARTAO = 4;
    public static final int EXPEDIDORA_MECANISMO_SEM_CARTAO = 5;
	
	private CartaoComandaEntity matched;
	private Date dataGiro;
	private int origem = 0;
	
	public TopDataExpedidoraDevice(DeviceEntity deviceEntity){
		this(deviceEntity.getIdentifier(), deviceEntity.getConfigurationGroupsTO());
		this.deviceEntity = deviceEntity;
		this.name = deviceEntity.getName();
		this.location = deviceEntity.getLocation();
		this.desiredStatus = deviceEntity.getDesiredStatus();
		this.defaultDevice = deviceEntity.getDefaultDevice();
		this.athleteScreenConfig = deviceEntity.getAthleteScreenConfig();
		String attachedDevices = deviceEntity.getAttachedDevices();
		
		Gson gson = new GsonBuilder().create();
		List<AttachedTO> list = gson.fromJson(attachedDevices, new TypeToken<List<AttachedTO>>() {}.getType());

		this.setAttachedDevices(list);
	}
	
	public TopDataExpedidoraDevice(String identifier){
		super(identifier);
		this.manufacturer = Manufacturer.TOP_DATA_EXPEDIDORA;
		this.name = "TopData Expedidora (" + this.tipo + ") " + this.innerNumber;
	}
	
	public TopDataExpedidoraDevice(String identifier, List<ConfigurationGroupTO> configurationGroups){
		super(identifier, configurationGroups);
		this.manufacturer = Manufacturer.TOP_DATA_EXPEDIDORA;
		this.name = "TopData Expedidora (" + this.tipo + ") " + this.innerNumber;
	}
	
	@Override
	public void connect(String... args) throws Exception {
		if("SAIDA".equals(this.tipo)) {
			super.connect(args);
		}else{
			
			//faz conexÃ£o propria
			easyInner = new EasyInner();
			int ret = 0;
			if(!portaAberta) {
				EasyInner.DefinirTipoConexao(2);
		        ret = EasyInner.AbrirPortaComunicacao(port);
		        if (ret != Enumeradores.RET_COMANDO_OK 
		        		&& ret != Enumeradores.RET_PORTA_JAABERTA) 
		        	throw new Exception("Erro ao abrir a porta de comunicaÃ§Ã£o: " + ret);
		        portaAberta = true;
			}
	        ret = Enumeradores.Limpar;
	        Long inicio = System.currentTimeMillis();
	        Long tempoDeEspera = getConfigurationValueAsLong("Tempo de espera para conectar") * 1000;
	        while (ret != Enumeradores.RET_COMANDO_OK && (System.currentTimeMillis() - inicio) < tempoDeEspera) {
	        	ret = testarConexaoInner(inner.Numero);
	        	Utils.sleep(50);
	        }
	        if (ret != Enumeradores.RET_COMANDO_OK) 
	        	throw new Exception("Não foi possível conectar.");
	        
	        sendConfiguration();
	        
	        setStatus(DeviceStatus.CONNECTED);
	        watchDogEnabled = true;
			workerEnabled = true;
			
			worker = new SwingWorker<Void, Void>(){
				@Override
				protected Void doInBackground() throws Exception {
					while (workerEnabled) {
						try {
							while (sendingConfiguration) {
								// aguarda configuracoes serem enviadas
								Utils.sleep(50);
							}
							
							coletarBilhetesOffLine();
							
							Long tempo = getConfigurationValueAsLong("Tempo de para coletar cartão/comanda");
							if(tempo == null)
								tempo = 3l;
							
							Utils.sleep(tempo * 1000);
							
							
						}catch (Exception e) {
							e.printStackTrace();
						}
					}
					return null;
				}
			};
			worker.execute();
			
	        watchDog = new SwingWorker<Void, Void>(){
				@Override
				protected Void doInBackground() throws Exception {
					while (watchDogEnabled) {
						Long sleepTime = null;
						try {
							if (!busy) 
								ping();
							sleepTime = getConfigurationValueAsLong("Tempo de ping") * 1000;
							
							if(DeviceStatus.DISCONNECTED.equals(lastStatus)
									&& DeviceStatus.CONNECTED.equals(getStatus())) {
								sendConfiguration();
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
			watchDog.execute();
	        
			
		}
	}
	
	@Override
	protected Integer testarConexaoInner(Integer Inner) {
		return "SAIDA".equals(this.tipo) 
				? super.testarConexaoInner(Inner) : 0;
	}
	
	@Override
	public void sendConfiguration() throws Exception {
		if("SAIDA".equals(this.tipo)) {
			super.sendConfiguration();
		}else {
			sendingConfiguration = true;
			configureInner();
			
			enviarConfiguracoesOffline();
			enviarMensagensOffline();
			enviarConfiguracaoMudancaOnlineOffline();
			
			coletarBilhetesOffLine();
				
			sendingConfiguration = false;
		}
	}
	
	@Override
	protected void registraGiro(int sentido, Date data) {
		
		//volta cartão para o status de AGUARDANDO
		if(matched != null) {
			
			//verifica sentido
			if("ENTRADA".equals(this.tipo)) {
				//bloqueia se entrada
				matched.setDataAlteracao(new Date());
				matched.setStatus(getConfigurationValueAsBoolean("Cartão expedido com status de LIBERADO") 
						? StatusCard.LIBERADO : StatusCard.BLOQUEADO);
			}else {
				//aguardando se saida
				matched.setDataAlteracao(new Date());
				matched.setStatus(StatusCard.AGUARDANDO);
			}
			
			HibernateUtil.save(CartaoComandaEntity.class, matched);
			
			//cria log de liberação (sem sincronizaÃ§Ã£o com web)
	       	LogCartaoComandaEntity log = new LogCartaoComandaEntity(matched);
	       	log.setUsuario(Main.internoLoggedUser);
	       	log.setTipoLiberacao(this.tipo+"_"+matched.getStatus().name());
	       	log.setOrigem("CATRACA");
	       	log.setData(new Date());
	       	HibernateUtil.save(LogCartaoComandaEntity.class, log);
			
			matched = null;
			allowedUserName = null;
		}
		
	}
	
	
	
	@SuppressWarnings("unchecked")
	@Override
	public void processAccessRequest(Object obj) {
		
		if(this.dataGiro == null)
			dataGiro = new Date();
		
		String cartao = obj.toString();
		if("ENTRADA".equals(this.tipo)) {
			cartao = Utils.toHEX(obj.toString().replaceAll("[^a-zA-Z0-9]+",""));
			cartao = cartao.substring(2);
			//System.out.println("Numero do cartão: " +cartao);
		}
		
		//procura cartão por cÃ³digo real
		HashMap<String, Object> args = new HashMap<>();
		args.put("numeroReal", cartao);
		args.put("removido"  , false);
		List<CartaoComandaEntity> cartoes = (List<CartaoComandaEntity>)
				HibernateUtil.getResultListWithDynamicParams(CartaoComandaEntity.class, null, args);
		
		if(cartoes != null && !cartoes.isEmpty()) {
			if(cartoes.size() > 1) {
				for (CartaoComandaEntity c : cartoes) {
					if(c.getRemovido() == null || Boolean.FALSE.equals(c.getRemovido())) {
						matched = c;
						break;
					}
				}
			}else
				matched = cartoes.get(0);
		}
		
		if(matched != null) {
			
			allowedUserName = matched.getNumeroAlternativo();
			
			if("ENTRADA".equals(this.tipo)) {
				//entrada sempre liberada
				verificationResult = VerificationResult.ALLOWED;
			}else {
				origem = inner.BilheteInner.Origem;
				
				//verificar se vai validar a origem
				if(inner.BilheteInner.Origem != 3)
					verificationResult = VerificationResult.NOT_ALLOWED_SENSOR;
				else {
					//verifica se cartão pode sair
					//esta no status de LIBERADO
					if(StatusCard.LIBERADO.equals(matched.getStatus())) {
						verificationResult = VerificationResult.ALLOWED;
						//catraca com urna
						//boolean usaUrna = getConfigurationValueAsBoolean("Lógica da catraca com urna");
			            //if(usaUrna)
			            //	EasyInner.AcionarRele2(inner.Numero);
					}
					else {
						verificationResult = VerificationResult.NOT_ALLOWED;
					}
				}
			}
		}else {
			//cartão nÃ£o encontrado
			verificationResult = VerificationResult.NOT_FOUND;
		}
		
		
		
		this.dataGiro = null;
	}
	
	@Override
	public void processAccessRequest(Object obj, Date data) {
		
		//logica propria
		this.dataGiro = data;
		
		processAccessRequest(obj);
	}
	
	@Override
	protected void validarAcesso(){
		try {
			validandoAcesso = true;
			System.out.print("\n" + sdf.format(new Date()) + "  VALIDAR ACESSO SAÃ�DA: ");
			System.out.print(" Origem: " + inner.BilheteInner.Origem);
			System.out.println("   Cartao: " + inner.BilheteInner.Cartao);
			
			
			if (inner.BilheteInner.Origem == 1 
					|| inner.BilheteInner.Origem == 2
					|| inner.BilheteInner.Origem == 3
					|| inner.BilheteInner.Origem == 12) { // Teclado, sensor de proximidade ou biometrico(cartao)
				processAccessRequest(inner.BilheteInner.Cartao.toString());
				
			}
			
			if (athleteScreen != null)
				athleteScreen.requisicaoPorDigital(null, verificationResult, allowedUserName, matchedAthleteAccess);
			
			if (VerificationResult.ALLOWED.equals(verificationResult)
					|| VerificationResult.TOLERANCE_PERIOD.equals(verificationResult)) {
				//catraca com urna
				boolean usaUrna = getConfigurationValueAsBoolean("Lógica da catraca com urna");
	            if(usaUrna && origem == 3) {
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
	
	@Override
	public void createDefaultConfiguration() {
		if("SAIDA".equals(this.tipo)) {
			super.createDefaultConfiguration();
		}else {
			
			List<ConfigurationTO> geralConfigurations = new ArrayList<ConfigurationTO>();
			geralConfigurations.add(new ConfigurationTO("Modo de trabalho", "Digitais no servidor_noServidor", FieldType.COMBOBOX, 
					"Digitais na catraca_naCatraca;Digitais no servidor_noServidor"));
			geralConfigurations.add(new ConfigurationTO("Envia digitais para catraca", "false", FieldType.CHECKBOX));
			geralConfigurations.add(new ConfigurationTO("Sentido da catraca", "HorÃ¡rio_clockwise", FieldType.COMBOBOX, 
					"HorÃ¡rio_clockwise;AntihorÃ¡rio_anticlockwise"));
			geralConfigurations.add(new ConfigurationTO("Tempo de liberação", "7", FieldType.NUMERIC_LIST, "5;1;15"));
			geralConfigurations.add(new ConfigurationTO("Tempo de mensagem negado", "5", FieldType.NUMERIC_LIST, "1;1;15"));
			geralConfigurations.add(new ConfigurationTO("Bloquear saída", "true", FieldType.CHECKBOX));
			geralConfigurations.add(new ConfigurationTO("Habilitar teclado", "true", FieldType.CHECKBOX));
			geralConfigurations.add(new ConfigurationTO("Ecoar asteriscos", "false", FieldType.CHECKBOX));
			geralConfigurations.add(new ConfigurationTO("Nível de segurança do reconhecimento", "6", FieldType.NUMERIC_LIST, "1;1;9"));
			geralConfigurations.add(new ConfigurationTO("Tempo teclado", "10", FieldType.NUMERIC_LIST, "5;1;20"));
			geralConfigurations.add(new ConfigurationTO("Tempo de mudança Online/Offline", "10", FieldType.NUMERIC_LIST, "6;1;20"));
			geralConfigurations.add(new ConfigurationTO("Tempo de ping", "5", FieldType.NUMERIC_LIST, "2;1;10"));
			geralConfigurations.add(new ConfigurationTO("Tempo de espera para conectar", "10", FieldType.NUMERIC_LIST, "5;1;20"));
			geralConfigurations.add(new ConfigurationTO("Tipo de leitor", "Proximidade AbaTrack2_2", FieldType.COMBOBOX, 
					"Código de barras_0;MagnÃ©tico_1;Proximidade AbaTrack2_2;Proximidade Wiegand_3;Proximidade Wiegand FC_33;"
					+ "Proximidade Wiegand FC Sem Separador_6;Proximidade Smart Card_4;QRCode_7;", 240));
			//if(Main.loggedUser != null && Main.loggedUser.getQtdePadraoDigitosCartão() != null) {
			//	geralConfigurations.add(new ConfigurationTO("Quantidade dígitos cartão", 
			//			Main.loggedUser.getQtdePadraoDigitosCartão().toString(), FieldType.NUMERIC_LIST, "4;1;16"));
			//} else {
				geralConfigurations.add(new ConfigurationTO("Quantidade dígitos cartão", "14", FieldType.NUMERIC_LIST, "4;1;16"));
			//}
			geralConfigurations.add(new ConfigurationTO("Modelo biométrico", "false", FieldType.CHECKBOX));
			geralConfigurations.add(new ConfigurationTO("Tipo biométrico", "LFD_lfd", FieldType.COMBOBOX, "LFD_lfd;LC_lc"));
			geralConfigurations.add(new ConfigurationTO("Dois leitores", "true", FieldType.CHECKBOX, "(usa para catracas com urna)", true));
			geralConfigurations.add(new ConfigurationTO("Leitor 1", "Somente entrada_1", FieldType.COMBOBOX, 
					"Desativado_0;Somente entrada_1;Somente saída_2;Entrada e saída_3;Saída e entrada_4"));
			geralConfigurations.add(new ConfigurationTO("Leitor 2", "Desativado_0", FieldType.COMBOBOX, 
					"Desativado_0;Somente entrada_1;Somente saída_2;Entrada e saída_3;Saída e entrada_4"));
			geralConfigurations.add(new ConfigurationTO("Identificação Biométrica", "Não_1", FieldType.COMBOBOX, "Sim_1;Não_0"));
			geralConfigurations.add(new ConfigurationTO("Verificação Biométrica", "Não_0", FieldType.COMBOBOX, "Sim_1;Não_0"));
			geralConfigurations.add(new ConfigurationTO("Padrão de cartão", "Padrão livre_1", FieldType.COMBOBOX, "Padrão livre_1;Padrão TopData_0"));
			geralConfigurations.add(new ConfigurationTO("Lógica da catraca com urna", "false", FieldType.CHECKBOX));
			
			String nomeAcademia = "SmartPonto;Controle Acesso";
	    	if (Main.loggedUser != null)
	    		nomeAcademia = Utils.formatAcademyName(Main.loggedUser.getName());
	    	if (nomeAcademia.length() > 16)
	    		nomeAcademia = nomeAcademia.substring(0, 16).trim() + ";" + nomeAcademia.substring(16, 32).trim();
	    	
			geralConfigurations.add(new ConfigurationTO("Tempo de para coletar cartão/comanda", "3", FieldType.NUMERIC_LIST, "3;1;15"));
			geralConfigurations.add(new ConfigurationTO("Cartão expedido com status de LIBERADO", "false", FieldType.CHECKBOX));
			
			String nomeEmpresa = "SmartPonto;Controle Acesso";
	    	if (Main.loggedUser != null)
	    		nomeEmpresa = Utils.formatAcademyName(Main.loggedUser.getName());
	    	if (nomeEmpresa.length() > 16)
	    		nomeEmpresa = nomeEmpresa.substring(0, 16).trim() + ";" + nomeEmpresa.substring(16, 32).trim();
	    	
			List<ConfigurationTO> customConfigurations = new ArrayList<ConfigurationTO>();
	    	customConfigurations.add(new ConfigurationTO("Mensagem online", nomeEmpresa, FieldType.MESSAGE_LINES));
	
			configurationGroups = new ArrayList<ConfigurationGroupTO>();
			configurationGroups.add(new ConfigurationGroupTO("Geral", geralConfigurations));
			configurationGroups.add(new ConfigurationGroupTO("PersonalizaÃ§Ã£o", customConfigurations));
		
		}
		
		
	}
	
	protected int ping(){
		
		if("SAIDA".equals(this.tipo)) {
			return super.ping();
		}else {
			int ret = 0;
			try {
				int countTentativasEnvioComando = 0;
				ret = EasyInner.PingOnLine(inner.Numero);
				while (ret != easyInner.RET_COMANDO_OK && countTentativasEnvioComando < 3) {
					Utils.sleep(300);
					ret = testarConexaoInner(inner.Numero);
					countTentativasEnvioComando++;
				}
				if (ret == easyInner.RET_COMANDO_OK) {
					inner.TempoInicialPingOnLine = System.currentTimeMillis();
					if(!coletandoDadosOffLine)
						setStatus(DeviceStatus.CONNECTED);
					inner.CountRepeatPingOnline = 0;
				
				} else 
					setStatus(DeviceStatus.DISCONNECTED);
			} catch (Exception ex) {
				ex.printStackTrace();
				setStatus(DeviceStatus.DISCONNECTED);
			}
			return ret;
		}
	}
	
	public boolean isTheSame(Object obj) {
	    Device other = (Device) obj;
	    if (!this.manufacturer.equals(other.manufacturer))
	    	return false;
	    
	    String [] partsThis  = this.identifier.split(";");
	    String [] partsOther = other.identifier.split(";"); 
	    if (!partsThis[0].equals(partsOther[0]))
	    	return false;
	    return true;
	}

}
