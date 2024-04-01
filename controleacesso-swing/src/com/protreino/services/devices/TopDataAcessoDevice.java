package com.protreino.services.devices;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;
import com.protreino.services.constants.Tipo;
import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.enumeration.FieldType;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.enumeration.VerificationResult;
import com.protreino.services.main.Main;
import com.protreino.services.to.AttachedTO;
import com.protreino.services.to.ConfigurationGroupTO;
import com.protreino.services.to.ConfigurationTO;
import com.protreino.services.utils.HibernateUtil;
import com.protreino.services.utils.Utils;
import com.topdata.EasyInner;

import static com.protreino.services.constants.TopDataAcessoDeviceConstatns.*;

@SuppressWarnings("serial")
public class TopDataAcessoDevice extends TopDataDevice {
	
	public TopDataAcessoDevice(DeviceEntity deviceEntity){
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
	
	public TopDataAcessoDevice(String identifier){
		super(identifier);
		this.manufacturer = Manufacturer.TOP_DATA_ACESSO;
		this.name = "TopData Acesso Inner " + this.innerNumber;
	}
	
	public TopDataAcessoDevice(String identifier, List<ConfigurationGroupTO> configurationGroups){
		super(identifier, configurationGroups);
		this.manufacturer = Manufacturer.TOP_DATA_ACESSO;
		this.name = "TopData Acesso Inner " + this.innerNumber;
	}
	
	@Override
	public void allowAccess() {
		procuraSeExisteMensagemParaPedestre();
		
		if(mensagemPermitido == null || mensagemPermitido.isEmpty())
			mensagemPermitido = formatMessage(VerificationResult.AUTHORIZED.getMessage());
		
		if(allowedUserName != null)
			mensagemPermitido = formatMessage(mensagemPermitido + ";" + allowedUserName);
		
		EasyInner.LigarLedVerde(inner.Numero);
		EasyInner.EnviarMensagemPadraoOnLine(inner.Numero, 0, mensagemPermitido);
		EasyInner.ManterRele1Acionado(inner.Numero);
		EasyInner.ManterRele2Acionado(inner.Numero);
		EasyInner.AcionarBipCurto(inner.Numero);
		
		Integer tempoAcionamentoRele = getConfigurationValueAsInteger(TEMPO_ACIONAMENTO_DO_RELE);
		if(tempoAcionamentoRele == null)
			tempoAcionamentoRele = 3;
		tempoAcionamentoRele*= 1000;

		Long inicio = System.currentTimeMillis();
		while ((System.currentTimeMillis() - inicio) < tempoAcionamentoRele) {
			Utils.sleep(1000);
		}
		
		EasyInner.DesabilitarRele1(inner.Numero);
		EasyInner.DesabilitarRele2(inner.Numero);
		
		//Boolean acionaRele2 = getConfigurationValueAsBoolean(ACIONA_RELE_2);
		
		Boolean usaTorniquete = getConfigurationValueAsBoolean(USA_TORNIQUETE);
		if(Boolean.TRUE.equals(usaTorniquete)) {
			return;
		}
		
		HashMap<String, Object> args = new HashMap<String, Object>();
		args.put("EQUIPAMENTO", "Inner Acesso " + inner.Numero);
		
		LogPedestrianAccessEntity ultimo = (LogPedestrianAccessEntity) HibernateUtil
							.getUniqueResultWithParams(LogPedestrianAccessEntity.class,
									"LogPedestrianAccessEntity.findByEquipamentDesc", args);
		
		if(ultimo == null) {
			return;
		}
		
		Integer sentido = (inner.BilheteInner.Complemento == 1 
								|| inner.BilheteInner.Complemento == 38) 
										? getConfigurationValueAsInteger(LEITOR_2) 
										: getConfigurationValueAsInteger(LEITOR_1);
		
		if(sentido == 1) {
			ultimo.setDirection(Tipo.ENTRADA);
		} else if(sentido == 2) {
			ultimo.setDirection(Tipo.SAIDA);
		}
		
		ultimo.setStatus("ATIVO");
		ultimo.setDataCriacao(new Date());
		registraGiro(sentido, null);
		
		HibernateUtil.save(LogPedestrianAccessEntity.class, ultimo);
		
		PedestrianAccessEntity pedestre = (PedestrianAccessEntity) HibernateUtil
				.getSingleResultById(PedestrianAccessEntity.class, ultimo.getIdPedestrian());

		if(pedestre == null) {
			return;
		}
		
		boolean ignoraRegras = getConfigurationValueAsBoolean(IGNORAR_REGRAS_DE_ACESSO);
		if(!ignoraRegras) {
			if(pedestre.temMensagens()) {
				pedestre.decrementaMensagens();
			}
			
			if(getConfigurationValueAsBoolean(BLOQUEAR_SAIDA) 
					&& Tipo.SAIDA.equals(ultimo.getDirection())) {
				pedestre.decrementaCreditos();
			}
			
			if("DINAMICO_USO".equals(pedestre.getTipoQRCode())) {
				pedestre.decrementaQRCodeUso();
			}
			
			HibernateUtil.save(PedestrianAccessEntity.class, pedestre);
		}
		
		try {
			enviarMensagemPadrao();
			configurarEntradasOnline();
		
		} catch (Exception e) {
			e.printStackTrace();
		}
		
	}
	
	@Override
	protected void registraGiro(int sentido, Date data) {
		HashMap<String, Object> args = new HashMap<String, Object>();
		args.put("EQUIPAMENTO", "Inner Acesso " + inner.Numero);
		
		LogPedestrianAccessEntity ultimoAcesso = (LogPedestrianAccessEntity) HibernateUtil
												.getUniqueResultWithParams(LogPedestrianAccessEntity.class,
													"LogPedestrianAccessEntity.findByEquipamentDesc", args);
		
		if(ultimoAcesso == null) {
			return;
		}
		
		sentido = inner.BilheteInner.Complemento == 1 || inner.BilheteInner.Complemento == 38 
					? getConfigurationValueAsInteger(LEITOR_2) 
					: getConfigurationValueAsInteger(LEITOR_1);
		System.out.println(" ultimo acesso " + ultimoAcesso.getDirection());
		if(sentido == 1)
			ultimoAcesso.setDirection(Tipo.ENTRADA);
		else if(sentido == 2)
			ultimoAcesso.setDirection(Tipo.SAIDA);
		
		ultimoAcesso.setStatus("ATIVO");
		ultimoAcesso.setDataCriacao(new Date());
		
		HibernateUtil.save(LogPedestrianAccessEntity.class, ultimoAcesso);
		
		PedestrianAccessEntity pedestre = (PedestrianAccessEntity) HibernateUtil
				.getSingleResultById(PedestrianAccessEntity.class, ultimoAcesso.getIdPedestrian());
		
		if(pedestre == null)
			return;
		
		
		boolean ignoraRegras = getConfigurationValueAsBoolean(IGNORAR_REGRAS_DE_ACESSO);
		if(!ignoraRegras) {
			
			if(pedestre.temMensagens()) {
				pedestre.decrementaMensagens();
			}
			
			if(Tipo.SAIDA.equals(ultimoAcesso.getDirection())) {
				pedestre.decrementaCreditos();
			}
			
			if("DINAMICO_USO".equals(pedestre.getTipoQRCode())) {
				pedestre.decrementaQRCodeUso();
			}
		
			HibernateUtil.save(PedestrianAccessEntity.class, pedestre);
		}
		
	}
	
	@Override
	public void processAccessRequest(Object obj) {
		try {
			Object[] retorno = HibernateUtil.processAccessRequest((String) obj, "Inner Acesso " + inner.Numero, 
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
	
	@Override
	public void createDefaultConfiguration() {
		List<ConfigurationTO> geralConfigurations = new ArrayList<ConfigurationTO>();
		geralConfigurations.add(new ConfigurationTO(MODO_DE_TRABALHO, "Digitais no servidor_noServidor", FieldType.COMBOBOX, 
				"Digitais na catraca_naCatraca;Digitais no servidor_noServidor"));
		geralConfigurations.add(new ConfigurationTO(ENVIA_DIGITAIS_PARA_CATRACA, "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(SENTIDO_DA_CATRACA, "Horário_clockwise", FieldType.COMBOBOX, 
				"Horário_clockwise;AntiHorário_anticlockwise"));
		geralConfigurations.add(new ConfigurationTO(TEMPO_DE_LIBERADO, "7", FieldType.NUMERIC_LIST, "5;1;15"));
		geralConfigurations.add(new ConfigurationTO(BLOQUEAR_SAIDA, "true", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(HABILITAR_TECLADO, "true", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(ECOAR_ASTERISCOS, "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(NIVEL_RECONHECIMENTO, "6", FieldType.NUMERIC_LIST, "1;1;9"));
		geralConfigurations.add(new ConfigurationTO(TEMPO_TECLADO, "10", FieldType.NUMERIC_LIST, "5;1;20"));
		geralConfigurations.add(new ConfigurationTO(TEMPO_MUDANCA_ONLINE_OFFLINE, "10", FieldType.NUMERIC_LIST, "6;1;20"));
		geralConfigurations.add(new ConfigurationTO(TEMPO_DE_PING, "5", FieldType.NUMERIC_LIST, "2;1;10"));
		geralConfigurations.add(new ConfigurationTO(TEMPO_ESPERA_PARA_CONECTAR, "10", FieldType.NUMERIC_LIST, "5;1;20"));
		geralConfigurations.add(new ConfigurationTO(TIPO_LEITOR, "Proximidade Wiegand_3", FieldType.COMBOBOX,
				"Código de barras_0;Magnético_1;Proximidade AbaTrack2_2;Proximidade Wiegand_3;Proximidade Wiegand FC_33;"
                        + "Proximidade Wiegand FC Sem Separador_6;Proximidade Smart Card_4;QRCode_7;", 240));
		geralConfigurations.add(new ConfigurationTO(QUANTIDADE_DIGITOS_CARTAO, "5", FieldType.NUMERIC_LIST, "4;1;16"));
		geralConfigurations.add(new ConfigurationTO(MODELO_BIOMETRICO, "true", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(TIPO_BIOMETRICO, "LFD_lfd", FieldType.COMBOBOX, "LFD_lfd;LC_lc"));
		geralConfigurations.add(new ConfigurationTO(DOIS_LEITORES, "true", FieldType.CHECKBOX, "(usa para catracas com urna)", true));
		geralConfigurations.add(new ConfigurationTO(LEITOR_1, "Somente entrada_1", FieldType.COMBOBOX,
				"Desativado_0;Somente entrada_1;Somente Saída_2"));
		geralConfigurations.add(new ConfigurationTO(LEITOR_2, "Somente Saída_2", FieldType.COMBOBOX,
				"Desativado_0;Somente entrada_1;Somente Saída_2"));
		geralConfigurations.add(new ConfigurationTO(IDENTIFICACAO_BIOMETRICA, "Sim_1", FieldType.COMBOBOX, "Sim_1;Não_0"));
		geralConfigurations.add(new ConfigurationTO(VERIFICACAO_BIOMETRICA, "Não_0", FieldType.COMBOBOX, "Sim_1;Não_0"));
		geralConfigurations.add(new ConfigurationTO(PADRAO_DE_CARTAO, "Padrão livre_1", FieldType.COMBOBOX, "Padrão livre_1;Padrão TopData_0"));
		geralConfigurations.add(new ConfigurationTO(LOGICA_DE_CATRACA_COM_URNA, "true", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(USA_TORNIQUETE, "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(ACIONA_RELE_2, "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(TEMPO_ACIONAMENTO_DO_RELE, "3", FieldType.NUMERIC_LIST, "0;1;10"));
		geralConfigurations.add(new ConfigurationTO(COLETA_CARTOES_OFFLINE, "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(IGNORAR_REGRAS_DE_ACESSO, "false", FieldType.CHECKBOX));
		
		String nomeEmpresa = "SmartPonto;Controle Acesso";
    	if (Main.loggedUser != null)
    		nomeEmpresa = Utils.formatAcademyName(Main.loggedUser.getName());
    	if (nomeEmpresa.length() > 16)
    		nomeEmpresa = nomeEmpresa.substring(0, 16).trim() + ";" + nomeEmpresa.substring(16, 32).trim();
    	
		List<ConfigurationTO> customConfigurations = new ArrayList<ConfigurationTO>();
    	customConfigurations.add(new ConfigurationTO(MENSAGEM_ONLINE, nomeEmpresa, FieldType.MESSAGE_LINES));

		configurationGroups = new ArrayList<ConfigurationGroupTO>();
		configurationGroups.add(new ConfigurationGroupTO("Geral", geralConfigurations));
		configurationGroups.add(new ConfigurationGroupTO("Personalização", customConfigurations));
	}

}
