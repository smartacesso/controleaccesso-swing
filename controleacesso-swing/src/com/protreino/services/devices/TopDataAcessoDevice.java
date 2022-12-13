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
		String attachedDevices = deviceEntity.getAttachedDevices();
		
		Gson gson = new GsonBuilder().create();
		List<AttachedTO> list = gson.fromJson(attachedDevices, new TypeToken<List<AttachedTO>>() {}.getType());

		this.setAttachedDevices(list);
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
		
		
		Integer tempoAcionamentoRelé = getConfigurationValueAsInteger("Tempo de acionamento do relé");
		if(tempoAcionamentoRelé == null)
			tempoAcionamentoRelé = 3;
		tempoAcionamentoRelé *= 1000;
		
		Long inicio = System.currentTimeMillis();
		while ((System.currentTimeMillis() - inicio) < tempoAcionamentoRelé) {
			Utils.sleep(1000);
		}
		
		EasyInner.DesabilitarRele1(inner.Numero);
		EasyInner.DesabilitarRele2(inner.Numero);
		
		
		Boolean AcionaRele2 = getConfigurationValueAsBoolean("Aciona relé 2");

//		if(Boolean.TRUE.equals(AcionaRele2)) {
//			
//			return;
//		}

		Boolean usaTorniquete = getConfigurationValueAsBoolean("Usa torniquete");
		if(Boolean.TRUE.equals(usaTorniquete))
			return;
		
		HashMap<String, Object> args = new HashMap<String, Object>();
		args.put("EQUIPAMENTO", "Inner Acesso " + inner.Numero);
		
		LogPedestrianAccessEntity ultimo = (LogPedestrianAccessEntity) HibernateUtil
							.getUniqueResultWithParams(LogPedestrianAccessEntity.class,
									"LogPedestrianAccessEntity.findByEquipamentDesc", args);
		
		if(ultimo == null)
			return;
		
		Integer sentido = (inner.BilheteInner.Complemento == 1 
								|| inner.BilheteInner.Complemento == 38) 
										? getConfigurationValueAsInteger("Leitor 2") 
										: getConfigurationValueAsInteger("Leitor 1");
		
		if(sentido == 1)
			ultimo.setDirection(Tipo.ENTRADA);
		else if(sentido == 2)
			ultimo.setDirection(Tipo.SAIDA);
		
		ultimo.setStatus("ATIVO");
		ultimo.setDataCriacao(new Date());
		registraGiro(sentido, null);
		
		
		HibernateUtil.save(LogPedestrianAccessEntity.class, ultimo);
		
		PedestrianAccessEntity pedestre = (PedestrianAccessEntity) HibernateUtil
				.getSingleResultById(PedestrianAccessEntity.class, ultimo.getIdPedestrian());

		if(pedestre == null)
			return;
		
		boolean ignoraRegras = getConfigurationValueAsBoolean("Ignorar regras de acesso");
		if(!ignoraRegras) {
			
			if(pedestre.getMensagens() != null && !pedestre.getMensagens().isEmpty())
				Utils.decrementaMensagens(pedestre.getMensagens());
			
			if(getConfigurationValueAsBoolean("Bloquear saída") 
					&& Tipo.SAIDA.equals(ultimo.getDirection()))
				Utils.decrementaCreditos(pedestre);
			
			if("DINAMICO_USO".equals(pedestre.getTipoQRCode()))
				Utils.decrementaQRCodeUso(pedestre);
			
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
		
		if(ultimoAcesso == null)
			return;
		
		sentido = inner.BilheteInner.Complemento == 1 || inner.BilheteInner.Complemento == 38 
					? getConfigurationValueAsInteger("Leitor 2") 
					: getConfigurationValueAsInteger("Leitor 1");
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
		
		
		boolean ignoraRegras = getConfigurationValueAsBoolean("Ignorar regras de acesso");
		if(!ignoraRegras) {
			
			if(pedestre.getMensagens() != null && !pedestre.getMensagens().isEmpty())
				Utils.decrementaMensagens(pedestre.getMensagens());
			
			if(Tipo.SAIDA.equals(ultimoAcesso.getDirection()))
				Utils.decrementaCreditos(pedestre);
			
			if("DINAMICO_USO".equals(pedestre.getTipoQRCode()))
				Utils.decrementaQRCodeUso(pedestre);
		
			HibernateUtil.save(PedestrianAccessEntity.class, pedestre);
			
		}
		
		
	}
	
	@Override
	public void processAccessRequest(Object obj) {
		try {
			Object[] retorno = HibernateUtil.processAccessRequest((String) obj, "Inner Acesso " + inner.Numero, 
					inner.BilheteInner.Origem, location, getConfigurationValueAsBoolean("Lógica da catraca com urna"), true, 
					getConfigurationValueAsBoolean("Ignorar regras de acesso"));
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
		geralConfigurations.add(new ConfigurationTO("Modo de trabalho", "Digitais no servidor_noServidor", FieldType.COMBOBOX, 
				"Digitais na catraca_naCatraca;Digitais no servidor_noServidor"));
		geralConfigurations.add(new ConfigurationTO("Envia digitais para catraca", "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO("Sentido da catraca", "Horário_clockwise", FieldType.COMBOBOX, 
				"Horário_clockwise;Antihorário_anticlockwise"));
		geralConfigurations.add(new ConfigurationTO("Tempo de liberação", "7", FieldType.NUMERIC_LIST, "5;1;15"));
		geralConfigurations.add(new ConfigurationTO("Bloquear saída", "true", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO("Habilitar teclado", "true", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO("Ecoar asteriscos", "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO("Nível de segurança do reconhecimento", "6", FieldType.NUMERIC_LIST, "1;1;9"));
		geralConfigurations.add(new ConfigurationTO("Tempo teclado", "10", FieldType.NUMERIC_LIST, "5;1;20"));
		geralConfigurations.add(new ConfigurationTO("Tempo de mudança Online/Offline", "10", FieldType.NUMERIC_LIST, "6;1;20"));
		geralConfigurations.add(new ConfigurationTO("Tempo de ping", "5", FieldType.NUMERIC_LIST, "2;1;10"));
		geralConfigurations.add(new ConfigurationTO("Tempo de espera para conectar", "10", FieldType.NUMERIC_LIST, "5;1;20"));
		geralConfigurations.add(new ConfigurationTO("Tipo de leitor", "Proximidade Wiegand_3", FieldType.COMBOBOX, 
				"Código de barras_0;Magnético_1;Proximidade AbaTrack2_2;Proximidade Wiegand_3;Proximidade Wiegand FC_33;"
				+ "Proximidade Wiegand FC Sem Separador_6;Proximidade Smart Card_4;QRCode_7;", 240));
		geralConfigurations.add(new ConfigurationTO("Quantidade dígitos cartão", "5", FieldType.NUMERIC_LIST, "4;1;16"));
		geralConfigurations.add(new ConfigurationTO("Modelo biométrico", "true", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO("Tipo biométrico", "LFD_lfd", FieldType.COMBOBOX, "LFD_lfd;LC_lc"));
		geralConfigurations.add(new ConfigurationTO("Dois leitores", "true", FieldType.CHECKBOX, "(usa para catracas com urna)", true));
		geralConfigurations.add(new ConfigurationTO("Leitor 1", "Somente entrada_1", FieldType.COMBOBOX, 
				"Desativado_0;Somente entrada_1;Somente saída_2"));
		geralConfigurations.add(new ConfigurationTO("Leitor 2", "Somente saída_2", FieldType.COMBOBOX, 
				"Desativado_0;Somente entrada_1;Somente saída_2"));
		geralConfigurations.add(new ConfigurationTO("Identificação Biométrica", "Sim_1", FieldType.COMBOBOX, "Sim_1;Não_0"));
		geralConfigurations.add(new ConfigurationTO("Verificação Biométrica", "Não_0", FieldType.COMBOBOX, "Sim_1;Não_0"));
		geralConfigurations.add(new ConfigurationTO("Padrão de cartão", "Padrçao livre_1", FieldType.COMBOBOX, "Padrão livre_1;Padrão TopData_0"));
		geralConfigurations.add(new ConfigurationTO("Lógica da catraca com urna", "true", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO("Usa torniquete", "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO("Aciona relé 2", "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO("Tempo de acionamento do relé", "3", FieldType.NUMERIC_LIST, "0;1;10"));
		geralConfigurations.add(new ConfigurationTO("Coleta cartões offline", "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO("Ignorar regras de acesso", "false", FieldType.CHECKBOX));
		
		String nomeEmpresa = "SmartPonto;Controle Acesso";
    	if (Main.loggedUser != null)
    		nomeEmpresa = Utils.formatAcademyName(Main.loggedUser.getName());
    	if (nomeEmpresa.length() > 16)
    		nomeEmpresa = nomeEmpresa.substring(0, 16).trim() + ";" + nomeEmpresa.substring(16, 32).trim();
    	
		List<ConfigurationTO> customConfigurations = new ArrayList<ConfigurationTO>();
    	customConfigurations.add(new ConfigurationTO("Mensagem online", nomeEmpresa, FieldType.MESSAGE_LINES));

		configurationGroups = new ArrayList<ConfigurationGroupTO>();
		configurationGroups.add(new ConfigurationGroupTO("Geral", geralConfigurations));
		configurationGroups.add(new ConfigurationGroupTO("Personalização", customConfigurations));
		
		
	}

}
