package com.protreino.services.devices;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import com.protreino.services.constants.Tipo;
import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.enumeration.FieldType;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.to.ConfigurationGroupTO;
import com.protreino.services.to.ConfigurationTO;
import com.protreino.services.utils.HibernateUtil;
import com.protreino.services.utils.Utils;

@SuppressWarnings("serial")
public class ControlIDUHFDevice extends ControlIdDevice {
	
	public ControlIDUHFDevice(DeviceEntity deviceEntity){
		this(deviceEntity.getIdentifier(), deviceEntity.getConfigurationGroupsTO());
		this.deviceEntity = deviceEntity;
		this.name = deviceEntity.getName();
		this.location = deviceEntity.getLocation();
		this.login = deviceEntity.getLogin();
		this.password = deviceEntity.getPassword();
		this.desiredStatus = deviceEntity.getDesiredStatus();
		this.defaultDevice = deviceEntity.getDefaultDevice();
		this.mirrorDevice = deviceEntity.getMirrorDevice();
		this.athleteScreenConfig = deviceEntity.getAthleteScreenConfig();
	}
	
	public ControlIDUHFDevice(String identifier){
		this(identifier, null);
	}
	
	public ControlIDUHFDevice(String identifier, List<ConfigurationGroupTO> configurationGroups){
		super(identifier, configurationGroups);
		this.manufacturer = Manufacturer.CONTROL_ID_UHF;
		this.name = "Control Id Antena UHF " + ip;
	}
	
	/**
	 * Construtor usado para varredura de ip
	 * @param ip
	 * @param port
	 */
	public ControlIDUHFDevice(Integer timeout, String identifier) {
		super(timeout, identifier);
		this.manufacturer = Manufacturer.CONTROL_ID_UHF;
		this.name = "Control Id Antena UHF " + ip;
	}
	
	@Override
	public void createDefaultConfiguration(){
		List<ConfigurationTO> geralConfigurations = new ArrayList<ConfigurationTO>();
		geralConfigurations.add(new ConfigurationTO("Habilita beep", "true", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO("Tempo da requisição", "10000", FieldType.NUMERIC_LIST, "2000;1000;20000")); // inicio;passo;fim
		geralConfigurations.add(new ConfigurationTO("Sentido da entrada", "Horário_clockwise", FieldType.COMBOBOX,
				"Horário_clockwise;Anti-horário_anticlockwise"));
		geralConfigurations.add(new ConfigurationTO("Tempo de abertura", "10000", FieldType.NUMERIC_LIST, "10000;10000;60000")); // inicio;passo;fim
		
		geralConfigurations.add(new ConfigurationTO("Bits de identificação", "32", FieldType.COMBOBOX, "26;32;34;66"));
		geralConfigurations.add(new ConfigurationTO("Ordem dos bytes", "default", FieldType.COMBOBOX, "default;lsb"));
		geralConfigurations.add(new ConfigurationTO("Tempo de leitura (milissegundos)", "250", FieldType.TEXT));
		geralConfigurations.add(new ConfigurationTO("Tempo de leitura mesmo cartão  (milissegundos)", "0", FieldType.TEXT));
		geralConfigurations.add(new ConfigurationTO("Potência de transmissão", "2400", FieldType.NUMERIC_LIST, "1500;100;2400"));
		geralConfigurations.add(new ConfigurationTO("Canal de trabalho", "1-10", FieldType.TEXT));
		geralConfigurations.add(new ConfigurationTO("Modo de operação", "cont", FieldType.COMBOBOX, "cont;trigger")); 
		geralConfigurations.add(new ConfigurationTO("Trigger timeout (milissegundos)", "250", FieldType.TEXT)); 
		geralConfigurations.add(new ConfigurationTO("Trigger ocioso", "0", FieldType.COMBOBOX, "0;1"));
		
		geralConfigurations.add(new ConfigurationTO("Ignorar regras de acesso", "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO("Usa lógica de calculo de Entrada/Saída", "true", FieldType.CHECKBOX));
		configurationGroups = new ArrayList<ConfigurationGroupTO>();
		configurationGroups.add(new ConfigurationGroupTO("Geral", geralConfigurations));
	}
	
	@Override
	public void sendConfiguration() throws Exception {
		if (configurationGroups == null || configurationGroups.isEmpty())
			return;
		Configuration configuration = new Configuration(serverIp, serverPort, serverId, 
			getConfigurationValueAsBoolean("Habilita beep"), getConfigurationValue("Tempo de giro"), 
			getConfigurationValue("Tempo da requisição"), Integer.valueOf(getConfigurationValue("Bits de identificação")),
			getConfigurationValue("Ordem dos bytes"), Integer.valueOf(getConfigurationValue("Tempo de leitura (milissegundos)")),
			Integer.valueOf(getConfigurationValue("Tempo de leitura mesmo cartão  (milissegundos)")), Integer.valueOf(getConfigurationValue("Potência de transmissão")),
			getConfigurationValue("Canal de trabalho"), getConfigurationValue("Modo de operação"),
			Integer.valueOf(getConfigurationValue("Trigger timeout (milissegundos)")), Integer.valueOf(getConfigurationValue("Trigger ocioso")));
		Object[] retorno = send("http://" + ip + "/set_configuration.fcgi?session=" + session, configuration);
		String erro = (String) retorno[0];
		if (erro != null)
			throw new Exception(erro);
		String responseString = (String) retorno[1];
		if (responseString == null)
			throw new Exception("Resposta nula.");
	}
	
	@Override
	protected List<Action> allowAccessRequest() {
		try {
			if(habilitaBeep)
				beep();
		
		} catch (Exception e) {
			e.printStackTrace();
		}
		List<Action> actions = new ArrayList<Action>();
		actions.add(new Action("door", "door=1"));
		return actions;
	}
	
	@Override
	public void allowAccess() {
		try {
			if(habilitaBeep)
				beep();
		} catch (Exception e) {
			e.printStackTrace();
		}
		List<Action> actions = new ArrayList<Action>();
		actions.add(new Action("door", "door=1"));
	}
	
	@Override
	protected void openGate(String side) throws Exception {
		List<Action> actions = new ArrayList<Action>();
		actions.add(new Action("door", "door=1"));
		Object[] retorno = send("http://" + ip + "/execute_actions.fcgi?session=" + session, new Request(actions));
		String erro = (String) retorno[0];
		if (erro != null)
			throw new Exception(erro);
	}
	
	
	@Override
	protected void registraGiro(Response notificacao) {
		
		HashMap<String, Object> args = new HashMap<String, Object>();
		args.put("EQUIPAMENTO", "Control " + serverId);
		LogPedestrianAccessEntity ultimo = (LogPedestrianAccessEntity) HibernateUtil
										.getUniqueResultWithParams(LogPedestrianAccessEntity.class,
										"LogPedestrianAccessEntity.findByEquipamentDesc", args);
		if(ultimo == null)
			return;
		
		
		String direction = Tipo.ENTRADA;
		if(getConfigurationValueAsBoolean("Usa lógica de calculo de Entrada/Saída"))
			direction =  ANTICLOCKWISE.equals(decideLadoLiberarCatraca(sentidoEntrada)) ? Tipo.ENTRADA : Tipo.SAIDA;// ANTICLOCKWISE.equals(sentidoEntrada) ? "ENTRADA" : "SAIDA";
		else
			direction =  ANTICLOCKWISE.equals(sentidoEntrada) ? Tipo.ENTRADA : Tipo.SAIDA;
		
		
		ultimo.setDirection(direction);
		ultimo.setStatus("ATIVO");
		
		HibernateUtil.save(LogPedestrianAccessEntity.class, ultimo);
		PedestrianAccessEntity pedestre = (PedestrianAccessEntity) HibernateUtil
							.getSingleResultById(PedestrianAccessEntity.class, ultimo.getIdPedestrian());
		
		if(pedestre == null)
			return;
		
		
		boolean ignoraRegras = getConfigurationValueAsBoolean("Ignorar regras de acesso");
		if(!ignoraRegras) {
			if(pedestre.getMensagens() != null && !pedestre.getMensagens().isEmpty())
				Utils.decrementaMensagens(pedestre.getMensagens());
			
			if(Tipo.SAIDA.equals(direction))
				Utils.decrementaCreditos(pedestre);
			
			HibernateUtil.save(PedestrianAccessEntity.class, pedestre);
	
			if(Tipo.ENTRADA.equals(direction))
				Utils.enviaSmsDeRegistro(pedestre);
		}
	}
	
	

}
