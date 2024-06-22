package com.protreino.services.devices;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import javax.swing.SwingWorker;

import com.protreino.services.entity.PedestrianAccessEntity;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.enumeration.DeviceMode;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.enumeration.DeviceType;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.enumeration.VerificationResult;
import com.protreino.services.main.Main;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.screens.PedestrianScreen;
import com.protreino.services.screens.BiometricDialog;
import com.protreino.services.to.AttachedTO;
import com.protreino.services.to.ConfigurationGroupTO;
import com.protreino.services.to.ConfigurationTO;
import com.protreino.services.utils.SelectItem;

public abstract class Device implements IDevice {
	
	private static final long serialVersionUID = 1L;
	private final Gson gson = new GsonBuilder().create();
	protected DeviceCard deviceCard;
	protected DeviceEntity deviceEntity;
	
	protected Manufacturer manufacturer;
	protected String identifier;
	protected String name;
	protected String location;
	
	protected int contador;
	protected boolean temMensagem;
	
	protected DeviceMode mode = DeviceMode.VERIFICATION;
	protected DeviceStatus status = DeviceStatus.DISCONNECTED;
	protected DeviceStatus lastStatus = DeviceStatus.DISCONNECTED;
	protected DeviceStatus desiredStatus = DeviceStatus.DISCONNECTED;
	
	protected String ip;
	protected Integer port;
	
	protected List<ConfigurationGroupTO> configurationGroups; // Lista ordenada e separada por categorias
	protected Map<String, String> configurationMap; // Map com todas as configuracoes juntas
	
	protected SwingWorker<Void, Void> worker;
	protected SwingWorker<Void, Void> watchDog;
	protected SwingWorker<Void, Void> synchronizer;
	protected boolean workerEnabled;
	protected boolean watchDogEnabled;
	protected boolean synchronizerEnabled;
	protected VerificationResult verificationResult = VerificationResult.ERROR;
	protected String allowedUserName = "";
	protected PedestrianAccessEntity matchedAthleteAccess;
	protected Long matchedFacialId;
	
	protected byte[] template;
	protected byte[] sample;
	protected boolean busy = false;
	
	protected String login;
	protected String password;
	
	protected boolean defaultDevice = false; // dispositivo padrao para as teclas de atalho
	protected boolean mirrorDevice = false; // indica que esta catraca recebe cadastros de outras catracas automaticamente
	protected boolean syncUsers = false; // sincroniza a lista de usuarios automaticamente com as catracas Henry 7X
	
	// usado apenas para leitores biometricos 
	protected BiometricDialog biometricDialog;
	protected PedestrianScreen athleteScreen; 
	protected String athleteScreenConfig = "NULL%null%false%false%false%"; // ticketGateManufacturer%ticketGateIdentifier%openAthleteScreenOnInit%fullScreenAthleteScreen%focusFieldAthleteScreen
	
	protected boolean cancelAction = false;
	
	protected Boolean tentandoConectar = false;
	
	protected Device catracaVinculada;
	
	private List<AttachedTO> attachedDevices = new ArrayList<>();
	private List<AttachedTO> attachedHikivisionCameras = new ArrayList<>();
	
	protected SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss:sss");

	public boolean coletandoDadosOffLine = false;
	
	public boolean isConnected() {
		return DeviceStatus.CONNECTED.equals(status);
	}
	
	public boolean isNotConnected() {
		return DeviceStatus.DISCONNECTED.equals(status);
	}
	
	@Override
	public void disconnect(String... args) throws Exception {
		if (worker != null) {
			worker.cancel(true);
		}
		if (watchDog != null) {
			watchDog.cancel(true);
		}
		if (synchronizer != null) {
			synchronizer.cancel(true);
		}
		watchDogEnabled = false;
		workerEnabled = false;
		synchronizerEnabled = false;
	}
	
	public  void switchMode(DeviceMode mode) {
		this.mode = mode;
	}
	
	public String toString() {
		return manufacturer + " - " + name;
	}
	
	public void setConfigurationValue(String configName, String configValue) {
		if(Objects.isNull(configurationGroups)) {
			return;
		}
		
		for (ConfigurationGroupTO configGroup : configurationGroups){
			for (ConfigurationTO config : configGroup.getConfigurations()) {
				if (config.getName().equals(configName)) {
					config.setValue(configValue);
					System.out.println("setConfigurationValue: " + configName + " = " + configValue);
				}
			}
		}
	}
	
	public String getConfigurationValue(String name) {
		try {
			if (configurationMap.containsKey(name)){
				SelectItem config = new SelectItem(configurationMap.get(name));
				return (String) config.getValue();
			}
		}
		catch (Exception e) {}
		return null;
	}
	
	public String getConfigurationValueAsString(String name) {
		try {
			if (configurationMap.containsKey(name)){
				SelectItem config = new SelectItem(configurationMap.get(name));
				return (String) config.getValue();
			}
		}
		catch (Exception e) {}
		return "";
	}
	
	public Boolean getConfigurationValueAsBoolean(String name) {
		try {
			if (configurationMap.containsKey(name)){
				SelectItem config = new SelectItem(configurationMap.get(name));
				return Boolean.valueOf((String) config.getValue());
			}
		}
		catch (Exception e) {}
		return false;
	}
	
	public Integer getConfigurationValueAsInteger(String name) {
		try {
			if (configurationMap.containsKey(name)){
				SelectItem config = new SelectItem(configurationMap.get(name));
				return Integer.valueOf((String) config.getValue());
			}
		}
		catch (Exception e) {}
		return 0;
	}
	
	public Long getConfigurationValueAsLong(String name) {
		try {
			if (configurationMap.containsKey(name)){
				SelectItem config = new SelectItem(configurationMap.get(name));
				return Long.valueOf((String) config.getValue());
			}
		}
		catch (Exception e) {}
		return 0l;
	}
	
	protected void createConfigurationMap(){
		if (configurationGroups == null)
			return;
		configurationMap = new HashMap<String, String>();
		for (ConfigurationGroupTO configGroup : configurationGroups) {
			for (ConfigurationTO config : configGroup.getConfigurations())
				configurationMap.put(config.getName(), config.getValue());
		}
	}
	
	public void allowAccess(PedestrianAccessEntity matchedAthleteAccess) {
		this.matchedAthleteAccess = matchedAthleteAccess;
		this.allowAccess();
	}
	
	public boolean isBusy(){
		return busy;
	}
	
	public boolean isLeitorBiometrico(){
		return DeviceType.BIOMETRIC_READER.equals(manufacturer.getType());
	}
	
	public boolean isCatraca(){
		return DeviceType.TICKET_GATE.equals(manufacturer.getType());
	}
	
	public boolean isCamera(){
		return DeviceType.CAMERA.equals(manufacturer.getType());
	}
	
	public Manufacturer getManufacturer() {
		return manufacturer;
	}

	public String getIdentifier() {
		return identifier;
	}

	public String getName() {
		return name;
	}
	
	public String getFullIdentifier() {
		return identifier;
	}

	public void setName(String name) {
		this.name = name;
		deviceEntity.setName(name);
		saveEntity();
	}

	public DeviceMode getMode() {
		return mode;
	}

	public String setMode(DeviceMode mode) {
		this.mode = mode;
		return "";
	}

	public DeviceStatus getStatus() {
		return status;
	}

	public void setStatus(DeviceStatus status) {
		this.lastStatus = DeviceStatus.valueOf(this.status.name());
		this.status = status;
		if (deviceCard != null) {
			deviceCard.setStatus(status);
		}
	}

	public String getIp() {
		return ip;
	}

	public void setIp(String ip) {
		this.ip = ip;
	}

	public Integer getPort() {
		return port;
	}

	public void setPort(Integer port) {
		this.port = port;
	}

	public SwingWorker<Void, Void> getWorker() {
		return worker;
	}

	public VerificationResult getVerificationResult() {
		return verificationResult;
	}

	public String getUserName() {
		return allowedUserName;
	}

	public byte[] getTemplate() {
		return template;
	}

	public byte[] getSample() {
		return sample;
	}

	public DeviceCard getDeviceCard() {
		return deviceCard;
	}

	public void setDeviceCard(DeviceCard deviceCard) {
		this.deviceCard = deviceCard;
	}

	public SwingWorker<Void, Void> getWatchDog() {
		return watchDog;
	}
	
	public SwingWorker<Void, Void> getSynchronizer() {
		return synchronizer;
	}

	public List<ConfigurationGroupTO> getConfigurationGroups() {
		return configurationGroups;
	}
	
	public void saveConfigurations(){
		deviceEntity.setConfigurationGroupsEntityFromTO(configurationGroups);
		deviceEntity.setLocation(location);
		deviceEntity.setName(name);
		
		deviceEntity.setAttachedDevices(gson.toJson(attachedDevices));
		deviceEntity.setAttachedHikivisionCameras(gson.toJson(attachedHikivisionCameras));
		
		saveEntity();
	}

	public String getLogin() {
		return login;
	}

	public void setLogin(String login) {
		this.login = login;
		deviceEntity.setLogin(login);
		saveEntity();
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
		deviceEntity.setPassword(password);
		saveEntity();
	}
	
	public void setCredentials(String login, String password){
		this.login = login;
		this.password = password;
		deviceEntity.setLogin(login);
		deviceEntity.setPassword(password);
		saveEntity();
	}

	public DeviceEntity getDeviceEntity() {
		return deviceEntity;
	}

	public void setDeviceEntity(DeviceEntity deviceEntity) {
		this.deviceEntity = deviceEntity;
	}

	public DeviceStatus getDesiredStatus() {
		return desiredStatus;
	}

	public void setDesiredStatus(DeviceStatus desiredStatus) {
		this.desiredStatus = desiredStatus;
		deviceEntity.setDesiredStatus(desiredStatus);
		saveEntity();
	}
	
	private void saveEntity(){
		deviceEntity = ((DeviceEntity) HibernateAccessDataFacade.update(DeviceEntity.class, deviceEntity)[0]);
	}

	public boolean isDefaultDevice() {
		return defaultDevice;
	}

	public void setDefaultDevice(boolean defaultDevice) {
		this.defaultDevice = defaultDevice;
		
		if (defaultDevice) {
			for (Device otherDevice : Main.devicesList){
				if (!this.equals(otherDevice)) {
					otherDevice.setDefaultDevice(false);
				}
			}
		}

		deviceEntity.setDefaultDevice(defaultDevice);
		saveEntity();
		deviceCard.setDefaultLabel();
	}

	public String getLocation() {
		return location;
	}

	public void setLocation(String location) {
		this.location = location;
		deviceEntity.setLocation(location);
		saveEntity();
	}

	public BiometricDialog getBiometricDialog() {
		return biometricDialog;
	}

	public void setBiometricDialog(BiometricDialog biometricDialog) {
		this.biometricDialog = biometricDialog;
	}
	
	public boolean isTheSame(Object obj) {
	    Device other = (Device) obj;
	    if (!this.manufacturer.equals(other.manufacturer))
	    	return false;
	    if (!this.identifier.equals(other.identifier))
	    	return false;
	    return true;
	}

	public boolean isMirrorDevice() {
		return mirrorDevice;
	}

	public void setMirrorDevice(boolean mirrorDevice) {
		this.mirrorDevice = mirrorDevice;
		deviceEntity.setMirrorDevice(mirrorDevice);
		saveEntity();
	}

	public PedestrianScreen getAthleteScreen() {
		return athleteScreen;
	}

	public void setAthleteScreen(PedestrianScreen athleteScreen) {
		this.athleteScreen = athleteScreen;
	}

	public String getAthleteScreenConfig() {
		if (athleteScreenConfig == null) {
			athleteScreenConfig = "NULL%null%false%false%false%";
		}

		return athleteScreenConfig;
	}

	public void setAthleteScreenConfig(String athleteScreenConfig) {
		this.athleteScreenConfig = athleteScreenConfig;
		deviceEntity.setAthleteScreenConfig(athleteScreenConfig);
		saveEntity();
	}

	public String getAllowedUserName() {
		return allowedUserName;
	}
	
	public boolean isRegistrationProcessStartedOnDevice(){
		return manufacturer.isRegistrationProcessStartedOnDevice();
	}

	public void setCancelAction(boolean cancelAction) {
		this.cancelAction = cancelAction;
	}

	public Boolean getTentandoConectar() {
		return tentandoConectar;
	}

	public void setTentandoConectar(Boolean tentandoConectar) {
		this.tentandoConectar = tentandoConectar;
	}

	public void setVerificationResult(VerificationResult verificationResult) {
		this.verificationResult = verificationResult;
	}

	public void setAllowedUserName(String allowedUserName) {
		this.allowedUserName = allowedUserName;
	}

	public void setMatchedAthleteAccess(PedestrianAccessEntity matchedAthleteAccess) {
		this.matchedAthleteAccess = matchedAthleteAccess;
	}

	public Device getCatracaVinculada() {
		return catracaVinculada;
	}

	public boolean isSyncUsers() {
		return syncUsers;
	}

	public void setSyncUsers(boolean syncUsers) {
		this.syncUsers = syncUsers;
		deviceEntity.setSyncUsers(syncUsers);
		saveEntity();
	}

	public List<AttachedTO> getAttachedDevices() {
		return attachedDevices;
	}

	public void setAttachedDevices(List<AttachedTO> attachedDevices) {
		this.attachedDevices = attachedDevices;
	}

	public Long getMatchedFacialId() {
		return matchedFacialId;
	}

	public void setMatchedFacialId(Long matchedFacialId) {
		this.matchedFacialId = matchedFacialId;
	}

	public PedestrianAccessEntity getMatchedAthleteAccess() {
		return matchedAthleteAccess;
	}

	public List<AttachedTO> getAttachedHikivisionCameras() {
		return attachedHikivisionCameras;
	}

	public void setAttachedHikivisionCameras(List<AttachedTO> attachedHikivisionCameras) {
		this.attachedHikivisionCameras = attachedHikivisionCameras;
	}

}
