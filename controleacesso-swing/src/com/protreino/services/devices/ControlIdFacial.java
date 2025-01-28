package com.protreino.services.devices;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.SwingWorker;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import com.protreino.services.devices.ControlIdDevice.Action;
import com.protreino.services.devices.ControlIdDevice.Login;
import com.protreino.services.devices.ControlIdDevice.Mode;
import com.protreino.services.devices.ControlIdDevice.Response;
import com.protreino.services.devices.ControlIdDevice.Result;
import com.protreino.services.devices.ControlIdDevice.Template;
import com.protreino.services.devices.ControlIdDevice.User;
import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.devices.Device;
import com.protreino.services.devices.ControlIdDevice.Configuration;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.enumeration.DeviceMode;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.enumeration.VerificationResult;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.screens.BiometricDialog;
import com.protreino.services.screens.PedestrianScreen;
import com.protreino.services.to.AttachedTO;
import com.protreino.services.to.ConfigurationGroupTO;
import com.protreino.services.utils.HttpRequestParser;
import com.protreino.services.utils.Utils;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.Base64;
import java.util.HashSet;

import javax.imageio.ImageIO;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

import com.protreino.services.entity.PedestrianMessagesEntity;
import com.protreino.services.constants.Tipo;
import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.enumeration.FieldType;
import com.protreino.services.main.Main;
import com.protreino.services.to.ConfigurationTO;
import com.protreino.services.usecase.EnviaSmsDeRegistroUseCase;
import com.protreino.services.usecase.ProcessAccessRequestUseCase;
import com.protreino.services.usecase.ReleaseAccessUseCase;

@SuppressWarnings("serial")
public class ControlIdFacial extends ControlIdDevice{
	
//	public ControlIdFacial(String identifier) {
//		super(identifier);
//		// TODO Auto-generated constructor stub
//	}



	protected String serverIp;
	protected String serverPort;
	protected String session;
	protected String serverId;
	private Boolean digitalColetada;
	private Gson gson;
	protected Integer timeout = 5000;
	private HashSet<Long> usuariosComFoto = new HashSet<Long>();
	
	protected static final String CLOCKWISE = "clockwise";
	protected static final String ANTICLOCKWISE = "anticlockwise";
	
	protected String sentidoEntrada;
	
	protected String messagePersonalizedInDevice;	

	
	protected Boolean habilitaBeeP;
	
	public ControlIdFacial(DeviceEntity deviceEntity){
		this(deviceEntity.getIdentifier());
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
	public ControlIdFacial(String identifier){
		this(identifier, null);
	}


	public ControlIdFacial(String identifier, List<ConfigurationGroupTO> configurationGroups){
		this.manufacturer = Manufacturer.CONTROL_ID_FACIAL;
		this.identifier = identifier;
		String[] partes = identifier.split(";");
		this.ip = partes[0];
		this.serverIp = partes[1];
		this.name = "Facial Control Id - " + ip;
		this.gson = new GsonBuilder().disableHtmlEscaping().create();
		if (configurationGroups != null)
			this.configurationGroups = configurationGroups;
		else 
			createDefaultConfiguration();
		createConfigurationMap();
	}
	
	

	/**
	 * Construtor usado para varredura de ip
	 * @param ip
	 * @param port
	 */
	//
	
	public ControlIdFacial (Integer timeout, String identifier) {
		String[] partes = identifier.split(";");
		this.ip = partes[0];
		this.serverIp = partes[1];
		this.serverPort = partes[2];
		this.login = partes[3];
		this.password = partes[4];
		this.timeout = timeout;
		this.gson = new GsonBuilder().disableHtmlEscaping().create();
	}
	
	@Override
	public void connect(String... args) throws Exception {
		
		try {
			if (login == null || password == null)
				throw new Exception("Login e senha são obrigatórios.");
			
			doLogin();
			
			if (session == null)
				throw new Exception("Nao foi possivel iniciar uma sessão.");
			
			setStatus(DeviceStatus.CONNECTED);
		}catch (Exception e) {
			// TODO: handle exception
		}
		super.connect(args);
	}
	
	private void doLogin(Integer... timeout) throws Exception {
		Object[] retorno = send("http://" + ip + "/login.fcgi", new Login(login, password), timeout);
		String erro = (String) retorno[0];
		String responseString = (String) retorno[1];
		if (erro != null) {
			if ("connect timed out".equals(erro))
				throw new SocketTimeoutException();
			if (erro.contains("Service Not Available"))
				throw new Exception("Catraca Nao responde. Verifique as conexões.");
			Response response = gson.fromJson(erro, Response.class);
			if (response.code != null && response.error != null) {
				if (response.code == 1)
					erro = "Login ou senha inválidos";
				else
					erro = response.error;
			}
			else
				erro = "Nao foi possivel conectar";
			throw new Exception(erro);
		}
		if (responseString == null)
			throw new Exception("Sessão nula retornada.");
		Response response = gson.fromJson(responseString, Response.class);
		session = response.session;
		System.out.println(sdf.format(new Date()) + "  Sessao: " + response.session);
		
		//getInfo
	}
	
	private Boolean isSessionValid(int timeout) throws Exception {
		Object[] retorno = send("http://" + ip + "/session_is_valid.fcgi?session=" + session, null, timeout);
		String erro = (String) retorno[0];
		if (erro != null) {
			if (erro.contains("Service Not Available"))
				return false;
			throw new Exception(erro);
		}
		String responseString = (String) retorno[1];
		if (responseString == null)
			throw new Exception("Resposta nula.");
		Response response = gson.fromJson(responseString, Response.class);
		return response.session_is_valid;
	}

	@Override
	public void disconnect(String... args) throws Exception {
		super.disconnect();
		logout();
		session = null;
		setStatus(DeviceStatus.DISCONNECTED);
		super.disconnect(args);
	}

	@Override
	public boolean quickConnect() throws Exception {
		// TODO Auto-generated method stub
		return super.quickConnect();
	}
	
	@Override
	protected void registraGiro(Response notificacao) {
		// TODO Auto-generated method stub
		super.registraGiro(notificacao);
	}

	@Override
	public void sendConfiguration() throws Exception {
		if (configurationGroups == null || configurationGroups.isEmpty())
			return;
		
	}

	@Override
	protected void procuraSeExisteMensagemParaPedestre() {
		// TODO Auto-generated method stub
		super.procuraSeExisteMensagemParaPedestre();
	}

	@Override
	protected List<Action> allowAccessRequest() {
		// TODO Auto-generated method stub
		return super.allowAccessRequest();
	}

	@Override
	public void allowAccess() {
		// TODO Auto-generated method stub
		super.allowAccess();
	}

	@Override
	public void denyAccess() {
		// TODO Auto-generated method stub
		super.denyAccess();
	}

	@Override
	public void processSampleForEnrollment(Object obj) {
		// TODO Auto-generated method stub
		super.processSampleForEnrollment(obj);
	}

	@Override
	public void processAccessRequest(Object obj) {
		// TODO Auto-generated method stub
		super.processAccessRequest(obj);
	}

	@Override
	public void sendPhotos() {
		// TODO Auto-generated method stub
		super.sendPhotos();
	}

	@Override
	public void resetToFactory() throws Exception {
		// TODO Auto-generated method stub
		super.resetToFactory();
	}

	@Override
	public void configure() throws Exception {
		// TODO Auto-generated method stub
		super.configure();
	}

	@Override
	public void createDefaultConfiguration() {
		// TODO Auto-generated method stub
		super.createDefaultConfiguration();
	}

	@Override
	protected void logout() {
		// TODO Auto-generated method stub
		super.logout();
	}

	@Override
	protected void beep() throws Exception {
		// TODO Auto-generated method stub
		super.beep();
	}

	@Override
	protected void openGate(String side) throws Exception {
		// TODO Auto-generated method stub
		super.openGate(side);
	}

	@Override
	protected Object[] send(String endereco, Object object, Integer... timeout) {
		// TODO Auto-generated method stub
		return super.send(endereco, object, timeout);
	}

	@Override
	public Set<Integer> getRegisteredUserList() throws Exception {
		// TODO Auto-generated method stub
		return super.getRegisteredUserList();
	}

	@Override
	public String cadastrateUser(PedestrianAccessEntity athleteAccessEntity) {
		// TODO Auto-generated method stub
		return super.cadastrateUser(athleteAccessEntity);
	}

	@Override
	public String removeUser(PedestrianAccessEntity athleteAccessEntity) {
		// TODO Auto-generated method stub
		return super.removeUser(athleteAccessEntity);
	}

	@Override
	public int cadastrarUsuario(User user) throws Exception {
		// TODO Auto-generated method stub
		return super.cadastrarUsuario(user);
	}

	@Override
	public int cadastrarTemplate(Template template) throws Exception {
		// TODO Auto-generated method stub
		return super.cadastrarTemplate(template);
	}

	@Override
	public String excluirUsuario(PedestrianAccessEntity acesso, Boolean sincronizarExclusao) {
		// TODO Auto-generated method stub
		return super.excluirUsuario(acesso, sincronizarExclusao);
	}

	@Override
	public boolean isConnected() {
		// TODO Auto-generated method stub
		return super.isConnected();
	}

	@Override
	public boolean isNotConnected() {
		// TODO Auto-generated method stub
		return super.isNotConnected();
	}

	@Override
	public void switchMode(DeviceMode mode) {
		// TODO Auto-generated method stub
		super.switchMode(mode);
	}

	@Override
	public String toString() {
		// TODO Auto-generated method stub
		return super.toString();
	}

	@Override
	public void setConfigurationValue(String configName, String configValue) {
		// TODO Auto-generated method stub
		super.setConfigurationValue(configName, configValue);
	}

	@Override
	public String getConfigurationValue(String name) {
		// TODO Auto-generated method stub
		return super.getConfigurationValue(name);
	}

	@Override
	public String getConfigurationValueAsString(String name) {
		// TODO Auto-generated method stub
		return super.getConfigurationValueAsString(name);
	}

	@Override
	public Boolean getConfigurationValueAsBoolean(String name) {
		// TODO Auto-generated method stub
		return super.getConfigurationValueAsBoolean(name);
	}

	@Override
	public Integer getConfigurationValueAsInteger(String name) {
		// TODO Auto-generated method stub
		return super.getConfigurationValueAsInteger(name);
	}

	@Override
	public Long getConfigurationValueAsLong(String name) {
		// TODO Auto-generated method stub
		return super.getConfigurationValueAsLong(name);
	}

	@Override
	protected void createConfigurationMap() {
		// TODO Auto-generated method stub
		super.createConfigurationMap();
	}

	@Override
	public void allowAccess(PedestrianAccessEntity matchedAthleteAccess) {
		// TODO Auto-generated method stub
		super.allowAccess(matchedAthleteAccess);
	}

	@Override
	public boolean isBusy() {
		// TODO Auto-generated method stub
		return super.isBusy();
	}

	@Override
	public boolean isLeitorBiometrico() {
		// TODO Auto-generated method stub
		return super.isLeitorBiometrico();
	}

	@Override
	public boolean isCatraca() {
		// TODO Auto-generated method stub
		return super.isCatraca();
	}

	@Override
	public boolean isCamera() {
		// TODO Auto-generated method stub
		return super.isCamera();
	}

	@Override
	public Manufacturer getManufacturer() {
		// TODO Auto-generated method stub
		return super.getManufacturer();
	}

	@Override
	public String getIdentifier() {
		// TODO Auto-generated method stub
		return super.getIdentifier();
	}

	@Override
	public String getName() {
		// TODO Auto-generated method stub
		return super.getName();
	}

	@Override
	public String getFullIdentifier() {
		// TODO Auto-generated method stub
		return super.getFullIdentifier();
	}

	@Override
	public void setName(String name) {
		// TODO Auto-generated method stub
		super.setName(name);
	}

	@Override
	public DeviceMode getMode() {
		// TODO Auto-generated method stub
		return super.getMode();
	}

	@Override
	public String setMode(DeviceMode mode) {
		// TODO Auto-generated method stub
		return super.setMode(mode);
	}

	@Override
	public DeviceStatus getStatus() {
		// TODO Auto-generated method stub
		return super.getStatus();
	}

	@Override
	public void setStatus(DeviceStatus status) {
		// TODO Auto-generated method stub
		super.setStatus(status);
	}

	@Override
	public String getIp() {
		// TODO Auto-generated method stub
		return super.getIp();
	}

	@Override
	public void setIp(String ip) {
		// TODO Auto-generated method stub
		super.setIp(ip);
	}

	@Override
	public Integer getPort() {
		// TODO Auto-generated method stub
		return super.getPort();
	}

	@Override
	public void setPort(Integer port) {
		// TODO Auto-generated method stub
		super.setPort(port);
	}

	@Override
	public SwingWorker<Void, Void> getWorker() {
		// TODO Auto-generated method stub
		return super.getWorker();
	}

	@Override
	public VerificationResult getVerificationResult() {
		// TODO Auto-generated method stub
		return super.getVerificationResult();
	}

	@Override
	public String getUserName() {
		// TODO Auto-generated method stub
		return super.getUserName();
	}

	@Override
	public byte[] getTemplate() {
		// TODO Auto-generated method stub
		return super.getTemplate();
	}

	@Override
	public byte[] getSample() {
		// TODO Auto-generated method stub
		return super.getSample();
	}

	@Override
	public DeviceCard getDeviceCard() {
		// TODO Auto-generated method stub
		return super.getDeviceCard();
	}

	@Override
	public void setDeviceCard(DeviceCard deviceCard) {
		// TODO Auto-generated method stub
		super.setDeviceCard(deviceCard);
	}

	@Override
	public SwingWorker<Void, Void> getWatchDog() {
		// TODO Auto-generated method stub
		return super.getWatchDog();
	}

	@Override
	public SwingWorker<Void, Void> getSynchronizer() {
		// TODO Auto-generated method stub
		return super.getSynchronizer();
	}

	@Override
	public List<ConfigurationGroupTO> getConfigurationGroups() {
		// TODO Auto-generated method stub
		return super.getConfigurationGroups();
	}

	@Override
	public void saveConfigurations() {
		// TODO Auto-generated method stub
		super.saveConfigurations();
	}

	@Override
	public String getLogin() {
		// TODO Auto-generated method stub
		return super.getLogin();
	}

	@Override
	public void setLogin(String login) {
		// TODO Auto-generated method stub
		super.setLogin(login);
	}

	@Override
	public String getPassword() {
		// TODO Auto-generated method stub
		return super.getPassword();
	}

	@Override
	public void setPassword(String password) {
		// TODO Auto-generated method stub
		super.setPassword(password);
	}

	@Override
	public void setCredentials(String login, String password) {
		// TODO Auto-generated method stub
		super.setCredentials(login, password);
	}

	@SuppressWarnings("unused")
	protected class Login{
		private String login = "admin";
		private String password = "admin";
		
		public Login(String login, String password){
			this.login = login;
			this.password = password;
		}
	}	
	
	@Override
	public DeviceEntity getDeviceEntity() {
		// TODO Auto-generated method stub
		
		
		return super.getDeviceEntity();
	}

	@Override
	public void setDeviceEntity(DeviceEntity deviceEntity) {
		// TODO Auto-generated method stub
		super.setDeviceEntity(deviceEntity);
	}

	@Override
	public DeviceStatus getDesiredStatus() {
		// TODO Auto-generated method stub
		return super.getDesiredStatus();
	}

	@Override
	public void setDesiredStatus(DeviceStatus desiredStatus) {
		// TODO Auto-generated method stub
		super.setDesiredStatus(desiredStatus);
	}

	@Override
	public boolean isDefaultDevice() {
		// TODO Auto-generated method stub
		return super.isDefaultDevice();
	}

	@Override
	public void setDefaultDevice(boolean defaultDevice) {
		// TODO Auto-generated method stub
		super.setDefaultDevice(defaultDevice);
	}

	@Override
	public String getLocation() {
		// TODO Auto-generated method stub
		return super.getLocation();
	}

	@Override
	public void setLocation(String location) {
		// TODO Auto-generated method stub
		super.setLocation(location);
	}

	@Override
	public BiometricDialog getBiometricDialog() {
		// TODO Auto-generated method stub
		return super.getBiometricDialog();
	}

	@Override
	public void setBiometricDialog(BiometricDialog biometricDialog) {
		// TODO Auto-generated method stub
		super.setBiometricDialog(biometricDialog);
	}

	@Override
	public boolean isTheSame(Object obj) {
		// TODO Auto-generated method stub
		return super.isTheSame(obj);
	}

	@Override
	public boolean isMirrorDevice() {
		// TODO Auto-generated method stub
		return super.isMirrorDevice();
	}

	@Override
	public void setMirrorDevice(boolean mirrorDevice) {
		// TODO Auto-generated method stub
		super.setMirrorDevice(mirrorDevice);
	}

	@Override
	public PedestrianScreen getAthleteScreen() {
		// TODO Auto-generated method stub
		return super.getAthleteScreen();
	}

	@Override
	public void setAthleteScreen(PedestrianScreen athleteScreen) {
		// TODO Auto-generated method stub
		super.setAthleteScreen(athleteScreen);
	}

	@Override
	public String getAthleteScreenConfig() {
		// TODO Auto-generated method stub
		return super.getAthleteScreenConfig();
	}

	@Override
	public void setAthleteScreenConfig(String athleteScreenConfig) {
		// TODO Auto-generated method stub
		super.setAthleteScreenConfig(athleteScreenConfig);
	}

	@Override
	public String getAllowedUserName() {
		// TODO Auto-generated method stub
		return super.getAllowedUserName();
	}

	@Override
	public boolean isRegistrationProcessStartedOnDevice() {
		// TODO Auto-generated method stub
		return super.isRegistrationProcessStartedOnDevice();
	}

	@Override
	public void setCancelAction(boolean cancelAction) {
		// TODO Auto-generated method stub
		super.setCancelAction(cancelAction);
	}

	@Override
	public Boolean getTentandoConectar() {
		// TODO Auto-generated method stub
		return super.getTentandoConectar();
	}

	@Override
	public void setTentandoConectar(Boolean tentandoConectar) {
		// TODO Auto-generated method stub
		super.setTentandoConectar(tentandoConectar);
	}

	@Override
	public void setVerificationResult(VerificationResult verificationResult) {
		// TODO Auto-generated method stub
		super.setVerificationResult(verificationResult);
	}

	@Override
	public void setAllowedUserName(String allowedUserName) {
		// TODO Auto-generated method stub
		super.setAllowedUserName(allowedUserName);
	}

	@Override
	public void setMatchedAthleteAccess(PedestrianAccessEntity matchedAthleteAccess) {
		// TODO Auto-generated method stub
		super.setMatchedAthleteAccess(matchedAthleteAccess);
	}

	@Override
	public boolean isSyncUsers() {
		// TODO Auto-generated method stub
		return super.isSyncUsers();
	}

	@Override
	public void setSyncUsers(boolean syncUsers) {
		// TODO Auto-generated method stub
		super.setSyncUsers(syncUsers);
	}

	@Override
	public List<AttachedTO> getAttachedDevices() {
		// TODO Auto-generated method stub
		return super.getAttachedDevices();
	}

	@Override
	public void setAttachedDevices(List<AttachedTO> attachedDevices) {
		// TODO Auto-generated method stub
		super.setAttachedDevices(attachedDevices);
	}

	@Override
	public Long getMatchedFacialId() {
		// TODO Auto-generated method stub
		return super.getMatchedFacialId();
	}

	@Override
	public void setMatchedFacialId(Long matchedFacialId) {
		// TODO Auto-generated method stub
		super.setMatchedFacialId(matchedFacialId);
	}

	@Override
	public PedestrianAccessEntity getMatchedAthleteAccess() {
		// TODO Auto-generated method stub
		return super.getMatchedAthleteAccess();
	}

}
