package com.protreino.services.devices;

import java.util.List;
import java.util.Objects;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.to.ConfigurationGroupTO;
import java.util.HashSet;

import com.protreino.services.to.controlIdDevice.LoginInput;
import com.protreino.services.usecase.ControlIdDeviceService;

@SuppressWarnings("serial")
public class ControlIdFacial extends ControlIdDevice {

	protected String serverIp;
	protected String serverPort;
	private String session; // Remover static
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

	private ControlIdDeviceService controlIdDeviceService = new ControlIdDeviceService();

	public ControlIdFacial(DeviceEntity deviceEntity) {
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

	public ControlIdFacial(String identifier) {
		this(identifier, null);
	}

	public ControlIdFacial(String identifier, List<ConfigurationGroupTO> configurationGroups) {
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

	public ControlIdFacial(Integer timeout, String identifier) {
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
			if (login == null || password == null) {
				throw new Exception("Login e senha são obrigatórios.");
			}
			LoginInput body = new LoginInput(login, password);

			// Agora usando a variável de instância session
			this.session = ControlIdDeviceService.login(body, ip);
			System.out.println("Sessão recebida: " + session);

			if (Objects.isNull(session)) {
				throw new Exception("Não foi possível iniciar uma sessão.");
			}

			setStatus(DeviceStatus.CONNECTED);
		} catch (Exception e) {
			// Trate a exceção de forma adequada (se necessário)
			e.printStackTrace();
		}
	}

	@Override
	public void disconnect(String... args) throws Exception {
		ControlIdDeviceService.logout(getSession(), ip);
		session = null;
		setStatus(DeviceStatus.DISCONNECTED);
	}

	public String getSession() {
		return session; // Agora retornando a variável de instância session
	}

	public void setSession(String session) {
		this.session = session; // Atualizando a variável de instância session
	}
}
