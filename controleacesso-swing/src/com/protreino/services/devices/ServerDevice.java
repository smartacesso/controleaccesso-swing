package com.protreino.services.devices;

import java.io.BufferedInputStream;
import java.io.ObjectInputStream;
import java.net.ConnectException;
import java.net.InetAddress;
import java.net.SocketException;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.SwingWorker;

import com.google.gson.Gson;
import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.enumeration.TcpMessageType;
import com.protreino.services.to.ConfigurationGroupTO;
import com.protreino.services.to.TcpMessageTO;
import com.protreino.services.utils.HibernateUtil;
import com.protreino.services.utils.Utils;

import tcpcom.TcpClient;

public class ServerDevice extends Device {
	
	private static final long serialVersionUID = 1L;
	
	private TcpClient client;
	private int timeout = 3000;
	private Gson gson = new Gson();
	private LogPedestrianAccessEntity logAccess;
	private Map<String, LogPedestrianAccessEntity> mapLogs = new HashMap<String, LogPedestrianAccessEntity>();
	
	public ServerDevice(DeviceEntity deviceEntity) {
		this(deviceEntity.getIdentifier(), deviceEntity.getConfigurationGroupsTO());
		this.deviceEntity = deviceEntity;
		this.name = deviceEntity.getName();
		this.location = deviceEntity.getLocation();
		this.desiredStatus = deviceEntity.getDesiredStatus();
		this.defaultDevice = deviceEntity.getDefaultDevice();
	}
	
	public ServerDevice(String identifier) {
		this(identifier, null);
	}
	
	public ServerDevice(String identifier, List<ConfigurationGroupTO> configurationGroups) {
		this.manufacturer = Manufacturer.SERVER;
		this.identifier = identifier;
		String[] partes = identifier.split(";");
		this.ip = partes[0];
		this.port = Integer.valueOf(partes[1]);
		this.name = "Servidor " + ip;
		this.client = new TcpClient(ip, port, timeout);
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
	public ServerDevice(Integer timeout, String identifier) {
		String[] partes = identifier.split(";");
		this.ip = partes[0];
		this.port = Integer.valueOf(partes[1]);
		this.client = new TcpClient(ip, port, timeout);
	}

	@Override
	public void connect(String... args) throws Exception {
		try {
			HibernateUtil.openConnection();
		} catch (ConnectException e) {
			disconnect("");
			return;
		}

		if (HibernateUtil.clientSocket != null && HibernateUtil.clientSocket.isConnected()) {
			watchDogEnabled = true;
			contador = 0;
			setStatus(DeviceStatus.CONNECTED);
			
			sendConfiguration();
			
			watchDog = new SwingWorker<Void, Void>(){
				@Override
				protected synchronized Void doInBackground() throws Exception {
					
					while (watchDogEnabled) {
						try {
							contador++;
							if (contador > 2) {
								setStatus(DeviceStatus.DISCONNECTED);
								
								try {
									HibernateUtil.openConnection();
								} catch (ConnectException e) {
									disconnect("");
									return null;
								}
								
								if (HibernateUtil.clientSocket.isConnected())
									contador = 0;
							} else {
								setStatus(DeviceStatus.CONNECTED);
								
								if(!HibernateUtil.executando) {
									HibernateUtil.executandoPing = true;
									
									HibernateUtil.outToServer.writeObject(new TcpMessageTO(TcpMessageType.PING));
									HibernateUtil.outToServer.flush();
									ObjectInputStream reader = new ObjectInputStream(new BufferedInputStream(HibernateUtil.clientSocket.getInputStream()));
									TcpMessageTO resp = (TcpMessageTO) reader.readObject();
									
									if(TcpMessageType.PING_RESPONSE.equals(resp.getType())) {
										contador = 0;
									}
								} else {
									contador = 0;
								}
							}
						} catch (SocketException e) {
							contador++;
						} catch (Exception e) {
							e.printStackTrace();
		                } finally {
		                	HibernateUtil.executandoPing = false;
							Utils.sleep(10000);
						}
					}
					return null;
				}
			};
			watchDog.execute();
			
		} else {
			InetAddress inetAddress = InetAddress.getByName(ip);
			if (inetAddress.isReachable(3000))
				throw new Exception("Servidor Não responde. Verifique se o aplicativo está rodando no servidor.");
			else
				throw new Exception("Servidor Não encontrado na rede.");
		}
	}
	
	@Override
	public void disconnect(String... args) throws Exception {
		watchDogEnabled = false;
		setStatus(DeviceStatus.DISCONNECTED);
		HibernateUtil.closeConnetion();
	}
	
	/**
	 * MÃ©todo usado para varredura de ip
	 */
	private boolean quickConnect() throws Exception {
		client.connect();
		if (client.isConnected()) {
			client.disconnect();
			return true;
		}
		return false;
	}
	
	@Override
	public void createDefaultConfiguration() {
		// nao faz nada
	}

	@Override
	public void sendConfiguration() throws Exception {
		// nao faz nada
	}

	@Override
	public void allowAccess() {
		TcpMessageTO tcpMessage = new TcpMessageTO(TcpMessageType.ACCESS_REQUEST);
		String id = String.valueOf(new Date().getTime());
		tcpMessage.setId(id);
		mapLogs.put(id, logAccess);
		String mensagem = gson.toJson(tcpMessage);
		//System.out.println(sdf.format(new Date()) + "  --> Enviando mensagem: " + mensagem);
		mensagem = mensagem + "\r\n";
		client.sendData(mensagem.toCharArray());
	}

	@Override
	public void denyAccess() {
		// nao faz nada
	}
	
	@Override
	public void processSampleForEnrollment(Object obj) {
		// nao faz nada
	}

	@Override
	public void processAccessRequest(Object obj) {
		// nao faz nada
	}

	@Override
	public Set<Integer> getRegisteredUserList() throws Exception {
		return null;
	}

	@Override
	public String cadastrateUser(PedestrianAccessEntity athleteAccessEntity) {
		return null;
	}

	@Override
	public String removeUser(PedestrianAccessEntity athleteAccessEntity) {
		return null;
	}

	public LogPedestrianAccessEntity getLogAccess() {
		return logAccess;
	}

	public void setLogAccess(LogPedestrianAccessEntity logAccess) {
		this.logAccess = logAccess;
	}
}
