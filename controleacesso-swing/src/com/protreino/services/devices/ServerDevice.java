package com.protreino.services.devices;

import java.io.BufferedInputStream;
import java.io.IOException;
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
import com.protreino.services.repository.HibernateServerAccessData;
import com.protreino.services.to.ConfigurationGroupTO;
import com.protreino.services.to.TcpMessageTO;
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
	        HibernateServerAccessData.openConnection();
	    } catch (ConnectException e) {
	        disconnect("");
	        return;
	    }

	    if (HibernateServerAccessData.clientSocket != null && HibernateServerAccessData.clientSocket.isConnected()) {
	        watchDogEnabled = true;
	        contador = 0;
	        setStatus(DeviceStatus.CONNECTED);
	        
	        sendConfiguration();
	        
	        // 🔹 Inicializa o ObjectInputStream apenas uma vez
	        HibernateServerAccessData.inFromServer = new ObjectInputStream(
	            new BufferedInputStream(HibernateServerAccessData.clientSocket.getInputStream())
	        );

	        // Inicia a thread para escutar mensagens do servidor
	        Thread listenerThread = new Thread(this::listenForServerMessages);
	        listenerThread.setDaemon(true);
	        listenerThread.start();
	        
	        // Código de Watchdog permanece igual
	        watchDog = new SwingWorker<Void, Void>() {
	            @Override
	            protected synchronized Void doInBackground() throws Exception {
	                while (watchDogEnabled) {
	                    try {
	                        contador++;
	                        if (contador > 2) {
	                            setStatus(DeviceStatus.DISCONNECTED);
	                            try {
	                                HibernateServerAccessData.openConnection();
	                            } catch (ConnectException e) {
	                                disconnect("");
	                                return null;
	                            }
	                            
	                            if (HibernateServerAccessData.clientSocket.isConnected()) {
	                                contador = 0;
	                            }
	                        } else {
	                            setStatus(DeviceStatus.CONNECTED);
	                            
	                            if (!HibernateServerAccessData.executando) {
	                                HibernateServerAccessData.executandoPing = true;
	                                
	                                HibernateServerAccessData.outToServer.writeObject(new TcpMessageTO(TcpMessageType.PING));
	                                HibernateServerAccessData.outToServer.flush();

	                                // 🔹 Reutiliza o mesmo ObjectInputStream
	                                TcpMessageTO resp = (TcpMessageTO) HibernateServerAccessData.inFromServer.readObject();
	                                
	                                if (TcpMessageType.PING_RESPONSE.equals(resp.getType())) {
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
	                        HibernateServerAccessData.executandoPing = false;
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
	            throw new Exception("Servidor Nao responde. Verifique se o aplicativo está rodando no servidor.");
	        else
	            throw new Exception("Servidor Nao encontrado na rede.");
	    }
	}

	private void listenForServerMessages() {
	    try {
	        // Certifique-se de que está criando o ObjectInputStream uma vez, não a cada iteração
	        if (HibernateServerAccessData.inFromServer == null) {
	            HibernateServerAccessData.inFromServer = new ObjectInputStream(
	                new BufferedInputStream(HibernateServerAccessData.clientSocket.getInputStream())
	            );
	        }

	        while (true) {
	            Object obj = HibernateServerAccessData.inFromServer.readObject();

	            if (obj instanceof TcpMessageTO) {
	                TcpMessageTO message = (TcpMessageTO) obj;

	                System.out.println("Mensagem recebida do servidor: " + message.getType());

	                // Filtra para processar apenas a mensagem do tipo EVENTO_RECEBIDO
	                if (TcpMessageType.EVENTO_RECEBIDO.equals(message.getType())) {
	                    processServerMessage(message);
	                } else {
	                    System.out.println("Mensagem ignorada: " + message.getType());
	                }
	            } else {
	                System.out.println("Objeto inesperado recebido: " + obj);
	            }
	        }
	    } catch (IOException | ClassNotFoundException e) {
	        System.out.println("Erro ao escutar mensagens do servidor: " + e.getMessage());
	        e.printStackTrace();
	    }
	}

	private void processServerMessage(TcpMessageTO message) {
	    System.out.println("Mensagem recebida do servidor: " + message.getType());
	}
	
	@Override
	public void disconnect(String... args) throws Exception {
		watchDogEnabled = false;
		setStatus(DeviceStatus.DISCONNECTED);
		HibernateServerAccessData.closeConnetion();
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
