package com.protreino.services.devices;

import java.io.*;
import java.net.ConnectException;
import java.net.InetAddress;
import java.net.SocketException;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;

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
	int count = 1;
	
	private static final long serialVersionUID = 1L;
	private TcpClient client;
	private int timeout = 3000;
	private Gson gson = new Gson();
	private LogPedestrianAccessEntity logAccess;
	private Map<String, LogPedestrianAccessEntity> mapLogs = new HashMap<>();
	private Thread messageListenerThread;
	private boolean watchDogEnabled = false;

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
		this.port = Integer.parseInt(partes[1]);
		this.name = "Servidor " + ip;
		this.client = new TcpClient(ip, port, timeout);
		if (configurationGroups != null)
			this.configurationGroups = configurationGroups;
		else
			createDefaultConfiguration();
		createConfigurationMap();
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
			setStatus(DeviceStatus.CONNECTED);
			sendConfiguration();

			// Inicia a thread de escuta de mensagens
			startMessageListener();
			
			
		} else {
			InetAddress inetAddress = InetAddress.getByName(ip);
			if (inetAddress.isReachable(3000))
				throw new Exception("Servidor não responde. Verifique se o aplicativo está rodando no servidor.");
			else
				throw new Exception("Servidor não encontrado na rede.");
		}
	}

	/**
	 * Thread dedicada para ouvir mensagens do servidor.
	 */
	private void startMessageListener() {
	    System.out.println("Iniciando listener de evento hiki");

	    messageListenerThread = new Thread(() -> {
	        try {
	            BufferedReader reader = new BufferedReader(new InputStreamReader(
	                    HibernateServerAccessData.clientSocket.getInputStream()));

	            while (watchDogEnabled) {
	                try {
	                    // 🔹 Verifica se há dados antes de ler para evitar travamentos
	                    if (reader.ready()) {
	                        String line = reader.readLine();
	                        if (line == null) {
	                            System.out.println("Conexão fechada pelo servidor.");
	                            break;
	                        }

	                        System.out.println("Recebido: " + line);

	                        // 🔹 Processar apenas mensagens formatadas corretamente
	                        if (line.contains("CARD_NUMBER") && line.contains("DEVICE_ID")) {
	                            processarMensagem(line);
	                        } else {
	                            System.out.println("⚠ Mensagem ignorada: formato inválido.");
	                        }
	                    } else {
	                        // 🔹 Pequena pausa para evitar uso excessivo da CPU
	                        Thread.sleep(100);
	                    }
	                } catch (IOException e) {
	                    System.out.println("Erro na leitura da mensagem: " + e.getMessage());
	                } catch (Exception e) {
	                    System.out.println("Erro inesperado no listener: " + e.getMessage());
	                    e.printStackTrace();
	                }
	            }
	        } catch (IOException e) {
	            System.out.println("Erro na thread de leitura: " + e.getMessage());
	        }
	    });

	    messageListenerThread.start();
	}

	private void processarMensagem(String line) {
	    try {
	        String[] partes = line.split(";");
	        Map<String, String> params = new HashMap<>();

	        for (String parte : partes) {
	            String[] keyValue = parte.split("=");
	            if (keyValue.length == 2) {
	                params.put(keyValue[0].trim(), keyValue[1].trim());
	            }
	        }

	        String cardNumber = params.get("CARD_NUMBER");
	        String deviceId = params.get("DEVICE_ID");
	        String catracaInner = params.get("CATRACA_INNER");

	        System.out.println("Evento processado:");
	        System.out.println("CARD_NUMBER: " + cardNumber);
	        System.out.println("DEVICE_ID: " + deviceId);
	        System.out.println("CATRACA_INNER: " + catracaInner);
	    } catch (Exception e) {
	        System.out.println("⚠ Erro ao processar mensagem: " + e.getMessage());
	    }
	}


	@Override
	public void disconnect(String... args) throws Exception {
		watchDogEnabled = false;
		if (messageListenerThread != null) {
			messageListenerThread.interrupt();
		}
		setStatus(DeviceStatus.DISCONNECTED);
		HibernateServerAccessData.closeConnetion();
		System.out.println("Desconectado.");
	}

	@Override
	public void createDefaultConfiguration() {
		// Não faz nada
	}

	@Override
	public void sendConfiguration() throws Exception {
		// Não faz nada
	}

	@Override
	public void allowAccess() {
		TcpMessageTO tcpMessage = new TcpMessageTO(TcpMessageType.ACCESS_REQUEST);
		String id = String.valueOf(new Date().getTime());
		tcpMessage.setId(id);
		mapLogs.put(id, logAccess);
		String mensagem = gson.toJson(tcpMessage);
		mensagem = mensagem + "\r\n";
		client.sendData(mensagem.toCharArray());
	}

	@Override
	public void denyAccess() {
		// Não faz nada
	}

	@Override
	public void processSampleForEnrollment(Object obj) {
		// Não faz nada
	}

	@Override
	public void processAccessRequest(Object obj) {
		// Não faz nada
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
