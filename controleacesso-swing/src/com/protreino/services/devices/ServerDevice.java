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
	
	private static final long serialVersionUID = 1L;
	private TcpClient client;
	private int timeout = 3000;
	private Gson gson = new Gson();
	private LogPedestrianAccessEntity logAccess;
	private Map<String, LogPedestrianAccessEntity> mapLogs = new HashMap<>();
	private Thread messageListenerThread;
	private AtomicBoolean watchDogEnabled = new AtomicBoolean(false);

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
			watchDogEnabled.set(true);
			setStatus(DeviceStatus.CONNECTED);
			sendConfiguration();

			// Inicia a thread de escuta de mensagens
			//startMessageListener();

			// Inicia a thread do watchdog para envio de PINGs
			new Thread(this::watchDogTask).start();
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
	    messageListenerThread = new Thread(() -> {
	        try {
	            BufferedReader reader = new BufferedReader(new InputStreamReader(
	                    HibernateServerAccessData.clientSocket.getInputStream()));

	            while (watchDogEnabled.get()) {
	                String line = reader.readLine();
	                if (line == null) {
	                    System.out.println("Conexão fechada pelo servidor.");
	                    break;
	                }

	                System.out.println("Recebido: " + line);

	                // Verifique a estrutura completa da mensagem
	                if (line.contains("EVENTO_HIKIVISION")) {
	                    // Tente converter ou fazer uma análise mais detalhada aqui
	                    try {
	                        // Supondo que TcpMessageTO é um objeto serializado, tente convertê-lo
	                        ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(line.getBytes());
	                        ObjectInputStream objectInputStream = new ObjectInputStream(byteArrayInputStream);
	                        TcpMessageTO resp = (TcpMessageTO) objectInputStream.readObject();

	                        // Verifique se o tipo é o esperado
	                        if (resp != null && resp.getType() == TcpMessageType.EVENTO_HIKIVISION) {
	                            // Extraia os parâmetros necessários
	                            Map<String, Object> params = resp.getParans();
	                            String cameraId = (String) params.get("cameraId");
	                            String cardNumber = (String) params.get("cardNumber");

	                            if (cameraId != null && cardNumber != null) {
	                                System.out.println("Evento Hikvision recebido:");
	                                System.out.println("cameraId: " + cameraId);
	                                System.out.println("cardNumber: " + cardNumber);
	                            } else {
	                                System.out.println("Evento Hikvision ignorado: falta 'cameraId' ou 'cardNumber'.");
	                            }
	                        } else {
	                            System.out.println("Mensagem ignorada: Tipo de mensagem não é EVENTO_HIKIVISION.");
	                        }
	                    } catch (IOException | ClassNotFoundException e) {
	                        System.out.println("Erro ao desserializar a mensagem: " + e.getMessage());
	                    }
	                } else {
	                    System.out.println("Mensagem ignorada: Não contém 'EVENTO_HIKIVISION'.");
	                }
	            }
	        } catch (IOException e) {
	            System.out.println("Erro na thread de leitura: " + e.getMessage());
	        }
	    });

	    messageListenerThread.start();
	}


	/**
	 * Thread do watchdog que envia PINGs ao servidor periodicamente.
	 */
	private void watchDogTask() {
		while (watchDogEnabled.get()) {
			try {
				setStatus(DeviceStatus.CONNECTED);

				if (!HibernateServerAccessData.executando) {
					HibernateServerAccessData.executandoPing = true;

					// Enviando PING como JSON
					TcpMessageTO pingMessage = new TcpMessageTO(TcpMessageType.PING);
					HibernateServerAccessData.outToServer.writeObject(pingMessage);
					HibernateServerAccessData.outToServer.flush();

					System.out.println("PING enviado ao servidor.");
				}
			} catch (SocketException e) {
				System.out.println("Erro de conexão, tentando reconectar...");
			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				HibernateServerAccessData.executandoPing = false;
				Utils.sleep(10000); // Aguarda 10 segundos antes de enviar o próximo PING
			}
		}
	}

	@Override
	public void disconnect(String... args) throws Exception {
		watchDogEnabled.set(false);
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
