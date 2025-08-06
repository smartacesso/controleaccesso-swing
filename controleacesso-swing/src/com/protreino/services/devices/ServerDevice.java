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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.gson.Gson;
import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.enumeration.TcpMessageType;
import com.protreino.services.main.Main;
import com.protreino.services.repository.HibernateServerAccessData;
import com.protreino.services.to.AttachedTO;
import com.protreino.services.to.ConfigurationGroupTO;
import com.protreino.services.to.TcpMessageTO;
import com.protreino.services.utils.Utils;

import tcpcom.TcpClient;

import javax.swing.SwingWorker;

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
	
	public ServerDevice(Integer timeout, String identifier) {
		String[] partes = identifier.split(";");
		this.ip = partes[0];
		this.port = Integer.valueOf(partes[1]);
		this.client = new TcpClient(ip, port, timeout);
	}
	/*
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
			startEventListener();
			
			
		} else {
			InetAddress inetAddress = InetAddress.getByName(ip);
			if (inetAddress.isReachable(3000))
				throw new Exception("Servidor não responde. Verifique se o aplicativo está rodando no servidor.");
			else
				throw new Exception("Servidor não encontrado na rede.");
		}
	}
	*/
	
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
			
			// Inicia a thread de escuta de mensagens
			if(Utils.isHabilitadoReceberEventos()) {
				startEventListener();
			}
			
			watchDog = new SwingWorker<Void, Void>(){
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
								
								if (HibernateServerAccessData.clientSocket.isConnected())
									contador = 0;
							} else {
								setStatus(DeviceStatus.CONNECTED);
								
								if(!HibernateServerAccessData.executando) {
									HibernateServerAccessData.executandoPing = true;
									
									HibernateServerAccessData.outToServer.writeObject(new TcpMessageTO(TcpMessageType.PING));
									HibernateServerAccessData.outToServer.flush();
									ObjectInputStream reader = new ObjectInputStream(new BufferedInputStream(HibernateServerAccessData.clientSocket.getInputStream()));
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

	private void startEventListener() {
	    System.out.println("Iniciando listener de evento hiki");

	    messageListenerThread = new Thread(() -> {
	        try {
	            BufferedReader reader = new BufferedReader(new InputStreamReader(
	                    HibernateServerAccessData.clientSocketEventos.getInputStream())); //cliente socket eventos

	            ObjectMapper objectMapper = new ObjectMapper(); // Jackson para desserialização

	            while (watchDogEnabled) {
	                try {
	                    if (reader.ready()) {
	                        String line = reader.readLine();

	                        if (line == null) {
	                            System.out.println("Conexão fechada pelo servidor.");
	                            break;
	                        }

	                        line = line.trim();

	                        if (line.isEmpty()) {
	                            continue;
	                        }

	                        try {
	                            // Convertendo JSON para objeto
	                            TcpMessageTO message = objectMapper.readValue(line, TcpMessageTO.class);
	                            System.out.println("Mensagem recebida: " + message);
	                            processarMensagem(message);
	                        } catch (Exception e) {
	                            System.out.println("Erro ao processar JSON: " + e.getMessage());
	                        }
	                    } else {
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

	

	private void processarMensagem(TcpMessageTO message) {
	    try {
	        // Extraindo os valores diretamente do objeto TcpMessageTO
	        String cardNumber = (String) message.getParans().get("card");
	        String deviceId = (String) message.getParans().get("facial");

	        List<Device> devicesList = Main.devicesList;
	        Device device = devicesList.get(0);
	        List<AttachedTO> attachedHikivisionCameras = device.getAttachedHikivisionCameras();

	        for (AttachedTO attachedTO : attachedHikivisionCameras) {
	            if (attachedTO.getIdDevice().equalsIgnoreCase(deviceId)) {
	                athleteScreen.requisicaoHivisionServer(cardNumber, 5000);
	            }
	        }

	    } catch (Exception e) {
	        System.out.println("Erro ao processar mensagem: " + e.getMessage());
	    }
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
