package com.protreino.services.utils;

import java.io.IOException;
import java.lang.reflect.Type;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Date;
import java.util.Enumeration;
import java.util.List;
import java.util.Objects;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonSerializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;
import com.google.gson.JsonPrimitive;
import com.google.gson.JsonSerializationContext;
import com.protreino.services.devices.Device;
import com.protreino.services.devices.TopDataDevice;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.entity.TemplateEntity;
import com.protreino.services.enumeration.BroadcastMessageType;
import com.protreino.services.main.Main;
import com.protreino.services.to.BroadcastMessageTO;

public class BroadcastServer {
	
	private int porta;
	private String lastHeader;
	private Gson gson = new GsonBuilder()
		.registerTypeAdapter(Date.class, new JsonDeserializer<Date>() { 
			public Date deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context) throws JsonParseException {
				return new Date(json.getAsJsonPrimitive().getAsLong()); 
			}
		})
		.registerTypeAdapter(Date.class, new JsonSerializer<Date>() { 
			public JsonElement serialize(Date date, Type typeOfT, JsonSerializationContext context) {
				return context.serialize(date.getTime());
			}
		})
		.registerTypeHierarchyAdapter(byte[].class, new ByteArrayToBase64TypeAdapter()).create();
	
	private SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss:sss");
	
	public BroadcastServer() {
		this.porta = Integer.valueOf(Utils.getPreference("broadcastServerSocketPort"));
		
		Thread serverThread = new Thread(new Runnable() {
			@Override
			public void run() {
				while (true) {
					byte[] receiveData = new byte[8192];
					DatagramPacket receivePacket = new DatagramPacket(receiveData, receiveData.length);

					try (DatagramSocket serverSocket = new DatagramSocket(porta)) {
						serverSocket.receive(receivePacket);
						String mensagem = new String(receivePacket.getData(), StandardCharsets.UTF_8).trim();
						if (mensagem.startsWith("PRO-TREINO") 
								&& (lastHeader == null || !mensagem.startsWith(lastHeader))) {
							new ProcessThread(mensagem).start();
						}
					} catch (IOException ex) {
						System.out.println(sdf.format(new Date()) + "  Server exception: " + ex.getMessage());
						ex.printStackTrace();
					}
				}
			}
		});
		serverThread.start();
		System.out.println(sdf.format(new Date()) + "  ... Broadcast server escutando na porta " + porta);
	}
	
	public class ProcessThread extends Thread {
		private String mensagem;
		
		public ProcessThread(String mensagem) {
			this.mensagem = mensagem;
		}
		
		public void run() {
			try {
				mensagem = mensagem.substring(mensagem.indexOf(":")+1);
				System.out.println(sdf.format(new Date()) + "  ... Broadcast server processando mensagem: " + mensagem);
				BroadcastMessageTO broadcastMessage = gson.fromJson(mensagem, BroadcastMessageTO.class);

				if (BroadcastMessageType.LOG_ACCESS.equals(broadcastMessage.getType())) {
					if(Main.temServidor()) {
						System.out.println(sdf.format(new Date()) + ": REGISTRAR LOG ACESSO");
					}

				} else if (BroadcastMessageType.REMOVE_TEMPLATES.equals(broadcastMessage.getType())) {
					
					if(Main.temServidor()) {
						System.out.println(sdf.format(new Date()) + ": REMOVE TEMPLATES");
					}
					
					removeCatracas(broadcastMessage);

				} else if (BroadcastMessageType.NEW_TEMPLATE.equals(broadcastMessage.getType())) {
					System.out.println(sdf.format(new Date()) + ": NOVO TEMPLATE");
					atualizaCatracas(broadcastMessage);
				} else if (BroadcastMessageType.REFRESH_TEMPLATES.equals(broadcastMessage.getType())) {
					System.out.println(sdf.format(new Date()) + ": REFRESH TEMPLATES");
					atualizaCatracas(broadcastMessage);
					
				}
				
				//System.out.println(sdf.format(new Date()) + "  ... Broadcast server mensagem processada!");
			} catch (Exception ex) {
				System.out.println(sdf.format(new Date()) + "  ... Broadcast server processing exception: " + ex.getMessage());
				ex.printStackTrace();
			}
		}

		private void removeCatracas(BroadcastMessageTO broadcastMessage) {
			//procura catraca para remover template caso 
			//a configuracoes seja para remoção diretamente na mesma
			for (Device d : Main.devicesList) {
				if(d instanceof TopDataDevice
						&& d.isConnected()
						&& d.isSyncUsers()
						&& broadcastMessage != null) {
					try {
						PedestrianAccessEntity p = new PedestrianAccessEntity();
						p.setId(broadcastMessage.getIdPedestrianAccess());
						
						TopDataDevice topData = (TopDataDevice) d;
						topData.removeDigitalInner(true, p);
					}catch (Exception e) {
						e.printStackTrace();
					}
				}
			}
		}

		private void atualizaCatracas(BroadcastMessageTO broadcastMessage) throws InterruptedException {
			for(Device d : Main.devicesList) {
				if(d != null && d instanceof TopDataDevice) {
					//verifica se pode adicionar
					TopDataDevice topData = (TopDataDevice) d;
					if(topData.getIndexSearchEngine() != null) {
						System.out.println(sdf.format(new Date()) + 
								"   Atualizando templates para indexSearch do Inner "+topData.getInnerNumber()+"...");
						topData.restartIndexSearchEngine();
						Thread.sleep(1000);
					}else if(topData.isConnected()
							&& d.isSyncUsers()
							&& broadcastMessage != null){
						try {
							if(broadcastMessage.getIdPedestrianAccess() != null) {
								//atualiza uma digital
								PedestrianAccessEntity p = new PedestrianAccessEntity();
								p.setId(broadcastMessage.getIdPedestrianAccess());
								topData.insereDigitalInner(true, p);
							} else if(Main.temServidor()) {
								//atualiza catraca inteira somente se tiver um servidor
								topData.atualizaDigitaisLFD(true, false, null);
							}

						} catch (Exception e) {
							e.printStackTrace();
						}
					}
				}
			}
		}

		private void adicionaTemplateCatracas(TemplateEntity template) throws InterruptedException {
			//adicionar tambem nas outras catracas conectadas
			for(Device d : Main.devicesList) {
				if(d != null && d instanceof TopDataDevice) {
					//verifica se pode adicionar
					TopDataDevice topData = (TopDataDevice) d;
					if(topData.getIndexSearchEngine() != null) {
						System.out.println(sdf.format(new Date()) + 
								"   Enviando template para indexSearch do Inner "+topData.getInnerNumber()+"...");
						topData.addTemplateToIndexSearch(template);
						Thread.sleep(1000);
					}
				}
			}
		}
	}

	public void sendMessage(BroadcastMessageTO broadcastMessage) {
		try {
			sendMessage(gson.toJson(broadcastMessage));

		} catch (Exception e){
			e.printStackTrace();
		}
	}

	public void sendMessage(String mensagem) {
		try {
			lastHeader = "PRO-TREINO_" + String.valueOf(System.nanoTime());
			mensagem = lastHeader + ":" + mensagem;
			System.out.println(sdf.format(new Date()) + "  --> Enviando mensagem: " + mensagem);
			List<InetAddress> broadcastAddresses = listAllBroadcastAddresses();
			
			if (broadcastAddresses != null && !broadcastAddresses.isEmpty()) {
				for (InetAddress address : broadcastAddresses) {
					DatagramSocket socket = new DatagramSocket();
					socket.setBroadcast(true);
					byte[] buffer = mensagem.getBytes("UTF-8");
					DatagramPacket packet = new DatagramPacket(buffer, buffer.length, address, porta);
					socket.send(packet);
					socket.close();
				}
			}
		
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private List<InetAddress> listAllBroadcastAddresses() throws SocketException {
		List<InetAddress> broadcastList = new ArrayList<>();
		Enumeration<NetworkInterface> interfaces = NetworkInterface.getNetworkInterfaces();

		while (interfaces.hasMoreElements()) {
			NetworkInterface networkInterface = interfaces.nextElement();

			if (networkInterface.isLoopback() || !networkInterface.isUp()) {
				continue;
			}
			
			networkInterface.getInterfaceAddresses().stream() 
				.map(a -> a.getBroadcast())
				.filter(Objects::nonNull)
				.forEach(broadcastList::add);
	    }
		return broadcastList;
	}
	
	private static class ByteArrayToBase64TypeAdapter implements JsonSerializer<byte[]>, JsonDeserializer<byte[]> {
		public byte[] deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context) throws JsonParseException {
			return Base64.getDecoder().decode(json.getAsString());
		}

		public JsonElement serialize(byte[] src, Type typeOfSrc, JsonSerializationContext context) {
			return new JsonPrimitive(Base64.getEncoder().encodeToString(src));
		}
	}
	
}
