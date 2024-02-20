package com.protreino.services.utils;

import java.io.BufferedReader;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.lang.reflect.Type;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.HashMap;
import java.util.Objects;
import java.util.TimeZone;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;
import com.protreino.services.constants.Tipo;
import com.protreino.services.devices.Device;
import com.protreino.services.devices.TopDataDevice;
import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.main.Main;
import com.protreino.services.to.AttachedTO;
import com.protreino.services.to.hikivision.EventListnerTO;

public class HikivisionTcpServer {

	private int porta;

	private static final SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss:sss");
	private static final SimpleDateFormat sdf2 = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXXX");
	private static final SimpleDateFormat responseDateFormat = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss 'GMT'");
	private static Gson gson;

	public HikivisionTcpServer() {
		this.porta = Integer.valueOf(Utils.getPreference("tcpServerHikivisionSocketPort"));
		gson = new GsonBuilder().registerTypeAdapter(Date.class, new JsonDeserializer<Date>() {
			public Date deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context)
					throws JsonParseException {
				try {
					return sdf2.parse(json.getAsString());
				} catch (Exception e) {
				}

				return null;
			}
		}).create();

		Thread serverThread = new Thread(new Runnable() {
			@Override
			public void run() {
				try (ServerSocket serverSocket = new ServerSocket(porta)) {
					System.out.println(
							sdf.format(new Date()) + "  ... Hikivision TCP server escutando na porta " + porta);

					while (true) {
						Socket socket = serverSocket.accept();
						socket.setTcpNoDelay(true);
						new ProcessThread(socket).start();
					}

				} catch (IOException ex) {
					System.out.println(
							sdf.format(new Date()) + "  ... Hikivision TCP server exception: " + ex.getMessage());
					ex.printStackTrace();
				}
			}
		});
		serverThread.start();
	}

	public class ProcessThread extends Thread {
		private Socket socket;

		public ProcessThread(Socket socket) {
			this.socket = socket;
		}

		public void run() {
			try (InputStream inputStream = socket.getInputStream();
					OutputStream outputStream = socket.getOutputStream();
					BufferedReader br = new BufferedReader(new InputStreamReader(inputStream))) {

				final StringBuilder sb = new StringBuilder();
				try {
					String line = "";
					while (!line.equals("--MIME_boundary--")) {
						line = br.readLine();
						sb.append(line);
					}
				} catch (IOException e) {
					e.printStackTrace();
				}

				final String requestBody = sb.toString();
				final String objectPayload = Utils.getFirstJsonFromString(requestBody);
				final String hikivisionCameraId = sb.toString().substring(requestBody.indexOf("/") + 1, requestBody.indexOf(" HTTP"));
				final EventListnerTO eventListnerTO = gson.fromJson(objectPayload, EventListnerTO.class);

				sendResponse(outputStream);

				if (Objects.nonNull(eventListnerTO) && Objects.nonNull(eventListnerTO.getAccessControllerEvent())
						&& Objects.nonNull(eventListnerTO.getAccessControllerEvent().getCardNo())) {
					final OffsetDateTime offsetDateTime = getOffsetDateTime(eventListnerTO.getDateTime());
					final OffsetDateTime secondsAgo = OffsetDateTime.from(ZonedDateTime.ofInstant(Instant.now(), ZoneId.systemDefault()).minusSeconds(20));
					
					final TopDataDevice attachedDevice = getAttachedDevice(hikivisionCameraId);

					if(Objects.isNull(attachedDevice)) {
						System.out.println("Sem catraca vinculada para a camera: " + hikivisionCameraId);

					} else {
						if (offsetDateTime.isBefore(secondsAgo)) {
							System.out.println("Evento offline: " + eventListnerTO.getAccessControllerEvent().getDeviceName()
									+ " | " + eventListnerTO.getAccessControllerEvent().getCardNo() + " | "
									+ eventListnerTO.getDateTime());
							
							if(DeviceStatus.CONNECTED == attachedDevice.getStatus()) {
								attachedDevice.disconnect(null);
							}
							
							LogPedestrianAccessEntity logsDeEventosOffline =  montaLogDePedestreOfflines(eventListnerTO.getAccessControllerEvent().getCardNo(),
									eventListnerTO.getAccessControllerEvent().getDeviceName());
							HibernateUtil.save(LogPedestrianAccessEntity.class, logsDeEventosOffline);
							
							
						} else {
							liberarAcessoPedestre(attachedDevice, eventListnerTO.getAccessControllerEvent().getCardNo());
						}
					}
					
				}

			} catch (EOFException eof) {
				eof.printStackTrace();
			} catch (SocketException se) {
				se.printStackTrace();
			} catch (Exception e) {
				System.out.println(sdf.format(new Date()) + "  ... TCP server exception: " + e.getMessage());
				e.printStackTrace();
			} finally {
			}
		}
		
		private LogPedestrianAccessEntity montaLogDePedestreOfflines(final String cardNumber, final String equipamento) {
			PedestrianAccessEntity pedestre = (PedestrianAccessEntity) HibernateUtil
					.getSingleResultByCardNumber(PedestrianAccessEntity.class, Long.valueOf(cardNumber));
			
		
			LogPedestrianAccessEntity lastAccess = HibernateUtil.buscaUltimoAcesso(pedestre.getId(), pedestre.getQtdAcessoAntesSinc());
			String direcaoDeUltimoSentido = Tipo.ENTRADA;
			if(Objects.nonNull(lastAccess)) {
				if(Objects.equals(Tipo.ENTRADA, lastAccess.getDirection())) {
					direcaoDeUltimoSentido = Tipo.SAIDA;
				}
			}
			return  new LogPedestrianAccessEntity(Main.loggedUser.getId(),
					pedestre.getId(), "Ativo","" ,"Offline" ,direcaoDeUltimoSentido , equipamento);

			
		}

		private void sendResponse(final OutputStream outputStream) throws IOException {
			responseDateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
			final String response = "HTTP/1.1 200\n" 
					+ "Content-Length: 0\n" 
					+ "Date: " + responseDateFormat.format(new Date()) + "\n";
			
			outputStream.write(response.getBytes());
			outputStream.flush();
		}

		private void liberarAcessoPedestre(final TopDataDevice selectedDevice, final String cardNo) {
			if (selectedDevice.getStatus() == DeviceStatus.DISCONNECTED) {
				try {
					selectedDevice.connect(null);
				} catch (Exception e) {
					e.printStackTrace();
				}
				
				// Gravar no banco que o usurio passou na catraca
				
			} else {
				selectedDevice.validaAcessoHikivision(cardNo);
			}
		}

		private TopDataDevice getAttachedDevice(String deviceId) {
			if (Objects.isNull(Main.devicesList)) {
				return null;
			}

			for (Device device : Main.devicesList) {
				if (Objects.isNull(device.getAttachedHikivisionCameras())) {
					continue;
				}

				for (AttachedTO camera : device.getAttachedHikivisionCameras()) {
					if (deviceId.equalsIgnoreCase(camera.getIdDevice()) && device instanceof TopDataDevice) {
						return (TopDataDevice) device;
					}
				}
			}

			return null;
		}
		
		private OffsetDateTime getOffsetDateTime(final String dataOriginal) {
	        final OffsetDateTime dataComFusoOriginal = OffsetDateTime.parse(dataOriginal, DateTimeFormatter.ISO_OFFSET_DATE_TIME);
	        final OffsetDateTime dataComFusoDesejado = dataComFusoOriginal.withOffsetSameInstant(java.time.ZoneOffset.ofHours(-3));
	        return dataComFusoDesejado;
	    }

	}
}
