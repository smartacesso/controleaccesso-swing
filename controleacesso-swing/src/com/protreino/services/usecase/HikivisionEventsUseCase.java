package com.protreino.services.usecase;

import java.lang.reflect.Type;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.Objects;

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
import com.protreino.services.enumeration.TcpMessageType;
import com.protreino.services.main.Main;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.repository.LogPedestrianAccessRepository;
import com.protreino.services.to.AttachedTO;
import com.protreino.services.to.TcpMessageTO;
import com.protreino.services.to.hikivision.EventListnerTO;
import com.protreino.services.utils.TcpServer;
import com.protreino.services.utils.Utils;

public class HikivisionEventsUseCase {

	private static final SimpleDateFormat sdf2 = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXXX");
	private final LogPedestrianAccessRepository logPedestrianAccessRepository = new LogPedestrianAccessRepository();
	private final HikivisionUseCases hikivisionUseCases = new HikivisionUseCases();
	private static Gson gson = new GsonBuilder().registerTypeAdapter(Date.class, new JsonDeserializer<Date>() {
		public Date deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context)
				throws JsonParseException {
			try {
				return sdf2.parse(json.getAsString());
			} catch (Exception e) {
			}

			return null;
		}
	}).create();

	public void execute(final String requestBody) { // Alterado de StringBuilder para String
	    final String objectPayload = Utils.getFirstJsonFromString(requestBody);

	    int startIndex = requestBody.indexOf("/") + 1;
	    int endIndex = requestBody.indexOf(" HTTP");

	    if (startIndex < 1 || endIndex < 0 || startIndex >= endIndex) {
	        System.out.println("Erro ao extrair hikivisionCameraId");
	        return;
	    }

	    final String hikivisionCameraId = requestBody.substring(startIndex, endIndex);
	    final EventListnerTO eventListnerTO = gson.fromJson(objectPayload, EventListnerTO.class);

	    if (Objects.isNull(eventListnerTO) 
	            || Objects.isNull(eventListnerTO.getAccessControllerEvent())
	            || Objects.isNull(eventListnerTO.getAccessControllerEvent().getCardNo())) {

	    	System.out.println("Evento de pedestre não reconhecido pela câmera: " + hikivisionCameraId);
	        System.out.println("Cartão recebido nulo da Hikvision. : " + eventListnerTO.getAccessControllerEvent().getCardNo());
	        System.out.println("Processamento encerrado.");
	        return;
	    }
	    
	    System.out.println(String.format("Evento do usuário com o cartão: %s", 
	            eventListnerTO.getAccessControllerEvent().getCardNo()));
	    
	    final TopDataDevice attachedDevice = getAttachedDevice(hikivisionCameraId);
	    
	    if (Objects.isNull(attachedDevice)) {
	        System.out.println("Sem catraca vinculada para a câmera: " + hikivisionCameraId);
	        return;
	    }
	    
	   TcpMessageTO message =  new TcpMessageTO(TcpMessageType.EVENTO_HIKIVISION);
	   message.getParans().put("card", eventListnerTO.getAccessControllerEvent().getCardNo());
	   message.getParans().put("facial", hikivisionCameraId);
	    
	   TcpServer.enviarMensagemParaClientesEventos(message);

	    final OffsetDateTime offsetDateTime = getOffsetDateTime(eventListnerTO.getDateTime());
	    final String cardNumber = eventListnerTO.getAccessControllerEvent().getCardNo();
	    
	    if (isEventOffline(offsetDateTime)) {
	        System.out.println("Evento offline (hora errada): " 
	                + eventListnerTO.getAccessControllerEvent().getDeviceName()
	                + " | " + cardNumber + " | " + eventListnerTO.getDateTime());

	        if (DeviceStatus.CONNECTED == attachedDevice.getStatus()) {
	            try {
	                attachedDevice.disconnect();
	            } catch (Exception e) {
	                e.printStackTrace();
	            }
	        }
	        processaEventoDePassagemComCatracaOffiline(cardNumber, attachedDevice, offsetDateTime, eventListnerTO.getAccessControllerEvent().getDeviceName());

	    } else {
	        liberarAcessoPedestre(attachedDevice, cardNumber, offsetDateTime, eventListnerTO.getAccessControllerEvent().getDeviceName());
	    }
	}


	private void processaEventoDePassagemComCatracaOffiline(final String cardNumber, final TopDataDevice device, final OffsetDateTime dataAcesso, final String HikivisionDeviceName) {
	    final PedestrianAccessEntity pedestre = (PedestrianAccessEntity) HibernateAccessDataFacade
	            .getSingleResultByCardNumber(PedestrianAccessEntity.class, Long.valueOf(cardNumber));
	    if (Objects.isNull(pedestre)) {
	        return;
	    }

	    if (device.ignorarAcesso()) {
	        System.out.println("Acesso ignorado catraca offline");
	        return;
	    }

	    final LogPedestrianAccessEntity logEventoOffline = salvaLogDeAcessoEventoCatracaOffline(pedestre, device, dataAcesso, HikivisionDeviceName);
	    
	    if(Objects.isNull(logEventoOffline)) {
	    	return;
	    }

	    System.out.println("Log de evento off do acesso : " + logEventoOffline.getDirection() + " | Equipamento :"
	            + logEventoOffline.getEquipament() + " | Pedestre : " + logEventoOffline.getIdPedestrian());

	    decrementaCreditosERemoveUsuarioDaCamera(pedestre, device, logEventoOffline);
	}

	private LogPedestrianAccessEntity salvaLogDeAcessoEventoCatracaOffline(final PedestrianAccessEntity pedestre, final TopDataDevice device, final OffsetDateTime dataAcesso, final String HikivisionDevice) {
	    String sentido;

	    // Busca o último acesso do pedestre
	    final LogPedestrianAccessEntity lastAccess = logPedestrianAccessRepository.buscaUltimoAcesso(pedestre.getId(), pedestre.getQtdAcessoAntesSinc());

	    // Se houver um último acesso, verifica o tempo
	    if (Objects.nonNull(lastAccess)) {
	        long diferencaSegundos = Math.abs(dataAcesso.toEpochSecond() - lastAccess.getAccessDate().toInstant().getEpochSecond());

	        if (diferencaSegundos < 10) {
	            System.out.println("Acesso ignorado: tentativa repetida em menos de 10 segundos.");
	            return null; // Ignora o acesso se for muito próximo do último
	        }
	    }

	    // Determina o sentido do acesso
	    if (Utils.getPreferenceAsBoolean("doisDispositivos") && Objects.nonNull(HikivisionDevice)) {
	        String dispositivoNormalizado = HikivisionDevice.toLowerCase();
	        if (dispositivoNormalizado.contains("entrada")) {
	            sentido = Tipo.ENTRADA;
	        } else if (dispositivoNormalizado.contains("saída") || dispositivoNormalizado.contains("saida") ) {
	            sentido = Tipo.SAIDA;
	        } else {
	            sentido = Tipo.ENTRADA; // Padrão caso o nome do dispositivo seja inválido
	        }
	    } else {
	        // Alterna o sentido baseado no último acesso
	        sentido = (Objects.nonNull(lastAccess) && Objects.equals(Tipo.ENTRADA.toString(), lastAccess.getDirection()))
	                ? Tipo.SAIDA : Tipo.ENTRADA;
	    }

	    LogPedestrianAccessEntity logEventoOffline = new LogPedestrianAccessEntity(Main.loggedUser.getId(),
	            pedestre.getId(), true, "", "Facial", sentido, device.getFullIdentifier(), pedestre.getCardNumber(), new Date(dataAcesso.toInstant().toEpochMilli()));

	    return (LogPedestrianAccessEntity) HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logEventoOffline)[0];
	}



	private void decrementaCreditosERemoveUsuarioDaCamera(final PedestrianAccessEntity pedestre, final TopDataDevice device, final LogPedestrianAccessEntity logEventoOffline) {
	    // Se o log for nulo, não há acesso válido para processar
	    if (logEventoOffline == null) {
	        System.out.println("Acesso ignorado: log de evento é nulo.");
	        return;
	    }
		
		// Constantes para configuração
	    final String IGNORAR_REGRAS = "IGNORAR_REGRAS_DE_ACESSO";
	    final String BLOQUEAR_SAIDA = "BLOQUEAR_SAIDA";
	    final String REMOVE_VISITANTE_CAMERA = "removeVisitanteCameraSaida";

	    // Verifica se deve ignorar regras de acesso
	    boolean ignoraRegras = device.getConfigurationValueAsBoolean(IGNORAR_REGRAS);
	    if (ignoraRegras) {
	        return; // Ignora o restante da execução
	    }

	    // Configurações específicas
	    boolean bloquearSaida = device.getConfigurationValueAsBoolean(BLOQUEAR_SAIDA);
	    Boolean removeVisitanteCamera = Utils.getPreferenceAsBoolean(REMOVE_VISITANTE_CAMERA);

	    // Log da quantidade de créditos antes da operação
	    System.out.println("Quantidade de créditos antes: " + pedestre.getQuantidadeCreditos());

	    // Decrementa créditos se for saída ou se saída não está bloqueada
	    if (logEventoOffline.isSaida()) {
	    	System.out.println("direcao :" +  logEventoOffline.getDirection() + " is saida : " + logEventoOffline.isSaida());
	    	System.out.println("saida librada :" + !bloquearSaida);
	        System.out.println("Decrementando créditos...");
	        pedestre.decrementaCreditos();
	    }

	    // Log da quantidade de créditos após a operação
	    System.out.println("Quantidade de créditos depois: " + pedestre.getQuantidadeCreditos());

	    // Verifica condições para saída antecipada
	    if ((pedestre.getQuantidadeCreditos() != null && pedestre.getQuantidadeCreditos() > 0) || !pedestre.isVisitante()) {
	        System.out.println("Existe créditos restantes ou pedestre não é visitante. Operação encerrada.");
	        return;
	    }

	    // Remoção do visitante
	    if (removeVisitanteCamera) {
	        System.out.println("Removendo visitante da câmera...");
	        hikivisionUseCases.removerUsuarioFromDevices(pedestre);
	        HibernateAccessDataFacade.save(PedestrianAccessEntity.class, pedestre);
	    }
	}

	private void liberarAcessoPedestre(final TopDataDevice selectedDevice, final String cardNo,
			final OffsetDateTime dataAcesso, String hikivisionDeviceName) {
		if (selectedDevice.getStatus() == DeviceStatus.DISCONNECTED
				|| selectedDevice.getStatus() == DeviceStatus.ONLY_ENABLED) {
			System.out.println("Evento online, catraca desconectada, cartao : " + cardNo + " | data : " + dataAcesso
					+ " | equipamento : " + selectedDevice.getName());

			if (selectedDevice.getStatus() == DeviceStatus.DISCONNECTED) {
				tryReconectDevice(selectedDevice);
			}

			if (selectedDevice.getStatus() == DeviceStatus.CONNECTED) {
				selectedDevice.validaAcessoHikivision(cardNo);
			} else {
				processaEventoDePassagemComCatracaOffiline(cardNo, selectedDevice, dataAcesso, hikivisionDeviceName);
			}

		} else {
			selectedDevice.validaAcessoHikivision(cardNo);
		}
	}

	private void tryReconectDevice(final TopDataDevice selectedDevice) {
		final Boolean reconectDeviceOnReceiveCurrentEvent = Utils.getPreferenceAsBoolean("reconectDeviceOnReceiveCurrentEvent");
		
		if(Boolean.TRUE.equals(reconectDeviceOnReceiveCurrentEvent)) {
			try {
				selectedDevice.connect("NOT_WAIT_TIME");
				
			} catch (Exception e) {
				e.printStackTrace();
			}
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
		final OffsetDateTime dataComFusoOriginal = OffsetDateTime.parse(dataOriginal,
				DateTimeFormatter.ISO_OFFSET_DATE_TIME);
		final OffsetDateTime dataComFusoDesejado = dataComFusoOriginal
				.withOffsetSameInstant(java.time.ZoneOffset.ofHours(-3));
		return dataComFusoDesejado;
	}
	
	private boolean isEventOffline(final OffsetDateTime offsetDateTime) {
		final OffsetDateTime secondsAgo = OffsetDateTime
				.from(ZonedDateTime.ofInstant(Instant.now(), ZoneId.systemDefault()).minusSeconds(20));
		
		return offsetDateTime.isBefore(secondsAgo);
	}

}
