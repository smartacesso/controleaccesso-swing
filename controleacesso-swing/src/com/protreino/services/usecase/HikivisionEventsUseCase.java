package com.protreino.services.usecase;

import static com.protreino.services.constants.TopDataDeviceConstants.BLOQUEAR_SAIDA;
import static com.protreino.services.constants.TopDataDeviceConstants.IGNORAR_REGRAS_DE_ACESSO;

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
import com.protreino.services.main.Main;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.repository.LogPedestrianAccessRepository;
import com.protreino.services.to.AttachedTO;
import com.protreino.services.to.hikivision.EventListnerTO;
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

	public void execute(final StringBuilder sb) {
		final String requestBody = sb.toString();
		final String objectPayload = Utils.getFirstJsonFromString(requestBody);
		final String hikivisionCameraId = sb.toString().substring(requestBody.indexOf("/") + 1,
				requestBody.indexOf(" HTTP"));
		final EventListnerTO eventListnerTO = gson.fromJson(objectPayload, EventListnerTO.class);

		if (Objects.isNull(eventListnerTO) 
				|| Objects.isNull(eventListnerTO.getAccessControllerEvent())
				|| Objects.isNull(eventListnerTO.getAccessControllerEvent().getCardNo())) {
			System.out.println("eventlistenre : ---- " + eventListnerTO);
			System.out.println("EVENTLISTENER GETACCSS : ---- " + eventListnerTO.getAccessControllerEvent());
			System.out.println("EVENTLISTENER GETACCSS NUMERO : ----" + eventListnerTO.getAccessControllerEvent().getCardNo());
			System.out.println("Evento de pedestre não reconhecido pela camera");
			return;
		}
		
		System.out.println(String.format("Evento do usuario com o cartão %s", eventListnerTO.getAccessControllerEvent().getCardNo()));

		final TopDataDevice attachedDevice = getAttachedDevice(hikivisionCameraId);

		if (Objects.isNull(attachedDevice)) {
			System.out.println("Sem catraca vinculada para a camera: " + hikivisionCameraId);
			return;
		}
		
		final OffsetDateTime offsetDateTime = getOffsetDateTime(eventListnerTO.getDateTime());
		final String cardNumber = eventListnerTO.getAccessControllerEvent().getCardNo();
		
		if (isEventOffline(offsetDateTime)) {
			System.out.println("Evento offline: " + eventListnerTO.getAccessControllerEvent().getDeviceName()
					+ " | " + cardNumber + " | " + eventListnerTO.getDateTime());

			if (DeviceStatus.CONNECTED == attachedDevice.getStatus()) {
				try {
					attachedDevice.disconnect();
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
			processaEventoDePassagemComCatracaOffiline(cardNumber, attachedDevice, offsetDateTime);

		} else {
			liberarAcessoPedestre(attachedDevice, cardNumber, offsetDateTime);
		}

	}

	private void processaEventoDePassagemComCatracaOffiline(final String cardNumber, final TopDataDevice device, final OffsetDateTime dataAcesso) {
		final PedestrianAccessEntity pedestre = (PedestrianAccessEntity) HibernateAccessDataFacade
				.getSingleResultByCardNumber(PedestrianAccessEntity.class, Long.valueOf(cardNumber));
		if (Objects.isNull(pedestre)) {
			return;
		}
		
		final LogPedestrianAccessEntity logEventoOffline = salvaLogDeAcessoEventoCatracaOffiline(pedestre, device, dataAcesso);
		decrementaCreditosERemoveUsuarioDaCamera(pedestre, device, logEventoOffline);
	}
	
	private LogPedestrianAccessEntity salvaLogDeAcessoEventoCatracaOffiline(final PedestrianAccessEntity pedestre, final TopDataDevice device, final OffsetDateTime dataAcesso) {
		final LogPedestrianAccessEntity lastAccess = logPedestrianAccessRepository.buscaUltimoAcesso(pedestre.getId(), pedestre.getQtdAcessoAntesSinc());
		String sentido = Tipo.ENTRADA;

		if (Objects.nonNull(lastAccess)) {
			if (Objects.equals(Tipo.ENTRADA.toString(), lastAccess.getDirection())) {
				sentido = Tipo.SAIDA;
			}
		}
		
		LogPedestrianAccessEntity logEventoOffline = new LogPedestrianAccessEntity(Main.loggedUser.getId(),
				pedestre.getId(), true, "", "Facial", sentido, device.getFullIdentifier(), pedestre.getCardNumber(), new Date(dataAcesso.toInstant().toEpochMilli()));
		
		return (LogPedestrianAccessEntity) HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logEventoOffline)[0];
	}
	
	private void decrementaCreditosERemoveUsuarioDaCamera(final PedestrianAccessEntity pedestre, final TopDataDevice device, final LogPedestrianAccessEntity logEventoOffline) {
		boolean ignoraRegras = device.getConfigurationValueAsBoolean(IGNORAR_REGRAS_DE_ACESSO);

		if(ignoraRegras) {
			return;
		}
		
		final boolean bloquearSaida = device.getConfigurationValueAsBoolean(BLOQUEAR_SAIDA);
		final Boolean removeVisitanteCamera = Utils.getPreferenceAsBoolean("removeVisitanteCameraSaida");

		if(logEventoOffline.isSaida() || !bloquearSaida) {
			 pedestre.decrementaCreditos();
		}
		
		if(pedestre.temCreditos() || !pedestre.isVisitante()) {
			return;
		}
		
		if(Utils.isHikivisionConfigValid() 
				&& Objects.nonNull(pedestre.getFoto()) 
				&& Objects.nonNull(pedestre.getDataCadastroFotoNaHikivision())
				&& removeVisitanteCamera) {
			hikivisionUseCases.removerUsuarioFromDevices(pedestre);
			HibernateAccessDataFacade.save(PedestrianAccessEntity.class, pedestre);
		}
		
	}

	private void liberarAcessoPedestre(final TopDataDevice selectedDevice, final String cardNo, final OffsetDateTime dataAcesso) {
		if (selectedDevice.getStatus() == DeviceStatus.DISCONNECTED 
				|| selectedDevice.getStatus() == DeviceStatus.ONLY_ENABLED) {
			System.out.println("Evento online, catraca desconectada: " + cardNo + " | " + dataAcesso);
			
			if (selectedDevice.getStatus() == DeviceStatus.DISCONNECTED) {
				tryReconectDevice(selectedDevice);
			}
			
			if(selectedDevice.getStatus() == DeviceStatus.CONNECTED) {
				selectedDevice.validaAcessoHikivision(cardNo);
			} else {
				processaEventoDePassagemComCatracaOffiline(cardNo, selectedDevice, dataAcesso);
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
