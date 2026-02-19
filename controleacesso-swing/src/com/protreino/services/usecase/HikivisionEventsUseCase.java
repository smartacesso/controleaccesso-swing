package com.protreino.services.usecase;

import java.lang.reflect.Type;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import javax.swing.plaf.synth.SynthOptionPaneUI;

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
import com.protreino.services.entity.PedestreRegraEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.enumeration.TcpMessageType;
import com.protreino.services.main.Main;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.repository.HibernateLocalAccessData;
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
	
	
	// Define o mapa uma única vez (pode ser estático, fora do método)
	private static final Map<Integer, String> EVENTOS_BLOQUEADOS = new HashMap<>();

	static {
	    EVENTOS_BLOQUEADOS.put(6, "INATIVO (AFASTADO OU DEMITIDO)");
	    EVENTOS_BLOQUEADOS.put(7, "FORA DO HORARIO");
	}

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
		final Integer subEvent = eventListnerTO.getAccessControllerEvent().getSubEventType();

		if (Objects.isNull(eventListnerTO) || Objects.isNull(eventListnerTO.getAccessControllerEvent())
				|| Objects.isNull(eventListnerTO.getAccessControllerEvent().getCardNo())) {

			return;
		}

		System.out.println(String.format("Evento do usuário com o cartão: %s",
				eventListnerTO.getAccessControllerEvent().getCardNo()));

		final TopDataDevice attachedDevice = getAttachedDevice(hikivisionCameraId);

		if (Objects.isNull(attachedDevice)) {
			System.out.println("Sem catraca vinculada para a câmera: " + hikivisionCameraId);
			return;
		}

		final OffsetDateTime offsetDateTime = getOffsetDateTime(eventListnerTO.getDateTime());
		final String cardNumber = eventListnerTO.getAccessControllerEvent().getCardNo();

		if (isEventOffline(offsetDateTime)) {
			System.out.println(
					"Evento offline (hora errada): " + eventListnerTO.getAccessControllerEvent().getDeviceName() + " | "
							+ cardNumber + " | " + eventListnerTO.getDateTime());

			if (DeviceStatus.CONNECTED == attachedDevice.getStatus()) {
				try {
					attachedDevice.disconnect();
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
			// adicionar configuração para habilitar isso
			processaEventoDePassagemComCatracaOffiline(cardNumber, attachedDevice, offsetDateTime,
					eventListnerTO.getAccessControllerEvent().getDeviceName(), subEvent);
		} else {
			if(Utils.isHabilitadoEnviarEventos()) {
				TcpMessageTO message = new TcpMessageTO(TcpMessageType.EVENTO_HIKIVISION);
				message.getParans().put("card", eventListnerTO.getAccessControllerEvent().getCardNo());
				message.getParans().put("facial", hikivisionCameraId);
				TcpServer.enviarMensagemParaClientesEventos(message);
			}

			liberarAcessoPedestre(attachedDevice, cardNumber, offsetDateTime,
					eventListnerTO.getAccessControllerEvent().getDeviceName(), subEvent);
		}
	}


	private void processaEventoDePassagemComCatracaOffiline(final String cardNumber, final TopDataDevice device, final OffsetDateTime dataAcesso, final String HikivisionDeviceName, final int subEvento) {

		// Chame sua função aqui
		final PedestrianAccessEntity pedestre = (PedestrianAccessEntity) HibernateLocalAccessData
				.getSingleResultByCardNumberString(PedestrianAccessEntity.class, cardNumber);
	    
	    if (Objects.isNull(pedestre)) {
	    	System.out.println("pedestre não encontrado");
	        return;
	    }

	    if(Utils.isAcessoHoraErradoIgnorada()) {
	        System.out.println("Acesso ignorado catraca offline - hora errada");
	        return;
	    }
	    
	    if (device.ignorarAcesso()) {
	        System.out.println("Acesso ignorado catraca offline");
	        return;
	    }
	    
	    if (Utils.isAcessoLiberado()) {
	        System.out.println("Acesso ignorado DSR");
	        return;
	    }
	    
	    final LogPedestrianAccessEntity logEventoOffline = salvaLogDeAcessoEventoCatracaOffline(pedestre, device, dataAcesso, HikivisionDeviceName, subEvento);
	    
	    if(Objects.isNull(logEventoOffline)) {
	    	return;
	    }

	    System.out.println("Log de evento off do acesso : " + logEventoOffline.getDirection() + " | Equipamento :"
	            + logEventoOffline.getEquipament() + " | Pedestre : " + logEventoOffline.getIdPedestrian());

	    //decrementa apenas de visitante
	    decrementaCreditosERemoveUsuarioDaCamera(pedestre, device, logEventoOffline, HikivisionDeviceName);
	}

	private LogPedestrianAccessEntity salvaLogDeAcessoEventoCatracaOffline(final PedestrianAccessEntity pedestre,
			final TopDataDevice device, final OffsetDateTime dataAcesso, final String HikivisionDevice,
			final int subEvento) {

		String sentido;
		String motivoBloqueio;
		String motivoLiberado = "FACIAL";

		// Busca o último acesso do pedestre
		final LogPedestrianAccessEntity lastAccess = logPedestrianAccessRepository.buscaUltimoAcesso(pedestre.getId(),
				pedestre.getQtdAcessoAntesSinc());

		// Se houver um último acesso, verifica o tempo
		if (Objects.nonNull(lastAccess)) {
			long diferencaSegundos = Math
					.abs(dataAcesso.toEpochSecond() - lastAccess.getAccessDate().toInstant().getEpochSecond());

			System.out.println(
					"Data ultimo acesso : " + lastAccess.getAccessDate() + ", diferença : " + diferencaSegundos);

			if (diferencaSegundos < 20) {
				System.out.println("Acesso ignorado: tentativa repetida em menos de 20 segundos.");
				return null;
			}
		}
		
	    // Verifica se é evento bloqueado
	    motivoBloqueio = getMotivoEvento(subEvento);

		if (motivoBloqueio == null) {
			// Determina o sentido do acesso
			if (Utils.getPreferenceAsBoolean("doisDispositivos") && Objects.nonNull(HikivisionDevice)) {
				String dispositivoNormalizado = HikivisionDevice.toLowerCase();
				if (dispositivoNormalizado.contains("entrada")) {
					sentido = "ENTRADA";
				} else if (dispositivoNormalizado.contains("saída") || dispositivoNormalizado.contains("saida")) {
					sentido = "SAIDA";
				} else {
					sentido = "FACIAL"; // Padrão caso o nome do dispositivo seja inválido
				}
			} else {
				// Alterna o sentido baseado no último acesso
				sentido = (Objects.nonNull(lastAccess)
						&& Objects.equals(Tipo.ENTRADA.toString(), lastAccess.getDirection())) ? Tipo.SAIDA
								: Tipo.ENTRADA;
			}

			LogPedestrianAccessEntity logEventoOffline = new LogPedestrianAccessEntity(Main.loggedUser.getId(),
					pedestre.getId(), true, "", motivoLiberado, sentido, device.getName(), pedestre.getCardNumber(),
					new Date(dataAcesso.toInstant().toEpochMilli()));

			return (LogPedestrianAccessEntity) HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class,
					logEventoOffline)[0];
		} else {
			
			sentido = "";

			LogPedestrianAccessEntity logEventoOffline = new LogPedestrianAccessEntity(Main.loggedUser.getId(),
					pedestre.getId(), true, "", motivoBloqueio, sentido, device.getName(), pedestre.getCardNumber(),
					new Date(dataAcesso.toInstant().toEpochMilli()));

			logEventoOffline.setStatus("INATIVO");

			return (LogPedestrianAccessEntity) HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class,
					logEventoOffline)[0];
		}

	}
	
	private String getMotivoEvento(int subEvento) {
	    return EVENTOS_BLOQUEADOS.get(subEvento); 
	}


	private void decrementaCreditosERemoveUsuarioDaCamera(final PedestrianAccessEntity pedestre,
			final TopDataDevice device, final LogPedestrianAccessEntity logEventoOffline, final String HikivisionDevice) {
		if (logEventoOffline == null) {
			System.out.println("Acesso ignorado: log de evento é nulo.");
			return;
		}

		final String IGNORAR_REGRAS = "IGNORAR_REGRAS_DE_ACESSO";
		final String BLOQUEAR_SAIDA = "BLOQUEAR_SAIDA";
		final String REMOVE_VISITANTE_CAMERA = "removeVisitanteCameraSaida";

		boolean ignoraRegras = device.getConfigurationValueAsBoolean(IGNORAR_REGRAS);
		if (ignoraRegras)
			return;

		boolean bloquearSaida = device.getConfigurationValueAsBoolean(BLOQUEAR_SAIDA);
		Boolean removeVisitanteCamera = Utils.getPreferenceAsBoolean(REMOVE_VISITANTE_CAMERA);

		// Só processa o restante se for uma SAÍDA
		if (logEventoOffline.isEntrada()) {
			System.out.println("Não é uma saída. Nenhuma ação realizada.");
			return;
		}

		System.out.println("direcao : " + logEventoOffline.getDirection());

		boolean permitido = validaRegraVisitante(pedestre);
		if (permitido) {
			return;
		}
		
		if (!Utils.saidaEspecificaHabilitada() || isSaidaPortaria(HikivisionDevice)) {
		    processaSaidaVisitante(pedestre, removeVisitanteCamera);
		}
	}
	
	private boolean isSaidaPortaria(String dispositivo) {
	    return dispositivo != null && dispositivo.toLowerCase().contains("portaria");
	}

	private void processaSaidaVisitante(PedestrianAccessEntity pedestre, boolean removeVisitanteCamera) {
	    if (pedestre.isVisitante()) {
	        System.out.println("Decrementando créditos...");
	        pedestre.decrementaCreditos();
	    }

	    if (pedestre.isVisitante() && (pedestre.getQuantidadeCreditos() == null || pedestre.getQuantidadeCreditos() <= 0)) {
	        if (removeVisitanteCamera) {
	            System.out.println("Removendo visitante da câmera...");
	            hikivisionUseCases.removerUsuarioFromDevices(pedestre);
	            HibernateAccessDataFacade.save(PedestrianAccessEntity.class, pedestre);
	        }
	    } else {
	        System.out.println("Não será removido: ainda possui créditos ou não é visitante.");
	    }
	}
	
	/**
	 * Retorna true para manter a foto, false para apagar.
	 */
	private boolean validaRegraVisitante(PedestrianAccessEntity pedestre) {
	    Optional<PedestreRegraEntity> optionalRegra = pedestre.getRegraAtiva();

	    if (!optionalRegra.isPresent()) {
	        return false; // Não tem regra ativa → apaga foto
	    }

	    PedestreRegraEntity regra = optionalRegra.get();

	    if (regra.isPeriodoValido()) {
	        return true; // Regra válida → mantém foto
	    }

	    if (regra.temCreditosTotal()) {
	        regra.decrementaCreditosTotal();
	        HibernateAccessDataFacade.save(PedestrianAccessEntity.class, pedestre);

	        return !regra.isUltimoCreditoTotal(); // Se ainda tinha mais → mantém; se era o último → apaga
	    }

	    return false; // Fora do período e sem créditos → apaga
	}


	private void liberarAcessoPedestre(final TopDataDevice selectedDevice, final String cardNo,
			final OffsetDateTime dataAcesso, String hikivisionDeviceName, final Integer subEvent) {
		if (selectedDevice.getStatus() == DeviceStatus.DISCONNECTED
				|| selectedDevice.getStatus() == DeviceStatus.ONLY_ENABLED) {
			System.out.println("Evento online, catraca desconectada, cartao : " + cardNo + " | data : " + dataAcesso
					+ " | equipamento : " + selectedDevice.getName());

			if (selectedDevice.getStatus() == DeviceStatus.DISCONNECTED) {
				tryReconectDevice(selectedDevice);
			}

			if (selectedDevice.getStatus() == DeviceStatus.CONNECTED) {
				selectedDevice.validaAcessoHikivision(cardNo, hikivisionDeviceName);
			} else {
				processaEventoDePassagemComCatracaOffiline(cardNo, selectedDevice, dataAcesso, hikivisionDeviceName, subEvent);
			}

		} else {
			selectedDevice.validaAcessoHikivision(cardNo, hikivisionDeviceName);
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
	    final OffsetDateTime agora = OffsetDateTime.now(ZoneId.systemDefault());
	    final Duration diferenca = Duration.between(offsetDateTime, agora);

	    // Qualquer evento com mais de 20 segundos de diferença é considerado offline
	    return Math.abs(diferenca.getSeconds()) > 120;
	}
	
}
