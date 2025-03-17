package com.protreino.services.usecase.controlId;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.to.controlIdDevice.AttachedRulesRequest;
import com.protreino.services.to.controlIdDevice.CreateUserAttachedRulesRequest;
import com.protreino.services.to.controlIdDevice.CreateUserRequest;
import com.protreino.services.to.controlIdDevice.LoginInput;
import com.protreino.services.to.controlIdDevice.SessionOutput;
import com.protreino.services.to.controlIdDevice.UserContentTypesrequest;
import com.protreino.services.to.controlIdDevice.ValidOutput;
import com.protreino.services.usecase.ControlIdDeviceService;
import com.protreino.services.utils.Utils;

public class ControlIdUseCase {

	Gson gson = new Gson();

	public static final String APPLICATION_JSON = "application/json";

	ControlIdDeviceService controlIdService = new ControlIdDeviceService();

	FacialUseCase facialUseCase = new FacialUseCase();

	public static String ip = Utils.getPreference("ControlIdIdentifierURL");

	public static String login = Utils.getPreference("controlIdUserConnection");

	public static String password = Utils.getPreference("controlIdPasswordConnection");

	public static String OCTECT_STREAM = "application/octet-stream";

	private String login(String ipConfig) {

		LoginInput loginInput = new LoginInput(login, password);

		if (Objects.isNull(login)) {
			System.err.println("LoginInput não pode ser nulo.");
			return null;
		}

		final String url = "http://" + ipConfig + "/login.fcgi";
		final String payload = gson.toJson(loginInput);
		try {
			return (String) controlIdService.postMessage(APPLICATION_JSON, url, payload, String.class);

		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

	public boolean logout(final String session, final String ip) {
		if (Objects.isNull(session) || session.isEmpty()) {
			System.err.println("Sessão não pode ser nula ou vazia.");
			return false;
		}

		final String url = "http://" + ip + "/logout.fcgi?session=" + session;
		String response = (String) controlIdService.getMessage("application/json", url, null);

		if (Objects.isNull(response) || response.trim().equals("{}")) {
			System.out.println("Logout realizado com sucesso.");
			return true;
		}
		System.err.println("Falha no logout.");
		return false;
	}

	public boolean isValidSession(final String session, String ip) {
		if (Objects.isNull(session) || session.isEmpty()) {
			System.err.println("Sessão não pode ser nula ou vazia.");
			return false;
		}

		final String url = "http://" + ip + "/session_is_valid.fcgi?session=" + session;
		String response = (String) controlIdService.getMessage(url, "application/json", null);

		if (Objects.isNull(response)) {

			ValidOutput validOutput = gson.fromJson(response, ValidOutput.class);
			if (validOutput != null && validOutput.getValid() != null) {
				System.out.println("Sessão obtida com sucesso: " + validOutput.getValid());
				return validOutput.isValid();
			} else {
				System.err.println("Falha ao obter a sessão da resposta.");
			}
		}
		return false;
	}

	private Void alterarUsuario(final String session, LoginInput login) {
		if (Objects.isNull(login)) {
			System.err.println("LoginInput não pode ser nulo.");
			return null;
		}
		if (Objects.isNull(session) || session.isEmpty()) {
			System.err.println("Sessão não pode ser nula ou vazia.");
			return null;
		}

		return null;
	}

	public List<Long> createObjects(final String session, final CreateUserRequest createUsers, final String ipDevice) {
		if (Objects.isNull(createUsers) || session.isEmpty()) {
			throw new IllegalArgumentException("A sessão é obrigatória.");
		}

		try {
			final String url = "http://" + ipDevice + "/create_objects.fcgi?session=" + session;

			final String jsonPayload = gson.toJson(createUsers);

			Object response = controlIdService.postMessage("application/json", url, jsonPayload, Object.class);

			Map<String, List<Long>> responseMap = gson.fromJson((String) response,
					new TypeToken<Map<String, List<Long>>>() {
					}.getType());
			return responseMap.get("ids");

		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	public final String ListagemdeUsers(final String session, final String ip) {

		final String url = "http://" + ip + "/user_list_images.fcgi?get_timestamp=1&session=" + session;
		final String response = (String) controlIdService.getMessage("application/json", url, null);

		if (response == null || response.trim().equals("{}")) {
			System.out.println("Users recebidos.");
			return response;
		}
		final String idsFacial = response;
		System.err.println("Erro em receber quantidade de usuarios.");
		return idsFacial;
	}

	public Void cadastrarUsuario(final PedestrianAccessEntity pedestre, String ipConfig) {

		if (Objects.isNull(ipConfig)) {
			ipConfig = ip;
		}

		String session = login(ipConfig);
		SessionOutput sessionOutput = gson.fromJson(session, SessionOutput.class);
		Long idUser = createUser(pedestre, sessionOutput, ipConfig);
		Long idRule = 1L;

		CreateUserAttachedRulesRequest userAttached = new CreateUserAttachedRulesRequest();
		userAttached.setObject("user_access_rules");

		List<AttachedRulesRequest> userContent = Collections.singletonList(new AttachedRulesRequest(idUser, idRule));
		userAttached.setUserContent(userContent);
		attachedRules(sessionOutput.getSession(), userAttached);

		// TODO: criar uma regra para o pedestre

		// TODO: salvar sessao no banco de dados de ver persisitencia dela

		facialUseCase.send(sessionOutput.getSession(), String.valueOf(idUser), pedestre.getFoto());

		return null;
	}

	private boolean attachedRules(final String session, CreateUserAttachedRulesRequest attachedRules) {
		if (Objects.isNull(session) || session.isEmpty()) {
			throw new IllegalArgumentException("A sessão é obrigatória.");
		}

		try {
			final String url = "http://" + ip + "/create_objects.fcgi?session=" + session;

			final String jsonPayload = gson.toJson(attachedRules);

			Object response = controlIdService.postMessage("application/json", url, jsonPayload, Object.class);

			Map<String, List<Long>> responseMap = gson.fromJson((String) response,
					new TypeToken<Map<String, List<Long>>>() {
					}.getType());
			return true;

		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
	}

	private Long createUser(final PedestrianAccessEntity pedestre, SessionOutput sessionOutput, String ipConfig) {
		CreateUserRequest createUser = new CreateUserRequest();
		createUser.setObject("users");

		List<UserContentTypesrequest> userContent = Collections
				.singletonList(new UserContentTypesrequest(pedestre.getName(), pedestre.getCardNumber(), "", ""));

		createUser.setUserContent(userContent);

		List<Long> users = createObjects(sessionOutput.getSession(), createUser, ipConfig);
		return users.get(0);
	}

	public Void removerUsuario(final PedestrianAccessEntity pedestre) {
		return null;

	}

	public void sincronizarNasCameras(PedestrianAccessEntity visitante) {

		HashMap<String, Object> args = new HashMap<>();
		args.put("MANUFACTURER", Manufacturer.CONTROL_ID_FACIAL);

		@SuppressWarnings("unchecked")
		List<DeviceEntity> devices = (List<DeviceEntity>) HibernateAccessDataFacade
				.getResultListWithParams(DeviceEntity.class, "DeviceEntity.findByManufacturer", args);

		Optional.ofNullable(devices).orElse(Collections.emptyList()).stream().forEach(device -> {
			cadastrarUsuario(visitante, device.getFacialControlId());
		});

	}

}
