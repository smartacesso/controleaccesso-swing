package com.protreino.services.usecase.controlId;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import com.protreino.services.entity.PedestrianAccessEntity;
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

	private void initPreferences() {

	}

	private String login() {

		LoginInput loginInput = new LoginInput(login, password);

		if (Objects.isNull(login)) {
			System.err.println("LoginInput não pode ser nulo.");
			return null;
		}

		final String url = "http://" + ip + "/login.fcgi";
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

		if (response == null || response.trim().equals("{}")) {
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
			// Parse da resposta para obter a sessão
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

		// terminar
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

	public List<Long> createObjects(final String session, final CreateUserRequest createUsers) {
		if (session == null || session.isEmpty()) {
			throw new IllegalArgumentException("A sessão é obrigatória.");
		}

		try {
			final String url = "http://" + ip + "/create_objects.fcgi?session=" + session;

			final String jsonPayload = gson.toJson(createUsers);

			// Envia a requisição usando o método genérico `send`
			Object response = controlIdService.postMessage("application/json", url, jsonPayload, Object.class);

			// Converte a resposta em um objeto contendo os IDs dos objetos criados
			Map<String, List<Long>> responseMap = gson.fromJson((String) response,
					new TypeToken<Map<String, List<Long>>>() {
					}.getType());
			return responseMap.get("ids");

		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	// Listagem de Usuarios na camera
	public final String ListagemdeUsers(String session, final String ip) {

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

	// Recebe fotos do facial
	// Precisa puxar uma variavel com um array de todos os IDs dentro do facial e
	// colocar essa variavel em idsFacial

	// Envio de foto para a camera
	// Inacabada

	public Void cadastrarUsuario(final PedestrianAccessEntity pedestre) {

		String session = login();
		SessionOutput sessionOutput = gson.fromJson(session, SessionOutput.class);
		Long idUser = createUser(pedestre, sessionOutput);

		// TODO: salvar sessao no banco de dados de ver persisitencia dela

		facialUseCase.send(sessionOutput.getSession(), String.valueOf(idUser), pedestre.getFoto());

		return null;

	}

	private Long createUser(final PedestrianAccessEntity pedestre, SessionOutput sessionOutput) {
		CreateUserRequest createUser = new CreateUserRequest();
		createUser.setObject("users");

		List<UserContentTypesrequest> userContent = new ArrayList<>();
		userContent.add(new UserContentTypesrequest(pedestre.getName(), pedestre.getCardNumber(), "123456", ""));

		List<Long> users = createObjects(sessionOutput.getSession(), createUser);
		return users.get(0);
	}

	public Void removerUsuario(final PedestrianAccessEntity pedestre) {
		return null;

	}

}
