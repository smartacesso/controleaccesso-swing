package com.protreino.services.usecase.controlId;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.to.controlIdDevice.LoginInput;
import com.protreino.services.to.controlIdDevice.SessionOutput;
import com.protreino.services.to.controlIdDevice.ValidOutput;
import com.protreino.services.usecase.ControlIdDeviceService;
import com.protreino.services.utils.Utils;

public class ControlIdUseCase {

	Gson gson = new Gson();

	public static final String APPLICATION_JSON = "application/json";

	ControlIdDeviceService controlIdService = new ControlIdDeviceService();

	FacialUseCase facialUseCase = new FacialUseCase();

	public static String ip = Utils.getPreference("TopdataServerRecognizerURL");

	public static String login = Utils.getPreference("controlIdUserConnection");

	public static String password = Utils.getPreference("controlIdPasswordConnection");

	public static String OCTECT_STREAM = "application/octet-stream";

	private void initPreferences() {

	}

	private SessionOutput login() {

		LoginInput loginInput = new LoginInput(login, password);

		if (Objects.isNull(login)) {
			System.err.println("LoginInput não pode ser nulo.");
			return null;
		}

		final String url = "http://" + ip + "/login.fcgi";
		final String payload = gson.toJson(login);

		SessionOutput sessionOutput;
		try {
			sessionOutput = (SessionOutput) controlIdService.postMessage(APPLICATION_JSON, url, payload,
					SessionOutput.class);
			if (Objects.isNull(sessionOutput)) {
				return null;
			}
			System.out.println("Sessão obtida com sucesso: " + sessionOutput.getSession());
			return sessionOutput;
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

	public List<Long> createObjects(final String session, final String object, List<Map<String, Object>> values,
			String ip) {
		if (session == null || session.isEmpty()) {
			throw new IllegalArgumentException("A sessão é obrigatória.");
		}
		if (object == null || object.isEmpty()) {
			throw new IllegalArgumentException("O tipo de objeto é obrigatório.");
		}
		if (values == null || values.isEmpty()) {
			throw new IllegalArgumentException("Os valores para criação dos objetos são obrigatórios.");
		}

		try {
			final String url = "http://" + ip + "/create_objects.fcgi?session=" + session;

			// Monta o payload da requisição
			Map<String, Object> payload = new HashMap<>();
			payload.put("object", object);
			payload.put("values", values);

			final String jsonPayload = gson.toJson(payload);

			// Envia a requisição usando o método genérico `send`
			Object response = controlIdService.postMessage("application/json", url, jsonPayload, Object.class);

			if (Objects.isNull(object)) {
				System.err.println("Resposta da API está vazia ou nula.");
				return null;
			}

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

		// TODO: salvar sessao no banco de dados de ver persisitencia dela
		SessionOutput session = login();
		facialUseCase.send(session.getSession(), pedestre.getCardNumber(), pedestre.getFoto());

		return null;

	}

	public Void removerUsuario(final PedestrianAccessEntity pedestre) {
		return null;

	}

}
