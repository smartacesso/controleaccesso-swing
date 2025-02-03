package com.protreino.services.usecase;

import java.util.Base64;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import com.protreino.services.to.controlIdDevice.LoginInput;
import com.protreino.services.to.controlIdDevice.SessionOutput;
import com.protreino.services.to.controlIdDevice.ValidOutput;

public class ControlIdUseCase {
	
	Gson gson = new Gson();
	
	ControlIdDeviceService controlIdService = new ControlIdDeviceService();
	
	public String login(LoginInput login, String ip) {
		
	    if (Objects.isNull(login)) {
	        System.err.println("LoginInput não pode ser nulo.");
	        return null;
	    }

	    final String url = "http://" + ip + "/login.fcgi";
	    String payload = gson.toJson(login);
	    //TODO: vamos trabalhar com objetos, nao com strings
	 
	    SessionOutput sessionOutput;
		try {
			sessionOutput = (SessionOutput) controlIdService.postMessage(url, "application/json", payload, SessionOutput.class);
			  if (Objects.isNull(sessionOutput)) {
			    	return null;
			        // Parse da resposta para obter a sessão
			    }
			    System.out.println("Sessão obtida com sucesso: " + sessionOutput.getSession());
		        return sessionOutput.getSession();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	    return null;
	}
	
	public boolean logout(String session, String ip) {
	    if (Objects.isNull(session) || session.isEmpty()) {
	        System.err.println("Sessão não pode ser nula ou vazia.");
	        return false;
	    }

	    String url = "http://" + ip + "/logout.fcgi?session=" + session;
	    String response = controlIdService.getMessage("application/json", url);

	    if (response == null || response.trim().equals("{}")) {
	        System.out.println("Logout realizado com sucesso.");
	        return true;
	    }
	    System.err.println("Falha no logout.");
	    return false;
	}
	
	public boolean isValidSession(String session, String ip) {
	    if (Objects.isNull(session) || session.isEmpty()) {
	        System.err.println("Sessão não pode ser nula ou vazia.");
	        return false;
	    }

	    String url = "http://" + ip + "/session_is_valid.fcgi?session=" + session;
	    String response = controlIdService.getMessage(url, "application/json");

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

	

	private Void alterarUsuario(String session, LoginInput login) {

		//terminar
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
	
	
	public List<Long> createObjects(String session, String object, List<Map<String, Object>> values, String ip) {
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
	        String url = "http://" + ip + "/create_objects.fcgi?session=" + session;

	        // Monta o payload da requisição
	        Map<String, Object> payload = new HashMap<>();
	        payload.put("object", object);
	        payload.put("values", values);

	        String jsonPayload = gson.toJson(payload);

	        // Envia a requisição usando o método genérico `send`
	        Object response = controlIdService.postMessage("application/json", url, jsonPayload, Object.class);

	        if (Objects.isNull(object)) {
	            System.err.println("Resposta da API está vazia ou nula.");
	            return null;
	        }

	        // Converte a resposta em um objeto contendo os IDs dos objetos criados
	        Map<String, List<Long>> responseMap = gson.fromJson((String) response, new TypeToken<Map<String, List<Long>>>() {}.getType());
	        return responseMap.get("ids");

	    } catch (Exception e) {
	        e.printStackTrace();
	        return null;
	    }
	}


	//Listagem de Usuarios na camera
	public String ListagemdeUsers(String session, String ip) {
		
	    String url = "http://" + ip + "/user_list_images.fcgi?get_timestamp=1&session=" + session;
	    String response = controlIdService.getMessage( "application/json", url);

	    if (response == null || response.trim().equals("{}")) {
	        System.out.println("Users recebidos.");
	        return response;
	    }
	    String idsFacial =  response;
	    System.err.println("Erro em receber quantidade de usuarios.");
	    return idsFacial;
	}
	
	//Recebe fotos do facial
	//Precisa puxar uma variavel com um array de todos os IDs dentro do facial e colocar essa variavel em idsFacial
	public boolean ColetadeFotoFacial(String session, String ip, String idsFacial) {
		
	    String url = "http://" + ip + "/user_get_image.fcgi?user_id="+ idsFacial +"&get_timestamp=0&session=" + session;
	    String response = controlIdService.getMessage("application/json", url);

	    if (response == null || response.trim().equals("{}")) {
	        System.out.println("Foto de usuario foi recebidos.");
	        return true;
	    }
	    System.err.println("Erro em receber fotos de usuarios dentro do facial.");
	    return false;
	}

	//Envio de foto para a camera
	//Inacabada
	public boolean EnviodeFoto(String session, String ip, String cardNumber, byte[] foto) { 
		
		try {

	        //Codifica a imagem em Base64
			String fotoBase64 = Base64.getEncoder().encodeToString(foto);

	        //Envia o Base64 para a câmera
	        String url = "http://" + ip + "/user_set_image.fcgi?user_id=" + cardNumber + "&match=1&timestamp=1624997578&session=" + session;
	        String response = (String) controlIdService.postMessage("application/octet-stream", url, fotoBase64, null);

	        if (response == null || response.trim().equals("{}")) {
	            System.out.println("Foto enviada com sucesso");
	            return true;
	        } else {
	            System.err.println("Erro ao enviar foto.");
	            return false;
	        }

	    } catch (Exception e) {
	        e.printStackTrace();
	        return false;
	    }
	}
	

}
