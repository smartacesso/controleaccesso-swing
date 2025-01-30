package com.protreino.services.usecase;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.SocketTimeoutException;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Objects;

import com.google.gson.Gson;
import com.protreino.services.to.controlIdDevice.LoginInput;
import com.protreino.services.to.controlIdDevice.SessionOutput;
import com.protreino.services.to.controlIdDevice.ValidOutput;
import com.protreino.services.to.hikivision.HikivisionUserInfoTO;

import com.protreino.services.entity.PedestrianAccessEntity;

//import org.apache.commons.codec.binary.Base64;
import java.util.Base64;

import okhttp3.MediaType;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;

public class ControlIdDeviceService {

	public String url;
	private  String ip;
	
	private static Gson gson;
	protected static SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");

	public ControlIdDeviceService() {
		gson = new Gson();
	}
	
	private static String send(String method, String url, String contentType, String body, int connectTimeout, int readTimeout) {
	    HttpURLConnection conn = null;
	    try {
	        System.out.println("Iniciando requisição para URL: " + url);

	        // Configura a conexão
	        URL endpoint = new URL(url);
	        conn = (HttpURLConnection) endpoint.openConnection();
	        conn.setRequestMethod(method);
	        conn.setRequestProperty("Content-Type", contentType);
	        conn.setConnectTimeout(connectTimeout);
	        conn.setReadTimeout(readTimeout);

	        if ("POST".equalsIgnoreCase(method)) {
	            conn.setDoOutput(true); // Habilita envio de dados

	            if (body == null || body.isEmpty()) {
	                conn.setRequestProperty("Content-Length", "0");
	                conn.getOutputStream().close(); // Fecha a saída para evitar bloqueio
	            } else {
	                try (OutputStream os = conn.getOutputStream()) {
	                    os.write(body.getBytes("UTF-8"));
	                    os.flush();
	                }
	            }
	        }

	        // Lê a resposta
	        int responseCode = conn.getResponseCode();
	        if (isFamily2XX(responseCode)) {
	            try (BufferedReader br = new BufferedReader(new InputStreamReader(conn.getInputStream()))) {
	                StringBuilder response = new StringBuilder();
	                String line;
	                while ((line = br.readLine()) != null) {
	                    response.append(line);
	                }
	                System.out.println("Resposta recebida: " + response);
	                return response.toString();
	            }
	        } else {
	            try (BufferedReader br = new BufferedReader(new InputStreamReader(conn.getErrorStream()))) {
	                StringBuilder errorResponse = new StringBuilder();
	                String line;
	                while ((line = br.readLine()) != null) {
	                    errorResponse.append(line);
	                }
	                System.err.println("Erro na requisição. Código: " + responseCode + ". Resposta: " + errorResponse);
	            }
	        }
	    } catch (SocketTimeoutException e) {
	        System.err.println("Erro: Timeout ao conectar à URL: " + url);
	    } catch (IOException e) {
	        System.err.println("Erro: Não foi possível conectar à URL: " + url);
	        e.printStackTrace();
	    } finally {
	        if (conn != null) {
	            conn.disconnect();
	        }
	    }
	    return null;
	}

	//TODO: Necessario tirar o Static
	public static String postMessage(final String contentType,final String url, final String body) {
		
	    if (Objects.isNull(body) || body.isEmpty()) {
           return null;// Fecha a saída para evitar bloqueio
        } 
		
	    HttpURLConnection conn = null;
	    try {
	        System.out.println("Iniciando requisição para URL: " + url);

	        // Configura a conexão
	        URL endpoint = new URL(url);
	        conn = (HttpURLConnection) endpoint.openConnection();
	        conn.setRequestMethod("POST");
	        conn.setRequestProperty("Content-Type", contentType);

	        conn.setDoOutput(true);
	        
	        OutputStream os = conn.getOutputStream();
	        
	        os.write(body.getBytes("UTF-8"));
            os.flush();
	        
	        int responseCode = conn.getResponseCode();
	        
	        if (!isFamily2XX(responseCode)) {
	        	return null;
	        }
	        
	        try (BufferedReader br = new BufferedReader(new InputStreamReader(conn.getInputStream()))) {
                String line = br.readLine();
                
                if (Objects.isNull(line) || line.isEmpty()) {
                    return null;
                }
                System.out.println("Resposta recebida: " + line);
                return line;
            }
	        
	    } catch (SocketTimeoutException e) {
	        System.err.println("Erro: Timeout ao conectar à URL: " + url);
	    } catch (IOException e) {
	        System.err.println("Erro: Não foi possível conectar à URL: " + url);
	        e.printStackTrace();
	    } catch (Exception e) {
	        System.err.println("Erro: não foi possível enviar a requisicao: " + e.getMessage());
	        e.printStackTrace();
	    }
	    finally {
	    	if(Objects.nonNull(conn))
	    	conn.disconnect();
	    }
	    return null;
	}
	
	public static String getMessage(final String contentType,final String url) { 
		
	    HttpURLConnection conn = null;
	    try {
	        System.out.println("Iniciando requisição para URL: " + url);

	        // Configura a conexão
	        URL endpoint = new URL(url);
	        conn = (HttpURLConnection) endpoint.openConnection();
	        conn.setRequestMethod("POST");
	        conn.setRequestProperty("Content-Type", contentType);

	        conn.setDoOutput(true);
	        
	        int responseCode = conn.getResponseCode();
	        
	        if (!isFamily2XX(responseCode)) {
	        	return null;
	        }
	        
	        try (BufferedReader br = new BufferedReader(new InputStreamReader(conn.getInputStream()))) {
                String line = br.readLine();
                
                if (Objects.isNull(line) || line.isEmpty()) {
                    return null;
                }
                System.out.println("Resposta recebida: " + line);
                return line;
            }
	        
	    } catch (SocketTimeoutException e) {
	        System.err.println("Erro: Timeout ao conectar à URL: " + url);
	    } catch (IOException e) {
	        System.err.println("Erro: Não foi possível conectar à URL: " + url);
	        e.printStackTrace();
	    } catch (Exception e) {
	        System.err.println("Erro: não foi possível enviar a requisicao: " + e.getMessage());
	        e.printStackTrace();
	    }
	    finally {
	    	if(Objects.nonNull(conn))
	    	conn.disconnect();
	    }
	    return null;
	}


	private static boolean isFamily2XX(int responseCode) {
		return responseCode >= 200 && responseCode <= 299;
	}

	public static String login(LoginInput login, String ip) {
	    if (Objects.isNull(login)) {
	        System.err.println("LoginInput não pode ser nulo.");
	        return null;
	    }

	    String url = "http://" + ip + "/login.fcgi";
	    String payload = gson.toJson(login);
	    String response = send("POST", url, "application/json", payload, 5000, 5000);
	    //TODO: vamos trabalhar com objetos, nao com strings
	    String responseLogin = postMessage(url, "application/json", payload);

	    if (response != null) {
	        // Parse da resposta para obter a sessão
	        SessionOutput sessionOutput = gson.fromJson(response, SessionOutput.class);
	        if (sessionOutput != null && sessionOutput.getSession() != null) {
	            System.out.println("Sessão obtida com sucesso: " + sessionOutput.getSession());
	            return sessionOutput.getSession();
	        } else {
	            System.err.println("Falha ao obter a sessão da resposta.");
	        }
	    }
	    return null;
	}
	
	
	public static boolean logout(String session, String ip) {
	    if (Objects.isNull(session) || session.isEmpty()) {
	        System.err.println("Sessão não pode ser nula ou vazia.");
	        return false;
	    }

	    String url = "http://" + ip + "/logout.fcgi?session=" + session;
	    String response = send("POST", url, "application/json", null, 5000, 5000);

	    if (response == null || response.trim().equals("{}")) {
	        System.out.println("Logout realizado com sucesso.");
	        return true;
	    }
	    System.err.println("Falha no logout.");
	    return false;
	}

	
	public static boolean isValidSession(String session, String ip) {
	    if (Objects.isNull(session) || session.isEmpty()) {
	        System.err.println("Sessão não pode ser nula ou vazia.");
	        return false;
	    }

	    String url = "http://" + ip + "/session_is_valid.fcgi?session=" + session;
	    String response = send("POST", url, "application/json", null, 5000, 5000);

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

	//Listagem de Usuarios na camera
	public static boolean ListagemdeUsers(String session, String ip) {
	
	    String url = "http://" + ip + "/user_list_images.fcgi?get_timestamp=1&session=" + session;
	    String response = send("GET", url, "application/json", null, 5000, 5000);

	    if (response == null || response.trim().equals("{}")) {
	        System.out.println("Users recebidos.");
	        return true;
	    }
	    System.err.println("Erro em receber quantos usuarios.");
	    return false;
	}
	
	//Recebe fotos do facial
	//Precisa puxar uma variavel com um array de todos os IDs dentro do facial e colocar essa variavel em idsFacial
	public static boolean ColetadeFotoFacial(String session, String ip) {
		
	    String url = "http://" + ip + "/user_get_image.fcgi?user_id="+ idsFacial +"&get_timestamp=0&session=" + session;
	    String response = send("GET", url, "application/json", null, 5000, 5000);

	    if (response == null || response.trim().equals("{}")) {
	        System.out.println("Foto de usuario foi recebidos.");
	        return true;
	    }
	    System.err.println("Erro em receber fotos de usuarios dentro do facial.");
	    return false;
	}

	//Envio de foto para a camera
	//precisa trocar a variavel imagePath para a recebe a foto
	public static boolean EnviodeFoto(String session, String ip, String cardNumber, byte[] foto) { 
		
		try {
			
	        
	        //Codifica a imagem em Base64
			String fotoBase64 = Base64.getEncoder().encodeToString(foto);

	        //Envia o Base64 para a câmera
	        String url = "http://" + ip + "/user_set_image.fcgi?user_id=" + cardNumber + "&match=1&timestamp=1624997578&session=" + session;
	        String response = send("POST", url, "application/octet-stream", fotoBase64, 5000, 5000);

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
