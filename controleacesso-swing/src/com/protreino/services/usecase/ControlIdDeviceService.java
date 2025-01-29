package com.protreino.services.usecase;

import java.io.BufferedReader;
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
	        if (responseCode >= 200 && responseCode <= 299) {
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


	public static String login(LoginInput login, String ip) {
	    if (Objects.isNull(login)) {
	        System.err.println("LoginInput não pode ser nulo.");
	        return null;
	    }

	    String url = "http://" + ip + "/login.fcgi";
	    String payload = gson.toJson(login);
	    String response = send("POST", url, "application/json", payload, 5000, 5000);

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

	    if (response != null) {
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

}
