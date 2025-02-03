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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
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
	public <T> Object postMessage(final String contentType, final String url, final String body, Class<T> entityClass) {
		
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


	



	
}
