package com.protreino.services.services;

import java.net.URI;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.util.Locale;

import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.protreino.services.to.PedestrianAccessTO;
import com.protreino.services.to.hikivision.WebSocketPedestrianAccessTO;
import com.protreino.services.usecase.RecebePedestreWebSocketUseCase;

public class WebSocketClientService {
	
	private WebSocketClient client;
    private String urlAtual;
    private String idClientAtual;
    private ObjectMapper mapper = new ObjectMapper();
    SimpleDateFormat sdf = new SimpleDateFormat("MMM dd, yyyy h:mm:ss a", Locale.US);
    private final RecebePedestreWebSocketUseCase recebePedestreWebSocketUseCase = new RecebePedestreWebSocketUseCase();
    
    public WebSocketClientService(String urlServer, String idClient) {
    	this.urlAtual = urlServer;
    	this.idClientAtual = idClient;    	
    }

	public void conectar() {
        try {
        	String protocoloWebSocket = urlAtual.startsWith("https") ? "wss" : "ws";
        	String host = urlAtual.replaceFirst("^https?://", ""); // Remove http:// ou https://
        	String urlFinal = protocoloWebSocket + "://" + host + "/ws/local/" + idClientAtual;

            client = new WebSocketClient(new URI(urlFinal)) {
				@Override
				public void onOpen(ServerHandshake handshake) {
					 System.out.println("Conectado ao servidor websocket");
				}
				
				@Override
				public void onMessage(String message) {
					try {
					    mapper.setDateFormat(sdf);
					    WebSocketPedestrianAccessTO pedestre = mapper.readValue(message, WebSocketPedestrianAccessTO.class);
						recebePedestreWebSocketUseCase.execute(pedestre);
					} catch (JsonMappingException e) {
						e.printStackTrace();
					} catch (JsonProcessingException e) {
						e.printStackTrace();
					}
					
				}
				
				@Override
				public void onError(Exception ex) {
					System.out.println("Error on websockert: " + ex.getMessage());
				}
				
				@Override
				public void onClose(int code, String reason, boolean remote) {
					System.out.println(String.format("Connection closed. Code: %s, reason: %s", code, reason));
				}
			};

            client.connect();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
	
	public void ping() {
		if(client.isClosed()) {
			System.out.println("Tentando conectar cliente novamente: " + idClientAtual);
			conectar();
			return;
		}
		
		System.out.println("Conexão com webSocket ainda aberta: " + LocalDateTime.now());
	}

}
