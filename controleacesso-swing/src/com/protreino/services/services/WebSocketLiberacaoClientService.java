package com.protreino.services.services;

import java.net.URI;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Locale;

import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.protreino.services.to.hikivision.HikivisionDeviceSimplificadoTO;
import com.protreino.services.to.hikivision.WebSocketPedestrianAccessTO;
import com.protreino.services.usecase.HikivisionUseCases;
import com.protreino.services.to.hikivision.HikivisionDeviceTO.Device;

public class WebSocketLiberacaoClientService {
	private WebSocketClient client;
    private String urlAtual;
    private String idClientAtual;
    private ObjectMapper mapper = new ObjectMapper();
    SimpleDateFormat sdf = new SimpleDateFormat("MMM dd, yyyy h:mm:ss a", Locale.US);
    private HikivisionUseCases hikivisionUseCases = new HikivisionUseCases();
    
    public WebSocketLiberacaoClientService(String urlServer, String idClient) {
    	this.urlAtual = urlServer;
    	this.idClientAtual = idClient;    	
    }

	public void conectar() {
        try {
        	String protocoloWebSocket = urlAtual.startsWith("https") ? "wss" : "ws";
        	String host = urlAtual.replaceFirst("^https?://", ""); // Remove http:// ou https://
        	String urlFinal = protocoloWebSocket + "://" + host + "/ws/liberacao/" + idClientAtual;

            client = new WebSocketClient(new URI(urlFinal)) {
				@Override
				public void onOpen(ServerHandshake handshake) {
					 System.out.println("Conectado ao servidor websocket");
				}
				
				@Override
				public void onMessage(String message) {
					hikivisionUseCases.liberaCameraRemoto(message);
					System.out.println("acesso liberado");
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
	
	
	public void enviarEquipamentos(List<HikivisionDeviceSimplificadoTO> devicesSimplificados) {
	    if (client != null && client.isOpen()) {
	        try {
	            String json = mapper.writeValueAsString(devicesSimplificados);
	            client.send(json);
	            System.out.println("Lista de equipamentos enviada: " + json);
	        } catch (JsonProcessingException e) {
	            e.printStackTrace();
	        }
	    } else {
	        System.out.println("Cliente WebSocket não está conectado.");
	    }
	}


}
