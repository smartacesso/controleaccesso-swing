package com.protreino.services.services;

import java.net.URI;

import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;

public class WebSocketClientService {
	
	private WebSocketClient client;
    private String urlAtual;
    private String unidadeAtual;
	
	
	public void conectar(String urlServer, String unidadeOrganizacional) {

        this.urlAtual = urlServer;
        this.unidadeAtual = unidadeOrganizacional;

        try {
        	
        	String protocoloWebSocket = urlServer.startsWith("https") ? "wss" : "ws";
        	String host = urlServer.replaceFirst("^https?://", ""); // Remove http:// ou https://
        	String urlFinal = protocoloWebSocket + "://" + host + "/ws/local/" + unidadeOrganizacional;

            client = new WebSocketClient(new URI(urlFinal)) {
				
				@Override
				public void onOpen(ServerHandshake handshake) {
					 System.out.println("Conectado ao servidor websocket");
//	                 reconnecting.set(false);
				}
				
				@Override
				public void onMessage(String message) {
					// TODO Auto-generated method stub
					System.out.println("Mensagem recebida: " + message);
				}
				
				@Override
				public void onError(Exception ex) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void onClose(int code, String reason, boolean remote) {
					// TODO Auto-generated method stub
					
				}
			};

            client.connect();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

}
