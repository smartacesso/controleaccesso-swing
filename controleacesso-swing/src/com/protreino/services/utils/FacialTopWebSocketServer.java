package com.protreino.services.utils;

import org.java_websocket.WebSocket;
import org.java_websocket.handshake.ClientHandshake;
import org.java_websocket.server.WebSocketServer;

import com.google.gson.Gson;
import com.protreino.services.entity.TopdataFacialErrorEntity;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.websocket.topdata.dto.CommandResponse;

import java.net.InetSocketAddress;
import java.util.Date;


public class FacialTopWebSocketServer extends WebSocketServer {

	private final Gson gson = new Gson();
	private Boolean resultadoCadastro;

    public FacialTopWebSocketServer(InetSocketAddress address, ServerRetorno svRet) {
        super(address);
    }
    
    public FacialTopWebSocketServer(InetSocketAddress address) {
        super(address);
    }
    
    @Override
    public void onOpen(WebSocket conn, ClientHandshake handshake) {
        System.out.println("Conexão aberta: " + conn.getRemoteSocketAddress());
    }

    @Override
    public void onClose(WebSocket conn, int code, String reason, boolean remote) {
        System.out.println("Conexão fechada: " + conn.getRemoteSocketAddress() + " Razão: " + reason);
    }

    @Override
    public void onMessage(WebSocket conn, String message) {
        System.out.println("Mensagem recebida de " + conn.getRemoteSocketAddress());
        try {
            CommandResponse response = gson.fromJson(message, CommandResponse.class);
            
            // Verifica se a mensagem é relevante (backupnum == 50)
            if (response.getBackupnum() == 50) {
            	if(!response.isResult()) {
            		final TopdataFacialErrorEntity topdataFacialErrorEntity = 
            				new TopdataFacialErrorEntity(conn.getRemoteSocketAddress().getHostString(), String.valueOf(response.getEnrollid()), 
            												response.getReason(), new Date());
            		
            		HibernateAccessDataFacade.save(TopdataFacialErrorEntity.class, topdataFacialErrorEntity);
            	}
            	
                resultadoCadastro = response.isResult();
            }
        } catch (Exception e) {
            System.err.println("Erro ao processar mensagem.");
            e.printStackTrace();
        }
    }

    @Override
    public void onError(WebSocket conn, Exception ex) {
        System.err.println("Erro na conexão: " + (conn != null ? conn.getRemoteSocketAddress() : "Server"));
        ex.printStackTrace();
    }

    @Override
    public void onStart() {
        System.out.println("Servidor WebSocket iniciado.");
    }
    
    public Boolean getResultadoCadastro() {
        return resultadoCadastro;
    }

    public void resetResultadoCadastro() {
        resultadoCadastro = null; // Reseta o valor de resultadoCadastro
    }

}
