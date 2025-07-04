package com.protreino.services.utils;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

import org.java_websocket.WebSocket;

import com.google.gson.Gson;
import com.protreino.services.websocket.topdata.dto.CommandCard;
import com.protreino.services.websocket.topdata.dto.CommandDeleteUser;
import com.protreino.services.websocket.topdata.dto.CommandFoto;
import com.protreino.services.websocket.topdata.dto.CommandInfoBasica;

public class FacialTopDataIntegrationService {
	
    private FacialTopWebSocketServer webSocketServer;
    
    private static Gson gson = new Gson();

	public FacialTopDataIntegrationService(final String ipAddress, final int port) {
		// Configura o endereço do servidor WebSocket (porta 9999, por exemplo)
        // porta do servidor definida na camera 	
        // porta 7792 e do equipamento nao do servidor
//        InetSocketAddress address = new InetSocketAddress("0.0.0.0", 9999);
        final InetSocketAddress address = new InetSocketAddress(ipAddress, port);

        // Cria o servidor WebSocket e o inicializa com a instância de ServerRetorno
        this.webSocketServer = new FacialTopWebSocketServer(address);
	}
    
    public void cadastrarPedestre(final long enrollid, final String nome, final byte[] foto) {
    	final String fotoBase64 = Base64.getEncoder().encodeToString(foto);
    	enviaComandoCadastrarInfoBasica(enrollid, nome, null);
    	enviaComandoCadastrarFoto(enrollid, fotoBase64, null);
    }
    
    public void cadastrarPedestreBySync(final long enrollid, final String nome, final String fotoBase64, List<String> devices) {
    	enviaComandoCadastrarInfoBasica(enrollid, nome, devices);
    	enviaComandoCadastrarFoto(enrollid, fotoBase64, devices);
    	
    }
    
    public void DeletePedestre(final long enrollid) {
    	enviaComandoDeleteUser(enrollid);
    }
    
    public void conectarPorta() {
        // Inicia o servidor
        this.webSocketServer.start();
        System.out.println("Servidor WebSocket iniciado");
    }
    
 //////////////////////////////////////////////////////////////////////////////////////////////
    
    // Método para enviar o comando de getuserinfo para buscar as informações do usuário
    public void enviarComandoGetUserInfo(int enrollid, int backupnum) {
        // Monta o comando JSON
        String comando = String.format(
            "{\"cmd\":\"getuserinfo\",\"enrollid\":%d,\"backupnum\":%d}",
            enrollid, backupnum
        );
        	sendCommandAllDevices(comando);
    }
    
    //Parte logica do cadastro do nome para o facial topdata
    public void enviaComandoCadastrarInfoBasica(final long enrollid, final String nome, List<String> devices) {
    	System.out.println("enviando info basica");
    	final CommandInfoBasica commandInfoBasica  = new CommandInfoBasica(enrollid, nome);
    	final String comando = gson.toJson(commandInfoBasica);
    	
    	if(Objects.isNull(devices) || devices.isEmpty()) {
    		System.out.println("Enviando para todos devices da rede");
    		sendCommandAllDevices(comando);    
    		return;
    	}
    	
    	for(String device : devices) {
    		sendCommandToDevice(device, comando);
    	}
    }
    
    //Parte logica do cadastro do cartão para o facial topdata
    public void enviaComandoCadastrarCartao(final long enrollid, final long cartao, List<String> devices) {
    	System.out.println("enviando cartao");
    	final CommandCard commandCard = new CommandCard(enrollid, cartao);
    	final String comando = gson.toJson(commandCard);
    	
    	if(Objects.isNull(devices) || devices.isEmpty()) {
    		System.out.println("Enviando para todos devices da rede");
    		sendCommandAllDevices(comando);   
    		return;
    	}
    	
    	for(String device : devices) {
    		sendCommandToDevice(device, comando);
    	}
    }
    
    //Parte logica do cadastro da foto para o facial topdata (decodifica para foto para base 64)
    public void enviaComandoCadastrarFoto(final long enrollid, final String fotoBase64, List<String> devices) {
    	System.out.println("enviando foto");
    	webSocketServer.resetResultadoCadastro();
    	final CommandFoto commandFoto = new CommandFoto(enrollid, fotoBase64);
    	final String comando = gson.toJson(commandFoto);
    	
    	if(Objects.isNull(devices) || devices.isEmpty()) {
    		System.out.println("Enviando para todos devices da rede");
    		sendCommandAllDevices(comando);  
    		return;
    	}
    	
    	for(String device : devices) {
    		sendCommandToDevice(device, comando);
    	}
    }
        
    //Parte logica para deletar o cadastro completo no facial topdata
    public void enviaComandoDeleteUser(final long enrollid) {
    	final CommandDeleteUser commandDeleteUser = new CommandDeleteUser(enrollid);
    	
    	sendCommandAllDevices(gson.toJson(commandDeleteUser));
    }
      
  //Parte logica do envio para o WebSocket
    private void sendCommandAllDevices(final String command) {
    	// Envia o comando para o servidor WebSocket
    	System.out.println("Total de conexões: " + webSocketServer.getConnections().size());
    	final Iterator<WebSocket> iterator = this.webSocketServer.getConnections().iterator();
    	
    	while(iterator.hasNext()) {
    		final WebSocket socket = iterator.next();
    		if (socket != null && socket.isOpen()) {
    			socket.send(command); // Envia o comando de getuserinfo
    			System.out.println("Comando enviado: " + command);
    		} else {
    			System.err.println("WebSocket não está conectado.");
    		}
    	}
    }
    
    private void sendCommandToDevice(final String ipAddress, final String command) {
    	WebSocket webSocketByIpAddress = getWebSocketByIpAddress(ipAddress);
    	
    	if(Objects.isNull(webSocketByIpAddress)) {
    		System.out.println("Device não encontrado!!");
    		return;
    	}
    	
    	webSocketByIpAddress.send(command); // Envia o comando de getuserinfo
		System.out.println("Comando enviado: " + command);
    }
    
    private WebSocket getWebSocketByIpAddress(final String ipAddress) {
    	final Iterator<WebSocket> iterator = this.webSocketServer.getConnections().iterator();
    	
    	while(iterator.hasNext()) {
    		final WebSocket socket = iterator.next();
    		
    		if(ipAddress.equals(socket.getRemoteSocketAddress().getHostString())) {
    			return socket;
    		}
    	}
    	
    	return null;
    }
    
    public List<WebSocket> getAllTopDataFacialDevicesConnected() {
    	final List<WebSocket> devicesConnected = new ArrayList<>();
    	
    	final Iterator<WebSocket> iterator = this.webSocketServer.getConnections().iterator();
    	
    	while(iterator.hasNext()) {
    		final WebSocket socket = iterator.next();
    		devicesConnected.add(socket);
    	}
    	
    	return devicesConnected;
    }

//////////////////////////////////////////
    
    public static void main(String[] args) {
    	
        // Cria o serviço e conecta na porta
        FacialTopDataIntegrationService service = new FacialTopDataIntegrationService("0.0.0.0", 9999); // "IP (string) , Porta (Int)"
        service.conectarPorta();
        
        try {
            Thread.sleep(2000); // 2 segundos de espera (ajuste conforme necessário)
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        
        //Primeiro cadastro de info basica
        //Cadastro do cartao (Se enviar algum numero de cartao ele converte, se nao enviar nada ele usa a matricula)
        //Cadastro de foto
        
       //Envia cadastro "numero do Id , Nome" 
       //O ID enviado para o facial vai ser o numero do cartao do pedestre
 //    service.enviaComandoCadastrarInfoBasica(9, "Gui gui"); 
       
       //nao precisa pois usa o proprio id
//     service.enviaComandoCadastrarCartao(6, 123456);
       
       //cadastro de foto "numero do Id (cartão do pedestre) , caminho da foto"
//       service.enviaComandoCadastrarFoto(9, "");

       //deleta todo cadastro passando id (cartão do pedestre)
//       service.enviaComandoDeleteUser(6);
       
       //Coleta informações do cadstro no facial "numero do Id (cartão do pedestre) , Cartão registrado no facial topdata"
//        service.enviarComandoGetUserInfo(2345,50);

    }
}
