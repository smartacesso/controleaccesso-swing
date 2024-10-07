package com.protreino.services.devices;
import java.io.*;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Set;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;
import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.to.AttachedTO;
import com.protreino.services.to.ConfigurationGroupTO;
import com.topdata.easyInner.entity.Inner;

public class AlmitecDevice extends Device{
    private String equipamentoIP;
    private int portaTCP1;
    private int portaTCP2;
    private int udpPorta;
    private Socket tcp1;
    private Socket tcp2;
    private DatagramSocket udpSocket;
    private boolean conectado = false;

    // Construtor teste
//    public AlmitecDevice(String ip, int portaTCP1, int portaTCP2, int udpPorta) throws SocketException {
//        this.equipamentoIP = ip;
//        this.portaTCP1 = portaTCP1;
//        this.portaTCP2 = portaTCP2;
//        this.udpPorta = udpPorta;
//        this.udpSocket = new DatagramSocket();
//    }
    
    
	public AlmitecDevice(DeviceEntity deviceEntity){
		this(deviceEntity.getIdentifier(), deviceEntity.getConfigurationGroupsTO());
		this.deviceEntity = deviceEntity;
		this.name = deviceEntity.getName();
		this.location = deviceEntity.getLocation();
		this.desiredStatus = deviceEntity.getDesiredStatus();
		this.defaultDevice = deviceEntity.getDefaultDevice();
		this.athleteScreenConfig = deviceEntity.getAthleteScreenConfig();

	}
	
	public AlmitecDevice(String identifier){
		this(identifier, null);
	}
	
	
	public AlmitecDevice(String identifier, List<ConfigurationGroupTO> configurationGroups){
		this.manufacturer = Manufacturer.ALMITEC;
		this.identifier = identifier;
		String partes[] = identifier.split(";");
		this.equipamentoIP = partes[0];
		this.portaTCP1 = Integer.parseInt(partes[1]);
		this.portaTCP2 = Integer.parseInt(partes[2]);
		this.udpPorta = Integer.parseInt(partes[3]);
		


		this.name = "Catraca Almitec";
		if (configurationGroups != null) {
			this.configurationGroups = configurationGroups;
		} else {
			createDefaultConfiguration();
		}
		
		createConfigurationMap();
	}

    /*
    public AlmTCP() throws SocketException {
    	
    	// Inicializa a porta UDP e IP da placa
    	
    	this.equipamentoIP = Utils.getPreference("catracaAlmitecURL");
    	this.portaTCP1 = Integer.valueOf(Utils.getPreference("tcpServerAlmitecSocketPort"));
    	 this.portaTCP2 = 2001;
    	this.udpPorta = Integer.valueOf(Utils.getPreference("tcpServerAlmitecUdpSocketPort"));
    	this.udpSocket = new DatagramSocket();
    	conectarTCP();
    }*/	
	
	private boolean tentativaConexao = true; // Flag para controle de tentativas de conexão
	private int intervaloTentativa = 5000; 
	
	@Override
	public void connect(String... args) throws Exception {
	    tentativaConexao = true;

	    Thread tcpThread = new Thread(() -> {
	        while (tentativaConexao) {
	            try {
	                tcp1 = new Socket(equipamentoIP, portaTCP1);
	                tcp2 = new Socket(equipamentoIP, portaTCP2);

	                conectado = true;
	                System.out.println("Conectado ao equipamento via TCP.");

	                // A partir de agora, ativar a leitura
	                leituraAtiva = true;

	                new Thread(() -> receberDadosTCP(tcp1, "Leitor 1")).start();
	                new Thread(() -> receberDadosTCP(tcp2, "Leitor 2")).start();

	                setStatus(DeviceStatus.CONNECTED);
	                break; // Conexão bem-sucedida
	            } catch (IOException e) {
	                System.err.println("Erro na conexão TCP: " + e.getMessage());
	                setStatus(DeviceStatus.DISCONNECTED);
	                conectado = false;
	                try {
	                    Thread.sleep(intervaloTentativa);
	                } catch (InterruptedException ex) {
	                    ex.printStackTrace();
	                }
	            }
	        }
	    });

	    tcpThread.start();
	}
    
	@Override
	public void disconnect(String... args) throws Exception {
	    // A lógica da desconexão é mantida
	    tentativaConexao = false; // Interrompe o loop de reconexão
	    leituraAtiva = false;     // Interrompe a leitura dos dados

	    try {
	        // Aguarda um pequeno tempo para garantir que as threads parem antes de fechar o socket
	        Thread.sleep(100); 

	        if (tcp1 != null && !tcp1.isClosed()) {
	            tcp1.close();
	            System.out.println("Conexão TCP1 fechada.");
	        }
	        if (tcp2 != null && !tcp2.isClosed()) {
	            tcp2.close();
	            System.out.println("Conexão TCP2 fechada.");
	        }

	        conectado = false;
	        setStatus(DeviceStatus.DISCONNECTED);
	    } catch (IOException e) {
	        System.err.println("Erro ao fechar a conexão TCP: " + e.getMessage());
	    } catch (InterruptedException e) {
	        System.err.println("Erro ao aguardar a interrupção das threads: " + e.getMessage());
	    }
	}
	
	// Método para reiniciar a tentativa de conexão caso caia
	public void retentarConexao() {
	    if (!conectado) {
	        System.out.println("Tentando reconectar...");
	        try {
	            // Reinicia a variável tentativaConexao antes de conectar
	            tentativaConexao = true; 
	            connect();
	        } catch (Exception e) {
	            System.err.println("Erro ao tentar reconectar: " + e.getMessage());
	        }
	    }
	}
	
	private volatile boolean leituraAtiva = true; // Controle para interromper a leitura

	private void receberDadosTCP(Socket socket, String descricaoLeitor) {
	    try (BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream()))) {
	        String linha;
	        while (leituraAtiva) {  // Mantenha a leitura ativa enquanto a variável for verdadeira
	            try {
	                // Ler a linha com um timeout, se necessário
	                if ((linha = reader.readLine()) != null) {
	                    System.out.println(descricaoLeitor + " recebeu: " + linha);
	                    // Processar a leitura do código de barras
	                }
	            } catch (SocketTimeoutException e) {
	                // Timeout alcançado, você pode lidar com isso conforme necessário
	                System.err.println("Timeout ao tentar ler de " + descricaoLeitor + ": " + e.getMessage());
	            } catch (IOException e) {
	                System.err.println("Erro ao receber dados via " + descricaoLeitor + ": " + e.getMessage());
	                leituraAtiva = false; // Encerra a leitura se ocorrer um erro
	            }
	        }
	    } catch (IOException e) {
	        System.err.println("Erro ao criar leitor para " + descricaoLeitor + ": " + e.getMessage());
	    }
	}

    // M�todo para acionar o RELE via UDP
    public void acionarRele(int rele, int tempoEmDecimosDeSegundo) throws IOException {
        byte[] comando = montarComandoRele(rele, tempoEmDecimosDeSegundo);
        udpSocket = new DatagramSocket();
        InetAddress endereco = InetAddress.getByName(equipamentoIP);
        DatagramPacket pacote = new DatagramPacket(comando, comando.length, endereco, udpPorta);
        udpSocket.send(pacote);
        udpSocket.close();
        System.out.println("Comando enviado para acionar RELE " + rele + " por " + (tempoEmDecimosDeSegundo * 100) + "ms.");
    }

    // M�todo para montar o comando de acionamento do RELE
    private byte[] montarComandoRele(int rele, int tempoEmDecimosDeSegundo) {
        byte[] header = {(byte) 0x55, (byte) 0xAA};
        byte[] comando = new byte[4];  // 2 bytes para o header + 2 bytes para comando e tempo

        // Copiar o header
        System.arraycopy(header, 0, comando, 0, header.length);

        // Definir qual RELE ser� acionado (1 ou 2)
        comando[2] = (byte) (rele == 1 ? 0x04 : 0x08);

        // Definir o tempo em d�cimos de segundo
        comando[3] = (byte) tempoEmDecimosDeSegundo;

        return comando;
    }

    // M�todo para testar o acionamento do RELE 1 (recolher comanda)
    public void recolherComanda() {
        try {
            acionarRele(1, 3); // Aciona o RELE 1 por 300ms
        } catch (IOException e) {
            System.err.println("Erro ao acionar RELE 1: " + e.getMessage());
        }
    }

    // M�todo para testar o acionamento do RELE 2 (devolver comanda)
    public void devolverComanda() {
        try {
            acionarRele(2, 3); // Aciona o RELE 2 por 300ms
        } catch (IOException e) {
            System.err.println("Erro ao acionar RELE 2: " + e.getMessage());
        }
    }





	@Override
	public void createDefaultConfiguration() {
		// TODO Auto-generated method stub
		
	}


	@Override
	public void sendConfiguration() throws Exception {
		// TODO Auto-generated method stub
		
	}


	@Override
	public void allowAccess() {
		// TODO Auto-generated method stub
		
	}


	@Override
	public void denyAccess() {
		// TODO Auto-generated method stub
		
	}


	@Override
	public void processSampleForEnrollment(Object obj) {
		// TODO Auto-generated method stub
		
	}


	@Override
	public void processAccessRequest(Object obj) {
		// TODO Auto-generated method stub
		
	}


	@Override
	public Set<Integer> getRegisteredUserList() throws Exception {
		// TODO Auto-generated method stub
		return null;
	}


	@Override
	public String cadastrateUser(PedestrianAccessEntity athleteAccessEntity) {
		// TODO Auto-generated method stub
		return null;
	}


	@Override
	public String removeUser(PedestrianAccessEntity athleteAccessEntity) {
		// TODO Auto-generated method stub
		return null;
	}
}