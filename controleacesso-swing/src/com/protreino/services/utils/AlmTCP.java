package com.protreino.services.utils;
import java.io.*;
import java.net.*;
import java.nio.charset.StandardCharsets;

public class AlmTCP {
    private String equipamentoIP;
    private int portaTCP1;
    private int portaTCP2;
    private int udpPorta;
    private Socket tcp1;
    private Socket tcp2;
    private DatagramSocket udpSocket;
    private boolean conectado = false;

    // Construtor teste
    public AlmTCP(String ip, int portaTCP1, int portaTCP2, int udpPorta) throws SocketException {
        this.equipamentoIP = ip;
        this.portaTCP1 = portaTCP1;
        this.portaTCP2 = portaTCP2;
        this.udpPorta = udpPorta;
        this.udpSocket = new DatagramSocket();
        conectarTCP();  // Inicia a conexão TCP automaticamente
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
    
    // Método para conectar ao equipamento via TCP
    private void conectarTCP() {
        Thread tcpThread = new Thread(() -> {
            try {
                // Conectar aos sockets TCP1 e TCP2
                tcp1 = new Socket(equipamentoIP, portaTCP1);
                tcp2 = new Socket(equipamentoIP, portaTCP2);

                conectado = true;
                System.out.println("Conectado ao equipamento via TCP.");

                // Receber dados de TCP1 (leitor de código de barras)
                new Thread(() -> receberDadosTCP(tcp1, "Leitor 1")).start();
                // Receber dados de TCP2 (se precisar)
                new Thread(() -> receberDadosTCP(tcp2, "Leitor 2")).start();
                
                recolherComanda();
            } catch (IOException e) {
                System.err.println("Erro na conexão TCP: " + e.getMessage());
            }
        });

        tcpThread.start();
    }

    // Método para receber dados das portas TCP
    private void receberDadosTCP(Socket socket, String descricaoLeitor) {
        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            String linha;
            while ((linha = reader.readLine()) != null) {
                System.out.println(descricaoLeitor + " recebeu: " + linha);
                // Aqui você pode processar a leitura do código de barras e acionar o relé com base nisso
            }
        } catch (IOException e) {
            System.err.println("Erro ao receber dados via " + descricaoLeitor + ": " + e.getMessage());
        }
    }

    // Método para acionar o RELE via UDP
    public void acionarRele(int rele, int tempoEmDecimosDeSegundo) throws IOException {
        byte[] comando = montarComandoRele(rele, tempoEmDecimosDeSegundo);
        udpSocket = new DatagramSocket();
        InetAddress endereco = InetAddress.getByName(equipamentoIP);
        DatagramPacket pacote = new DatagramPacket(comando, comando.length, endereco, udpPorta);
        udpSocket.send(pacote);
        udpSocket.close();
        System.out.println("Comando enviado para acionar RELE " + rele + " por " + (tempoEmDecimosDeSegundo * 100) + "ms.");
    }

    // Método para montar o comando de acionamento do RELE
    private byte[] montarComandoRele(int rele, int tempoEmDecimosDeSegundo) {
        byte[] header = {(byte) 0x55, (byte) 0xAA};
        byte[] comando = new byte[4];  // 2 bytes para o header + 2 bytes para comando e tempo

        // Copiar o header
        System.arraycopy(header, 0, comando, 0, header.length);

        // Definir qual RELE será acionado (1 ou 2)
        comando[2] = (byte) (rele == 1 ? 0x04 : 0x08);

        // Definir o tempo em décimos de segundo
        comando[3] = (byte) tempoEmDecimosDeSegundo;

        return comando;
    }

    // Método para testar o acionamento do RELE 1 (recolher comanda)
    public void recolherComanda() {
        try {
            acionarRele(1, 3); // Aciona o RELE 1 por 300ms
        } catch (IOException e) {
            System.err.println("Erro ao acionar RELE 1: " + e.getMessage());
        }
    }

    // Método para testar o acionamento do RELE 2 (devolver comanda)
    public void devolverComanda() {
        try {
            acionarRele(2, 3); // Aciona o RELE 2 por 300ms
        } catch (IOException e) {
            System.err.println("Erro ao acionar RELE 2: " + e.getMessage());
        }
    }
}