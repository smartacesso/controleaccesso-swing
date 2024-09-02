package com.protreino.services.utils;

import java.net.*;
import java.io.*;
import java.text.SimpleDateFormat;

import java.util.Date;
import java.util.Objects;
import java.util.TimeZone;



public class AlmTCP {



	private int porta;
    private String placaIP;
    private int udpPorta;

    
	private static final SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss:sss");
	
	private static final SimpleDateFormat responseDateFormat = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss 'GMT'");


	public AlmTCP() {
		
        // Inicializa a porta UDP e IP da placa
		
		this.placaIP = Utils.getPreference("catracaAlmitecURL");
		this.porta = Integer.valueOf(Utils.getPreference("tcpServerAlmitecSocketPort"));
		this.udpPorta = Integer.valueOf(Utils.getPreference("tcpServerAlmitecUdpSocketPort"));
		
		Thread serverThread = new Thread(new Runnable() {
			@Override
			public void run() {
				try (ServerSocket serverSocket = new ServerSocket(porta)) {
					System.out.println(
							sdf.format(new Date()) + "  ... Almitec TCP server escutando na porta " + porta);

					while (true) {
						Socket socket = serverSocket.accept();
						socket.setTcpNoDelay(true);
						new ProcessThread(socket).start();
					}

				} catch (IOException ex) {
					System.out.println(
							sdf.format(new Date()) + "  ... Almitec TCP server exception: " + ex.getMessage());
					ex.printStackTrace();
				}
			}
		});
		serverThread.start();
	}

	public class ProcessThread extends Thread {
		private Socket socket;

		public ProcessThread(Socket socket) {
			this.socket = socket;
		}

		public void run() {
			try (InputStream inputStream = socket.getInputStream();
					OutputStream outputStream = socket.getOutputStream();
					BufferedReader br = new BufferedReader(new InputStreamReader(inputStream))) {
				
				Object received = "";
				
				while(socket.isConnected()) {
					if(!Objects.isNull(received)) {
						System.out.println("RECEBIDO SERVIDOR AlMITEC : " + received);
						acionarRele2();
					}
				}
									
			} catch (EOFException eof) {
				eof.printStackTrace();
			} catch (SocketException se) {
				se.printStackTrace();
			} catch (Exception e) {
				System.out.println(sdf.format(new Date()) + "  ... Almitec TCP server exception: " + e.getMessage());
				e.printStackTrace();
			}
		}
		
		
	    public void enviarComandoRele(int rele, int tempoEmDecimosDeSegundo) throws Exception {
	        byte[] header = {(byte) 0x55, (byte) 0xAA};
	        byte[] comando = new byte[header.length + 2];
	        
	        System.arraycopy(header, 0, comando, 0, header.length);
	        comando[2] = (byte) (rele == 1 ? 0x04 : 0x08);  // Define o RELE
	        comando[3] = (byte) tempoEmDecimosDeSegundo;     // Define o tempo

	        DatagramSocket socket = new DatagramSocket();
	        InetAddress endereco = InetAddress.getByName(placaIP);
	        DatagramPacket pacote = new DatagramPacket(comando, comando.length, endereco, udpPorta);

	        socket.send(pacote);
	        socket.close();
	    }
	    
	    // Métodos para acionar os dois relés
	    public void acionarRele1() throws Exception {
	        enviarComandoRele(1, 3);  // 300ms = 3 décimos de segundo
	    }

	    public void acionarRele2() throws Exception {
	        enviarComandoRele(2, 3);  // 300ms = 3 décimos de segundo
	    }

	}
	
}
