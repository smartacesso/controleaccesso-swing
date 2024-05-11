package com.protreino.services.utils;

import java.io.BufferedReader;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import com.protreino.services.usecase.HikivisionEventsUseCase;

public class HikivisionTcpServer {

	private int porta;

	private static final SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss:sss");
	
	private static final SimpleDateFormat responseDateFormat = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss 'GMT'");
	private final HikivisionEventsUseCase hikivisionEventsUseCase;

	public HikivisionTcpServer() {
		this.hikivisionEventsUseCase = new HikivisionEventsUseCase();

		if(!Utils.isHikivisionConfigValid()) {
			return;
		}
		
		this.porta = Integer.valueOf(Utils.getPreference("tcpServerHikivisionSocketPort"));

		Thread serverThread = new Thread(new Runnable() {
			@Override
			public void run() {
				try (ServerSocket serverSocket = new ServerSocket(porta)) {
					System.out.println(
							sdf.format(new Date()) + "  ... Hikivision TCP server escutando na porta " + porta);

					while (true) {
						Socket socket = serverSocket.accept();
						socket.setTcpNoDelay(true);
						new ProcessThread(socket).start();
					}

				} catch (IOException ex) {
					System.out.println(
							sdf.format(new Date()) + "  ... Hikivision TCP server exception: " + ex.getMessage());
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
				final StringBuilder sb = new StringBuilder();
				try {
					String line = "";
					while (!line.equals("--MIME_boundary--")) {
						line = br.readLine();
						sb.append(line);
					}
				} catch (IOException e) {
					e.printStackTrace();
				}
				
				hikivisionEventsUseCase.execute(sb);
				sendResponse(outputStream);


			} catch (EOFException eof) {
				eof.printStackTrace();
			} catch (SocketException se) {
				se.printStackTrace();
			} catch (Exception e) {
				System.out.println(sdf.format(new Date()) + "  ... TCP server exception: " + e.getMessage());
				e.printStackTrace();
			}
		}
		
		private void sendResponse(final OutputStream outputStream) throws IOException {
			responseDateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
			final String response = "HTTP/1.1 200\n" 
					+ "Content-Length: 0\n" 
					+ "Date: " + responseDateFormat.format(new Date()) + "\n";
			
			outputStream.write(response.getBytes());
			outputStream.flush();
		}

	}
}
