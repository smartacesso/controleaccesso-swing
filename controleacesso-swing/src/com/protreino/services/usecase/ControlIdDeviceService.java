package com.protreino.services.usecase;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.SocketTimeoutException;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Objects;

import com.google.gson.Gson;

public class ControlIdDeviceService {

	private String ip;

	private static Gson gson;
	protected static SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");

	public ControlIdDeviceService() {
		gson = new Gson();
	}

	public <T> Object postMessage(final String contentType, final String url, final String body, Class<T> entityClass) {

		if (Objects.isNull(body) || body.isEmpty()) {
			return null;
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

			BufferedReader br = new BufferedReader(new InputStreamReader(conn.getInputStream()));
			String line = br.readLine();

			if (Objects.isNull(line) || line.isEmpty()) {
				return null;
			}
			System.out.println("Resposta recebida: " + line);
			return line;

		} catch (SocketTimeoutException e) {
			System.err.println("Erro: Timeout ao conectar à URL: " + url);
		} catch (IOException e) {
			System.err.println("Erro: Não foi possível conectar à URL: " + url);
			e.printStackTrace();
		} catch (Exception e) {
			System.err.println("Erro: não foi possível enviar a requisicao: " + e.getMessage());
			e.printStackTrace();
		} finally {
			if (Objects.nonNull(conn))
				conn.disconnect();
		}
		return null;
	}

	public <T> Object postImsage(final String contentType, final String url, final byte[] body, Class<T> entityClass) {
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
			os.write(body);
			os.flush();

			int responseCode = conn.getResponseCode();

			if (!isFamily2XX(responseCode)) {
				return null;
			}

			BufferedReader br = new BufferedReader(new InputStreamReader(conn.getInputStream()));

			String line;
			while ((line = br.readLine()) != null) {
				System.out.println("linha " + line);
			}

			System.out.println("Resposta recebida: " + line);

			return line;

		} catch (SocketTimeoutException e) {
			System.err.println("Erro: Timeout ao conectar à URL: " + url);
		} catch (IOException e) {
			System.err.println("Erro: Não foi possível conectar à URL: " + url);
			e.printStackTrace();
		} catch (Exception e) {
			System.err.println("Erro: não foi possível enviar a requisicao: " + e.getMessage());
			e.printStackTrace();
		} finally {
			if (Objects.nonNull(conn))
				conn.disconnect();
		}
		return null;
	}

	public <T> Object getMessage(final String contentType, final String url, Class<T> entityClass) {

		HttpURLConnection conn = null;
		try {
			System.out.println("Iniciando requisição para URL: " + url);

			URL endpoint = new URL(url);
			conn = (HttpURLConnection) endpoint.openConnection();
			conn.setRequestMethod("POST");
			conn.setRequestProperty("Content-Type", contentType);

			conn.setDoOutput(true);

			int responseCode = conn.getResponseCode();

			if (!isFamily2XX(responseCode)) {
				return null;
			}

			BufferedReader br = new BufferedReader(new InputStreamReader(conn.getInputStream()));
			String line = br.readLine();

			if (Objects.isNull(line) || line.isEmpty()) {
				return null;
			}
			System.out.println("Resposta recebida: " + line);
			return line;

		} catch (SocketTimeoutException e) {
			System.err.println("Erro: Timeout ao conectar à URL: " + url);
		} catch (IOException e) {
			System.err.println("Erro: Não foi possível conectar à URL: " + url);
			e.printStackTrace();
		} catch (Exception e) {
			System.err.println("Erro: não foi possível enviar a requisicao: " + e.getMessage());
			e.printStackTrace();
		} finally {
			if (Objects.nonNull(conn))
				conn.disconnect();
		}
		return null;
	}

	private static boolean isFamily2XX(int responseCode) {
		return responseCode >= 200 && responseCode <= 299;
	}

}
