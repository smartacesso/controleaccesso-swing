package com.protreino.services.usecase;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.SocketTimeoutException;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Objects;

import com.google.gson.Gson;
import com.protreino.services.to.controlIdDevice.LoginInput;
import com.protreino.services.to.controlIdDevice.SessionOutput;
import com.protreino.services.to.hikivision.HikivisionUserInfoTO;

import okhttp3.MediaType;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;

public class ControlIdDeviceService {

	public String url;
	private static String user;
	private static String password;
	private static Gson gson;
	private String ip;
	protected SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");

	public ControlIdDeviceService(String url, String user, String password, String ip) {
		this.url = "http://192.168.15.13";
		this.user = "admin";
		this.password = "admin";
		this.ip = "192.168.15.13";
		gson = new Gson();
	}

	private SessionOutput login(LoginInput login) {
		
		if (Objects.isNull(login)) {
			return null;
		}
		try {
			URL url = new URL("http://" + ip);
			HttpURLConnection conn = (HttpURLConnection) url.openConnection();
			conn.setRequestMethod("POST");
			conn.setRequestProperty("Content-type", "application/json");
			conn.setDoInput(true);
			conn.setDoOutput(true);

			System.out.println("login body: " + gson.toJson(login));
			byte[] controlIdPayload = gson.toJson(login).getBytes();

			// testar depois se precisa dessa conversao

			OutputStream os = conn.getOutputStream();
			os.write(controlIdPayload);

			if (conn.getResponseCode() < 199 || conn.getResponseCode() > 299) {
				BufferedReader br = new BufferedReader(new InputStreamReader(conn.getErrorStream()));
				String output, result = "";

				while ((output = br.readLine()) != null) {
					result += output;
				}
				System.out.println(sdf.format(new Date()) + " Result login error: " + result);
				return null;
			}

			BufferedReader br = new BufferedReader(new InputStreamReader(conn.getInputStream()));
			String output = br.readLine();
			
			SessionOutput userResponse = null;

			if (Objects.nonNull(output) && !output.isEmpty()) {
				userResponse = gson.fromJson(output, SessionOutput.class);
			}

			conn.disconnect();
			return null;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	private Void alterarUsuario(LoginInput loginInput) {
		
		if (Objects.isNull(loginInput)) {
			return null;
		}

		try {
			URL url = new URL("http://" + ip);
			HttpURLConnection conn = (HttpURLConnection) url.openConnection();
			conn.setRequestMethod("POST");
			conn.setRequestProperty("Content-type", "application/json");
			conn.setDoInput(true);
			conn.setDoOutput(true);
		
			System.out.println("login body: " + gson.toJson(loginInput));
			byte[] controlIdPayload = gson.toJson(loginInput).getBytes();

			OutputStream os = conn.getOutputStream();
			os.write(controlIdPayload);

			if (conn.getResponseCode() < 199 || conn.getResponseCode() > 299) {
				BufferedReader br = new BufferedReader(new InputStreamReader(conn.getErrorStream()));
				String output, result = "";

				while ((output = br.readLine()) != null) {
					result += output;
				}
				System.out.println(sdf.format(new Date()) + " Result recriate Users error: " + result);
				return null;
			}

			BufferedReader br = new BufferedReader(new InputStreamReader(conn.getInputStream()));
			String output = br.readLine();
			
			SessionOutput userResponse = null;

			if (Objects.nonNull(output) && !output.isEmpty()) {
				userResponse = gson.fromJson(output, SessionOutput.class);
			}
			
			conn.disconnect();
			return userResponse;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;

	}

}
