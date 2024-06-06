package com.protreino.services.services;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

import com.burgstaller.okhttp.AuthenticationCacheInterceptor;
import com.burgstaller.okhttp.CachingAuthenticatorDecorator;
import com.burgstaller.okhttp.digest.CachingAuthenticator;
import com.burgstaller.okhttp.digest.Credentials;
import com.burgstaller.okhttp.digest.DigestAuthenticator;
import com.google.gson.Gson;
import com.protreino.services.utils.Utils;

import okhttp3.MediaType;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;

public class ControlIdIntegrationService {

	private static String url;
	private static String user;
	private static String password;
	private static Gson gson;
	
	private static ControlIdIntegrationService instance;
	
	private ControlIdIntegrationService() {
	}
	
	public static ControlIdIntegrationService getInstance() {
		if(Objects.isNull(instance)) {
			url = Utils.getPreference("controlIdURL");
			user = Utils.getPreference("controlIdUser");
			password = Utils.getPreference("controlIdPassword");

			gson = new Gson();

			instance = new ControlIdIntegrationService();
		}
		
		return instance;
	}
	
	public void login() {
		final String body = "{"
				+ 	"\"login\": \""+ user +"\","
				+ 	"\"password\": \""+ password +"\""
				+ "}";
		
		RequestBody requestBody = RequestBody.create(body, MediaType.parse("application/json"));
		OkHttpClient client = getOkHttpClient();
		
		Request request = new Request.Builder().url(url + "/login.fcgi")
				.post(requestBody).addHeader("Content-Type", "application/json").build();

		try (Response response = client.newCall(request).execute();) {
			System.out.println(response.body().string());
			
		} catch (Exception e) {
			e.printStackTrace();
		}
		
	}
	
	private OkHttpClient getOkHttpClient() {
		return new OkHttpClient.Builder()
				.readTimeout(5000, TimeUnit.MILLISECONDS)
				.writeTimeout(5000, TimeUnit.MILLISECONDS).build();
	}
}
