package com.protreino.services.utils;

import java.io.IOException;
import java.util.concurrent.TimeUnit;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import com.burgstaller.okhttp.digest.Credentials;
import com.burgstaller.okhttp.AuthenticationCacheInterceptor;
import com.burgstaller.okhttp.CachingAuthenticatorDecorator;
import com.burgstaller.okhttp.digest.CachingAuthenticator;
import com.burgstaller.okhttp.digest.DigestAuthenticator;

import okhttp3.Call;
import okhttp3.Callback;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;

public class HIkiVisionIntegrationService {

	private String url;
	private String user;
	private String password;

	private static HIkiVisionIntegrationService instance;

	private HIkiVisionIntegrationService() {
	}

	public HIkiVisionIntegrationService getInstace() {
		if (instance == null) {
			url = Utils.getPreference("hikivisionServerRecognizerURL");
			user = Utils.getPreference("hikivisionUserServerConnection");
			password = Utils.getPreference("hikivisionPasswordServerConnection");

			if (Utils.isNullOrEmpty(url) || Utils.isNullOrEmpty(user) || Utils.isNullOrEmpty(password)) {
				throw new IllegalArgumentException("Url connection não pode ser nula");
			}
			
			instance = new HIkiVisionIntegrationService();
		}

		return instance;
	}

	public void getSystemInformation() {
		OkHttpClient client = getOkHttpClient();

		Request request = new Request.Builder()
				.url(url + "/ISAPI/System/deviceInfo?format=json")
				.get()
				.addHeader("Content-Type", "application/json")
				.build();
		
//		Response response = client.newCall(request).execute();

		client.newCall(request)
		.enqueue(new Callback() {
			public void onResponse(Call call, Response response) throws IOException {
				System.out.println(response.body().string());
			}

			public void onFailure(Call call, IOException e) {
				e.printStackTrace();
			}
		});

	}
	
	private OkHttpClient getOkHttpClient() {
		final DigestAuthenticator authenticator = new DigestAuthenticator(new Credentials(user, password));
		final Map<String, CachingAuthenticator> authCache = new ConcurrentHashMap<>();
		
		return new OkHttpClient.Builder()
				.authenticator(new CachingAuthenticatorDecorator(authenticator, authCache))
				.addInterceptor(new AuthenticationCacheInterceptor(authCache))
				.readTimeout(1000, TimeUnit.MILLISECONDS)
				.writeTimeout(1000, TimeUnit.MILLISECONDS)
				.build();
	}
	
}
