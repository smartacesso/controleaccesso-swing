package com.protreino.services.utils;

import java.util.Base64;
import java.util.concurrent.TimeUnit;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import com.burgstaller.okhttp.digest.Credentials;
import com.burgstaller.okhttp.AuthenticationCacheInterceptor;
import com.burgstaller.okhttp.CachingAuthenticatorDecorator;
import com.burgstaller.okhttp.digest.CachingAuthenticator;
import com.burgstaller.okhttp.digest.DigestAuthenticator;
import com.google.gson.Gson;
import com.protreino.services.to.hikivision.HikivisionDeviceTO;

import okhttp3.MediaType;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;

public class HikiVisionIntegrationService {

	private static String url;
	private static String user;
	private static String password;
	private static Gson gson;

	private static HikiVisionIntegrationService instance;

	private HikiVisionIntegrationService() {
	}

	public static HikiVisionIntegrationService getInstace() {
		if (instance == null) {
			url = Utils.getPreference("hikivisionServerRecognizerURL");
			user = Utils.getPreference("hikivisionUserServerConnection");
			password = Utils.getPreference("hikivisionPasswordServerConnection");

			if (Utils.isNullOrEmpty(url) || Utils.isNullOrEmpty(user)) {
				throw new IllegalArgumentException("Url connection não pode ser nula");
			}
			
			gson = new Gson();
			
			instance = new HikiVisionIntegrationService();
		}

		return instance;
	}

	public boolean getSystemInformation() {
		OkHttpClient client = getOkHttpClient();

		Request request = new Request.Builder()
				.url(url + "/ISAPI/System/deviceInfo?format=json")
				.get()
				.addHeader("Content-Type", "application/json")
				.build();
		
		try {
			Response response = client.newCall(request).execute();
			return response.isSuccessful();
			
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		return false;
	}
	
	public boolean isUsuarioJaCadastrado(final String deviceId, final String id) {
		final String uuid = UUID.randomUUID().toString();
		final String body = "{"
				+ "		\"UserInfoSearchCond\": {"
				+ "			\"searchID\": \""+ uuid +"\","
				+ "			\"searchResultPosition\": 0,"
				+ "			\"maxResults\": 1,"
				+ "			\"EmployeeNoList\" : [{"
				+ "				\"employeeNo\": \""+ id +"\""
				+ "			}]"
				+ "		}"
				+ "	}";

		RequestBody requestBody = RequestBody.create(body, MediaType.parse("application/json"));
		OkHttpClient client = getOkHttpClient();

		Request request = new Request.Builder()
				.url(url + "/ISAPI/AccessControl/UserInfo/Search?format=json&devIndex=" + deviceId)
				.post(requestBody)
				.addHeader("Content-Type", "application/json")
				.build();
		
		try {
			Response response = client.newCall(request).execute();
			return response.isSuccessful();
			
		} catch (Exception e) {
			e.printStackTrace();
		}

		return false;
	}
	
	public void adicionarUsuario(final String deviceId, final String id, final String name) {
		final String body = "{" +
			"\"UserInfo\" : [{" +
				"\"employeeNo\": \""+ id +"\"," +
				"\"name\": \""+ name +"\"," +
				"\"Valid\" : {" +
					"\"beginTime\": \"2017-08-01T17:30:08\"," +
					"\"endTime\": \"2037-12-31T23:59:59\"," +
				"}" +
			"}]" +
		"}";

		RequestBody requestBody = RequestBody.create(body, MediaType.parse("application/json"));
		OkHttpClient client = getOkHttpClient();

		Request request = new Request.Builder()
				.url(url + "/ISAPI/AccessControl/UserInfo/Search?format=json&devIndex=" + deviceId)
				.post(requestBody)
				.addHeader("Content-Type", "application/json")
				.build();

		try {
			Response response = client.newCall(request).execute();

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public boolean adicionarFotoUsuario(final String deviceId, final String id, final byte[] foto) {
		final String boundary = "-----------------------------7e13971310878";

		final String userInfoBody = "\r\n\r\n" +
				"{" +
				"    \"FaceInfo\": {" +
				"        \"employeeNo\": \""+ id +"\"" +
				"    }" +
				"}";

		final String fotoBody = "\r\n\r\n" + new String(Base64.getDecoder().decode(foto));

		final String body = "\r\n\r\n" +
				boundary +
				"Content-Disposition: form-data; name=\"FaceDataRecord\";" +
				"Content-Type: application/json" +
				"Content-Length: " + userInfoBody.getBytes().length +
				userInfoBody +
				boundary +
				"Content-Disposition: form-data; name=\"FaceImage\";" +
				"Content-Type: image/jpeg" +
				"Content-Length: " + fotoBody.getBytes().length +
				fotoBody +
				boundary + "--";

		RequestBody requestBody = RequestBody.create(body, MediaType.parse("text/html"));
		OkHttpClient client = getOkHttpClient();

		Request request = new Request.Builder()
				.url(url + "/ISAPI/Intelligent/FDLib/FaceDataRecord?format=json&devIndex=" + deviceId)
				.post(requestBody)
				.addHeader("Content-Type", "multipart/form-data; boundary=" + boundary)
				.addHeader("Content-Length", String.valueOf(body.getBytes().length))
				.addHeader("Cache-Control", "no-cache")
				.build();

		try {
			Response response = client.newCall(request).execute();
			return response.isSuccessful();

		} catch (Exception e) {
			e.printStackTrace();
		}

		return false;
	}

	public void apagarUsuario(final String deviceId, final String id) {
		final String body = "{" +
				"\"UserInfoDetail\" : {" +
					"\"mode\": \"byEmployeeNo\"," +
					"\"EmployeeNoList\" : [{" +
					"\"employeeNo\": \""+ id +"\"" +
					"}]" +
				"}" +
			"}";

		RequestBody requestBody = RequestBody.create(body, MediaType.parse("application/json"));
		OkHttpClient client = getOkHttpClient();

		Request request = new Request.Builder()
				.url(url + "/ISAPI/AccessControl/UserInfoDetail/Delete?format=json&devIndex=" + deviceId)
				.put(requestBody)
				.addHeader("Content-Type", "application/json")
				.build();

		try {
			Response response = client.newCall(request).execute();

		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public HikivisionDeviceTO listarDisposivos() {
		final String body = "{"
				+ "    \"SearchDescription\": {"
				+ "        \"position\": 0,"
				+ "        \"maxResult\": 100,"
				+ "        \"Filter\": {"
				+ "            \"protocolType\": ["
				+ "					\"ehomeV5\", "
				+ "					\"ISAPI\""
				+ "            ],"
				+ "            \"devStatus\": ["
				+ "					\"online\","
				+ "					\"offline\","
				+ "					\"sleep\""
				+ "            ]"
				+ "        }"
				+ "    }"
				+ "}";

		RequestBody requestBody = RequestBody.create(body, MediaType.parse("application/json"));
		OkHttpClient client = getOkHttpClient();

		Request request = new Request.Builder()
				.url(url + "/ISAPI/ContentMgmt/DeviceMgmt/deviceList?format=json")
				.post(requestBody)
				.addHeader("Content-Type", "application/json")
				.build();
		
		try {
			Response response = client.newCall(request).execute();
			if(response.isSuccessful()) {
				return gson.fromJson(response.peekBody(2048).string(), HikivisionDeviceTO.class);
			}
			
		} catch (Exception e) {
			e.printStackTrace();
		}

		return null;
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
