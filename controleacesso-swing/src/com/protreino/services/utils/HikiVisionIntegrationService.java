package com.protreino.services.utils;

import com.burgstaller.okhttp.AuthenticationCacheInterceptor;
import com.burgstaller.okhttp.CachingAuthenticatorDecorator;
import com.burgstaller.okhttp.digest.CachingAuthenticator;
import com.burgstaller.okhttp.digest.Credentials;
import com.burgstaller.okhttp.digest.DigestAuthenticator;
import com.google.gson.Gson;
import com.protreino.services.to.hikivision.*;

import okhttp3.*;

import java.io.IOException;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

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

		Request request = new Request.Builder().url(url + "/ISAPI/System/deviceInfo?format=json").get()
				.addHeader("Content-Type", "application/json").build();

		try (Response response = client.newCall(request).execute();) {
			final boolean isSuccessFul = response.isSuccessful();

			return isSuccessFul;

		} catch (Exception e) {
			System.out.println(e.getMessage());
		}

		return false;
	}

	public boolean isUsuarioJaCadastrado(final String deviceId, final String idUser) {
		final String uuid = UUID.randomUUID().toString();
		final String body = "{" 
				+ "		\"UserInfoSearchCond\": {" 
				+ "			\"searchID\": \"" + uuid + "\","
				+ "			\"searchResultPosition\": 0," 
				+ "			\"maxResults\": 1,"
				+ "			\"EmployeeNoList\" : [{" 
				+ "				\"employeeNo\": \"" + idUser + "\""
				+ "			}]" 
				+ "		}" 
				+ "	}";

		RequestBody requestBody = RequestBody.create(body, MediaType.parse("application/json"));
		OkHttpClient client = getOkHttpClient();

		Request request = new Request.Builder()
				.url(url + "/ISAPI/AccessControl/UserInfo/Search?format=json&devIndex=" + deviceId).post(requestBody)
				.addHeader("Content-Type", "application/json").build();

		try (Response response = client.newCall(request).execute()) {
			if (response.isSuccessful()) {
				final HikivisionUserInfoTO responseBody = gson.fromJson(response.body().string(),
						HikivisionUserInfoTO.class);
				final boolean isUsuarioCadastrado = responseBody.UserInfoSearch.responseStatusStrg.equals("OK");

				System.out.println(String.format("Usuario %s já cadastrado no device %s: %b", idUser, deviceId,
						isUsuarioCadastrado));

				return isUsuarioCadastrado;
			}

			return false;

		} catch (Exception e) {
			e.printStackTrace();
		}

		return false;
	}

	public boolean isFotoUsuarioJaCadastrada(final String deviceId, final String idUser) {
		final String uuid = UUID.randomUUID().toString().substring(0, 6);
		final String body = "{" 
				+ "		\"FaceInfoSearchCond\": {" 
				+ "			\"searchID\": \"" + uuid + "\","
				+ "			\"searchResultPosition\": 0," 
				+ "			\"maxResults\": 1,"
				+ "			\"employeeNo\": \"" + idUser + "\"" 
				+ "		}" 
				+ "}";

		RequestBody requestBody = RequestBody.create(body, MediaType.parse("application/json"));
		OkHttpClient client = getOkHttpClient();

		Request request = new Request.Builder()
				.url(url + "/ISAPI/Intelligent/FDLib/FDSearch?format=json&devIndex=" + deviceId).post(requestBody)
				.addHeader("Content-Type", "application/json").build();

		try (Response response = client.newCall(request).execute();) {
			if (response.isSuccessful()) {
				final FaceInfoSearchTO responseBody = gson.fromJson(response.body().string(),
						FaceInfoSearchTO.class);
				final boolean isUsuarioCadastrado = responseBody.FaceInfoSearch.responseStatusStrg.equals("OK");

				System.out.println(String.format("Foto do wsuario %s já cadastrado no device %s: %b", idUser, deviceId,
						isUsuarioCadastrado));

				return isUsuarioCadastrado;
			}

			return false;

		} catch (Exception e) {
			e.printStackTrace();
		}

		return false;
	}

	public boolean adicionarUsuario(final String deviceId, final String idUser, final String name) {
		final String body = "{" 
				+ "\"UserInfo\" : [{" 
					+ "\"employeeNo\": \"" + idUser + "\"," 
					+ "\"name\": \"" + name+ "\"," 
					+ "\"Valid\" : {"
						+ "\"beginTime\": \"2017-08-01T17:30:08\","
						+ "\"endTime\": \"2037-12-31T23:59:59\""
						+ "}" 
					+ "}]" 
				+ "}";

		RequestBody requestBody = RequestBody.create(body, MediaType.parse("application/json"));
		OkHttpClient client = getOkHttpClient();

		Request request = new Request.Builder()
				.url(url + "/ISAPI/AccessControl/UserInfo/Record?format=json&devIndex=" + deviceId).post(requestBody)
				.addHeader("Content-Type", "application/json").build();

		try (Response response = client.newCall(request).execute();) {
			if (response.isSuccessful()) {
				final UserInfoTO responseBody = gson.fromJson(response.body().string(), UserInfoTO.class);

				final boolean isCadastradoComSucesso = responseBody.UserInfoOutList.UserInfoOut.get(0).statusString.equalsIgnoreCase("OK");

				System.out.println(String.format("Usuario %s cadastrado no device %s com sucesso: %b", idUser, deviceId,
						isCadastradoComSucesso));

				return isCadastradoComSucesso;
			}

		} catch (Exception e) {
			e.printStackTrace();
		}

		return false;
	}

	@SuppressWarnings("deprecation")
	public boolean adicionarFotoUsuario(final String deviceId, final String idUser, final byte[] foto) {
		OkHttpClient client = getOkHttpClient();

		final String jsonRequestBody = "{\"FaceInfo\": {\"employeeNo\": \"" + idUser + "\" } }";

		RequestBody body = new MultipartBody.Builder().setType(MultipartBody.FORM)
				.addFormDataPart("FaceDataRecord", null,
						RequestBody.create(MediaType.parse("application/json"), jsonRequestBody.getBytes()))
				.addFormDataPart("FaceImage", "/C:/Users/User/Pictures/b036985f4cff537ad8c8dd370a31a283.jpg",
						RequestBody.create(MediaType.parse("application/octet-stream"), foto))
				.build();

		Request request = new Request.Builder()
				.url(url + "/ISAPI/Intelligent/FDLib/FaceDataRecord?format=json&devIndex=" + deviceId)
				.method("POST", body).build();

		try (Response response = client.newCall(request).execute();) {
			final FaceDataRecordResponseTO responseBody = gson.fromJson(response.body().string(),
					FaceDataRecordResponseTO.class);
			final boolean isCadastradoComSucesso = responseBody.statusString.equalsIgnoreCase("OK");

			System.out.println(String.format("Foto do usuario %s cadastrada no device %s com sucesso: %b", idUser,
					deviceId, isCadastradoComSucesso));

			return isCadastradoComSucesso;
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		return false;

	}

	public boolean adicionarCartaoDePedestre(final String deviceId, final String idUser) {
		final String body = "{" 
				+ "		\"CardInfo\": {" 
				+ "			\"employeeNo\": \"" + idUser + "\","
				+ "			\"cardNo\": \"" + idUser + "\"" 
				+ "		}" 
				+ "}";

		RequestBody requestBody = RequestBody.create(body, MediaType.parse("application/json"));
		OkHttpClient client = getOkHttpClient();

		Request request = new Request.Builder()
				.url(url + "/ISAPI/AccessControl/CardInfo/Record?format=json&devIndex=" + deviceId).post(requestBody)
				.addHeader("Content-Type", "application/json").build();

		try (Response response = client.newCall(request).execute();) {
			if (response.isSuccessful()) {
                final ResponseStatusTO responseBody = gson.fromJson(response.body().string(), ResponseStatusTO.class);
                final boolean isCartaoAdicionado = responseBody.statusString.equalsIgnoreCase("OK");
                System.out.println(String.format("Foto do usuario %s apagada no device %s com sucesso: %b", idUser, deviceId, isCartaoAdicionado));
                
                return isCartaoAdicionado;
            }

		} catch (Exception e) {
			e.printStackTrace();
		}

		return false;
	}

	public boolean isCartaoJaCadastrado(final String deviceId, final String idUser) {
		final String uuid = UUID.randomUUID().toString().substring(0, 6);
		final String body = "{" + "		\"CardInfoSearchCond\": {" 
				+ "			\"searchID\": \"" + uuid + "\","
				+ "			\"searchResultPosition\": 0," 
				+ "			\"maxResults\": 1,"
				+ "			\"CardNoList\": [" 
				+ "				{" 
				+ "					\"cardNo\": \"" + idUser + "\""
				+ "				}" 
				+ "			]" 
				+ "		}" 
				+ "}";

		RequestBody requestBody = RequestBody.create(body, MediaType.parse("application/json"));
		OkHttpClient client = getOkHttpClient();

		Request request = new Request.Builder()
				.url(url + "/ISAPI/AccessControl/CardInfo/Search?format=json&devIndex=" + deviceId).post(requestBody)
				.addHeader("Content-Type", "application/json").build();

		try (Response response = client.newCall(request).execute();) {
			final CardInfoSearchTO responseBody = gson.fromJson(response.body().string(), CardInfoSearchTO.class);

			if (response.code() != 200) {
				return false;
			}

			final boolean isCartaoJaCadastrado = "OK".equalsIgnoreCase(responseBody.CardInfoSearch.responseStatusStrg);
			System.out.println(String.format("Cartão do usuario %s já cadastrado no device %s: %b", idUser, deviceId,
					isCartaoJaCadastrado));

			return isCartaoJaCadastrado;

		} catch (Exception e) {
			e.printStackTrace();
		}

		return false;
	}

	@SuppressWarnings("deprecation")
	public void atualizarFotoUsuario(final String deviceId, final String idUser, final byte[] foto) {
		OkHttpClient client = getOkHttpClient();

		final String jsonRequestBody = "{\"FPID\": \"" + idUser + "\" }";
		RequestBody body = new MultipartBody.Builder().setType(MultipartBody.FORM)
				.addFormDataPart("FaceDataRecord", null,
						RequestBody.create(MediaType.parse("application/json"), jsonRequestBody.getBytes()))
				.addFormDataPart("FaceImage", "/C:/Users/User/Pictures/b036985f4cff537ad8c8dd370a31a283.jpg",
						RequestBody.create(MediaType.parse("application/octet-stream"), foto))
				.build();

		Request request = new Request.Builder()
				.url(url + "/ISAPI/Intelligent/FDLib/FDSetUp?format=json&devIndex=" + deviceId).method("PUT", body)
				.build();

		try (Response response = client.newCall(request).execute();) {
			final FaceDataRecordResponseTO responseBody = gson.fromJson(response.body().string(),
					FaceDataRecordResponseTO.class);
			final boolean isCadastradoComSucesso = responseBody.statusString.equalsIgnoreCase("OK");

			System.out.println(String.format("Foto do usuario %s atualizada no device %s com sucesso: %b", idUser,
					deviceId, isCadastradoComSucesso));

		} catch (Exception e) {
			e.printStackTrace();
		}

	}

	public void adicionarListenerParaEventosCamera(final String deviceId, final String applicationIp, final Integer portNumber) {
		 final String body = "{"
		 		+ "    \"HttpHostNotificationList\": ["
		 		+ "        {"
		 		+ "            \"HttpHostNotification\": {"
		 		+ "                \"id\": \"1\","
		 		+ "                \"url\": \"/"+ deviceId +"\","
		 		+ "                \"protocolType\": \"HTTP\","
		 		+ "                \"addressingFormatType\": \"ipaddress\","
		 		+ "                \"ipAddress\": \""+ applicationIp +"\","
		 		+ "                \"portNo\": " + portNumber + ","
		 		+ "					\"SubscribeEvent\": {"
		 		+ "						\"eventMode\": \"list\","
		 		+ "						\"minorEvent\": \"75\""
		 		+ "					}"
		 		+ "            }"
		 		+ "        }"
		 		+ "    ]"
		 		+ "}";

	        RequestBody requestBody = RequestBody.create(body, MediaType.parse("application/json"));
	        OkHttpClient client = getOkHttpClient();

	        Request request = new Request.Builder()
	                .url(url + "/ISAPI/Event/notification/httpHosts?format=json&devIndex=" + deviceId)
	                .post(requestBody)
	                .addHeader("Content-Type", "application/json")
	                .build();

	        try(Response response = client.newCall(request).execute();) {
	            if (response.isSuccessful()) {
	                final ResponseStatusTO responseBody = gson.fromJson(response.body().string(), ResponseStatusTO.class);
	                final boolean isCadastradoComSucesso = responseBody.statusString.equalsIgnoreCase("OK");
	                System.out.println(String.format("Listerner adicionado para o device %s com sucesso: %b", deviceId, isCadastradoComSucesso));
	            } else {
	            	System.out.println("Erro ao enviar listerner: " + response.body().string());
	            }

	        } catch (Exception e) {
	            e.printStackTrace();
	        }

	}

	public boolean apagarUsuario(final String deviceId, final String idUser) {
		final String body = "{" 
				+ "\"UserInfoDetail\" : {" 
					+ "\"mode\": \"byEmployeeNo\"," 
					+ "\"EmployeeNoList\" : [{"
						+ "\"employeeNo\": \"" + idUser + "\"" 
					+ "}]" 
				+ "}" 
			+ "}";

		RequestBody requestBody = RequestBody.create(body, MediaType.parse("application/json"));
		OkHttpClient client = getOkHttpClient();

		Request request = new Request.Builder()
				.url(url + "/ISAPI/AccessControl/UserInfoDetail/Delete?format=json&devIndex=" + deviceId)
				.put(requestBody).addHeader("Content-Type", "application/json").build();

		try (Response response = client.newCall(request).execute();) {
			final ResponseStatusTO responseBody = gson.fromJson(response.body().string(), ResponseStatusTO.class);
            final boolean isApagadoComSucesso = responseBody.statusString.equalsIgnoreCase("OK");
            System.out.println(String.format("Usuario %s apagado no device %s com sucesso: %b", idUser, deviceId, isApagadoComSucesso));
            
            return isApagadoComSucesso;

		} catch (Exception e) {
			e.printStackTrace();
		}
		
		return false;
	}

	public boolean apagarFotoUsuario(String deviceId, String idUser) {
		final String body = "{" 
				+ "\"FaceInfoDelCond\" : {" 
					+ "\"EmployeeNoList\" : [{" 
						+ "\"employeeNo\": \"" + idUser+ "\"" 
					+ "}]" 
				+ "}" 
			+ "}";

		RequestBody requestBody = RequestBody.create(body, MediaType.parse("application/json"));
		OkHttpClient client = getOkHttpClient();

		Request request = new Request.Builder()
				.url(url + "/ISAPI/Intelligent/FDLib/FDSearch/Delete?format=json&devIndex=" + deviceId).put(requestBody)
				.addHeader("Content-Type", "application/json").build();

		try (Response response = client.newCall(request).execute();) {
			if (response.isSuccessful()) {
                final ResponseStatusTO responseBody = gson.fromJson(response.body().string(), ResponseStatusTO.class);
                final boolean isApagadoComSucesso = responseBody.statusString.equalsIgnoreCase("OK");
                System.out.println(String.format("Foto do usuario %s apagada no device %s com sucesso: %b", idUser, deviceId, isApagadoComSucesso));
                
                return isApagadoComSucesso;
            }
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		return false;
	}

	public HikivisionDeviceTO listarDispositivos() {
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

		Request request = new Request.Builder().url(url + "/ISAPI/ContentMgmt/DeviceMgmt/deviceList?format=json")
				.post(requestBody).addHeader("Content-Type", "application/json").build();

		try (Response response = client.newCall(request).execute();) {
			if (response.isSuccessful()) {
				final HikivisionDeviceTO responseBody = gson.fromJson(response.body().string(), HikivisionDeviceTO.class);

				return responseBody;
			}

		} catch (Exception e) {
			e.printStackTrace();
		}

		return null;
	}
	
	public void adicionarDispositivo(final String address, final Integer port, final String user, final String password,
			final String deviceName) {
		final String body = "{" 
						+ "\"DeviceInList\": [{" 
							+ "\"Device\": {" 
								+ "\"protocolType\": \"ISAPI\","
								+ "\"EhomeParams\": {" 
								+ "\"EhomeID\": \"111\"," 
								+ "\"EhomeKey\": \"\"" 
							+ "}," 
						+ "\"ISAPIParams\": {"
							+ "\"addressingFormatType\": \"IPV4Address\"," 
							+ "\"address\": \"" + address + "\"," 
							+ "\"portNo\": "+ port + "," 
							+ "\"userName\": \"" + user + "\"," 
							+ "\"password\": \"" + password + "\"" 
						+ "},"
						+ "\"devName\": \"" + deviceName + "\"," 
						+ "\"devType\": \"AccessControl\"" 
					+ "}" 
				+ "}" 
			+ "]" 
		+ "}";

		RequestBody requestBody = RequestBody.create(body, MediaType.parse("application/json"));
		OkHttpClient client = getOkHttpClient();

		Request request = new Request.Builder().url(url + "/ISAPI/ContentMgmt/DeviceMgmt/addDevice?format=json")
				.post(requestBody).addHeader("Content-Type", "application/json").build();

		try (Response response = client.newCall(request).execute();) {

		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	public void captureFaceLocally() {
		OkHttpClient client = getOkHttpClient();
		
		final String body =  "<CaptureFaceDataCond version=\"2.0\" xmlns=\"http://www.isapi.org/ver20/XMLSchema\">"
				+ "<captureInfrared>false</captureInfrared><dataType>binary</dataType></CaptureFaceDataCond>";
			
		
		RequestBody requestBody = RequestBody.create(body, MediaType.parse("application/xml"));

		Request request = new Request.Builder().url(url + "/ISAPI/AccessControl/CaptureFaceData").post(requestBody)
				.addHeader("Content-Type", "application/json").build();

		try {
			Response response = client.newCall(request).execute();
			System.out.println(response.body());
			System.out.println("rsdada");
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	private OkHttpClient getOkHttpClient() {
		final DigestAuthenticator authenticator = new DigestAuthenticator(new Credentials(user, password));
		final Map<String, CachingAuthenticator> authCache = new ConcurrentHashMap<>();

		return new OkHttpClient.Builder().authenticator(new CachingAuthenticatorDecorator(authenticator, authCache))
				.addInterceptor(new AuthenticationCacheInterceptor(authCache)).readTimeout(10000, TimeUnit.MILLISECONDS)
				.writeTimeout(10000, TimeUnit.MILLISECONDS).build();
	}

}
