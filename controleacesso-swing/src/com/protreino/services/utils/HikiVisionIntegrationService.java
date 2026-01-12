package com.protreino.services.utils;

import com.burgstaller.okhttp.AuthenticationCacheInterceptor;
import com.burgstaller.okhttp.CachingAuthenticatorDecorator;
import com.burgstaller.okhttp.digest.CachingAuthenticator;
import com.burgstaller.okhttp.digest.Credentials;
import com.burgstaller.okhttp.digest.DigestAuthenticator;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.protreino.services.enumeration.DiaSemana;
import com.protreino.services.enumeration.Finger;
import com.protreino.services.exceptions.InvalidPhotoException;
import com.protreino.services.to.hikivision.*;
import com.protreino.services.to.hikivision.CaptureFingerPrintTO.CaptureFingerPrint;
import com.protreino.services.to.hikivision.UserInfoTO.UserInfoOut;


import okhttp3.*;

import java.io.IOException;
import java.net.SocketException;
import java.nio.charset.StandardCharsets;
import java.text.Format;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
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

			/*
			if (Utils.isNullOrEmpty(url) || Utils.isNullOrEmpty(user)) {
				throw new IllegalArgumentException("Url connection nao pode ser nula");
			}
			*/

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
	
	public void liberaRemotoCamera(final String deviceId) {
	      final String body = "{\n" +
	            "  \"RemoteControlDoor\": {\n" +
	            "    \"cmd\": \"open\"\n" +
	            "  }\n" +
	            "}"; 
				
	      RequestBody requestBody = RequestBody.create(body, MediaType.parse("application/json"));
	      OkHttpClient client = getOkHttpClient();
			Request request = new Request.Builder()
					.url(url + "/ISAPI/AccessControl/RemoteControl/door/65535?format=json&devIndex=" + deviceId).put(requestBody)
					.addHeader("Content-Type", "application/json").build();
			
		
			try (Response response = client.newCall(request).execute()) {
				if (response.isSuccessful()) {
					final HikivisionUserInfoTO responseBody = gson.fromJson(response.body().string(),
							HikivisionUserInfoTO.class);
				
					System.out.println("OK liberado remotamente");
				}

			} catch (Exception e) {
				e.printStackTrace();
			}
			
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

				System.out.println(String.format("Usuario %s ja cadastrado no device %s: %b", idUser, deviceId,
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

				System.out.println(String.format("Foto do usuario %s ja cadastrado no device %s: %b", idUser, deviceId,
						isUsuarioCadastrado));

				return isUsuarioCadastrado;
			}

			return false;

		} catch (Exception e) {
			e.printStackTrace();
		}

		return false;
	}
	
	public Optional<CaptureFingerPrint> capturaDigitalUsuario(final String deviceId, final Finger fingerNo) {
		final String body = "{"
	            + "		\"CaptureFingerPrintCond\": {"
	            + "			\"fingerNo\": " + fingerNo.getPosition() + ""
	            + "		}"
	            + "	}";
		RequestBody requestBody = RequestBody.create(body, MediaType.parse("application/json"));
		OkHttpClient client = getOkHttpClient();

		Request request = new Request.Builder()
				.url(url + "/ISAPI/AccessControl/CaptureFingerPrint?format=json&devIndex=" + deviceId).post(requestBody)
				.addHeader("Content-Type", "application/json").build();
		
		try (Response response = client.newCall(request).execute();) {
			if (response.isSuccessful()) {
				final CaptureFingerPrintTO responseBody = gson.fromJson(response.body().string(), CaptureFingerPrintTO.class);
				System.out.println(responseBody.CaptureFingerPrint.fingerData);
				return Optional.of(responseBody.CaptureFingerPrint);
			}

		} catch (Exception e) {
			e.printStackTrace();
		}
		
		return Optional.empty();
	}
	
	public boolean vinculaDigitalUsuario(final String deviceId, final Long idUser, final Finger fingerNo, final String fingerData) {
		final String body =
			       "{"
			       + "		\"FingerPrintCfg\": {"
			       + "			\"employeeNo\": \"" + String.valueOf(idUser) + "\","
			       + "			\"fingerPrintID\": " + fingerNo.getPosition() + ","
			       + "			\"fingerData\": \"" + fingerData + "\""
			       + "		}"
			       + "	}";
		
				RequestBody requestBody = RequestBody.create(body, MediaType.parse("application/json"));
		OkHttpClient client = getOkHttpClient();

		Request request = new Request.Builder()
				.url(url + "/ISAPI/AccessControl/FingerPrintDownload?format=json&devIndex=" + deviceId).post(requestBody)
				.addHeader("Content-Type", "application/json").build();
		
		try (Response response = client.newCall(request).execute();) {
			if (response.isSuccessful()) {
				final UserInfoOut responseBody = gson.fromJson(response.body().string(), UserInfoOut.class);

				final boolean isCadastradoComSucesso = "OK".equalsIgnoreCase(responseBody.statusString);

				System.out.println(String.format("Usuario %s com a digital vinculada no device %s com sucesso: %b", idUser, deviceId,
						isCadastradoComSucesso));

				return isCadastradoComSucesso;
			}

		} catch (Exception e) {
			e.printStackTrace();
		}

		return false;
	}
	
	public boolean apagarDigitalUsuario(final String deviceId, final Long idUser, final Finger fingerNo) {
		final String body = 
			       "{"
			       + "		\"FingerPrintDelete\": {"
			       + "			\"EmployeeNoDetail\": {"	
			       + "				\"employeeNo\": \"" + idUser + "\","
			       + "				\"fingerPrintID\": [" + fingerNo + "],"
			       + "		   	}"
			       + "		}"
			       + "	}";

		RequestBody requestBody = RequestBody.create(body, MediaType.parse("application/json"));
		OkHttpClient client = getOkHttpClient();

		Request request = new Request.Builder()
				.url(url + "/ISAPI/AccessControl/FingerPrint/Delete?format=json&devIndex=" + deviceId).put(requestBody)
				.addHeader("Content-Type", "application/json").build();
		
		try (Response response = client.newCall(request).execute();) {
			if (response.isSuccessful()) {
				final UserInfoTO responseBody = gson.fromJson(response.body().string(), UserInfoTO.class);

				final boolean isCadastradoComSucesso = responseBody.UserInfoOutList.UserInfoOut.get(0).statusString.equalsIgnoreCase("OK");

				System.out.println(String.format("Usuario %s removido no device %s com sucesso: %b", idUser, deviceId,
						isCadastradoComSucesso));

				return isCadastradoComSucesso;
			}

		} catch (Exception e) {
			e.printStackTrace();
		}

		return false;
	}
	


	public boolean adicionarUsuario(final String deviceId, final String idUser, final String name, String tipoUsuario) {
		Map<String, Object> valid = new LinkedHashMap<>();
		valid.put("enable", true);
		valid.put("beginTime", "2017-08-01T17:30:08");
		valid.put("endTime", "2037-12-31T23:59:59");

		Map<String, Object> userInfo = new LinkedHashMap<>();
		userInfo.put("employeeNo", idUser);
		userInfo.put("name", name);
		userInfo.put("userType", tipoUsuario); // forçado
		userInfo.put("Valid", valid);

		Map<String, Object> bodyMap = new LinkedHashMap<>();
		bodyMap.put("UserInfo", Collections.singletonList(userInfo));

		String body = gson.toJson(bodyMap);

	    MediaType JSON = MediaType.parse("application/json; charset=utf-8");
	    RequestBody requestBody = RequestBody.create(body, JSON);
	    
		OkHttpClient client = getOkHttpClient();

	    Request request = new Request.Builder()
	            .url(url + "/ISAPI/AccessControl/UserInfo/Record?format=json&devIndex=" + deviceId)
	            .addHeader("Content-Type", "application/json; charset=utf-8")
	            .addHeader("Accept", "application/json")
	            .post(requestBody)
	            .build();

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

			if("Invalid Content".equalsIgnoreCase(responseBody.statusString)) {
				System.out.println(String.format("Foto do usuario %s nao foi aceita no device %s", idUser, deviceId));
				throw new InvalidPhotoException(responseBody.statusString);
			}
			
			final boolean isCadastradoComSucesso = responseBody.statusString.equalsIgnoreCase("OK");

			System.out.println(String.format("Foto do usuario %s cadastrada no device %s com sucesso: %b", idUser,
					deviceId, isCadastradoComSucesso));

			return isCadastradoComSucesso;
		} catch (InvalidPhotoException ife) {
			throw ife;

		} catch (Exception e) {
			e.printStackTrace();
		}
		
		return false;
	}

	public boolean adicionarCartaoDePedestre(final String deviceId, final String idUser) {
		String numeroCartao = idUser;
		
		if(Utils.adicionaZeroEsquerdaNoCartaoHikivision()) {		
			while (numeroCartao.length() < 12) {
				numeroCartao = "0" + numeroCartao;
			}
		}
		
		final String body = "{" 
				+ "		\"CardInfo\": {" 
				+ "			\"employeeNo\": \"" + idUser + "\","
				+ "			\"cardNo\": \"" + numeroCartao + "\"" 
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
                System.out.println(String.format("Cartao  %s cadastrado no device %s com sucesso: %b", idUser, deviceId, isCartaoAdicionado));
                
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
			System.out.println(String.format("Cartao do usuario %s ja cadastrado no device %s: %b", idUser, deviceId,
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
		 		+ "						\"eventMode\": \"all\","
		 		+ "						\"minorEvent\": \"1,38,75,153,181\""
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
	
	
	public boolean editarUsuario(final String deviceId, final String idUser, final String name,
			final String tipoUsuario) {

		// "Valid" object
		Map<String, Object> valid = new LinkedHashMap<>();
		valid.put("enable", true);
		valid.put("beginTime", "2017-08-01T17:30:08");
		valid.put("endTime", "2037-12-31T23:59:59");

		// "UserInfo" object
		Map<String, Object> userInfo = new LinkedHashMap<>();
		userInfo.put("employeeNo", idUser);
		userInfo.put("name", name);
		userInfo.put("userType", tipoUsuario);
		userInfo.put("Valid", valid);
		
		// JSON root object
		Map<String, Object> bodyMap = new LinkedHashMap<>();
		bodyMap.put("UserInfo", userInfo); // <-- sem lista aqui

		String body = gson.toJson(bodyMap);

		MediaType JSON = MediaType.parse("application/json; charset=utf-8");
		RequestBody requestBody = RequestBody.create(body, JSON);

		OkHttpClient client = getOkHttpClient();

		Request request = new Request.Builder()
				.url(url + "/ISAPI/AccessControl/UserInfo/Modify?format=json&devIndex=" + deviceId)
				.addHeader("Content-Type", "application/json; charset=utf-8").addHeader("Accept", "application/json")
				.put(requestBody).build();


		try (Response response = client.newCall(request).execute();) {
			if (response.isSuccessful()) {
				final UserEditTo responseBody = gson.fromJson(response.body().string(), UserEditTo.class);

				final boolean isCadastradoComSucesso = responseBody.statusString.equalsIgnoreCase("OK");

				System.out.println(String.format("Usuario %s editado no device %s com sucesso: %b", idUser, deviceId,
						isCadastradoComSucesso));

				return isCadastradoComSucesso;
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
	
	public void criarPlanoDeHorario(final String deviceId, final int weekPlanId, PlanoHorarioHikivision config) {
		System.out.println("Criando intervalos de horarios");
		Gson gson = new Gson();

		String bodyInterno = gson.toJson(config);
		String body = "{\"UserRightWeekPlanCfg\": " + bodyInterno + " }";
		
		
		System.out.println(body);
		
		RequestBody requestBody = RequestBody.create(body, MediaType.get("application/json"));
		OkHttpClient client = getOkHttpClient();

		Request request = new Request.Builder().url(
				url + "/ISAPI/AccessControl/UserRightWeekPlanCfg/" + weekPlanId + "?format=json&devIndex=" + deviceId)
				.put(requestBody).build();

		try (Response response = client.newCall(request).execute()) {
			if (response.isSuccessful()) {
				System.out.println(
						String.format("Configuração do horario de acesso %d atualizada no dispositivo %s com sucesso!",
								weekPlanId, deviceId));
			} else {
				System.err.println(
						"Erro ao atualizar configuração de horario: " + response.code() + " - " + response.message());
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	 
	public void criarTemplateComHorario(final String deviceId, final int planTemplateId, final int weekPlanId, final String nome) {
		System.out.println("Criando templates");
		
		final String body = "{" 
								+ "\"UserRightPlanTemplate\": {" 
									+ "\"enable\": " + true + "," 
									+ "\"templateName\": \"" + nome + "\"," + 
									"\"weekPlanNo\": " + weekPlanId + "," 
									+ "\"holidayGroupNo\": \"\"" 
								+ "}" 
							+ "}";
		
		RequestBody requestBody = RequestBody.create(body, MediaType.get("application/json"));
		OkHttpClient client = getOkHttpClient();

		Request request = new Request.Builder().url(url + "/ISAPI/AccessControl/UserRightPlanTemplate/" + planTemplateId
				+ "?format=json&devIndex=" + deviceId).put(requestBody).build();

		try (Response response = client.newCall(request).execute()) {
			if (response.isSuccessful()) {
				System.out.println(
						String.format("Configuração de acesso do plano %d atualizada no dispositivo %s com sucesso!",
								planTemplateId, deviceId));
			} else {
				System.err.println(
						"Erro ao atualizar configuração de template: " + response.code() + " - " + response.message());
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	 
	public void vincularTemplateNoUsuario(final String deviceId, final String idUser, final int planTemplateId, String userType) {
		boolean validEnable = true;
		String beginTime = "2010-12-01T17:30:08";
		String endTime = "2032-08-01T17:30:08";
		String doorRight = "1";
		int doorNo = 1;

		String body = "{\n" +
		         "    \"UserInfo\": {\n" +
		         "        \"employeeNo\": \"" + idUser + "\",\n" +
		         "        \"userType\": \"" + userType + "\",\n" +
		         "        \"Valid\": {\n" +
		         "            \"enable\": " + validEnable + ",\n" +
		         "            \"beginTime\": \"" + beginTime + "\",\n" +
		         "            \"endTime\": \"" + endTime + "\"\n" +
		         "        },\n" +
		         "        \"doorRight\": \"" + doorRight + "\",\n" +
		         "        \"RightPlan\": [\n" +
		         "            {\n" +
		         "                \"doorNo\": " + doorNo + ",\n" +
		         "                \"planTemplateNo\": \"" + planTemplateId + "\"\n" +
		         "            }\n" +
		         "        ]\n" +
		         "    }\n" +
		         "}";
		
        RequestBody requestBody = RequestBody.create(body, MediaType.get("application/json"));
        OkHttpClient client = getOkHttpClient();
        
        Request request = new Request.Builder()
                .url(url + "/ISAPI/AccessControl/UserInfo/SetUp?format=json&devIndex=" + deviceId)
                .put(requestBody)
                .build();

        try (Response response = client.newCall(request).execute()) {
            if (response.isSuccessful()) {
                System.out.println(String.format("Configuração de horario do usuario atualizada no dispositivo com sucesso! no template %d", planTemplateId));
            } else {
                System.err.println("Erro ao atualizar configuração de vinculo de usuario: " + response.code() + " - " + response.message());
            }
        } catch (Exception e) {
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
