package com.protreino.services.usecase.controlId;

import java.util.Base64;

import com.protreino.services.usecase.ControlIdDeviceService;
import com.protreino.services.utils.Utils;

public class FacialUseCase {

	ControlIdDeviceService controlIdService = new ControlIdDeviceService();

	public static String ip = Utils.getPreference("controlIdServerURL");

	public static String OCTECT_STREAM = "application/octet-stream";

	protected boolean send(final String session, final String cardNumber, final byte[] foto) {
		try {

			// Codifica a imagem em Base64
			String fotoBase64 = Base64.getEncoder().encodeToString(foto);

			// Envia o Base64 para a câmera
			String url = "http://" + ip + "/user_set_image.fcgi?user_id=" + cardNumber
					+ "&match=1&timestamp=1624997578&session=" + session;
			String response = (String) controlIdService.postMessage(OCTECT_STREAM, url, fotoBase64, null);

			if (response == null || response.trim().equals("{}")) {
				System.out.println("Foto enviada com sucesso");
				return true;
			} else {
				System.err.println("Erro ao enviar foto.");
				return false;
			}

		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
	}

	protected boolean collect(final String session, String ip, String idsFacial) {

		String url = "http://" + ip + "/user_get_image.fcgi?user_id=" + idsFacial + "&get_timestamp=0&session="
				+ session;
		String response = (String) controlIdService.getMessage("application/json", url, null);

		if (response == null || response.trim().equals("{}")) {
			System.out.println("Foto de usuario foi recebidos.");
			return true;
		}
		System.err.println("Erro em receber fotos de usuarios dentro do facial.");
		return false;
	}

}
