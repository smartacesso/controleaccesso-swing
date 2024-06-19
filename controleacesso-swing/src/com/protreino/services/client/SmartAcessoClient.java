package com.protreino.services.client;

import com.protreino.services.main.Main;
import com.protreino.services.utils.HttpConnection;

public class SmartAcessoClient {

	public static void removeTemplatesFromServer(Long idPedestrianAccess) {
		try {
			HttpConnection con = new HttpConnection(
					Main.urlApplication + "/restful-services/access/deleteBiometry?idPedestrian=" + idPedestrianAccess);
			Integer responseCode = con.getResponseCode();
			if (responseCode != 200) {
				throw new Exception(con.getErrorString());
			}

		} catch (Exception e) {
			e.printStackTrace();
			Main.mainScreen.addEvento("Falha ao remover templates do servidor: " + e.getMessage());
		}
	}
	
}
