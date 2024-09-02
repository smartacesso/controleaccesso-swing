package com.protreino.services.client;

import java.io.BufferedReader;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Objects;

import org.apache.commons.codec.binary.Base64;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;
import com.google.gson.reflect.TypeToken;
import com.protreino.services.main.Main;
import com.protreino.services.to.PedestrianAccessTO;
import com.protreino.services.utils.HttpConnection;
import com.protreino.services.utils.Utils;

public class SmartAcessoFotoServiceClient {

	private static Gson gson = new GsonBuilder().registerTypeAdapter(Date.class, new JsonDeserializer<Date>() {
        public Date deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context) throws JsonParseException {
            try {
                return Utils.convertDataJson(json);
            } catch (Exception e) {
            }

            return null;
        }
    }).create();
	
	public byte[] buscaFotoDoPedestre(final Long idPedestre, final boolean resize, final int imageSize) {
        final String id = idPedestre + ";";

        try {
        	final HttpConnection con= new HttpConnection(Main.urlApplication + "/restful-services/photo/request?ids="
                    + id + "&type=PEDESTRES&resize=" + resize 
                    + "&imageSize=" + imageSize);
            
            final int responseCode = con.getResponseCode();
            
            if (responseCode == 200) {
                BufferedReader bufferedReader = con.getResponseReader();
                Type type = new TypeToken<List<PedestrianAccessTO>>() {
                }.getType();

                List<PedestrianAccessTO> pedestrianTOList = gson.fromJson(bufferedReader, type);
                if (Objects.nonNull(pedestrianTOList) && !pedestrianTOList.isEmpty()) {
                	final PedestrianAccessTO pedestrianAccessTO = pedestrianTOList.get(0);
                	return Base64.decodeBase64(pedestrianAccessTO.getFotoBase64());
                }
            }
            
        } catch (Exception e) {
        	e.printStackTrace();
		}
        
        return null;
	}
	
	public List<String> buscaIdsDePedestreComFotoAlterada(final long lastSync) {
		try {
			final HttpConnection con = new HttpConnection(Main.urlApplication + "/restful-services/photo/query?client="
					+ Main.loggedUser.getIdClient() + "&lastsync=" + lastSync);
			
			final int responseCode = con.getResponseCode();
			
			if (responseCode != 200) {
				return null;
			}
			
			String resposta = con.getResponseReader().readLine();
			if (Utils.isNullOrEmpty(resposta)) {
				return null;
			}
			
			return Arrays.asList(resposta.split(";"));

		} catch (Exception e) {
			e.printStackTrace();
		}
		
		return null;
	}
	
	public List<PedestrianAccessTO> buscaFotoDePedestres(final String idsParameter, final Integer imageSize, final boolean resize, 
			final Integer targetWidth, final Integer targetHeight) {
		try {
			final HttpConnection con = new HttpConnection(Main.urlApplication + "/restful-services/photo/request?ids="
	                + idsParameter 
	                + "&resize=" + resize 
	                + "&imageSize=" + imageSize
	                + "&targetWidth=" + targetWidth
	                + "&targetHeight=" + targetHeight);

			final int responseCode = con.getResponseCode();
	        
	        if (responseCode != 200) {
	            return null;
	        }
	        
	        BufferedReader bufferedReader = con.getResponseReader();
	        Type type = new TypeToken<List<PedestrianAccessTO>>() {
	        }.getType();
	        
	        return gson.fromJson(bufferedReader, type);
	        
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		return null;
	}
}
