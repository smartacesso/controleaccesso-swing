package com.protreino.services.client;

import java.io.BufferedReader;
import java.lang.reflect.Type;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;
import com.google.gson.reflect.TypeToken;
import com.protreino.services.constants.Configurations;
import com.protreino.services.entity.ParametroEntity;
import com.protreino.services.entity.PlanoEntity;
import com.protreino.services.entity.UserEntity;
import com.protreino.services.main.Main;
import com.protreino.services.to.EmpresaTO;
import com.protreino.services.to.RegraTO;
import com.protreino.services.utils.HttpConnection;
import com.protreino.services.utils.Utils;

public class SmartAcessoClient {

	private static SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss:sss");
	
	private static Gson gson = new GsonBuilder().registerTypeAdapter(Date.class, new JsonDeserializer<Date>() {
        public Date deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context) throws JsonParseException {
            try {
                return Utils.convertDataJson(json);
            } catch (Exception e) {
            }

            return null;
        }
    }).create();
	
	public List<UserEntity> requestAllUsers(final Long lastSyncGetUsers) {
		try {
			HttpConnection con = new HttpConnection(Main.urlApplication + "/restful-services/access/requestAllUsers"
	                + "?client=" + Main.loggedUser.getIdClient()
	                + "&lastsync=" + lastSyncGetUsers
	                + "&version=" + Configurations.VERSION);

	        Integer responseCode = con.getResponseCode();

	        if (responseCode == 404) {
	            System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: sem registros de usuarios para receber");
	            return null;
	        }

	        if (responseCode != 200) {
	            System.out.println(sdf.format(new Date()) + "  ERRO NA SINCRONIZACAO: Error String: "
	                    + con.getErrorString());
	            return null;
	        }

	        BufferedReader bufferedReader = con.getResponseReader();
	        Type type = new TypeToken<List<UserEntity>>() {
	        }.getType();
	        
	        return gson.fromJson(bufferedReader, type);

		} catch (Exception e) {
			e.printStackTrace();
		}
		
		return null;
	}
	
	public List<EmpresaTO> requestAllEmpresas(final Long lastSyncGetEmpresas) {
		try {
			HttpConnection con = new HttpConnection(Main.urlApplication + "/restful-services/access/requestAllEmpresas"
                    + "?client=" + Main.loggedUser.getIdClient()
                    + "&lastsync=" + lastSyncGetEmpresas
                    + "&version=" + Configurations.VERSION);

            Integer responseCode = con.getResponseCode();

            if (responseCode == 404) {
                System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: sem registros de empresas para receber");
                return null;
            }

            if (responseCode != 200) {
                System.out.println(sdf.format(new Date())
                        + "  ERRO NA SINCRONIZACAO DE EMPRESAS: Error String: " + con.getErrorString());
                return null;
            }

            BufferedReader bufferedReader = con.getResponseReader();
            Type type = new TypeToken<List<EmpresaTO>>() {
            }.getType();
            
            return gson.fromJson(bufferedReader, type);
            
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		return null;
	}
	
	public List<RegraTO> requestAllRegras(final Long lastSyncGetRegras) {
		try {
			HttpConnection con = new HttpConnection(Main.urlApplication + "/restful-services/access/requestAllRegras"
	                + "?client=" + Main.loggedUser.getIdClient()
	                + "&lastsync=" + lastSyncGetRegras);

	        Integer responseCode = con.getResponseCode();

	        if (responseCode == 404) {
	            System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: sem registros de regras para receber");
	            return null;
	        }

	        if (responseCode != 200) {
	            System.out.println(sdf.format(new Date())
	                    + "  ERRO NA SINCRONIZACAO DE REGRAS: Error String: " + con.getErrorString());
	            return null;
	        }

	        BufferedReader bufferedReader = con.getResponseReader();
	        Type type = new TypeToken<List<RegraTO>>() {
	        }.getType();
	        
	        return gson.fromJson(bufferedReader, type);
	        
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		return null;
	}
	
	public List<ParametroEntity> requestAllParametros(final Long lastSyncGetParametros) {
		try {
			HttpConnection con = new HttpConnection(Main.urlApplication + "/restful-services/access/requestAllParametros"
	                + "?client=" + Main.loggedUser.getIdClient()
	                + "&lastsync=" + lastSyncGetParametros);

	        Integer responseCode = con.getResponseCode();

	        if (responseCode == 404) {
	            System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: sem registros de parametros para receber");
	            return null;
	        }

	        if (responseCode != 200) {
	            System.out.println(sdf.format(new Date())
	                    + "  ERRO NA SINCRONIZACAO DE PARAMETROS: Error String: " + con.getErrorString());
	            return null;
	        }

	        BufferedReader bufferedReader = con.getResponseReader();
	        Type type = new TypeToken<List<ParametroEntity>>() {
	        }.getType();
	        
	        return gson.fromJson(bufferedReader, type);
	        
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		return null;
	}
	
	public List<PlanoEntity> requestAllPlanos(final Long lastSyncGetPlanos) {
		try {
			HttpConnection con = new HttpConnection(Main.urlApplication + "/restful-services/access/requestAllPlanos"
                    + "?client=" + Main.loggedUser.getIdClient()
                    + "&lastsync=" + lastSyncGetPlanos);

            Integer responseCode = con.getResponseCode();

            if (responseCode == 404) {
                System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: sem registros de planos para receber");
                return null;
            }

            if (responseCode != 200) {
                System.out.println(sdf.format(new Date())
                        + "  ERRO NA SINCRONIZACAO DE PLANOS: Error String: " + con.getErrorString());
                return null;
            }

            BufferedReader bufferedReader = con.getResponseReader();
            Type type = new TypeToken<List<PlanoEntity>>() {
            }.getType();
            
            return gson.fromJson(bufferedReader, type);
			
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		return null;
	}
	
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
