package com.protreino.services.utils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonSyntaxException;

public class HttpConnection {
	
	private URL url;
	private HttpURLConnection con;
	
	public HttpConnection(){
	}
	
	
	public HttpConnection(String stringURL) throws IOException{
		makeConnection(stringURL);
	}
	
	
	public void makeConnection(String stringURL) throws IOException{
		url = new URL(stringURL);
		con = (HttpURLConnection) url.openConnection();
		con.setConnectTimeout(120000);
		con.setReadTimeout(120000);
	}
	
	
	public int getResponseCode() throws IOException{
		if (con != null){
			return con.getResponseCode();
		}
		else
			return 0;
	}
	
	
	public String getResponseString() {
		try {
			if (con != null){
				BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(con.getInputStream(), StandardCharsets.UTF_8));
				StringBuilder stringBuilder = new StringBuilder();
				String bufferedString = null;
				while (null != (bufferedString = bufferedReader.readLine()))
					stringBuilder.append(bufferedString);
				return stringBuilder.toString();
			}
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		return "";
	}
	
	
	public BufferedReader getResponseReader() throws IOException {
		if (con != null){
			BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(con.getInputStream(), StandardCharsets.UTF_8));
			return bufferedReader;
		}
		else
			return null;
	}
	
	
	public JsonObject getResponseJsonObject() throws JsonSyntaxException, IOException{
		if (con != null){
			JsonParser parser = new JsonParser();
			return parser.parse(getResponseReader()).getAsJsonObject();
		}
		else
			return null;
	}
	
	
	public String getErrorString() {
		String retorno = "";
		try {
			retorno = getResponseString();
			if ("".equals(retorno)) {
				if (con != null){
					BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(con.getErrorStream(), StandardCharsets.UTF_8));
					StringBuilder stringBuilder = new StringBuilder();
					String bufferedString = null;
					while (null != (bufferedString = bufferedReader.readLine()))
						stringBuilder.append(bufferedString);
					return stringBuilder.toString();
				}
			}
		}
		catch (Exception e){
			e.printStackTrace();
		}
		return retorno;	
	}
	
	
	public int sendResponse(String responseString) throws IOException{
		if (con != null){
			con.setDoOutput(true);
			con.setRequestMethod("POST");
			con.setRequestProperty( "Content-Type", "application/json; charset=UTF-8");
			con.setRequestProperty( "Content-Length", Integer.toString(responseString.length()));
			OutputStream os = con.getOutputStream();
	        os.write(responseString.getBytes("UTF-8"));
	        os.close();
	        return con.getResponseCode();
		}
		else
			return 0;
	}

	public HttpURLConnection getHttpURLConnection() {
		return con;
	}

	public void setRequestMethod(String method) throws IOException {
		if (con != null)
			con.setRequestMethod(method);
	}
}
