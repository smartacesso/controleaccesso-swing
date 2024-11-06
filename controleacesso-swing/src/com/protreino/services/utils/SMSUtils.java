package com.protreino.services.utils;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URLEncoder;
import java.text.SimpleDateFormat;
import java.util.Date;

public class SMSUtils {
	
	private static final String URL_COMTELE 	  = "https://sms.comtele.com.br/api/v2/send";
	private static final String URL_COMTELE_SALDO = "https://sms.comtele.com.br/api/v2/credits/Username";
	private String key = "8cbd2b5b-358e-4de8-8a7c-a881bc023251"; //chave padrao do Pro-Trieno
	
	public SMSUtils() {
		
	}
	
	public SMSUtils(String key) {
		this.key = key;
	}
	
	public void enviaSMS(String telefone, String texto) throws Exception {
		doGet(URL_COMTELE
				+ "?Sender="    + "ControleAcesso"
				+ "&Receivers=" + telefone
				+ "&Content="   + URLEncoder.encode(texto, "UTF-8"));
	}
	
	public void agendaSMS(String telefone, String texto, Date dataAgendamento) throws Exception {
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy-mm-dd hh:mm:ss");
		
		doGet(URL_COMTELE
				+ "?Sender="       + "Controle de acesso"
				+ "&Receivers="    + telefone
				+ "&Content="      + URLEncoder.encode(texto, "UTF-8")
				+ "&ScheduleDate=" + sdf.format(dataAgendamento));
	}
	
	private String doGet(String url) {
		StringBuilder retorno = new StringBuilder();
		String s;
        Process p;
        try {
        	p = Runtime.getRuntime().exec("curl --request GET --url "+url+" \\"
        								+ "		--header auth-key:"+key+" \\\n"
        								+ "		--header content-type:application/json ");
            
        	BufferedReader br = new BufferedReader(new InputStreamReader(p.getInputStream()));

            while ((s = br.readLine()) != null) {
                System.out.println("line: " + s);
                retorno.append(s+"\n");
            }
            
            p.waitFor();
            System.out.println ("exit: " + p.exitValue());
            p.destroy();
        } catch (Exception e) {}
        
		return retorno.toString();
	}
	
	public class ComTeleSaldo {
		public boolean Sucess;
		public Integer Object;
		public String Message;
		
	}
}
