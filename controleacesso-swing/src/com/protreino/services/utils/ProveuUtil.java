package com.protreino.services.utils;

import com.protreino.services.enumeration.NotificationType;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;

public class ProveuUtil {
	
	public IProveuTupaJna interfaceJna;
	
	public IProveuTupaJna getInterfaceJna(){
		if (interfaceJna != null)
			return interfaceJna;
		try {
			if (Platform.isWindows()){
				if ("32".equals(Utils.getJvmArchitecture())){
					interfaceJna =  (IProveuTupaJna) Native.loadLibrary(Utils.getInstallationPath() 
							+ "lib/IProveuTupa_32", IProveuTupaJna.class);
					interfaceJna.setup(Utils.getInstallationPath());
					interfaceJna.inicializaComunicador(Utils.getAppDataFolder() + "/logs/");
				}
				else
					Utils.createNotification("Catraca Proveu Tupá requer JVM 32 bits", NotificationType.BAD);
			}
			else 
				Utils.createNotification("Catraca Proveu Tupá requer sistema Windows", NotificationType.BAD);
		}
		catch(Exception e){
			e.printStackTrace();
		}
		return interfaceJna;
	}
	
	public interface IProveuTupaJna extends Library {
		void setup(String installationPath);
		int inicializaComunicador(String logPath);
		String localizarEquipamentos();
		int conectar(String ip, String porta, String senha); // retorna index do equipamento na lista
		int getStatus(); // 0=conectado 1=desconectado
		int desconectar(); // retorna index. Index = -1 desconectou
		String receberSolicitacoes();
		void responderSolicitacao(Boolean permitido, String mensagem, Boolean liberadoPeloSistema);
    }
}
