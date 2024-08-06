package com.protreino.services.enumeration;

public enum DeviceStatus {
	
	CONNECTED,
	ONLY_ENABLED,
	DISCONNECTED;
	
	public String toString(){
		if (this.equals(CONNECTED)) {
			return "Conectado";
		}
		
		if (this.equals(ONLY_ENABLED)) {
			return "Somente Modo Habilitado";
		}
		
		return "Desconectado";
	}
	
	public static DeviceStatus valueFromImport(String string) {
		if ("Conectado".equals(string)) {
			return CONNECTED;
		}
		
		if("Somente Modo Habilitado".equals(string)) {
			return ONLY_ENABLED;
		}
			
		return DISCONNECTED;
	}
}
