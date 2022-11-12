package com.protreino.services.enumeration;

public enum DeviceStatus {
	
	CONNECTED,
	DISCONNECTED;
	
	public String toString(){
		if (this.equals(CONNECTED))
			return "Conectado";
		return "Desconectado";
	}
	
	public static DeviceStatus valueFromImport(String string) {
		if ("Conectado".equals(string))
			return CONNECTED;
		return DISCONNECTED;
	}
}
