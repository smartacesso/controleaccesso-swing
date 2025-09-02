package com.protreino.services.to.hikivision;

public class HikivisionDeviceSimplificadoTO {
	
	public HikivisionDeviceSimplificadoTO(String nome, String id) {
		this.devName = nome;
		this.devIndex = id;
	}
	
    private String devName;
    private String devIndex;
    
    
	public String getDevName() {
		return devName;
	}
	public void setDevName(String devName) {
		this.devName = devName;
	}
	public String getDevIndex() {
		return devIndex;
	}
	public void setDevIndex(String devIndex) {
		this.devIndex = devIndex;
	}
    
}
