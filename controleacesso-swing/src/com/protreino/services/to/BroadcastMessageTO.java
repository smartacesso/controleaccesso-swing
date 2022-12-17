package com.protreino.services.to;

import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.entity.TemplateEntity;
import com.protreino.services.enumeration.BroadcastMessageType;

public class BroadcastMessageTO {
	
	private BroadcastMessageType type;
	private LogPedestrianAccessEntity logAccess;
	private TemplateEntity template;
	private Long idPedestrianAccess;
	private String deviceIdentifier;
	private String ipDestination;
	
	public BroadcastMessageTO(){
	}
	
	public BroadcastMessageTO(BroadcastMessageType type) {
		this.type = type;
	}
	
	public BroadcastMessageTO(BroadcastMessageType type, LogPedestrianAccessEntity logAccess) {
		this.type = type;
		this.logAccess = logAccess;
	}
	
	public BroadcastMessageTO(BroadcastMessageType type, Long idPedestrianAccess) {
		this.type = type;
		this.idPedestrianAccess = idPedestrianAccess;
	}
	
	public BroadcastMessageTO(BroadcastMessageType type, TemplateEntity template) {
		this.type = type;
		this.template = template;
	}
	
	public BroadcastMessageType getType() {
		return type;
	}

	public void setType(BroadcastMessageType type) {
		this.type = type;
	}

	public LogPedestrianAccessEntity getLogAccess() {
		return logAccess;
	}

	public void setLogAccess(LogPedestrianAccessEntity logAccess) {
		this.logAccess = logAccess;
	}

	public String getDeviceIdentifier() {
		return deviceIdentifier;
	}

	public void setDeviceIdentifier(String deviceIdentifier) {
		this.deviceIdentifier = deviceIdentifier;
	}

	public String getIpDestination() {
		return ipDestination;
	}

	public void setIpDestination(String ipDestination) {
		this.ipDestination = ipDestination;
	}


	public TemplateEntity getTemplate() {
		return template;
	}

	public void setTemplate(TemplateEntity template) {
		this.template = template;
	}

	public Long getIdPedestrianAccess() {
		return idPedestrianAccess;
	}

	public void setIdPedestrianAccess(Long idPedestrianAccess) {
		this.idPedestrianAccess = idPedestrianAccess;
	}

}
