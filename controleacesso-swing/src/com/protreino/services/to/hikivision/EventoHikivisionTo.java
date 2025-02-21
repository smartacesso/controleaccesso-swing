package com.protreino.services.to.hikivision;

import java.io.Serializable;

public class EventoHikivisionTo implements Serializable {

	private static final long serialVersionUID = -5907399646090897264L;

	private String card; // Tipo do evento
	private String deviceId; // Dados do evento (ou outros campos relevantes)
	
	public String getCard() {
		return card;
	}
	public void setCard(String card) {
		this.card = card;
	}
	public String getDeviceId() {
		return deviceId;
	}
	public void setDeviceId(String deviceId) {
		this.deviceId = deviceId;
	}
	
    @Override
    public String toString() {
        return "EventMessageTO{" +
               "card='" + card + '\'' +
               ", deviceId='" + deviceId + '\'' +
               '}';
    }
	

}
