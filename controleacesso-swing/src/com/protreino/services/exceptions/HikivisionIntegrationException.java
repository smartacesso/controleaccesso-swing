package com.protreino.services.exceptions;

import com.protreino.services.enumeration.HikivisionAction;

public class HikivisionIntegrationException extends RuntimeException {

	private static final long serialVersionUID = 4837428485537972905L;
	
	private final String cardNumber;
	private final String deviceId;
	private final HikivisionAction hikivisionAction;
	
	public HikivisionIntegrationException(final String message, final String cardNumber, final String deviceId, HikivisionAction hikivisionAction) {
		super(message);
		this.cardNumber = cardNumber;
		this.deviceId = deviceId;
		this.hikivisionAction = hikivisionAction;
	}

	public String getCardNumber() {
		return cardNumber;
	}

	public String getDeviceId() {
		return deviceId;
	}

	public HikivisionAction getHikivisionAction() {
		return hikivisionAction;
	}
}
