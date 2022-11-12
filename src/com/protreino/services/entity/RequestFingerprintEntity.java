package com.protreino.services.entity;

import com.protreino.services.enumeration.RequestFingerprintStatus;

@SuppressWarnings("serial")
public class RequestFingerprintEntity extends BaseEntity {

	private Long id;
	private BiometricEntity biometric;
	private RequestFingerprintStatus status;
	private String ip;
	private String userId;
	private String cardNumber;

	public RequestFingerprintEntity() {
	}
	
	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public BiometricEntity getBiometric() {
		return biometric;
	}

	public void setBiometric(BiometricEntity biometric) {
		this.biometric = biometric;
	}

	public RequestFingerprintStatus getStatus() {
		return status;
	}

	public void setStatus(RequestFingerprintStatus status) {
		this.status = status;
	}

	public String getIp() {
		return ip;
	}

	public void setIp(String ip) {
		this.ip = ip;
	}

	public String getUserId() {
		return userId;
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	public String getCardNumber() {
		return cardNumber;
	}

	public void setCardNumber(String cardNumber) {
		this.cardNumber = cardNumber;
	}
	
}
