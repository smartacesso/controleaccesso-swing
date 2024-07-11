package com.protreino.services.entity;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;

public class HikivisonFingerErrorEntity extends BaseEntity implements Serializable {


	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name="ID_HIKIVISION_FINGER", nullable=false)
	private Long id;
	
	@Column(name="ID_USER", nullable=false, length=100)
	private String idUser;
	
	@Column(name="FINGER_NO", nullable=false)
	private Integer fingerNo = 0;
	
	@Column(name="ERROR_MESSAGE", nullable=false, length=100)
	private String errorMessage;
	
	@Column(name="FINGER_DATA", nullable=false, length=100)
	private String fingerData;
	
	public HikivisonFingerErrorEntity() {
	}
	
	public HikivisonFingerErrorEntity(Long id, String idUser, Integer fingerNo, String errorMessage,
			String fingerData) {
		this.idUser = idUser;
		this.fingerNo = fingerNo;
		this.errorMessage = errorMessage;
		this.fingerData = fingerData;
	}
	

	public String getIdUser() {
		return idUser;
	}

	public void setIdUser(String idUser) {
		this.idUser = idUser;
	}

	public Integer getFingerNo() {
		return fingerNo;
	}

	public void setFingerNo(Integer fingerNo) {
		this.fingerNo = fingerNo;
	}

	public String getErrorMessage() {
		return errorMessage;
	}

	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	public String getFingerData() {
		return fingerData;
	}

	public void setFingerData(String fingerData) {
		this.fingerData = fingerData;
	}

	public Long getId() {
		return id;
	}
	


}
