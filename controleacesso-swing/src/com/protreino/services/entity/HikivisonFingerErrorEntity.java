package com.protreino.services.entity;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@SuppressWarnings("serial")
@Entity
@Table(name="TB_HIKIVISION_FINGER_ERROR")
public class HikivisonFingerErrorEntity extends BaseEntity implements ObjectWithId, Serializable {


	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name="ID_HIKIVISION_FINGER_ERROR", nullable=false)
	private Long id;
	
	@Column(name="ID_USER", nullable=false)
	private Long idUser = 0L;
	
	
	@Column(name="ERROR_MESSAGE", nullable=false, length=100)
	private String errorMessage;
	
	@Column(name="DEVICE_ID", nullable=false, length=200)
	private String deviceId;
	
	
	public HikivisonFingerErrorEntity() {
	}
	
	public HikivisonFingerErrorEntity(Long idUser, String errorMessage, String deviceId) {
		this.idUser = idUser;
		this.errorMessage = errorMessage;
		this.deviceId = deviceId;
	}
	

	public Long getIdUser() {
		return idUser;
	}

	public void setIdUser(Long idUser) {
		this.idUser = idUser;
	}

	public String getErrorMessage() {
		return errorMessage;
	}

	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	public Long getId() {
		return id;
	}

	@Override
	public void setId(Long id) {
		// TODO Auto-generated method stub
		
	}
	


}
