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
@Table(name="TB_HIKIVISION_FINGER")
public class HikivisionFingerEntity extends BaseEntity implements ObjectWithId {


	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name="ID_HIKIVISION_FINGER", nullable=false)
	private Long id;
	
	@Column(name="FINGER_NO", nullable=false)
	private Integer fingerNo = 0;
	
	@Column(name="FINGER_DATA", nullable=false, length=700)
	private String fingerData;
	
	@Column(name="ERROR_MESSAGE", nullable=false, length=100)
	private String errorMessage;
	
	@Column(name="ID_USER", nullable=false, length=100)
	private String idUser;
	
	public HikivisionFingerEntity() {
	}
	
	public HikivisionFingerEntity(Integer fingerNo, String fingerData, String idUser) {
		this.fingerNo = fingerNo;
		this.fingerData = fingerData;
		this.idUser = idUser;
	}

	public Integer getFingerNo() {
		return fingerNo;
	}

	public void setFingerNo(Integer fingerNo) {
		this.fingerNo = fingerNo;
	}

	public String getFingerData() {
		return fingerData;
	}

	public void setFingerData(String fingerData) {
		this.fingerData = fingerData;
	}

	public String getIdUser() {
		return idUser;
	}

	public void setIdUser(String idUser) {
		this.idUser = idUser;
	}

	public Long getId() {
		return id;
	}

	@Override
	public void setId(Long id) {
		// TODO Auto-generated method stub
		
	}

	public String getErrorMessage() {
		return errorMessage;
	}

	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}


	
}
