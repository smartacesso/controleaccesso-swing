package com.protreino.services.entity;

import java.io.Serializable;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import com.protreino.services.enumeration.Finger;


@SuppressWarnings("serial")
@Entity
@Table(name="TB_HIKIVISION_FINGER_ERROR")
@NamedQueries({
	@NamedQuery(name = "HikivisonFingerErrorEntity.findAllBiometricWithErrors", 
				query = "select obj from HikivisonFingerErrorEntity obj "
						+ "order by obj.id asc")
})
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
	
	@Enumerated(EnumType.STRING)
	@Column(name="FINGER_NO", nullable=true, length=100)
	private Finger fingerNo;
	
	
	public HikivisonFingerErrorEntity() {
	}
	
	public HikivisonFingerErrorEntity(Long idUser, String errorMessage, String deviceId, Finger fingerNo) {
		this.idUser = idUser;
		this.errorMessage = errorMessage;
		this.deviceId = deviceId;
		this.fingerNo = fingerNo;
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

	public String getDeviceId() {
		return deviceId;
	}

	public void setDeviceId(String deviceId) {
		this.deviceId = deviceId;
	}

	public Finger getFingerNo() {
		return fingerNo;
	}

	public void setFingerNo(Finger fingerNo) {
		this.fingerNo = fingerNo;
	}




}
