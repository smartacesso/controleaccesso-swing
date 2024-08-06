package com.protreino.services.entity;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import com.protreino.services.enumeration.Finger;


@SuppressWarnings("serial")
@Entity
@Table(name="TB_HIKIVISION_FINGER")
public class HikivisionFingerEntity extends BaseEntity implements ObjectWithId {


	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name="ID_HIKIVISION_FINGER", nullable=false)
	private Long id;
	
	@Enumerated(EnumType.STRING)
	@Column(name="FINGER_NO", nullable=true, length=100)
	private Finger fingerNo;
	
	@Column(name="FINGER_DATA", nullable=false, length=700)
	private String fingerData;
	
	@Column(name="ID_USER", nullable=false, length=100)
	private Long idUser;
	
	public HikivisionFingerEntity() {
	}
	
	public HikivisionFingerEntity(Finger fingerNo, String fingerData, Long idUser) {
		this.fingerNo = fingerNo;
		this.fingerData = fingerData;
		this.idUser = idUser;
	}

	public Finger getFingerNo() {
		return fingerNo;
	}

	public void setFingerNo(Finger fingerNo) {
		this.fingerNo = fingerNo;
	}

	public String getFingerData() {
		return fingerData;
	}

	public void setFingerData(String fingerData) {
		this.fingerData = fingerData;
	}

	public Long getIdUser() {
		return idUser;
	}

	public void setIdUser(Long idUser) {
		this.idUser = idUser;
	}

	public Long getId() {
		return id;
	}

	@Override
	public void setId(Long id) {
		this.id = id;
	}



	
}
