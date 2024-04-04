package com.protreino.services.entity;

import java.io.Serializable;
import java.util.Objects;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

import com.protreino.services.enumeration.HikivisionAction;

@SuppressWarnings("serial")
@Entity
@Table(name="TB_HIKIVISION_INTEGRATION_ERROR")
@NamedQueries({
	@NamedQuery(name = "HikivisionIntegrationErrorEntity.findAll", 
				query = "select obj from HikivisionIntegrationErrorEntity obj "
						+ "order by obj.id asc"),
	@NamedQuery(name = "HikivisionIntegrationErrorEntity.findById", 
				query = "select obj from HikivisionIntegrationErrorEntity obj "
					  + "where obj.id = :ID "),
	@NamedQuery(name = "HikivisionIntegrationErrorEntity.findByLatest", 
				query = "select new com.protreino.services.entity.HikivisionIntegrationErrorEntity(obj.message) "
					  + "from HikivisionIntegrationErrorEntity obj "
					  + "order by obj.dataCriacao desc "),
	@NamedQuery(name = "HikivisionIntegrationErrorEntity.findByCardNumberAndDeviceId",
				query = "select obj from HikivisionIntegrationErrorEntity obj "
						+ "where obj.cardNumber = :CARD_NUMBER "
						+ "and obj.deviceId = :DEVICE_ID "),
	@NamedQuery(name = "HikivisionIntegrationErrorEntity.findAllWhereRetriesAreLessThan", 
				query = "select obj from HikivisionIntegrationErrorEntity obj "
						+ "where obj.retries < :MAX_RETRIES "
						+ "order by obj.id asc"),
})
public class HikivisionIntegrationErrorEntity extends BaseEntity implements ObjectWithId, Serializable {

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name="ID_HIKIVISION_INTEGRATION_ERROR", nullable=false)
	private Long id;
	
	@Column(name="MESSAGE", nullable=false, length=256)
	private String message;
	
	@Column(name="CARD_NUMBER", nullable=false, length=100)
	private String cardNumber;

	@Column(name="DEVICE_ID", nullable=false, length=100)
	private String deviceId;
	
	@Column(name="RETRIES", nullable=false)
	private Long retries = 0L;
	
	@Enumerated(EnumType.STRING)
	@Column(name="HIKIVISION_ACTION", nullable=true, length=100)
	private HikivisionAction hikivisionAction;
	
	public HikivisionIntegrationErrorEntity() {
	}
	
	
	public HikivisionIntegrationErrorEntity(final String message, final String cardNumber, final String deviceId, HikivisionAction hikivisionAction) {
		this.message = message;
		this.cardNumber = cardNumber;
		this.deviceId = deviceId;
		this.hikivisionAction = hikivisionAction;
	}
	
	public HikivisionIntegrationErrorEntity(String message) {
		this.message = message;
	}
	
	public void incrementRetry() {
		retries = retries + 1;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getCardNumber() {
		return cardNumber;
	}

	public void setCardNumber(String cardNumber) {
		this.cardNumber = cardNumber;
	}

	public String getDeviceId() {
		return deviceId;
	}

	public void setDeviceId(String deviceId) {
		this.deviceId = deviceId;
	}

	public HikivisionAction getHikivisionAction() {
		return hikivisionAction;
	}

	public void setHikivisionAction(HikivisionAction hikivisionAction) {
		this.hikivisionAction = hikivisionAction;
	}


	public Long getRetries() {
		return retries;
	}


	public void setRetries(Long retries) {
		this.retries = retries;
	}

}
