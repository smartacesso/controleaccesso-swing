package com.protreino.services.entity;

import java.util.Date;
import java.util.UUID;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.hibernate.annotations.Type;


@SuppressWarnings("serial")
@Entity
@Table(name="TB_TOPDATA_FACIAL_ERROR")
@NamedQueries({
	@NamedQuery(name = "TopdataFacialErrorEntity.findAll", 
				query = "select obj from TopdataFacialErrorEntity obj "
						+ "order by obj.id asc"),
	@NamedQuery(name = "TopdataFacialErrorEntity.findById", 
				query = "select obj from TopdataFacialErrorEntity obj "
					  + "where obj.id = :ID "),
	@NamedQuery(name  = "TopdataFacialErrorEntity.findAllByCardNumber", 
				query = "select obj "
						+ "from TopdataFacialErrorEntity obj "
						+ "where obj.cardNumber = :CARD_NUMBER "	
						+ "order by obj.id asc"),
	@NamedQuery(name = "TopdataFacialErrorEntity.findAllOrderByErrorDate", 
				query = "select obj from TopdataFacialErrorEntity obj "
						+ "left join fetch obj.pedestre p "
						+ "order by obj.errorDate desc"),
	@NamedQuery(name = "TopdataFacialErrorEntity.countAllOrderByErrorDate", 
				query = "select count(obj) from TopdataFacialErrorEntity obj "),
})
public class TopdataFacialErrorEntity extends BaseEntity implements ObjectWithId {
	
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name="ID_TOPDATA_FACIAL_ERROR", nullable=false)
	private Long id;
	
	@Column(name="IP_FACIAL", nullable=true, length=100)
	private String ipFacial;
	
	@Column(name="CARD_NUMBER", nullable=false, length=700)
	private String cardNumber;
	
	@Column(name="REASON_ID", nullable=false, length=100)
	private Integer reasonId;
	
	@Column(name="ERROR_DATE", nullable=false, length=100)
	private Date errorDate;
	
	@ManyToOne(cascade={}, fetch=FetchType.LAZY)
	@JoinColumn(name="ID_PEDESTRIAN_ACCESS", nullable=true)
	private PedestrianAccessEntity pedestre;

	public TopdataFacialErrorEntity() {
	
	}
	
	public TopdataFacialErrorEntity(Long id, String ipFacial, String cardNumber, Integer reasonId, Date errorDate,
			Long pedestreId, String pedestreCardNumber, String pedestreName) {
		this.id = id;
		this.ipFacial = ipFacial;
		this.cardNumber = cardNumber;
		this.reasonId = reasonId;
		this.errorDate = errorDate;

		if (pedestreId != null) {
			this.pedestre = new PedestrianAccessEntity();
			this.pedestre.setId(pedestreId);
			this.pedestre.setCardNumber(pedestreCardNumber);
			this.pedestre.setName(pedestreName);
		}
	}

	
	public TopdataFacialErrorEntity(String ipFacial, String cardNumber, Integer reasonId, Date errorDate, PedestrianAccessEntity pedestre) {
		super();
		this.ipFacial = ipFacial;
		this.cardNumber = cardNumber;
		this.reasonId = reasonId;
		this.errorDate = errorDate;
		this.pedestre = pedestre;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getIpFacial() {
		return ipFacial;
	}

	public void setIpFacial(String ipFacial) {
		this.ipFacial = ipFacial;
	}

	public String getCardNumber() {
		return cardNumber;
	}

	public void setCardNumber(String cardNumber) {
		this.cardNumber = cardNumber;
	}

	public Integer getReasonId() {
		return reasonId;
	}

	public void setReasonId(Integer reasonId) {
		this.reasonId = reasonId;
	}

	public Date getErrorDate() {
		return errorDate;
	}

	public void setErrorDate(Date errorDate) {
		this.errorDate = errorDate;
	}

	public PedestrianAccessEntity getPedestre() {
		return pedestre;
	}

	public void setPedestre(PedestrianAccessEntity pedestre) {
		this.pedestre = pedestre;
	}

}
