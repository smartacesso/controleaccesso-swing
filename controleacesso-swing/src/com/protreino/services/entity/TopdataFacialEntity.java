package com.protreino.services.entity;

import java.util.Date;
import java.util.UUID;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.hibernate.annotations.Type;


@SuppressWarnings("serial")
@Entity
@Table(name="TB_TOPDATA_FACIAL")
@NamedQueries({
	@NamedQuery(name = "TopdataFacialEntity.findAll", 
				query = "select obj from TopdataFacialEntity obj "
						+ "order by obj.id asc"),
	@NamedQuery(name = "TopdataFacialEntity.findById", 
				query = "select obj from TopdataFacialEntity obj "
					  + "where obj.id = :ID "),
	@NamedQuery(name  = "TopdataFacialEntity.findAllNaoRemovidosOrdered", 
				query = "select obj "
						+ "from TopdataFacialEntity obj "
						+ "where (obj.removed is null or obj.removed = false) "	
						+ "order by obj.id asc"),
})
public class TopdataFacialEntity extends BaseEntity implements ObjectWithId{
	
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name="ID_TOPDATA_FACIAL", nullable=false)
	private Long id;
	
	@Column(name="IP_FACIAL", nullable=true, length=100)
	private String ipFacial;
	
	@Column(name="PORTA_FACIAL", nullable=false, length=700)
	private String portaFacial;
	
	@Column(name="NOME_FACIAL", nullable=false, length=100)
	private String nomeFacial;
	
	@Column(name="IDENTIFIEER", nullable=false, length=100)
	private String identifier = UUID.randomUUID().toString();
	
	@Type(type="org.hibernate.type.NumericBooleanType")
	@Column(name="REMOVED", nullable=true, length=30)
	private Boolean removed;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="DATA_REMOVIDO", nullable=true, length=11)
	private Date dataRemovido = null;

	public Boolean getRemoved() {
		return removed;
	}

	public void setRemoved(Boolean removed) {
		this.removed = removed;
	}

	public Date getDataRemovido() {
		return dataRemovido;
	}

	public void setDataRemovido(Date dataRemovido) {
		this.dataRemovido = dataRemovido;
	}

	public TopdataFacialEntity() {
		
	}
	
	public TopdataFacialEntity(String ipFacial, String portaFacial, String nomeFacial) {
		super();
		this.ipFacial = ipFacial;
		this.portaFacial = portaFacial;
		this.nomeFacial = nomeFacial;
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

	public String getPortaFacial() {
		return portaFacial;
	}

	public void setPortaFacial(String portaFacial) {
		this.portaFacial = portaFacial;
	}

	public String getNomeFacial() {
		return nomeFacial;
	}

	public void setNomeFacial(String nomeFacial) {
		this.nomeFacial = nomeFacial;
	}

	public String getIdentifier() {
		return identifier;
	}

	public void setIdentifier(String identifier) {
		this.identifier = identifier;
	}

}
