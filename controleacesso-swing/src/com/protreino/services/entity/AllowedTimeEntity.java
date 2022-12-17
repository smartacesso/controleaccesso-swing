package com.protreino.services.entity;

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

@SuppressWarnings("serial")
@Entity
@Table(name="TB_ALLOWED_TIME")
@NamedQueries({
	@NamedQuery(name = "AllowedTimeEntity.findAll", query = "select obj from AllowedTimeEntity obj"),
	@NamedQuery(name = "AllowedTimeEntity.findById", query = "select obj from AllowedTimeEntity obj "
			+ " join fetch obj.pedestrianAccess a"
			+ " where obj.id = :ID")
})
public class AllowedTimeEntity extends BaseEntity {
	
	@Id
	@GeneratedValue(strategy=GenerationType.IDENTITY)
	@Column(name="ID_ALLOWED_TIME", nullable=false, length=11)
	private Long id;
	
	@ManyToOne(cascade={}, fetch=FetchType.LAZY)
	@JoinColumn(name="ID_PEDESTRIAN_ACCESS", nullable=true)
	private PedestrianAccessEntity pedestrianAccess;

	@Column(name="START_TIME", nullable=true, length=100)
	private String inicio;
	
	@Column(name="END_TIME", nullable=true, length=100)
	private String fim;
	
	@Column(name="ALLOWED_DAYS", nullable=true, length=100)
	private String diasPermitidos;
	
	
	public AllowedTimeEntity() {
	}

	public AllowedTimeEntity(PedestrianAccessEntity pedestrianAccess, String inicio, String fim, String diasPermitidos) {
		this.pedestrianAccess = pedestrianAccess;
		this.inicio = inicio;
		this.fim = fim;
		this.diasPermitidos = diasPermitidos;
	}
	
	public String toString(){
		return inicio + "_" + fim + "_" + diasPermitidos;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getInicio() {
		return inicio;
	}

	public void setInicio(String inicio) {
		this.inicio = inicio;
	}

	public String getFim() {
		return fim;
	}

	public void setFim(String fim) {
		this.fim = fim;
	}

	public String getDiasPermitidos() {
		return diasPermitidos;
	}

	public void setDiasPermitidos(String diasPermitidos) {
		this.diasPermitidos = diasPermitidos;
	}

	public PedestrianAccessEntity getPedestrianAccess() {
		return pedestrianAccess;
	}

	public void setPedestrianAccess(PedestrianAccessEntity pedestrianAccess) {
		this.pedestrianAccess = pedestrianAccess;
	}

}
