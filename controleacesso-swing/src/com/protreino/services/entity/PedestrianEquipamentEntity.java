package com.protreino.services.entity;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
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
@Table(name="TB_PEDESTRIAN_EQUIPAMENT")
@NamedQueries({
	@NamedQuery(name = "PedestrianEquipamentEntity.findAll", query = "select obj from PedestrianEquipamentEntity obj"),
	@NamedQuery(name = "PedestrianEquipamentEntity.findById", query = "select obj from PedestrianEquipamentEntity obj "
			+ " join fetch obj.pedestrianAccess a"
			+ " where obj.id = :ID")
})
public class PedestrianEquipamentEntity extends BaseEntity{
	
	@Id
	@Column(name="ID_PEDESTRIAN_EQUIPAMENT", nullable=false, length=11)
	private Long id;
	
	@Column(name="ID_EQUIPAMENT", nullable=false, length=100)
	private String idEquipamento;
	
	@ManyToOne(cascade={}, fetch=FetchType.LAZY)
	@JoinColumn(name="ID_PEDESTRIAN_ACCESS", nullable=true)
	private PedestrianAccessEntity pedestrianAccess;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="VALIDADE_EQUIPAMENTO", nullable=true, length=30)
	private Date validadeEquipamento;
	
	@Column(name="NOME_EQUIPAMENTO", nullable=true, length=100)
	private String nomeEquipamento;
	
	@Type(type="org.hibernate.type.NumericBooleanType")
	@Column(name="CADASTRADO_NO_DESKTOP", nullable=true, length=30)
	private Boolean cadastradoNoDesktop = false;
	
	@Type(type="org.hibernate.type.NumericBooleanType")
	@Column(name="REMOVIDO_NO_DESKTOP", nullable=true, length=30)
	private Boolean removidoNoDesktop = false;
	
	public PedestrianEquipamentEntity() {
		
	}

	public PedestrianEquipamentEntity(PedestrianAccessEntity pedestrianAccessEntity,
			PedestrianEquipamentEntity eq) {
		this.id = eq.getId();
		this.idEquipamento = eq.getIdEquipamento();
		this.pedestrianAccess = pedestrianAccessEntity;
		this.validadeEquipamento = eq.getValidadeEquipamento();
		this.nomeEquipamento = eq.getNomeEquipamento();
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getIdEquipamento() {
		return idEquipamento;
	}

	public void setIdEquipamento(String idEquipamento) {
		this.idEquipamento = idEquipamento;
	}

	public PedestrianAccessEntity getPedestrianAccess() {
		return pedestrianAccess;
	}

	public void setPedestrianAccess(PedestrianAccessEntity pedestrianAccess) {
		this.pedestrianAccess = pedestrianAccess;
	}

	public Date getValidadeEquipamento() {
		return validadeEquipamento;
	}

	public void setValidadeEquipamento(Date validadeEquipamento) {
		this.validadeEquipamento = validadeEquipamento;
	}

	public String getNomeEquipamento() {
		return nomeEquipamento;
	}

	public void setNomeEquipamento(String nomeEquipamento) {
		this.nomeEquipamento = nomeEquipamento;
	}

	public Boolean getCadastradoNoDesktop() {
		return cadastradoNoDesktop;
	}

	public void setCadastradoNoDesktop(Boolean cadastradoNoDesktop) {
		this.cadastradoNoDesktop = cadastradoNoDesktop;
	}

	public Boolean getRemovidoNoDesktop() {
		return removidoNoDesktop;
	}

	public void setRemovidoNoDesktop(Boolean removidoNoDesktop) {
		this.removidoNoDesktop = removidoNoDesktop;
	}
	
}
