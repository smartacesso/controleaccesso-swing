package com.protreino.services.entity;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
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

import com.protreino.services.enumeration.Status;

@SuppressWarnings("serial")
@Entity
@Table(name="TB_PEDESTRIAN_MESSAGES")
@NamedQueries({
	@NamedQuery(name = "PedestrianMessagesEntity.findAll", query = "select obj from PedestrianMessagesEntity obj"),
	@NamedQuery(name = "PedestrianMessagesEntity.findById", query = "select obj from PedestrianMessagesEntity obj "
			+ " join fetch obj.pedestrianAccess a"
			+ " where obj.id = :ID")
})
public class PedestrianMessagesEntity extends BaseEntity {
	
	@Id
	@Column(name="ID_PEDESTRIAN_MESSAGES", nullable=false, length=11)
	private Long id;
	
	@Column(name="NOME_MENSAGEM", nullable=true, length=30)
	private String nome;
	
	@Enumerated(EnumType.STRING)
	@Column(name="STATUS", nullable=true, length=100)
	private Status status;
	
	@Column(name="MENSAGEM", nullable=true, length=100)
	private String mensagem;
	
	@Column(name="QUANTIDADE", nullable=true, length=100)
	private Long quantidade;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="VALIDADE_MENSAGEM", nullable=true, length=30)
	private Date validade;
	
	@ManyToOne(cascade={}, fetch=FetchType.LAZY)
	@JoinColumn(name="ID_PEDESTRIAN_ACCESS", nullable=true)
	private PedestrianAccessEntity pedestrianAccess;
	
	@Type(type="org.hibernate.type.NumericBooleanType")
	@Column(name="CADASTRADO_NO_DESKTOP", nullable=true, length=30)
	private Boolean cadastradoNoDesktop = false;
	
	@Type(type="org.hibernate.type.NumericBooleanType")
	@Column(name="REMOVIDO_NO_DESKTOP", nullable=true, length=30)
	private Boolean removidoNoDesktop = false;
	
	public PedestrianMessagesEntity() {
		
	}

	public PedestrianMessagesEntity(PedestrianAccessEntity pedestrianAccessEntity, PedestrianMessagesEntity m) {
		this.id = m.getId();
		this.nome = m.getNome();
		this.status = m.getStatus();
		this.mensagem = m.getMensagem();
		this.quantidade = m.getQuantidade();
		this.validade = m.getValidade();
		this.pedestrianAccess = pedestrianAccessEntity;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getMensagem() {
		return mensagem;
	}

	public void setMensagem(String mensagem) {
		this.mensagem = mensagem;
	}

	public Long getQuantidade() {
		return quantidade;
	}

	public void setQuantidade(Long quantidade) {
		this.quantidade = quantidade;
	}

	public PedestrianAccessEntity getPedestrianAccess() {
		return pedestrianAccess;
	}

	public void setPedestrianAccess(PedestrianAccessEntity pedestrianAccess) {
		this.pedestrianAccess = pedestrianAccess;
	}

	public String getNome() {
		return nome;
	}

	public void setNome(String nome) {
		this.nome = nome;
	}

	public Status getStatus() {
		return status;
	}

	public void setStatus(Status status) {
		this.status = status;
	}

	public Date getValidade() {
		return validade;
	}

	public void setValidade(Date validade) {
		this.validade = validade;
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
