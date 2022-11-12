package com.protreino.services.entity;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.Lob;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.hibernate.annotations.Type;

import com.protreino.services.to.DocumentoTo;

@Entity
@Table(name="TB_DOCUMENTO")
@NamedQueries({
	@NamedQuery(name  = "DocumentoEntity.findAll", 
				query = "select obj "
				      + "from DocumentoEntity obj "
					  + "order by obj.id asc"),
	@NamedQuery(name  = "DocumentoEntity.findById", 
				query = "select obj from DocumentoEntity obj "
					  + "where obj.id = :ID order by obj.id asc")
})
@SuppressWarnings("serial")
public class DocumentoEntity extends BaseEntity implements ObjectWithId, Serializable {

	@Id
	@Column(name="ID_DOCUMENTO", nullable=false, length=4)
	private Long id;
	
	@Column(name="NOME", nullable=true, length=255)
	private String nome;
	
	@Lob
	@Column(name="ARQUIVO", nullable=true)
	private byte[] arquivo;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="VALIDADE", nullable=true, length=11)
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

	public DocumentoEntity() {}
	
	public DocumentoEntity(PedestrianAccessEntity pedestrianAccessEntity, DocumentoTo d) {
		this.id = d.getId();
		this.nome = d.getNome();
		this.validade = d.getValidade();
		this.pedestrianAccess = pedestrianAccessEntity;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getNome() {
		return nome;
	}

	public void setNome(String nome) {
		this.nome = nome;
	}

	public byte[] getArquivo() {
		return arquivo;
	}

	public void setArquivo(byte[] arquivo) {
		this.arquivo = arquivo;
	}

	public Date getValidade() {
		return validade;
	}

	public void setValidade(Date validade) {
		this.validade = validade;
	}

	public PedestrianAccessEntity getPedestrianAccess() {
		return pedestrianAccess;
	}

	public void setPedestrianAccess(PedestrianAccessEntity pedestrianAccess) {
		this.pedestrianAccess = pedestrianAccess;
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
