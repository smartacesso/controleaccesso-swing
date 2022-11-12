package com.protreino.services.entity;

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

import org.hibernate.annotations.Type;

import com.protreino.services.enumeration.StatusCard;

@Entity
@Table(name="TB_CARTAO_COMANDA")
@NamedQueries({
	@NamedQuery(name  = "CartaoComandaEntity.findAll", 
				query = "select obj "
				      + "from CartaoComandaEntity obj "
					  + "order by obj.id asc"),
	@NamedQuery(name  = "CartaoComandaEntity.countNaoRemovidosOrdered", 
				query = "select count(*) "
				      + "from CartaoComandaEntity obj "
					  + "where (obj.removido is null or obj.removido = false) "),
	@NamedQuery(name  = "CartaoComandaEntity.findAllNaoRemovidosOrdered", 
				query = "select obj "
				      + "from CartaoComandaEntity obj "
					  + "where (obj.removido is null or obj.removido = false) "	
					  + "order by obj.id asc"),
	@NamedQuery(name  = "CartaoComandaEntity.findById", 
				query = "select obj from CartaoComandaEntity obj "
					  + "where obj.id = :ID"
					  + "	   and (obj.removido is null or obj.removido = false) "
					  + "order by obj.id asc"),
})
@SuppressWarnings("serial")
public class CartaoComandaEntity extends BaseEntity implements ObjectWithId {
	
	@Id
	@GeneratedValue(strategy=GenerationType.IDENTITY)
	@Column(name="ID_CARTAO_COMANDA", nullable=false, length=4)
	private Long id;
	
	@Column(name="NUMERO_REAL", nullable=true, length=255)
	private String numeroReal;
	
	@Column(name="NUMERO_ALTERNATIVO", nullable=true, length=255)
	private String numeroAlternativo;
	
	@Enumerated(EnumType.STRING)
	@Column(name="STATUS", nullable=true, length=255)
	private StatusCard status;
	
	@Type(type="org.hibernate.type.NumericBooleanType")
	@Column(name="REMOVIDO", nullable=true)
	private Boolean removido = false;

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getNumeroReal() {
		return numeroReal;
	}

	public void setNumeroReal(String numeroReal) {
		this.numeroReal = numeroReal;
	}

	public String getNumeroAlternativo() {
		return numeroAlternativo;
	}

	public void setNumeroAlternativo(String numeroAlternativo) {
		this.numeroAlternativo = numeroAlternativo;
	}

	public StatusCard getStatus() {
		return status;
	}

	public void setStatus(StatusCard status) {
		this.status = status;
	}

	public Boolean getRemovido() {
		return removido;
	}

	public void setRemovido(Boolean removido) {
		this.removido = removido;
	}

	
	

}
