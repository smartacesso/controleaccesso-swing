package com.protreino.services.entity;

import java.util.Date;

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

@Entity
@Table(name="TB_CENTRO_CUSTO")
@NamedQueries({
	@NamedQuery(name  = "CentroCustoEntity.findAll", 
				query = "select obj "
				      + "from CentroCustoEntity obj "
					  + "order by obj.id asc"),
	@NamedQuery(name  = "CentroCustoEntity.findById", 
				query = "select obj from CentroCustoEntity obj "
					  + "where obj.id = :ID order by obj.id asc"),
	@NamedQuery(name = "CentroCustoEntity.findAllByIdEmpresa",
				query = "select obj from CentroCustoEntity obj "
					  + "where obj.empresa.id = :ID_EMPRESA "
					  + "and obj.status = 'ATIVO' "
					  + "and (obj.removed = false or obj.removed is null) "
					  + "order by obj.id asc")
})
@SuppressWarnings("serial")
public class CentroCustoEntity extends BaseEntity {
	
	@Id
	@Column(name="ID_CENTRO_CUSTO", nullable=false, length=4)
	private Long id;
	
	@Column(name="NOME", nullable=true, length=255)
	private String nome;
	
	@Column(name="STATUS", nullable=true, length=30)
	private String status;
	
	@Type(type="org.hibernate.type.NumericBooleanType")
	@Column(name="REMOVED", nullable=true, length=30)
	private Boolean removed;
	
	@Temporal( TemporalType.TIMESTAMP)
	@Column(name="DATA_REMOVIDO", nullable=true, length=11)
	private Date dataRemovido = null;
	
	@ManyToOne(cascade={}, fetch=FetchType.LAZY)
	@JoinColumn(name="ID_EMPRESA", nullable=true)
	private EmpresaEntity empresa;
	
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
	public EmpresaEntity getEmpresa() {
		return empresa;
	}
	public void setEmpresa(EmpresaEntity empresa) {
		this.empresa = empresa;
	}
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
	public String getStatus() {
		return status;
	}
	public void setStatus(String status) {
		this.status = status;
	}
	
}
