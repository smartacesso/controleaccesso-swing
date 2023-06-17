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

import com.protreino.services.to.PedestreRegraTO;

@Entity
@Table(name="TB_PEDESTRE_REGRA")
@NamedQueries({
	@NamedQuery(name  = "PedestreRegraEntity.findAll", 
				query = "select obj "
				      + "from PedestreRegraEntity obj "
					  + "order by obj.id asc"),
	@NamedQuery(name  = "PedestreRegraEntity.findById", 
				query = "select obj from PedestreRegraEntity obj "
					  + "where obj.id = :ID order by obj.id asc")
})
@SuppressWarnings("serial")
public class PedestreRegraEntity extends BaseEntity {

	@Id
	@Column(name="ID_PEDESTRE_REGRA", nullable=false, length=4)
	private Long id;
	
	@ManyToOne(cascade={}, fetch=FetchType.EAGER)
	@JoinColumn(name="ID_REGRA", nullable=true)
	private RegraEntity regra;
	
	@ManyToOne(cascade={}, fetch=FetchType.LAZY)
	@JoinColumn(name="ID_PEDESTRE", nullable=true)
	private PedestrianAccessEntity pedestrianAccess;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="VALIDADE", nullable=true, length=11)
	private Date validade;
	
	@Column(name="QTDE_CREDITOS", nullable=true, length=10)
	private Long qtdeDeCreditos;
	
	@Column(name="QTDE_TOTAL_CREDITOS", nullable=true, length=10)
	private Long qtdeTotalDeCreditos;
	
	@Column(name="DIAS_VALIDADE_CREDITO", nullable=true, length=10)
	private Long diasValidadeCredito;
	
	@Temporal(TemporalType.DATE)
	@Column(name="DATA_INICIO_PERIODO", nullable=true, length=11)
	private Date dataInicioPeriodo;
	
	@Temporal(TemporalType.DATE)
	@Column(name="DATA_FIM_PERIODO", nullable=true, length=11)
	private Date dataFimPeriodo;
	
	@Type(type="org.hibernate.type.NumericBooleanType")
	@Column(name="CADASTRADO_NO_DESKTOP", nullable=true, length=30)
	private Boolean cadastradoNoDesktop = false;
	
	@Type(type="org.hibernate.type.NumericBooleanType")
	@Column(name="REMOVIDO_NO_DESKTOP", nullable=true, length=30)
	private Boolean removidoNoDesktop = false;
	
	public PedestreRegraEntity() {}
	
	public PedestreRegraEntity(PedestrianAccessEntity pedestrianAccess, PedestreRegraTO pr, RegraEntity regra) {
		this.id = pr.getId();
		this.regra = regra;
		this.pedestrianAccess = pedestrianAccess;
		this.validade = pr.getValidade();
		this.qtdeDeCreditos = pr.getQtdeDeCreditos();
		this.qtdeTotalDeCreditos = pr.getQtdeTotalDeCreditos();
		this.diasValidadeCredito = pr.getDiasValidadeCredito();
		this.dataInicioPeriodo = pr.getDataInicioPeriodo();
		this.dataFimPeriodo = pr.getDataFimPeriodo();
	}
	
	public Long getId() {
		return id;
	}
	public void setId(Long id) {
		this.id = id;
	}
	public RegraEntity getRegra() {
		return regra;
	}
	public void setRegra(RegraEntity regra) {
		this.regra = regra;
	}
	public Date getValidade() {
		return validade;
	}
	public void setValidade(Date validade) {
		this.validade = validade;
	}
	public Long getQtdeDeCreditos() {
		return qtdeDeCreditos;
	}
	public void setQtdeDeCreditos(Long qtdeDeCreditos) {
		this.qtdeDeCreditos = qtdeDeCreditos;
	}
	public Long getDiasValidadeCredito() {
		return diasValidadeCredito;
	}
	public void setDiasValidadeCredito(Long diasValidadeCredito) {
		this.diasValidadeCredito = diasValidadeCredito;
	}
	public Date getDataInicioPeriodo() {
		return dataInicioPeriodo;
	}
	public void setDataInicioPeriodo(Date dataInicioPeriodo) {
		this.dataInicioPeriodo = dataInicioPeriodo;
	}
	public Date getDataFimPeriodo() {
		return dataFimPeriodo;
	}
	public void setDataFimPeriodo(Date dataFimPeriodo) {
		this.dataFimPeriodo = dataFimPeriodo;
	}
	public Long getQtdeTotalDeCreditos() {
		return qtdeTotalDeCreditos;
	}
	public void setQtdeTotalDeCreditos(Long qtdeTotalDeCreditos) {
		this.qtdeDeCreditos = qtdeTotalDeCreditos;
		this.qtdeTotalDeCreditos = qtdeTotalDeCreditos;
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
