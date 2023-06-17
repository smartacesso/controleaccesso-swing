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

import org.hibernate.annotations.FilterDef;
import org.hibernate.annotations.Type;

@Entity
@Table(name="TB_HORARIO")
@NamedQueries({
	@NamedQuery(name  = "HorarioEntity.findAll", 
				query = "select obj "
				      + "from HorarioEntity obj "
					  + "order by obj.id asc"),
	@NamedQuery(name  = "HorarioEntity.findById", 
				query = "select obj from HorarioEntity obj "
					  + "where obj.id = :ID order by obj.id asc")
})
@SuppressWarnings("serial")
public class HorarioEntity extends BaseEntity implements ObjectWithId  {

	@Id
	@Column(name="ID_HORARIO", nullable=false, length=4)
	private Long id;
	
	@Column(name="NOME", nullable=true, length=255)
	private String nome;
	
	@ManyToOne(cascade={}, fetch=FetchType.LAZY)
	@JoinColumn(name="ID_REGRA", nullable=true)
	private RegraEntity regra;
	
	@Column(name="DIAS_SEMANA", nullable=true, length=100)
	private String diasSemana;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="HORARIO_INI", nullable=true, length=11)
	private Date horarioInicio;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="HORARIO_FIM", nullable=true, length=11)
	private Date horarioFim;
	
	@Column(name="STATUS", nullable=true, length=20)
	private String status;
	
	@Type(type="org.hibernate.type.NumericBooleanType")
	@Column(name="REMOVED", nullable=true, length=30)
	private Boolean removed;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="DATA_REMOVIDO", nullable=true, length=11)
	private Date dataRemovido = null;

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

	public RegraEntity getRegra() {
		return regra;
	}

	public void setRegra(RegraEntity regra) {
		this.regra = regra;
	}

	public String getDiasSemana() {
		return diasSemana;
	}

	public void setDiasSemana(String diasSemana) {
		this.diasSemana = diasSemana;
	}

	public Date getHorarioInicio() {
		return horarioInicio;
	}

	public void setHorarioInicio(Date horarioInicio) {
		this.horarioInicio = horarioInicio;
	}

	public Date getHorarioFim() {
		return horarioFim;
	}

	public void setHorarioFim(Date horarioFim) {
		this.horarioFim = horarioFim;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
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
	
}
