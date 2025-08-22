package com.protreino.services.entity;

import java.util.Calendar;
import java.util.Date;
import java.util.Objects;

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

import com.protreino.services.utils.Utils;

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
	
	@Column(name="QTDE_DE_CREDITOS", nullable=true, length=4)
	private Long qtdeDeCreditos;
	
	@ManyToOne(cascade={}, fetch=FetchType.LAZY)
	@JoinColumn(name="ID_PEDESTRE_REGRA", nullable=true)
	private PedestreRegraEntity pedestreRegra;
	
	public boolean isDiaPermitido(final Date data) {
		Calendar cHoje = Calendar.getInstance();
		if (data != null) {
			cHoje.setTime(data);
		}

		Integer hoje = cHoje.get(Calendar.DAY_OF_WEEK);

		hoje--; // ajusta a numeracao dos dias para coincidir com a numeracao do pro-treino
		if (hoje == 0) {
			hoje = 7;
		}

		return Objects.nonNull(diasSemana) && diasSemana.contains(hoje.toString());
	}
	
	public boolean isDentroDoHorarioPermitido(final Date data) {
		if(Objects.nonNull(dataRemovido)) {
			return false;
		}
		
		Calendar cHoje = Calendar.getInstance();
		if (data != null) {
			cHoje.setTime(data);
		}

		Integer hoje = cHoje.get(Calendar.DAY_OF_WEEK);

		hoje--; // ajusta a numeracao dos dias para coincidir com a numeracao do pro-treino
		if (hoje == 0)  {
			hoje = 7;
		}
		
		Calendar c = Calendar.getInstance();
		c.setTime(horarioInicio);
		final String horarioInicio = c.get(Calendar.HOUR_OF_DAY) + ":" + c.get(Calendar.MINUTE);
		
		c.setTime(horarioFim);
		final String horarioFim = c.get(Calendar.HOUR_OF_DAY) + ":" + c.get(Calendar.MINUTE);
		
		return Utils.isDentroDoHorario(horarioInicio, horarioFim, data);
	}
	
	public boolean temCreditos() {
		return Objects.isNull(qtdeDeCreditos) || qtdeDeCreditos > 0;
	}
	
	public boolean temCreditosValidos() {
	    return this.qtdeDeCreditos != null && this.qtdeDeCreditos > 0;
	}

	
	public void decrementaCreditos() {
		if(Objects.isNull(qtdeDeCreditos)) {
			return;
		}
		
		qtdeDeCreditos = qtdeDeCreditos - 1;
		
		if(qtdeDeCreditos < 0) {
			qtdeDeCreditos = 0l;
		}
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

	public Long getQtdeDeCreditos() {
		return qtdeDeCreditos;
	}

	public void setQtdeDeCreditos(Long qtdeDeCreditos) {
		this.qtdeDeCreditos = qtdeDeCreditos;
	}

	public PedestreRegraEntity getPedestreRegra() {
		return pedestreRegra;
	}

	public void setPedestreRegra(PedestreRegraEntity pedestreRegra) {
		this.pedestreRegra = pedestreRegra;
	}
	
}
