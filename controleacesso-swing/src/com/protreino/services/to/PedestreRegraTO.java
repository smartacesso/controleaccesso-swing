package com.protreino.services.to;

import java.util.Date;
import java.util.List;

public class PedestreRegraTO {

	private Long id;
	private Long idRegra;
	private Date validade;
	private Long qtdeDeCreditos;
	private Long qtdeTotalDeCreditos;
	private Long diasValidadeCredito;
	private Date dataInicioPeriodo;
	private Date dataFimPeriodo;
	private Date dataInicioEscala3_3;
	private List<HorarioTO> horarios;
	
	public Long getId() {
		return id;
	}
	public void setId(Long id) {
		this.id = id;
	}
	public Long getIdRegra() {
		return idRegra;
	}
	public void setIdRegra(Long idRegra) {
		this.idRegra = idRegra;
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
	public Long getQtdeTotalDeCreditos() {
		return qtdeTotalDeCreditos;
	}
	public void setQtdeTotalDeCreditos(Long qtdeTotalDeCreditos) {
		this.qtdeTotalDeCreditos = qtdeTotalDeCreditos;
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
	public Date getDataInicioEscala3_3() {
		return dataInicioEscala3_3;
	}
	public void setDataInicioEscala3_3(Date dataInicioEscala3_3) {
		this.dataInicioEscala3_3 = dataInicioEscala3_3;
	}
	public List<HorarioTO> getHorarios() {
		return horarios;
	}
	public void setHorarios(List<HorarioTO> horarios) {
		this.horarios = horarios;
	}
}
