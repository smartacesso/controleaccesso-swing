package com.protreino.services.to;

import java.util.ArrayList;
import java.util.Date;

import com.protreino.services.entity.HorarioEntity;
import com.protreino.services.enumeration.TipoEscala;
import com.protreino.services.enumeration.TipoPedestre;
import com.protreino.services.enumeration.TipoRegra;

public class RegraTO {

	private Long id;
	private String nome;
	private Date validade;
	private Long idEmpresa;
	private TipoRegra tipo;
	private TipoPedestre tipoPedestre;
	private String descricao;
	private Date dataInicioPeriodo;
	private Date dataFimPeriodo;
	private TipoEscala tipoEscala;
	private Date horarioInicioTurno;
	private Long qtdeDeCreditos;
	private Long diasValidadeCredito;
	private Integer idTemplate;
	private Integer idPlano;
	
	private String status;
	private Boolean removed;
	private Date dataRemovido;
	
	private ArrayList<HorarioEntity> horarios;
	
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
	public Date getValidade() {
		return validade;
	}
	public void setValidade(Date validade) {
		this.validade = validade;
	}
	public Long getIdEmpresa() {
		return idEmpresa;
	}
	public void setIdEmpresa(Long idEmpresa) {
		this.idEmpresa = idEmpresa;
	}
	public TipoRegra getTipo() {
		return tipo;
	}
	public void setTipo(TipoRegra tipo) {
		this.tipo = tipo;
	}
	public TipoPedestre getTipoPedestre() {
		return tipoPedestre;
	}
	public void setTipoPedestre(TipoPedestre tipoPedestre) {
		this.tipoPedestre = tipoPedestre;
	}
	public String getDescricao() {
		return descricao;
	}
	public void setDescricao(String descricao) {
		this.descricao = descricao;
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
	public TipoEscala getTipoEscala() {
		return tipoEscala;
	}
	public void setTipoEscala(TipoEscala tipoEscala) {
		this.tipoEscala = tipoEscala;
	}
	public Date getHorarioInicioTurno() {
		return horarioInicioTurno;
	}
	public void setHorarioInicioTurno(Date horarioInicioTurno) {
		this.horarioInicioTurno = horarioInicioTurno;
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
	public ArrayList<HorarioEntity> getHorarios() {
		return horarios;
	}
	public void setHorarios(ArrayList<HorarioEntity> horarios) {
		this.horarios = horarios;
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
	public Integer getIdTemplate() {
		return idTemplate;
	}
	public void setIdTemplate(Integer idTemplate) {
		this.idTemplate = idTemplate;
	}
	public Integer getIdPlano() {
		return idPlano;
	}
	public void setIdPlano(Integer idPlano) {
		this.idPlano = idPlano;
	}
	
}
