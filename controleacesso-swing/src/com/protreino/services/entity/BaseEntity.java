package com.protreino.services.entity;
import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.MappedSuperclass;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Version;

@SuppressWarnings("serial")
@MappedSuperclass
public class BaseEntity implements Serializable {

	@Temporal( TemporalType.TIMESTAMP)
	@Column(name="DATA_ALTERACAO", nullable=false, length=11)
	private Date dataAlteracao = new Date();
	
	@Temporal( TemporalType.TIMESTAMP)
	@Column(name="DATA_CRIACAO", nullable=false, length=11)
	private Date dataCriacao = new Date();
	
	@Version
	@Column(name="VERSAO", nullable=false, length=4)
	private Integer versao;

	public Date getDataAlteracao() {
		return dataAlteracao;
	}

	public void setDataAlteracao(Date dataAlteracao) {
		this.dataAlteracao = dataAlteracao;
	}

	public Date getDataCriacao() {
		return dataCriacao;
	}

	public void setDataCriacao(Date dataCriacao) {
		this.dataCriacao = dataCriacao;
	}

	public Integer getVersao() {
		return versao;
	}

	public void setVersao(Integer versao) {
		this.versao = versao;
	}
	
}
