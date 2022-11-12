package com.protreino.services.entity;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Id;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import com.protreino.services.enumeration.Status;

@Entity
@Table(name="TB_PLANO")
@NamedQueries({
	@NamedQuery(name  = "PlanoEntity.findAll", 
				query = "select obj "
				      + "from PlanoEntity obj "
					  + "order by obj.id asc"),
	@NamedQuery(name  = "PlanoEntity.findById", 
				query = "select obj from PlanoEntity obj "
					  + "where obj.id = :ID order by obj.id asc"),
	@NamedQuery(name  = "PlanoEntity.findMaiorDataVencimentoAndAtivo",
				query = "select obj from PlanoEntity obj "
					  + "where obj.status = 'ATIVO' "
					  + "and obj.idClient = :ID_CLIENTE "
					  + "order by obj.fim desc")
})
@SuppressWarnings("serial")
public class PlanoEntity extends BaseEntity implements ObjectWithId {

	@Id
	@Column(name="ID_PLANO", nullable=false, length=4)
	private Long id;
	
	@Column(name="NOME", nullable=true, length=255)
	private String nome;
	
	@Enumerated(EnumType.STRING)
	@Column(name="STATUS", nullable=true, length=10)
	private Status status;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="DATA_INICIO", nullable=true, length=11)
	private Date inicio;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="DATA_FIM", nullable=true, length=11)
	private Date fim;
	
	@Column(name="PERIODICIDADE", nullable=true, length=255)
	private String periodicidadeCobranca;
	
	@Column(name="DIA_VENCIMENTO", nullable=true, length=255)
	private Long diaVencimento;
	
	@Column(name="VALOR", nullable=true, length=10, precision = 5)
	private Double valor;
	
	@Column(name="ID_CLIENT", nullable=false, length=10)
	private String idClient;
	
	public void update(PlanoEntity plano) {
		this.nome = plano.getNome();
		this.status = plano.getStatus();
		this.inicio = plano.getInicio();
		this.fim = plano.getFim();
		this.periodicidadeCobranca = plano.getPeriodicidadeCobranca();
		this.diaVencimento = plano.getDiaVencimento();
		this.valor = plano.getValor();
	}

	public Long getId() {
		return id;
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

	public Date getInicio() {
		return inicio;
	}

	public void setInicio(Date inicio) {
		this.inicio = inicio;
	}

	public Date getFim() {
		return fim;
	}

	public void setFim(Date fim) {
		this.fim = fim;
	}

	public String getPeriodicidadeCobranca() {
		return periodicidadeCobranca;
	}

	public void setPeriodicidadeCobranca(String periodicidadeCobranca) {
		this.periodicidadeCobranca = periodicidadeCobranca;
	}

	public Long getDiaVencimento() {
		return diaVencimento;
	}

	public void setDiaVencimento(Long diaVencimento) {
		this.diaVencimento = diaVencimento;
	}

	public Double getValor() {
		return valor;
	}

	public void setValor(Double valor) {
		this.valor = valor;
	}

	public String getIdClient() {
		return idClient;
	}

	public void setIdClient(String idClient) {
		this.idClient = idClient;
	}

	public void setId(Long id) {
		this.id = id;
	}
	
}
