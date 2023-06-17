package com.protreino.services.entity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

@Entity
@Table(name="TB_PARAMETRO")
@NamedQueries({
	@NamedQuery(name  = "ParametroEntity.findAll", 
				query = "select obj "
				      + "from ParametroEntity obj "
					  + "order by obj.id asc"),
	@NamedQuery(name  = "ParametroEntity.findById", 
				query = "select obj from ParametroEntity obj "
					  + "where obj.id = :ID order by obj.id asc"),
	@NamedQuery(name  = "ParametroEntity.findByName",
				query = "select obj from ParametroEntity obj "
					  + "where obj.nome = :NOME_PARAM "
					  + "and obj.idClient = :ID_CLIENTE "
					  + "order by obj.id asc")
})
@SuppressWarnings("serial")
public class ParametroEntity extends BaseEntity implements ObjectWithId {

	@Id
	@Column(name="ID_PARAMETRO", nullable=false, length=4)
	private Long id;
	
	@Column(name="NOME", nullable=true, length=255)
	private String nome;
	
	@Column(name="VALOR", nullable=true, length=255)
	private String valor;
	
	@Column(name="ID_CLIENT", nullable=false, length=10)
	private String idClient;
	
	public void update(ParametroEntity parametro) {
		this.nome = parametro.getNome();
		this.valor = parametro.getValor();
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

	public String getValor() {
		return valor;
	}

	public void setValor(String valor) {
		this.valor = valor;
	}

	public String getIdClient() {
		return idClient;
	}

	public void setIdClient(String idClient) {
		this.idClient = idClient;
	}
	
}
