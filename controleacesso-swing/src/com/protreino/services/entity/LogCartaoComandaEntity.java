package com.protreino.services.entity;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
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

import com.protreino.services.enumeration.StatusCard;

@Entity
@Table(name="TB_LOG_CARTAO_COMANDA")
@NamedQueries({
	@NamedQuery(name  = "LogCartaoComandaEntity.findAll", 
				query = "select obj "
				      + "from LogCartaoComandaEntity obj "
					  + "order by obj.id asc"),
	@NamedQuery(name  = "LogCartaoComandaEntity.findAllNaoRemovidosOrdered", 
				query = "select obj "
				      + "from LogCartaoComandaEntity obj "
					  + "where (obj.removido is null or obj.removido = false) "	
					  + "order by obj.id asc"),
	@NamedQuery(name  = "LogCartaoComandaEntity.findById", 
				query = "select obj from LogCartaoComandaEntity obj "
					  + "where obj.id = :ID"
					  + "	   and (obj.removido is null or obj.removido = false) "
					  + "order by obj.id asc"),
})
@SuppressWarnings("serial")
public class LogCartaoComandaEntity extends BaseEntity implements ObjectWithId {
	
	@Id
	@GeneratedValue(strategy=GenerationType.IDENTITY)
	@Column(name="ID_CARTAO_COMANDA", nullable=false, length=4)
	private Long id;
	
	@Column(name="NUMERO_REAL", nullable=true, length=255)
	private String numeroReal;
	
	@Column(name="NUMERO_ALTERNATIVO", nullable=true, length=255)
	private String numeroAlternativo;
	
	@Column(name="ORIGEM", nullable=true, length=255)
	private String origem;
	
	@Column(name="TIPO_LIBERACAO", nullable=true, length=255)
	private String tipoLiberacao;
	
	@ManyToOne(cascade={}, fetch=FetchType.LAZY)
	@JoinColumn(name="ID_USUARIO", nullable=true)
	private UserEntity usuario;
	
	@Temporal( TemporalType.TIMESTAMP)
	@Column(name="DATA_ACAO", nullable=false, length=11)
	private Date data;
	
	@Type(type="org.hibernate.type.NumericBooleanType")
	@Column(name="REMOVIDO", nullable=true)
	private Boolean removido = false;

	
	public LogCartaoComandaEntity() {
	}
	
	public LogCartaoComandaEntity(CartaoComandaEntity cartao) {
		this.numeroReal = cartao.getNumeroReal();
		this.numeroAlternativo = cartao.getNumeroAlternativo();
	}

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

	public String getOrigem() {
		return origem;
	}

	public void setOrigem(String origem) {
		this.origem = origem;
	}

	public String getTipoLiberacao() {
		return tipoLiberacao;
	}

	public void setTipoLiberacao(String tipoLiberacao) {
		this.tipoLiberacao = tipoLiberacao;
	}

	public UserEntity getUsuario() {
		return usuario;
	}

	public void setUsuario(UserEntity usuario) {
		this.usuario = usuario;
	}

	public Date getData() {
		return data;
	}

	public void setData(Date data) {
		this.data = data;
	}

	public Boolean getRemovido() {
		return removido;
	}

	public void setRemovido(Boolean removido) {
		this.removido = removido;
	}
	
	

}
