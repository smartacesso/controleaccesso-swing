package com.protreino.services.entity;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.hibernate.annotations.Fetch;
import org.hibernate.annotations.FetchMode;
import org.hibernate.annotations.Type;

import com.protreino.services.enumeration.TipoEscala;
import com.protreino.services.enumeration.TipoPedestre;
import com.protreino.services.enumeration.TipoRegra;
import com.protreino.services.to.RegraTO;

@Entity
@Table(name="TB_REGRA")
@NamedQueries({
	@NamedQuery(name  = "RegraEntity.findAll", 
				query = "select obj "
				      + "from RegraEntity obj "
					  + "order by obj.id asc"),
	@NamedQuery(name  = "RegraEntity.findById", 
				query = "select obj from RegraEntity obj "
					  + "where obj.id = :ID order by obj.id asc"),
	@NamedQuery(name  = "RegraEntity.findAllByTipoPedestre",
				query = "select obj from RegraEntity obj "
					  + "where (obj.tipoPedestre = :TIPO_PEDESTRE "
					  + "			or obj.tipoPedestre = 'AMBOS') "
					  + "and obj.status = 'ATIVO' "
					  + "and (obj.removed = false or obj.removed is null) "
					  + "order by obj.id asc"),
	@NamedQuery(name  = "RegraEntity.findByNome",
				query = "select obj from RegraEntity obj "
					  + "where obj.nome = :NOME_REGRA "
					  + "and (obj.cadastradoNoDesktop = :CADASTRADO_NO_DESKTOP "
					  + "		or obj.cadastradoNoDesktop is null) "
					  + "order by obj.id asc"),
	@NamedQuery(name  = "RegraEntity.findAllComHorario", 
				query = "select distinct obj from RegraEntity obj "
				      + "join fetch obj.horarios "
				      + "where (obj.removed is null or obj.removed = false) "
					  + "order by obj.id asc"),
	@NamedQuery(name  = "RegraEntity.findAllComPlanoETemplate", 
				query = "select obj from RegraEntity obj "
				      + "where (obj.removed is null or obj.removed = false) "
				      + "and obj.idPlano is not null "
				      + "and obj.idTemplate is not null  "
					  + "order by obj.id asc")
})
@SuppressWarnings("serial")
public class RegraEntity extends BaseEntity implements ObjectWithId {

	@Id
	@Column(name="ID_REGRA", nullable=false, length=4)
	private Long id;
	
	@Column(name="NOME", nullable=true, length=255)
	private String nome;
	
	@Temporal(TemporalType.DATE)
	@Column(name="VALIDADE", nullable=true, length=11)
	private Date validade;
	
	@ManyToOne(cascade={}, fetch=FetchType.LAZY)
	@JoinColumn(name="ID_EMPRESA", nullable=true)
	private EmpresaEntity empresa;

	@Enumerated(EnumType.STRING)
	@Column(name="TIPO", nullable=true, length=100)
	private TipoRegra tipo;
	
	@Enumerated(EnumType.STRING)
	@Column(name="TIPO_PEDESTRE", nullable=true, length=100)
	private TipoPedestre tipoPedestre;
	
	@Column(name="DESCRICAO", nullable=true, length=255)
	private String descricao;
	
	@Temporal(TemporalType.DATE)
	@Column(name="DATA_INICIO_PERIODO", nullable=true, length=11)
	private Date dataInicioPeriodo;
	
	@Temporal(TemporalType.DATE)
	@Column(name="DATA_FIM_PERIODO", nullable=true, length=11)
	private Date dataFimPeriodo;
	
	@Enumerated(EnumType.STRING)
	@Column(name="TIPO_ESCALA", nullable=true, length=100)
	private TipoEscala tipoEscala;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="HORARIO_INICIO_TURNO", nullable=true, length=11)
	private Date horarioInicioTurno;
	
	@Column(name="QTDE_CREDITOS", nullable=true, length=10)
	private Long qtdeDeCreditos;
	
	@Column(name="DIAS_VALIDADE_CREDITO", nullable=true, length=10)
	private Long diasValidadeCredito;
	
	@Column(name="ID_CLIENT", nullable=false, length=10)
	private String idClient;
	
	@Column(name="ID_TEMPLATE", nullable=true, length=10)
	private Integer idTemplate;
	
	@Column(name="ID_PLANO", nullable=true, length=10)
	private Integer idPlano;
	
	@Type(type="org.hibernate.type.NumericBooleanType")
	@Column(name="CADASTRADO_NO_DESKTOP", nullable=true, length=30)
	private Boolean cadastradoNoDesktop;
	
	@Column(name="STATUS", nullable=true, length=20)
	private String status;
	
	@Type(type="org.hibernate.type.NumericBooleanType")
	@Column(name="REMOVED", nullable=true, length=30)
	private Boolean removed;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="DATA_REMOVIDO", nullable=true, length=11)
	private Date dataRemovido = null;
	
	@OneToMany(cascade=CascadeType.ALL, fetch=FetchType.EAGER, 
			   orphanRemoval=true, targetEntity=HorarioEntity.class,
			   mappedBy="regra")
	@Fetch(FetchMode.SUBSELECT)
	private List<HorarioEntity> horarios;
	
	public RegraEntity() {
	}
	
	public RegraEntity(RegraTO regraTO) {
		this.id = regraTO.getId();
		this.nome = regraTO.getNome();
		this.validade = regraTO.getValidade();
		this.tipo = regraTO.getTipo();
		this.tipoPedestre = regraTO.getTipoPedestre();
		this.descricao = regraTO.getDescricao();
		this.dataInicioPeriodo = regraTO.getDataInicioPeriodo();
		this.dataFimPeriodo = regraTO.getDataFimPeriodo();
		this.tipoEscala = regraTO.getTipoEscala();
		this.horarioInicioTurno = regraTO.getHorarioInicioTurno();
		this.qtdeDeCreditos = regraTO.getQtdeDeCreditos();
		this.diasValidadeCredito = regraTO.getDiasValidadeCredito();
		this.status = regraTO.getStatus();
		this.removed = regraTO.getRemoved();
		this.dataRemovido = regraTO.getDataRemovido();
		this.idPlano = regraTO.getIdPlano();
		this.idTemplate = regraTO.getIdTemplate();
		
		if(regraTO.getHorarios() != null && !regraTO.getHorarios().isEmpty()) {
			this.horarios = new ArrayList<>();
			this.horarios.addAll(regraTO.getHorarios());
			
			horarios.forEach(horario -> {
				horario.setRegra(this);
			});
		}
	}
	
	public void update(RegraEntity regra) {
		this.nome = regra.getNome();
		this.validade = regra.getValidade();
		this.tipo = regra.getTipo();
		this.tipoPedestre = regra.getTipoPedestre();
		this.descricao = regra.getDescricao();
		this.dataInicioPeriodo = regra.getDataInicioPeriodo();
		this.dataFimPeriodo = regra.getDataFimPeriodo();
		this.tipoEscala = regra.getTipoEscala();
		this.horarioInicioTurno = regra.getHorarioInicioTurno();
		this.qtdeDeCreditos = regra.getQtdeDeCreditos();
		this.diasValidadeCredito = regra.getDiasValidadeCredito();
		this.status = regra.getStatus();
		this.removed = regra.getRemoved();
		this.dataRemovido = regra.getDataRemovido();
		this.idPlano = regra.getIdPlano();
		this.idTemplate = regra.getIdTemplate();
		
		if(regra.getHorarios() != null && !regra.getHorarios().isEmpty()) {
			if(this.horarios == null) {
				this.horarios = new ArrayList<>();
			}
			
			atualizaHorarios(regra.getHorarios());
		}
		
		this.setDataAlteracao(new Date());
	}

	private void atualizaHorarios(List<HorarioEntity> novosHorarios) {
		for(HorarioEntity newHorario : novosHorarios) {
			boolean horarioExiste = false;
			
			for(HorarioEntity oldHorario : this.getHorarios()) {
				if(!oldHorario.getId().equals(newHorario.getId()))
					continue;
				
				oldHorario.setNome(newHorario.getNome());
				oldHorario.setDiasSemana(newHorario.getDiasSemana());
				oldHorario.setHorarioInicio(newHorario.getHorarioInicio());
				oldHorario.setHorarioFim(newHorario.getHorarioFim());
				oldHorario.setStatus(newHorario.getStatus());
				oldHorario.setRemoved(newHorario.getRemoved());
				oldHorario.setDataRemovido(newHorario.getDataRemovido());
				
				horarioExiste = true;
				break;
			}
			
			if(!horarioExiste) {
				newHorario.setRegra(this);
				this.getHorarios().add(newHorario);
			}
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

	public Date getValidade() {
		return validade;
	}

	public void setValidade(Date validade) {
		this.validade = validade;
	}

	public EmpresaEntity getEmpresa() {
		return empresa;
	}

	public void setEmpresa(EmpresaEntity empresa) {
		this.empresa = empresa;
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

	public String getIdClient() {
		return idClient;
	}

	public void setIdClient(String idClient) {
		this.idClient = idClient;
	}

	public List<HorarioEntity> getHorarios() {
		return horarios;
	}

	public void setHorarios(List<HorarioEntity> horarios) {
		this.horarios = horarios;
	}

	public Boolean getCadastradoNoDesktop() {
		return cadastradoNoDesktop;
	}

	public void setCadastradoNoDesktop(Boolean cadastradoNoDesktop) {
		this.cadastradoNoDesktop = cadastradoNoDesktop;
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
