package com.protreino.services.entity;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
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

import com.protreino.services.to.HorarioTO;
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
					  + "where obj.id = :ID order by obj.id asc"),
	@NamedQuery(name  = "PedestreRegraEntity.findAllByIdPedestre", 
				query = "select obj from PedestreRegraEntity obj "
					  + "where obj.pedestrianAccess.id = :ID_PEDESTRE "
					  + "and obj.removidoNoDesktop = 0 " 
					  + "order by obj.id asc")
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
	
	@Temporal(TemporalType.DATE)
	@Column(name="DATA_INICIO_ESCALA_3_3", nullable=true, length=11)
	private Date dataInicioEscala3_3;
	
	@OneToMany(cascade=CascadeType.ALL, orphanRemoval=true, fetch=FetchType.EAGER,
			 targetEntity=HorarioEntity.class, mappedBy="pedestreRegra")
	@Fetch(FetchMode.SUBSELECT)
	private List<HorarioEntity> horarios;
	
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
		this.dataInicioEscala3_3 = pr.getDataInicioEscala3_3();
		adicionaHorarios(pr);
	}
	
	private void adicionaHorarios(final PedestreRegraTO pr) {
		if(Objects.isNull(pr.getHorarios()) || pr.getHorarios().isEmpty()) {
			if(Objects.nonNull(this.horarios)) {
				horarios.clear();
			}

			return;
		}
		
		if(horarios == null || horarios.isEmpty()) {
			horarios = new ArrayList<HorarioEntity>();
			
			for(HorarioTO horarioTO : pr.getHorarios()) {
				horarios.add(horarioTO.toEntity(this));
			}
		
		} else {
			List<HorarioEntity> naoEcontrados = new ArrayList<>();
			
			for(HorarioEntity horarioExistente : this.horarios) {
				Optional<HorarioTO> first = pr.getHorarios().stream()
					.filter(horarioTO -> horarioTO.getId().equals(horarioExistente.getId()))
					.findFirst();
				
				if(!first.isPresent()) {
					naoEcontrados.add(horarioExistente);
				}
			}

			this.horarios.removeAll(naoEcontrados);
			
			for(HorarioTO newHorario : pr.getHorarios()) {
				boolean horarioJaExiste = false;
				
				for(HorarioEntity horarioExistente : this.horarios) {
					if(horarioExistente.getId().equals(newHorario.getId())) {
						horarioExistente.setQtdeDeCreditos(newHorario.getQtdeDeCreditos());
						horarioExistente.setDiasSemana(newHorario.getDiasSemana());
						
						horarioJaExiste = true;
						break;
					}
				}
				
				if(!horarioJaExiste) {
					this.horarios.add(newHorario.toEntity(this));
				}
			}
		}
		
	}
	
	public boolean isRegraComHorarios() {
		return Objects.nonNull(horarios) && !horarios.isEmpty();
	}
	
	public boolean temCreditos() {
		return Objects.nonNull(qtdeDeCreditos) && qtdeDeCreditos > 0;
	}
	
	public boolean temRegraDeHorariosComCredito() {
		if(Objects.isNull(horarios) || horarios.isEmpty()) {
			return false;
		}
		
		return horarios.stream()
				.anyMatch(horario -> Objects.nonNull(horario.getQtdeDeCreditos()));
	}
	
	public void decrementaCreditoFromHorario(final Date date) {
		if(Objects.isNull(horarios) || horarios.isEmpty()) {
			return;
		}
		
		horarios.stream()
			.filter(horario -> horario.isDiaPermitido(date) && horario.isDentroDoHorarioPermitido(date) && Objects.nonNull(horario.getQtdeDeCreditos()))
			.forEach(horario -> horario.decrementaCreditos());
	}
	
	public boolean isNaoRemovidoNoDesktop() {
		return Objects.isNull(removidoNoDesktop)
				|| Boolean.FALSE.equals(removidoNoDesktop);
	}
	
	public void decrementaCreditos() {
		if(Objects.isNull(qtdeDeCreditos)) {
			return;
		}
		
		setQtdeDeCreditos(getQtdeDeCreditos() - 1);
	}
	
	public boolean isUltimoCredito() {
		return Long.valueOf(1).equals(qtdeDeCreditos);
	}
	
	public boolean isPeriodoValido() {
		if (Objects.isNull(dataInicioPeriodo) || Objects.isNull(dataFimPeriodo)) {
			return false;
		}

		Date dataAtual = new Date();
		return dataInicioPeriodo.compareTo(dataAtual) >= 0 && dataFimPeriodo.compareTo(dataAtual) <= 0
				&& Objects.nonNull(validade) ? validade.compareTo(dataAtual) >= 0 : true;
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

	public Date getDataInicioEscala3_3() {
		return dataInicioEscala3_3;
	}

	public void setDataInicioEscala3_3(Date dataInicioEscala3_3) {
		this.dataInicioEscala3_3 = dataInicioEscala3_3;
	}

	public List<HorarioEntity> getHorarios() {
		return horarios;
	}

	public void setHorarios(List<HorarioEntity> horarios) {
		this.horarios = horarios;
	}
}
