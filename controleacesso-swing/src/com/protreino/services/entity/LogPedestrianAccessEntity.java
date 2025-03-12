                                                                                                                                                                                                                                                                                                                         package com.protreino.services.entity;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
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

import com.protreino.services.constants.Tipo;

@Entity
@Table(name="TB_LOG_PEDESTRIAN_ACCESS")
@NamedQueries({
	@NamedQuery(name  = "LogPedestrianAccessEntity.findAll", query = "select obj from LogPedestrianAccessEntity obj"),
	@NamedQuery(name  = "LogPedestrianAccessEntity.findByDate", 
				query = "select obj from LogPedestrianAccessEntity obj "
					  + " where obj.accessDate > :LAST_SYNC"
					  + " and (obj.onlyLocal is null or obj.onlyLocal = false)"),
	
	@NamedQuery(name  = "LogPedestrianAccessEntity.findByAccessDateCount", 
				query = "select count(obj) from LogPedestrianAccessEntity obj "
					  + " where obj.accessDate between :LAST_SYNC and :NEW_LAST_SYNC "
					  + " and (obj.offline is null or obj.offline = false) "
					  + " and (obj.onlyLocal is null or obj.onlyLocal = false)"),
	@NamedQuery(name  = "LogPedestrianAccessEntity.findByAccessDate", 
				query = "select obj from LogPedestrianAccessEntity obj "
					  + " where obj.accessDate between :LAST_SYNC and :NEW_LAST_SYNC "
					  + " and (obj.offline is null or obj.offline = false) "
					  + " and (obj.onlyLocal is null or obj.onlyLocal = false)"),
	
	@NamedQuery(name  = "LogPedestrianAccessEntity.findByCreateDateCount", 
				query = "select count(obj) from LogPedestrianAccessEntity obj "
					  + " where obj.dataCriacao between :LAST_SYNC and :NEW_LAST_SYNC "
					  + " and obj.offline = true "
					  + " and (obj.onlyLocal is null or obj.onlyLocal = false) "),
	@NamedQuery(name  = "LogPedestrianAccessEntity.findByCreateDate", 
				query = "select obj from LogPedestrianAccessEntity obj "
					  + " where obj.dataCriacao between :LAST_SYNC and :NEW_LAST_SYNC  "
					  + " and obj.offline = true "
					  + " and (obj.onlyLocal is null or obj.onlyLocal = false) "),
	
	@NamedQuery(name  = "LogPedestrianAccessEntity.findUnsubmittedLogsCount", 
				query = "select count(obj) from LogPedestrianAccessEntity obj "
					  + " where obj.failAtSync = true "),
	@NamedQuery(name  = "LogPedestrianAccessEntity.findUnsubmittedLogs", 
				query = "select obj from LogPedestrianAccessEntity obj "
					  + " where obj.failAtSync = true "),

	@NamedQuery(name  = "LogPedestrianAccessEntity.findLocalByDate", 
				query = "select obj "
					  + "from LogPedestrianAccessEntity obj, "
					  + "     PedestrianAccessEntity p "
					  + "where obj.accessDate > :LAST_SYNC "
					  + "      and p.id = obj.idPedestrian "
			          + "      and (p.cadastradoNoDesktop = null or p.cadastradoNoDesktop = false) "
			          + " 	   and (obj.onlyLocal is null or obj.onlyLocal = false) "),
	@NamedQuery(name  = "LogPedestrianAccessEntity.countByPeriod", 
				query = "select count(*) "
					  + "from LogPedestrianAccessEntity obj "
					  + "	left join obj.pedestre p "
					  + " where obj.accessDate >= :DATA_INICIO "
					  + " and obj.accessDate <= :DATA_FIM "
					  + "and (p.name like :NOME or :NOME = 'vazio') "
					  + "and (p.tipo like :TIPO or :TIPO = 'vazio') "),
	@NamedQuery(name  = "LogPedestrianAccessEntity.findByPeriod", 
				query = "select new com.protreino.services.entity.LogPedestrianAccessEntity(obj, p.name, p.id, p.cardNumber, p.tipo) "
					  + "from LogPedestrianAccessEntity obj "
					  + "	left join obj.pedestre p "
					  + " where obj.accessDate >= :DATA_INICIO "
					  + " and obj.accessDate <= :DATA_FIM "
					  + "and (p.name like :NOME or :NOME = 'vazio') "
					  + "and (p.tipo like :TIPO or :TIPO = 'vazio') "
					  + "order by obj.accessDate desc "),
	@NamedQuery(name  = "LogPedestrianAccessEntity.findAllByPedestre", 
				query = "select obj from LogPedestrianAccessEntity obj "
					  + " where obj.idPedestrian = :ID_PEDESTRE "
					  + " order by obj.id desc "),
	@NamedQuery(name  = "LogPedestrianAccessEntity.findByPedestre", 
				query = "select obj from LogPedestrianAccessEntity obj "
					  + " where obj.idPedestrian = :ID_PEDESTRE "
					  + " and obj.status = 'ATIVO' "
					  + " order by obj.id desc "),
	@NamedQuery(name  = "LogPedestrianAccessEntity.findByPedestreNaoIgnorado", 
				query = "select obj from LogPedestrianAccessEntity obj "
					  + " where obj.idPedestrian = :ID_PEDESTRE "
					  + " and obj.status = 'ATIVO' "
					  + " and ( obj.reason is null or obj.reason != 'Regras ignoradas' ) "
					  + " order by obj.id desc "),
	@NamedQuery(name  = "LogPedestrianAccessEntity.findByPedestreEntrada", 
				query = "select obj from LogPedestrianAccessEntity obj "
					  + " where obj.idPedestrian = :ID_PEDESTRE "
					  + " and obj.status = 'ATIVO' "
					  + " and obj.direction = 'ENTRADA' "
					  + " order by obj.id desc "),
	@NamedQuery(name  = "LogPedestrianAccessEntity.findTodayByAthlete", 
				query = "select obj from LogPedestrianAccessEntity obj "
					  + " where DATE(obj.accessDate) >= DATE(CURRENT_DATE) "
					  + " and obj.idPedestrian = :ID_ATLETA "
					  + " and obj.status = 'ATIVO'"),
	@NamedQuery(name  = "LogPedestrianAccessEntity.findWeekByAthlete", 
				query = "select obj from LogPedestrianAccessEntity obj "
					  + " where DATE(obj.accessDate) between :INI_WEEK and :END_WEEK  "
					  + " and obj.idPedestrian = :ID_ATLETA "
					  + " and obj.status = 'ATIVO'"),
	@NamedQuery(name  = "LogPedestrianAccessEntity.findByEquipamentDesc", 
				query = "select obj from LogPedestrianAccessEntity obj "
					  + " where obj.equipament = :EQUIPAMENTO "
					  //+ " and obj.status = 'INDEFINIDO'"
					  + " order by obj.id desc "),
	@NamedQuery(name  = "LogPedestrianAccessEntity.findByEquipamentSemDirectionDesc",
				query = "select obj from LogPedestrianAccessEntity obj "
					  + "where obj.equipament = :EQUIPAMENTO "
					  + "and obj.direction is null "
					  + "order by obj.id desc "),
	@NamedQuery(name  = "LogPedestrianAccessEntity.findByEquipamentSemDirectionAndSemCartaoRecebido",
				query = "select obj from LogPedestrianAccessEntity obj "
					  + "where obj.equipament = :EQUIPAMENTO "
					  + "and obj.direction is null "
					  + "and obj.cartaoAcessoRecebido is null "
					  + "and obj.idPedestrian is not null "
					  + "order by obj.id desc "),
	@NamedQuery(name  = "LogPedestrianAccessEntity.findByEquipamentSemDirectionAndComCartaoRecebido",
				query = "select obj from LogPedestrianAccessEntity obj "
					  + "where obj.equipament = :EQUIPAMENTO "
					  + "and obj.direction is null "
					  + "and obj.idPedestrian is not null "
					  + "and obj.cartaoAcessoRecebido = :NUMERO_CARTAO_RECEBIDO "
					  + "order by obj.id desc "),
	@NamedQuery(name  = "LogPedestrianAccessEntity.findByLastAccessbyIdPedestrian",
	query = "select obj from LogPedestrianAccessEntity obj "
		  + "where obj.idPedestrian = :ID_PEDESTRE "
		  + "and obj.direction is not null "
		  + "and obj.dataCriacao is not null "
		  + "order by obj.dataCriacao desc "),
	@NamedQuery(name  = "LogPedestrianAccessEntity.findByLastAccessbyIdPedestrianAndDate",
	query = "select obj from LogPedestrianAccessEntity obj "
		  + "where obj.idPedestrian = :ID_PEDESTRE "
		  + "and obj.direction is not null "
		  + "and obj.dataCriacao is not null "
		  + "and obj.dataCriacao >= :DATE "
		  + "order by obj.dataCriacao desc "),
	@NamedQuery(name  = "LogPedestrianAccessEntity.findByCurrentDate",
	query = "select obj from LogPedestrianAccessEntity obj "
		  + "where obj.dataCriacao is not null "
		  + "and  obj.dataCriacao between :CURRENT_DATE_INICIO and :CURRENT_DATE_FIM "
		  + "order by obj.dataCriacao desc ")
})
public class LogPedestrianAccessEntity implements ObjectWithId {
	
	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy=GenerationType.IDENTITY)
	@Column(name="ID_LOG_ATHLETE_ACCESS", nullable=false, length=4)
	private Long id;
	
	@Column(name="ID_LOGGED_USER", nullable=false, length=4)
	private Long idLoggedUser;
	
	@Column(name="ID_PEDESTRIAN", nullable=true, length=4)
	private Long idPedestrian; // idUser
	
	@ManyToOne(cascade={}, fetch=FetchType.LAZY)
	@JoinColumn(name="ID_PEDESTRIAN", nullable=true, insertable = false, updatable = false)
	private PedestrianAccessEntity pedestre;
	
	@Temporal( TemporalType.TIMESTAMP)
	@Column(name="ACCESS_DATE", nullable=false, length=11)
	private Date accessDate = new Date();
	
	@Column(name="STATUS", nullable=false, length=30)
	private String status;
	
	@Column(name="LOCATION", nullable=true, length=200)
	private String location;
	
	@Column(name="REASON", nullable=true, length=200)
	private String reason;
	
	@Column(name="DIRECTION", nullable=true, length=200)
	private String direction;
	
	@Column(name="EQUIPAMENT", nullable=true, length=200)
	private String equipament;
	
	@Type(type = "org.hibernate.type.NumericBooleanType")
	@Column(name="ONLY_LOCAL", nullable=false, length=30)
	private Boolean onlyLocal = false;
	
	@Column(name="BLOQUEAR_SAIDA", nullable=true, length=10)
	private Boolean bloquearSaida;
	
	@Column(name="CARTAO_ACESSO_RECEBIDO", nullable=true, length=100)
	private String cartaoAcessoRecebido;
	
	@Temporal( TemporalType.TIMESTAMP)
	@Column(name="DATA_CRIACAO", nullable=true, length=11)
	private Date dataCriacao;
	
	@Type(type = "org.hibernate.type.NumericBooleanType")
	@Column(name="OFF_LINE", nullable=true, length=30)
	private Boolean offline = false;
	
	@Type(type = "org.hibernate.type.NumericBooleanType")
	@Column(name="FAIL_AT_SYNC", nullable=true, length=30)
	private Boolean failAtSync = false;
	
	public LogPedestrianAccessEntity(){
	}
	
	public LogPedestrianAccessEntity(LogPedestrianAccessEntity obj, String nome, Long id, String cartao, String tipo) {
		this.idLoggedUser = obj.idLoggedUser;
		this.idPedestrian = obj.idPedestrian;
		this.status = obj.status;
		this.location = obj.location;
		this.reason = obj.reason;
		this.direction = obj.direction;
		this.equipament = obj.equipament;
		this.accessDate = obj.accessDate;
		
		if(this.pedestre == null)
			this.pedestre = new PedestrianAccessEntity();
		this.pedestre.setName(nome);
		this.pedestre.setId(id);
		this.pedestre.setCardNumber(cartao);
		this.pedestre.setTipo(tipo);
	}
		
	public LogPedestrianAccessEntity(Long idLoggedUser, Long idPedestrian, String status, String location, String reason) {
		super();
		this.idLoggedUser = idLoggedUser;
		this.idPedestrian = idPedestrian;
		this.status = status;
		this.location = location;
		this.reason = reason;
		this.status = "ATIVO";
	}
	
	public LogPedestrianAccessEntity(Long idLoggedUser, Long idPedestrian, String status, String location, String reason, String direction, String equipament) {
		super();
		this.idLoggedUser = idLoggedUser;
		this.idPedestrian = idPedestrian;
		this.status = status;
		this.location = location;
		this.reason = reason;
		this.direction = direction;
		this.equipament = equipament;
		this.status = "ATIVO";
	}
	
	public LogPedestrianAccessEntity(Long idLoggedUser, Long idPedestrian, String status, String location, String reason, String equipament) {
		super();
		this.idLoggedUser = idLoggedUser;
		this.idPedestrian = idPedestrian;
		this.status = status;
		this.location = location;
		this.reason = reason;
		this.equipament = equipament;
		this.status = "ATIVO";
	}
	
	
	public LogPedestrianAccessEntity(Long idLoggedUser, Long idPedestrian, Boolean offline, String location, String reason, 
			String direction, String equipament, String cartaoAcessoRecebido, Date accessDate) {
		super();
		this.idLoggedUser = idLoggedUser;
		this.idPedestrian = idPedestrian;
		this.offline = offline;
		this.location = location;
		this.reason = reason;
		this.direction = direction;
		this.equipament = equipament;
		this.status = "ATIVO";
		this.cartaoAcessoRecebido = cartaoAcessoRecebido;
		this.accessDate = accessDate;
		this.dataCriacao = new Date();
	}
	
	public boolean isEntrada() {
		return Tipo.ENTRADA.equalsIgnoreCase(direction);
	}
	
	public boolean isSaida() {
		return Tipo.SAIDA.equalsIgnoreCase(direction);
	}
	
	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}
	
	public Long getIdLoggedUser() {
		return idLoggedUser;
	}

	public void setIdLoggedUser(Long idLoggedUser) {
		this.idLoggedUser = idLoggedUser;
	}

	public Date getAccessDate() {
		return accessDate;
	}

	public void setAccessDate(Date accessDate) {
		this.accessDate = accessDate;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public String getLocation() {
		return location;
	}

	public void setLocation(String location) {
		this.location = location;
	}

	public String getReason() {
		return reason;
	}

	public void setReason(String reason) {
		this.reason = reason;
	}

	public Boolean getOnlyLocal() {
		return onlyLocal;
	}

	public void setOnlyLocal(Boolean onlyLocal) {
		this.onlyLocal = onlyLocal;
	}

	public Long getIdPedestrian() {
		return idPedestrian;
	}

	public void setIdPedestrian(Long idPedestrian) {
		this.idPedestrian = idPedestrian;
	}

	public String getDirection() {
		return direction;
	}

	public void setDirection(String direction) {
		this.direction = direction;
	}

	public String getEquipament() {
		return equipament;
	}

	public void setEquipament(String equipament) {
		this.equipament = equipament;
	}

	public Boolean getBloquearSaida() {
		return bloquearSaida;
	}

	public void setBloquearSaida(Boolean bloquearSaida) {
		this.bloquearSaida = bloquearSaida;
	}

	public PedestrianAccessEntity getPedestre() {
		return pedestre;
	}

	public void setPedestre(PedestrianAccessEntity pedestre) {
		this.pedestre = pedestre;
	}

	public String getCartaoAcessoRecebido() {
		return cartaoAcessoRecebido;
	}

	public void setCartaoAcessoRecebido(String cartaoAcessoRecebido) {
		this.cartaoAcessoRecebido = cartaoAcessoRecebido;
	}

	public Date getDataCriacao() {
		return dataCriacao;
	}

	public void setDataCriacao(Date dataCriacao) {
		this.dataCriacao = dataCriacao;
	}

	public Boolean getOffline() {
		return offline;
	}

	public void setOffline(Boolean offline) {
		this.offline = offline;
	}

	public Boolean getFailAtSync() {
		return failAtSync;
	}

	public void setFailAtSync(Boolean failAtSync) {
		this.failAtSync = failAtSync;
	}
	
	
	
}
