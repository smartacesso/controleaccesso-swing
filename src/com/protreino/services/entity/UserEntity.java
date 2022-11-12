package com.protreino.services.entity;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Lob;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;

import org.hibernate.annotations.Type;

@SuppressWarnings("serial")
@Entity
@Table(name="TB_USER")
@NamedQueries({
	@NamedQuery(name = "UserEntity.findAll", query = "select obj from UserEntity obj"),
	@NamedQuery(name = "UserEntity.findByLoginName",
				query = "select obj from UserEntity obj "
					  + "where obj.loginName = :LOGIN_NAME "
					  + "and obj.password = :PASSWORD "
					  + "and obj.status = 'ATIVO' "
					  + "and (obj.removed = false or obj.removed is null) "),
	@NamedQuery(name = "UserEntity.findById", query = "select obj from UserEntity obj where obj.id = :ID")
})
public class UserEntity extends BaseEntity implements ObjectWithId, Serializable {
	
	@Id
	@Column(name="ID_USER", nullable=false, length=10)
	private Long id;
	
	@Column(name="EMAIL", nullable=true, length=100)
	private String email;
	
	@Column(name="LOGIN_NAME", nullable=false, length=100)
	private String loginName;
	
	@Column(name="UNIT_NAME", nullable=true, length=100)
	private String unitName;
	
	@Column(name="PASSWORD", nullable=true, length=100)
	private String password;
	
	@Column(name="STATUS", nullable=false, length=30)
	private String status;
	
	@Temporal( TemporalType.TIMESTAMP)
	@Column(name="CREATION_DATE", nullable=false, length=11)
	private Date creationDate;
	
	@Column(name="ID_CLIENT", nullable=false, length=10)
	private String idClient;
	
	@Column(name="NAME", nullable=false, length=100)
	private String name;
	
	@Temporal( TemporalType.TIMESTAMP)
	@Column(name="LAST_SYNC", nullable=true, length=11)
	private Date lastSync;
	
	@Temporal( TemporalType.TIMESTAMP)
	@Column(name="LAST_SYNC_LOG", nullable=true, length=11)
	private Date lastSyncLog;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="LAST_SYNC_USER", nullable=true, length=11)
	private Date lastSyncUser;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="LAST_SYNC_EMPRESA", nullable=true, length=11)
	private Date lastSyncEmpresa;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="LAST_SYNC_REGRA", nullable=true, length=11)
	private Date lastSyncRegra;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="LAST_SYNC_PARAMETRO", nullable=true, length=11)
	private Date lastSyncParametro;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="LAST_SYNC_PLANO", nullable=true, length=11)
	private Date lastSyncPlano;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="LAST_SYNC_UPLOAD_PHOTOS", nullable=true, length=11)
	private Date lastSyncUploadPhotos;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="DATE_NEW_ACCESS", nullable=true, length=11)
	private Date dateNewAccess;
	
	@Type(type = "org.hibernate.type.NumericBooleanType")
	@Column(name="USE_BIOMETRY", nullable=true, length=30)
	private Boolean useBiometry = false;
	
	@Type(type="text")
	@Column(name="BACKUP_PREFERENCES", nullable=true)
	private String backupPreferences;
	
	@Lob
	@Column(name="BACKUP_DEVICES", nullable=true)
	private byte [] backupDevicesBinaries;
	
	@Transient
	private String backupDevices;
	
	@Type(type = "org.hibernate.type.NumericBooleanType")
	@Column(name="BACKUP_CHANGED", nullable=true, length=30)
	private Boolean backupChanged = false; // flag indicando que o backup foi precisa ser enviado. Ver metodo Utils.sendBackupToServer()
	
	@Column(name="QTD_DIGITOS_CARTAO", nullable=true)
	private Integer qtdePadraoDigitosCartao;
	
	@Column(name="CHAVE_INTEGRACAO_COMTELE", nullable=true)
	private String chaveIntegracaoComtele;
	
	@Type(type="org.hibernate.type.NumericBooleanType")
	@Column(name="REMOVED", nullable=true, length=30)
	private Boolean removed;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="DATA_REMOVIDO", nullable=true, length=11)
	private Date dataRemovido = null;
	
	public UserEntity() {}

	public UserEntity(Long id, String loginName, String status, Date creationDate, String idClient, String name,
			Date lastSync) {
		this.id = id;
		this.loginName = loginName;
		this.status = status;
		this.creationDate = creationDate;
		this.idClient = idClient;
		this.name = name;
		this.lastSync = lastSync;
	}
	
	public UserEntity(Long id, String loginName, String status, Date creationDate, String idClient, String name,
			Date lastSync, Integer qtdePadraoDigitosCartao, String unidade, String chaveIntegracaoComtele) {
		this.id = id;
		this.loginName = loginName;
		this.status = status;
		this.creationDate = creationDate;
		this.idClient = idClient;
		this.name = name;
		this.lastSync = lastSync;
		this.qtdePadraoDigitosCartao = qtdePadraoDigitosCartao;
		this.unitName = unidade;
		this.chaveIntegracaoComtele = chaveIntegracaoComtele;
	}
	
	public UserEntity(Long id, String loginName, String password, String status, Date creationDate, String idClient, String name,
			Date lastSync, Integer qtdePadraoDigitosCartao, String unidade, String chaveIntegracaoComtele) {
		this.id = id;
		this.loginName = loginName;
		this.password = password;
		this.status = status;
		this.creationDate = creationDate;
		this.idClient = idClient;
		this.name = name;
		this.lastSync = lastSync;
		this.qtdePadraoDigitosCartao = qtdePadraoDigitosCartao;
		this.unitName = unidade;
		this.chaveIntegracaoComtele = chaveIntegracaoComtele;
	}
	
	public void update(UserEntity user) {
		this.setEmail(user.getEmail() != null ? user.getEmail() : null);
		this.setLoginName(user.getLoginName() != null ? user.getLoginName() : "");
		this.setPassword(user.getPassword() != null ? user.getPassword() : null);
		this.setStatus(user.getStatus() != null ? user.getStatus() : "INATIVO");
		this.setName(user.getName() != null ? user.getName() : "");
		this.setIdClient(user.getIdClient());
		this.setRemoved(user.getRemoved());
		this.setDataRemovido(user.getDataRemovido());
		this.setDataAlteracao(new Date());
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public Date getCreationDate() {
		return creationDate;
	}

	public void setCreationDate(Date creationDate) {
		this.creationDate = creationDate;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Date getLastSync() {
		return lastSync;
	}

	public void setLastSync(Date lastSync) {
		this.lastSync = lastSync;
	}

	public Date getLastSyncLog() {
		return lastSyncLog;
	}

	public void setLastSyncLog(Date lastSyncLog) {
		this.lastSyncLog = lastSyncLog;
	}

	public Boolean getUseBiometry() {
		return useBiometry;
	}

	public void setUseBiometry(Boolean useBiometry) {
		this.useBiometry = useBiometry;
	}

	public String getBackupPreferences() {
		return backupPreferences;
	}

	public void setBackupPreferences(String backupPreferences) {
		this.backupPreferences = backupPreferences;
	}

	public String getBackupDevices() {
		if(backupDevicesBinaries != null)
			backupDevices = new String(backupDevicesBinaries);
		return backupDevices;
	}

	public void setBackupDevices(String backupDevices) {
		this.backupDevicesBinaries = backupDevices != null ? backupDevices.getBytes() : null;
		this.backupDevices = backupDevices;
	}

	public Boolean getBackupChanged() {
		return backupChanged;
	}

	public void setBackupChanged(Boolean backupChanged) {
		this.backupChanged = backupChanged;
	}

	public String getIdClient() {
		return idClient;
	}

	public void setIdClient(String idClient) {
		this.idClient = idClient;
	}

	public String getLoginName() {
		return loginName;
	}

	public void setLoginName(String loginName) {
		this.loginName = loginName;
	}

	public Integer getQtdePadraoDigitosCartao() {
		return qtdePadraoDigitosCartao;
	}

	public void setQtdePadraoDigitosCartao(Integer qtdePadraoDigitosCartao) {
		this.qtdePadraoDigitosCartao = qtdePadraoDigitosCartao;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getUnitName() {
		return unitName;
	}

	public void setUnitName(String unitName) {
		this.unitName = unitName;
	}

	public Date getLastSyncUser() {
		return lastSyncUser;
	}

	public void setLastSyncUser(Date lastSyncUser) {
		this.lastSyncUser = lastSyncUser;
	}

	public Date getLastSyncUploadPhotos() {
		return lastSyncUploadPhotos;
	}

	public void setLastSyncUploadPhotos(Date lastSyncUploadPhotos) {
		this.lastSyncUploadPhotos = lastSyncUploadPhotos;
	}

	public String getChaveIntegracaoComtele() {
		return chaveIntegracaoComtele;
	}

	public void setChaveIntegracaoComtele(String chaveIntegracaoComtele) {
		this.chaveIntegracaoComtele = chaveIntegracaoComtele;
	}

	public Date getLastSyncEmpresa() {
		return lastSyncEmpresa;
	}

	public void setLastSyncEmpresa(Date lastSyncEmpresa) {
		this.lastSyncEmpresa = lastSyncEmpresa;
	}

	public Date getLastSyncRegra() {
		return lastSyncRegra;
	}

	public void setLastSyncRegra(Date lastSyncRegra) {
		this.lastSyncRegra = lastSyncRegra;
	}

	public Date getLastSyncParametro() {
		return lastSyncParametro;
	}

	public void setLastSyncParametro(Date lastSyncParametro) {
		this.lastSyncParametro = lastSyncParametro;
	}

	public Date getLastSyncPlano() {
		return lastSyncPlano;
	}

	public void setLastSyncPlano(Date lastSyncPlano) {
		this.lastSyncPlano = lastSyncPlano;
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

	public Date getDateNewAccess() {
		if(dateNewAccess == null)
			return getDataCriacao();
		return dateNewAccess;
	}

	public void setDateNewAccess(Date dateNewAccess) {
		this.dateNewAccess = dateNewAccess;
	}

	public byte[] getBackupDevicesBinaries() {
		return backupDevicesBinaries;
	}

	public void setBackupDevicesBinaries(byte[] backupDevicesBinaries) {
		this.backupDevicesBinaries = backupDevicesBinaries;
	}

}
