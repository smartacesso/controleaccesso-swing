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
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;

import org.hibernate.annotations.Type;

import com.protreino.services.devices.Device;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.to.ConfigurationGroupTO;
import com.protreino.services.utils.HibernateUtil;

@SuppressWarnings("serial")
@Entity
@Table(name="TB_DEVICE")
@NamedQueries({
	@NamedQuery(name = "DeviceEntity.findAll", query = "select obj from DeviceEntity obj order by id asc"),
	@NamedQuery(name = "DeviceEntity.findById", query = "select obj from DeviceEntity obj where obj.id = :ID")
})
public class DeviceEntity extends BaseEntity implements ObjectWithId{
	
	@Id
	@GeneratedValue(strategy=GenerationType.IDENTITY)
	@Column(name="ID_DEVICE", nullable=false, length=10)
	private Long id;
	
	@Enumerated(EnumType.STRING)
	@Column(name="MANUFACTURER", nullable=false, length=100)
	private Manufacturer manufacturer;
	
	@Column(name="IDENTIFIER", nullable=false, length=100)
	private String identifier;
	
	@Column(name="NAME", nullable=true, length=100)
	private String name;
	
	@Column(name="LOGIN", nullable=true, length=100)
	private String login;
	
	@Column(name="PASSWORD", nullable=true, length=100)
	private String password;
	
	@Column(name="LOCATION", nullable=true, length=100)
	private String location;
	
	@Enumerated(EnumType.STRING)
	@Column(name="DESIRED_STATUS", nullable=false, length=100)
	private DeviceStatus desiredStatus;
	
	@Type(type = "org.hibernate.type.NumericBooleanType")
	@Column(name="DEFAULT_DEVICE", nullable=false, length=30)
	private Boolean defaultDevice = false;
	
	@Type(type = "org.hibernate.type.NumericBooleanType")
	@Column(name="MIRROR_DEVICE", nullable=false, length=30)
	private Boolean mirrorDevice = false;
	
	@Type(type = "org.hibernate.type.NumericBooleanType")
	@Column(name="SYNC_USERS", nullable=true, length=30)
	private Boolean syncUsers = false;
	
	@Column(name="ATHLETE_SCREEN_CONFIG", nullable=true, length=100)
	private String athleteScreenConfig; // ticketGateManufacturer%ticketGateIdentifier%openAthleteScreenOnInit%fullScreenAthleteScreen
	
	@OneToMany(cascade=CascadeType.ALL, fetch=FetchType.EAGER, 
			   orphanRemoval=true, mappedBy="deviceEntity")
	private List<ConfigurationGroupEntity> configurationGroups;
	
	@Column(name="ATTACEHD_DEVICES", nullable=true, length=1000)
	private String attachedDevices;
	
	@Temporal( TemporalType.TIMESTAMP)
	@Column(name="LAST_SYNC", nullable=true, length=11)
	private Date ultimaAtualizacao;
	
	@Transient
	private Device device;
	
	public DeviceEntity(){
	}
	
	public DeviceEntity(Device device){
		this.device = device;
		this.manufacturer = device.getManufacturer();
		this.identifier = device.getIdentifier();
		this.name = device.getName();
		this.location = device.getLocation();
		this.desiredStatus = device.getDesiredStatus();
		this.defaultDevice = device.isDefaultDevice();
		this.mirrorDevice = device.isMirrorDevice();
		this.syncUsers = device.isSyncUsers();
		this.athleteScreenConfig = device.getAthleteScreenConfig();
		if (device.getConfigurationGroups() != null){
			configurationGroups = new ArrayList<ConfigurationGroupEntity>();
			for (ConfigurationGroupTO configGroupTO : device.getConfigurationGroups())
				configurationGroups.add(new ConfigurationGroupEntity(this, configGroupTO));
		}
	}

	public Device recoverDevice(){
		Device recoveredDevice = manufacturer.recoverDevice(this);
		this.device = recoveredDevice;
		return recoveredDevice;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public Manufacturer getManufacturer() {
		return manufacturer;
	}

	public void setManufacturer(Manufacturer manufacturer) {
		this.manufacturer = manufacturer;
	}

	public String getIdentifier() {
		return identifier;
	}

	public void setIdentifier(String identifier) {
		this.identifier = identifier;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public DeviceStatus getDesiredStatus() {
		return desiredStatus;
	}

	public void setDesiredStatus(DeviceStatus desiredStatus) {
		this.desiredStatus = desiredStatus;
	}

	public List<ConfigurationGroupEntity> getConfigurationGroups() {
		return configurationGroups;
	}
	
	public List<ConfigurationGroupTO> getConfigurationGroupsTO() {
		List<ConfigurationGroupTO> configuracoes = new ArrayList<ConfigurationGroupTO>();
		if (configurationGroups == null)
			return null;
		
		for (ConfigurationGroupEntity groupEntity : configurationGroups)
			configuracoes.add(new ConfigurationGroupTO(groupEntity));
		
		return configuracoes;
	}

	public void setConfigurationGroupsEntityFromTO(List<ConfigurationGroupTO> configurationGroupsTO) {
		if (configurationGroups != null) {
			for (ConfigurationGroupEntity configurationGroupEntity : configurationGroups) {
				HibernateUtil.remove(configurationGroupEntity);
			}
			configurationGroups.clear();
		}
		
		if (configurationGroupsTO == null) {
			configurationGroups = null;
			return;
		}
		
		configurationGroups = new ArrayList<ConfigurationGroupEntity>();
		for (ConfigurationGroupTO configGroupTO : configurationGroupsTO)
			configurationGroups.add(new ConfigurationGroupEntity(this, configGroupTO));
	}
	
	public void setConfigurationGroups(List<ConfigurationGroupEntity> configurationGroups) {
		this.configurationGroups = configurationGroups;
	}

	public Device getDevice() {
		return device;
	}

	public void setDevice(Device device) {
		this.device = device;
	}

	public String getLogin() {
		return login;
	}

	public void setLogin(String login) {
		this.login = login;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public Boolean getDefaultDevice() {
		return defaultDevice;
	}

	public void setDefaultDevice(Boolean defaultDevice) {
		this.defaultDevice = defaultDevice;
	}

	public String getLocation() {
		return location;
	}

	public void setLocation(String location) {
		this.location = location;
	}

	public Boolean getMirrorDevice() {
		return mirrorDevice;
	}

	public void setMirrorDevice(Boolean mirrorDevice) {
		this.mirrorDevice = mirrorDevice;
	}

	public String getAthleteScreenConfig() {
		if (athleteScreenConfig == null)
			athleteScreenConfig = "NULL%null%false%false%false%";
		return athleteScreenConfig;
	}

	public void setAthleteScreenConfig(String athleteScreenConfig) {
		this.athleteScreenConfig = athleteScreenConfig;
	}

	public Boolean getSyncUsers() {
		return syncUsers;
	}

	public void setSyncUsers(Boolean syncUsers) {
		this.syncUsers = syncUsers;
	}

	public String getAttachedDevices() {
		return attachedDevices;
	}

	public void setAttachedDevices(String attachedDevices) {
		this.attachedDevices = attachedDevices;
	}

	public Date getUltimaAtualizacao() {
		return ultimaAtualizacao;
	}

	public void setUltimaAtualizacao(Date ultimaAtualizacao) {
		this.ultimaAtualizacao = ultimaAtualizacao;
	}

}
