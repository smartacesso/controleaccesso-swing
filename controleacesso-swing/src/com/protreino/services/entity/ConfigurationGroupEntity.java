package com.protreino.services.entity;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
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
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.protreino.services.to.ConfigurationGroupTO;
import com.protreino.services.to.ConfigurationTO;

@SuppressWarnings("serial")
@Entity
@Table(name="TB_CONFIGURATION_GROUP")
@NamedQueries({
	@NamedQuery(name = "ConfigurationGroupEntity.findAll", query = "select obj from ConfigurationGroupEntity obj order by id asc"),
	@NamedQuery(name = "ConfigurationGroupEntity.findById", query = "select obj from ConfigurationGroupEntity obj where obj.id = :ID")
})
public class ConfigurationGroupEntity extends BaseEntity implements ObjectWithId {
	
	@Id
	@GeneratedValue(strategy=GenerationType.IDENTITY)
	@Column(name="ID_CONFIGURATION_GROUP", nullable=false, length=10)
	private Long id;

	@ManyToOne(cascade={}, fetch=FetchType.LAZY)
	@JoinColumn(name="ID_DEVICE", nullable=true)
	private DeviceEntity deviceEntity;
	
	@Column(name="NAME", nullable=false, length=100)
	private String name;
	
	@OneToMany(cascade=CascadeType.ALL, fetch=FetchType.EAGER, 
			   orphanRemoval=true, targetEntity=ConfigurationEntity.class,
			   mappedBy="group")
	private List<ConfigurationEntity> configurations;
	
	public ConfigurationGroupEntity(){
	}
	
	public ConfigurationGroupEntity(DeviceEntity deviceEntity, ConfigurationGroupTO configGroupTO) {
		this.deviceEntity = deviceEntity;
		this.name = configGroupTO.getName();
		this.configurations = new ArrayList<ConfigurationEntity>();
		if (configGroupTO.getConfigurations() != null){
			for (ConfigurationTO configTO : configGroupTO.getConfigurations())
				this.configurations.add(new ConfigurationEntity(this, configTO));
		}
	}
	
	public ConfigurationGroupEntity(DeviceEntity deviceEntity, String name,
			List<ConfigurationEntity> configurations) {
		this.deviceEntity = deviceEntity;
		this.name = name;
		this.configurations = configurations;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public DeviceEntity getDeviceEntity() {
		return deviceEntity;
	}

	public void setDeviceEntity(DeviceEntity deviceEntity) {
		this.deviceEntity = deviceEntity;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public List<ConfigurationEntity> getConfigurations() {
		return configurations;
	}

	public void setConfigurations(List<ConfigurationEntity> configurations) {
		this.configurations = configurations;
	}
	
	
	
}
