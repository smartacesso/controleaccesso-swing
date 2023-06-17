package com.protreino.services.to;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.protreino.services.entity.ConfigurationEntity;
import com.protreino.services.entity.ConfigurationGroupEntity;

public class ConfigurationGroupTO implements Serializable {

	private static final long serialVersionUID = 1L;
	
	private String name;
	private List<ConfigurationTO> configurations;
	
	public ConfigurationGroupTO(String name, List<ConfigurationTO> configurations) {
		this.name = name;
		this.configurations = configurations != null ? configurations : new ArrayList<ConfigurationTO>();
	}
	
	public ConfigurationGroupTO(ConfigurationGroupEntity configGroupEntity) {
		this.name = configGroupEntity.getName();
		this.configurations = new ArrayList<ConfigurationTO>();
		if (configGroupEntity.getConfigurations() != null){
			for (ConfigurationEntity configEntity : configGroupEntity.getConfigurations())
				this.configurations.add(new ConfigurationTO(configEntity));
		}
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public List<ConfigurationTO> getConfigurations() {
		return configurations;
	}

	public void setConfigurations(List<ConfigurationTO> configurations) {
		this.configurations = configurations;
	}
	
}
