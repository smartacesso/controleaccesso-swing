package com.protreino.services.to;

import java.io.Serializable;

import com.protreino.services.entity.ConfigurationEntity;
import com.protreino.services.enumeration.FieldType;

public class ConfigurationTO implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	private String name;
	private String id;
	private String value;
	private FieldType type;
	// uma string com os valores separados por ';' ou para listas numericas a sequencia 'inicio;passo;fim'
	// Ex: "Horário_clockwise;Antihorário_anticlockwise;Ambos_both" ou para listas numericas "3000;1000;10000"
	private String comboboxValues; 
	private Integer maxCharacteres;
	private Integer minCharacteres;
	private Boolean numeric = false;
	private Boolean required = true;
	
	private String nameAux;
	
	public ConfigurationTO(String name, FieldType type, Boolean required) {
		this.name = name;
		this.type = type;
		this.required = required;
	}
	
	public ConfigurationTO(String name, String defaultValue, FieldType type) {
		this.name = name;
		this.value = defaultValue;
		this.type = type;
	}
	
	public ConfigurationTO(String name, String defaultValue, FieldType type, Boolean numeric, Boolean required) {
		this.name = name;
		this.value = defaultValue;
		this.type = type;
		this.numeric = numeric;
		this.required = required;
	}
	
	public ConfigurationTO(String name, String defaultValue, FieldType type, String nameAux, Boolean use) {
		this.name = name;
		this.value = defaultValue;
		this.type = type;
		this.nameAux = nameAux;
	}
	
	public ConfigurationTO(String name, String id, String defaultValue, FieldType type) {
		this.name = name;
		this.id = id;
		this.value = defaultValue;
		this.type = type;
	}
	
	public ConfigurationTO(String name, String defaultValue, FieldType type, String comboboxValues) {
		this.name = name;
		this.value = defaultValue;
		this.type = type;
		this.comboboxValues = comboboxValues;
	}
	
	public ConfigurationTO(String name, String defaultValue, FieldType type, String comboboxValues, Integer maxCharacteres) {
		this.name = name;
		this.value = defaultValue;
		this.type = type;
		this.comboboxValues = comboboxValues;
		this.maxCharacteres = maxCharacteres;
	}
	
	public ConfigurationTO(String name, String defaultValue, FieldType type, Integer maxCharacteres) {
		this.name = name;
		this.value = defaultValue;
		this.type = type;
		this.maxCharacteres = maxCharacteres;
	}
	
	public ConfigurationTO(String name, String defaultValue, FieldType type,  
			Integer maxCharacteres, Integer minCharacteres) {
		this.name = name;
		this.value = defaultValue;
		this.type = type;
		this.maxCharacteres = maxCharacteres;
		this.minCharacteres = minCharacteres;
	}
	
	public ConfigurationTO(String name, String defaultValue, FieldType type, String listPattern,
			Integer maxCharacteres, Integer minCharacteres) {
		this.name = name;
		this.value = defaultValue;
		this.type = type;
		this.comboboxValues = listPattern;
		this.maxCharacteres = maxCharacteres;
		this.minCharacteres = minCharacteres;
	}
	
	public ConfigurationTO(String name, String defaultValue, FieldType type,
			Boolean numeric) {
		this.name = name;
		this.value = defaultValue;
		this.type = type;
		this.numeric = numeric;
	}
	
	public ConfigurationTO(String name, String defaultValue, FieldType type,
			Integer maxCharacteres, Integer minCharacteres, Boolean numeric) {
		this.name = name;
		this.value = defaultValue;
		this.type = type;
		this.maxCharacteres = maxCharacteres;
		this.minCharacteres = minCharacteres;
		this.numeric = numeric;
	}
	
	public ConfigurationTO(ConfigurationEntity configEntity) {
		this.name = configEntity.getName();
		this.value = configEntity.getValue();
		this.type = configEntity.getType();
		this.comboboxValues = configEntity.getComboboxValues();
		this.maxCharacteres = configEntity.getMaxCharacteres();
		this.minCharacteres = configEntity.getMinCharacteres();
		this.numeric = configEntity.getNumeric();
		this.required = configEntity.getRequired();
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	public FieldType getType() {
		return type;
	}

	public void setType(FieldType type) {
		this.type = type;
	}

	public String getComboboxValues() {
		return comboboxValues;
	}

	public void setComboboxValues(String comboboxValues) {
		this.comboboxValues = comboboxValues;
	}

	public Integer getMaxCharacteres() {
		return maxCharacteres;
	}

	public void setMaxCharacteres(Integer maxCharacteres) {
		this.maxCharacteres = maxCharacteres;
	}

	public Integer getMinCharacteres() {
		return minCharacteres;
	}

	public void setMinCharacteres(Integer minCharacteres) {
		this.minCharacteres = minCharacteres;
	}

	public Boolean getNumeric() {
		return numeric;
	}

	public void setNumeric(Boolean numeric) {
		this.numeric = numeric;
	}

	public Boolean getRequired() {
		return required;
	}

	public void setRequired(Boolean required) {
		this.required = required;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getNameAux() {
		if(name.equals("Dois leitores"))
			return name + " (usa para catracas com urna)";
		if(nameAux == null)
			return name;
		return name + " " + nameAux;
	}

	public void setNameAux(String nameAux) {
		this.nameAux = nameAux;
	}
	
}
