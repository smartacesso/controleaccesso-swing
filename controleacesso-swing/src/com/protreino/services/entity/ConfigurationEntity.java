package com.protreino.services.entity;

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

import org.hibernate.annotations.Type;

import com.protreino.services.enumeration.FieldType;
import com.protreino.services.to.ConfigurationTO;

@SuppressWarnings("serial")
@Entity
@Table(name="TB_CONFIGURATION")
@NamedQueries({
	@NamedQuery(name = "ConfigurationEntity.findAll", query = "select obj from ConfigurationEntity obj order by id asc"),
	@NamedQuery(name = "ConfigurationEntity.findById", query = "select obj from ConfigurationEntity obj where obj.id = :ID")
})
public class ConfigurationEntity extends BaseEntity implements ObjectWithId{
	
	@Id
	@GeneratedValue(strategy=GenerationType.IDENTITY)
	@Column(name="ID_CONFIGURATION", nullable=false, length=10)
	private Long id;

	@ManyToOne(cascade={}, fetch=FetchType.EAGER)
	@JoinColumn(name="ID_CONFIGURATION_GROUP", nullable=false)
	private ConfigurationGroupEntity group;
	
	@Column(name="NAME", nullable=false, length=100)
	private String name;
	
	@Type(type="text")
	@Column(name="VALUE", nullable=true)
	private String value;
	
	@Enumerated(EnumType.STRING)
	@Column(name="TYPE", nullable=false, length=100)
	private FieldType type;
	
	@Column(name="COMBOBOX_VALUES", nullable=true, length=1000)
	private String comboboxValues; // uma string com os possiveis valores separados por ';'
	
	@Column(name="MAX_CHARACTERES", nullable=true, length=100)
	private Integer maxCharacteres;
	
	@Column(name="MIN_CHARACTERES", nullable=true, length=100)
	private Integer minCharacteres;
	
	@Type(type = "org.hibernate.type.NumericBooleanType")
	@Column(name="NUMERIC_ONLY", nullable=true, length=100)
	private Boolean numeric;
	
	@Type(type = "org.hibernate.type.NumericBooleanType")
	@Column(name="REQUIRED", nullable=true, length=100)
	private Boolean required;
	
	public ConfigurationEntity(){
	}
	
	public ConfigurationEntity(ConfigurationGroupEntity group, ConfigurationTO configTO) {
		this.group = group;
		this.name = configTO.getName();
		this.value = configTO.getValue();
		this.type = configTO.getType();
		this.comboboxValues = configTO.getComboboxValues();
		this.maxCharacteres = configTO.getMaxCharacteres();
		this.minCharacteres = configTO.getMinCharacteres();
		this.numeric = configTO.getNumeric();
		this.required = configTO.getRequired();
	}
	
	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public ConfigurationGroupEntity getGroup() {
		return group;
	}

	public void setGroup(ConfigurationGroupEntity group) {
		this.group = group;
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

}
