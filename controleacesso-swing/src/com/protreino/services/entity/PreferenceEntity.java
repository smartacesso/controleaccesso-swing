package com.protreino.services.entity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Lob;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

import org.hibernate.annotations.Type;

import com.protreino.services.enumeration.FieldType;
import com.protreino.services.enumeration.PreferenceGroup;
import com.protreino.services.to.PreferenceTO;

@SuppressWarnings("serial")
@Entity
@Table(name="TB_PREFERENCE")
@NamedQueries({
	@NamedQuery(name = "PreferenceEntity.findAll", query = "select obj from PreferenceEntity obj"),
	@NamedQuery(name = "PreferenceEntity.findByKey", query = "select obj from PreferenceEntity obj"
			+ " where obj.key = :CHAVE ")
})
public class PreferenceEntity extends BaseEntity  implements ObjectWithId {
	
	@Id
	@GeneratedValue(strategy=GenerationType.IDENTITY)
	@Column(name="ID_PREFERENCE", nullable=false, length=10)
	private Long id;
	
	@Enumerated(EnumType.STRING)
	private PreferenceGroup preferenceGroup;
	
	@Column(name="CHAVE", nullable=false, length=100)
	private String key;
	
	@Column(name="LABEL", nullable=false, length=500)
	private String label;
	
	@Enumerated(EnumType.STRING)
	private FieldType fieldType;
	
	@Type(type="text")
	@Column(name="VALUE", nullable=true)
	private String value;
	
	@Lob
	@Column(name="IMAGE_VALUE", nullable=true)
	private byte[] imageValue;
	
	@Type(type = "org.hibernate.type.NumericBooleanType")
	@Column(name="IS_NUMERIC", nullable=true, length=30)
	private Boolean numeric;
	
	@Column(name="TEXT_FIELD_SIZE", nullable=true, length=10)
	private Integer textFieldSize;
	
	public PreferenceEntity(){
	}
	
	public PreferenceEntity(PreferenceTO preferenceTO){
		this.preferenceGroup = preferenceTO.getPreferenceGroup();
		this.key = preferenceTO.getKey();
		this.label = preferenceTO.getLabel();
		this.fieldType = preferenceTO.getFieldType();
		this.value = preferenceTO.getValue();
		this.numeric = preferenceTO.getNumeric();
		this.textFieldSize = preferenceTO.getTextFieldSize();
		this.imageValue = preferenceTO.getImageValue();
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public PreferenceGroup getPreferenceGroup() {
		return preferenceGroup;
	}

	public void setPreferenceGroup(PreferenceGroup preferenceGroup) {
		this.preferenceGroup = preferenceGroup;
	}

	public String getKey() {
		return key;
	}

	public void setKey(String key) {
		this.key = key;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public FieldType getFieldType() {
		return fieldType;
	}

	public void setFieldType(FieldType fieldType) {
		this.fieldType = fieldType;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	public Boolean getNumeric() {
		return numeric;
	}

	public void setNumeric(Boolean numeric) {
		this.numeric = numeric;
	}

	public Integer getTextFieldSize() {
		return textFieldSize;
	}

	public void setTextFieldSize(Integer textFieldSize) {
		this.textFieldSize = textFieldSize;
	}

	public byte[] getImageValue() {
		return imageValue;
	}

	public void setImageValue(byte[] imageValue) {
		this.imageValue = imageValue;
	}

	
}
