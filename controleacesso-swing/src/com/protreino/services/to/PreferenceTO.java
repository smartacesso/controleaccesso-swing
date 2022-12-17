package com.protreino.services.to;

import com.protreino.services.entity.PreferenceEntity;
import com.protreino.services.enumeration.FieldType;
import com.protreino.services.enumeration.PreferenceGroup;

public class PreferenceTO {
	
	private PreferenceGroup preferenceGroup;
	private String key;
	private String label;
	private FieldType fieldType;
	private String value;
	private Boolean numeric;
	private Integer textFieldSize;
	private byte[] imageValue;
	private String numericListSequency;
	
	public PreferenceTO(PreferenceEntity preferenceEntity) {
		this.preferenceGroup = preferenceEntity.getPreferenceGroup();
		this.key = preferenceEntity.getKey();
		this.label = preferenceEntity.getLabel();
		this.fieldType = preferenceEntity.getFieldType();
		this.value = preferenceEntity.getValue();
		this.numeric = preferenceEntity.getNumeric();
		this.textFieldSize = preferenceEntity.getTextFieldSize();
		this.imageValue = preferenceEntity.getImageValue();
	}
	
	public PreferenceTO(PreferenceGroup preferenceGroup, String key, String label, FieldType type, 
			String value, Boolean numeric, Integer textFieldSize) {
		this.preferenceGroup = preferenceGroup;
		this.key = key;
		this.label = label;
		this.fieldType = type;
		this.value = value;
		this.numeric = numeric;
		this.textFieldSize = textFieldSize;
	}
	
	public PreferenceTO(PreferenceGroup preferenceGroup, String key, String label, FieldType type, String value) {
		this.preferenceGroup = preferenceGroup;
		this.key = key;
		this.label = label;
		this.fieldType = type;
		this.value = value;
	}
	
	public PreferenceTO(PreferenceGroup preferenceGroup, String key, String label, FieldType type, 
			String value, String numericListSequency) {
		this.preferenceGroup = preferenceGroup;
		this.key = key;
		this.label = label;
		this.fieldType = type;
		this.value = value;
		this.numericListSequency = numericListSequency;
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

	public String getNumericListSequency() {
		return numericListSequency;
	}

	public void setNumericListSequency(String numericListSequency) {
		this.numericListSequency = numericListSequency;
	}

}
