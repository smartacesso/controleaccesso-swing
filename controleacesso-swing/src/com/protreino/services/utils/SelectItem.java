package com.protreino.services.utils;

public class SelectItem {
	
	private String label;
	private Object value;
	
	/**
	 * 
	 * @param label_value Pode ser uma string do tipo LABEL_VALUE ou apenas uma string simples caso LABEL e VALUE sejam iguais.
	 */
	public SelectItem(String label_value) {
		String[] partes = label_value.split("_");
		label = partes[0];
		value = partes.length > 1 ? partes[1] : partes[0];
	}
	
	public SelectItem(String label, Object value) {
		this.label = label;
		this.value = value;
	}

	public String getLabel() {
		return label;
	}
	
	public void setLabel(String label) {
		this.label = label;
	}
	
	public Object getValue() {
		return value;
	}
	
	public void setValue(Object value) {
		this.value = value;
	}
	
	public String toString(){
		return label;
	}
	
	@Override
	public boolean equals(Object o) {
	    /* Comentado para não ter que sobreescrever tambem o hashcode
	     * if (o == this) {
	      return true;
	    }*/
	    if (!(o instanceof SelectItem)) {
	      return false;
	    }
	    if (value == null)
	    	return false;
	    SelectItem other = (SelectItem) o;
	    return value.equals(other.value);
	  }

}
