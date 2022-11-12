package com.protreino.services.enumeration;

public enum TipoEscala {

	ESCALA_12_36("12/36 (horas)"),
	ESCALA_24_04("24/4 (horas/dias)"),
	ESCALA_06_02("6/2 (dias)"),
	ESCALA_07_01("7/1 (dias)"),
	ESCALA_05_01("5/1 (dias)");
	
	private String descricao;
	
	private TipoEscala(String descricao) {
		this.descricao = descricao;
	}

	public String getDescricao() {
		return descricao;
	}
}
