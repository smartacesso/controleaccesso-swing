package com.protreino.services.enumeration;

public enum TipoRegra {

	ACESSO_HORARIO("Acesso por horário"),
	ACESSO_PERIODO("Acesso por período"),
	ACESSO_ESCALA("Acesso por turno/escala"),
	ACESSO_CREDITO("Acesso via crédito"),
	ACESSO_UNICO("Acesso Único"),
	ACESSO_ESCALA_3_3("Acesso escala 3/3 3/3");
	
	private String descricao;
	
	private TipoRegra(String descricao) {
		this.descricao = descricao;
	}

	public String getDescricao() {
		return descricao;
	}
}
