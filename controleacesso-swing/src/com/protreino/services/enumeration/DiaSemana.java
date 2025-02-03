package com.protreino.services.enumeration;

public enum DiaSemana {
	Sunday(7),
	Monday(1),
	Tuesday(2),
	Wednesday(3),
	Thursday(4),
	Friday(5),
	Saturday(6);
	
	
	private final Integer numeroDia;
	
	DiaSemana(Integer numero){
		this.numeroDia = numero;
	}

	public Integer getNumeroDia() {
		return numeroDia;
	}
	
	public static DiaSemana intForDia(Integer numero) {
		for(DiaSemana dia : values()) {
			if(dia.getNumeroDia() == numero) {
				return dia;
			}
		}
		return null;
	}
}
