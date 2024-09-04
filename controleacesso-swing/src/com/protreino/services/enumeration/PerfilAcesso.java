package com.protreino.services.enumeration;

import java.util.stream.Stream;

public enum PerfilAcesso {

	ADMINISTRADOR,
	GERENTE,
	OPERADOR,
	PORTEIRO;
	
	public static PerfilAcesso get(final String perfilAcesso) {
		return Stream.of(values())
				.filter(p -> p.name().equalsIgnoreCase(perfilAcesso))
				.findFirst()
				.orElseThrow(() -> new IllegalArgumentException("Perfil não encontrado!"));
	}
}
