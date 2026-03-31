package com.protreino.services.exceptions;

@SuppressWarnings("serial")
public class UsuarioInvalidException extends RuntimeException{
	public UsuarioInvalidException (final String message) {
		super(message);
	}

}
