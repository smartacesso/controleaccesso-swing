package com.protreino.services.exceptions;

@SuppressWarnings("serial")
public class CartaoInvalidException extends RuntimeException {

	public CartaoInvalidException (final String message) {
		super(message);
	}
}
