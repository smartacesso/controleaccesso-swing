package com.protreino.services.exceptions;

@SuppressWarnings("serial")
public class InvalidPhotoException extends RuntimeException {

	public InvalidPhotoException(final String message) {
		super(message);
	}
}
