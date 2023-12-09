package com.protreino.services.exceptions;

public class TimeoutException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	 public TimeoutException(String s, int errorOffset) {
	        super(s);
	       
	    }


}
