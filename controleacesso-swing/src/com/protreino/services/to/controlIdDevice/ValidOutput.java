package com.protreino.services.to.controlIdDevice;

public class ValidOutput {
	private String valid;

	public String getValid() {
		return valid;
	}

	public void setValid(String session) {
		this.valid = session;
	}
	
	public Boolean isValid() {
	    return "true".equalsIgnoreCase(valid);
	}
}
