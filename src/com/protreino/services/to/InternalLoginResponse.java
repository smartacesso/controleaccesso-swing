package com.protreino.services.to;

import java.io.Serializable;

public class InternalLoginResponse implements Serializable {
	
	private static final long serialVersionUID = -248030108508772223L;
	
	private boolean success;
	private String errorMessage;
	private SimpleUser user;
	
	public InternalLoginResponse(boolean success) {
		this.success = success; 
	}

	public boolean getSuccess() {
		return success;
	}

	public void setSuccess(boolean success) {
		this.success = success;
	}

	public String getErrorMessage() {
		return errorMessage;
	}

	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	public SimpleUser getUser() {
		return user;
	}

	public void setUser(SimpleUser user) {
		this.user = user;
	}

}
