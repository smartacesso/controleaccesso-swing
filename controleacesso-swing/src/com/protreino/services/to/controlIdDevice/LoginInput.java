package com.protreino.services.to.controlIdDevice;

public class LoginInput  {
	
	private String login;
	private String password;
	
	public LoginInput(String login, String password) {
		// TODO Auto-generated constructor stub
		this.login = login;
		this.password = password;
	}

	private String getLogin() {
		return login;
	}
	
	private void setLogin(String login) {
		this.login = login;	
	}
	
	private String getPassword() {
		return password;
	}
	
	private void setPassword(String password) {
		this.password = password;	
	}

}
