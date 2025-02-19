package com.protreino.services.to.controlIdDevice;

public class UserContentTypesrequest {

	private String name;
	private String registration;
	private String password;
	private String salt;

	public UserContentTypesrequest(String name, String registration, String password, String salt) {
		super();
		this.name = name;
		this.registration = registration;
		this.password = password;
		this.salt = salt;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getRegistration() {
		return registration;
	}

	public void setRegistration(String registration) {
		this.registration = registration;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getSalt() {
		return salt;
	}

	public void setSalt(String salt) {
		this.salt = salt;
	}

}
