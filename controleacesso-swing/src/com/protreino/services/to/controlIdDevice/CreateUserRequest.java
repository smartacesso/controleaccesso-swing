package com.protreino.services.to.controlIdDevice;

import java.util.List;

public class CreateUserRequest {

	private String object;
	private List<UserContentTypesrequest> values;

	public CreateUserRequest() {
	}

	public CreateUserRequest(String object, List<UserContentTypesrequest> values) {
		super();
		this.object = object;
		this.values = values;
	}

	public String getObject() {
		return object;
	}

	public void setObject(String object) {
		this.object = object;
	}

	public List<UserContentTypesrequest> getUserContent() {
		return values;
	}

	public void setUserContent(List<UserContentTypesrequest> values) {
		this.values = values;
	}

}
