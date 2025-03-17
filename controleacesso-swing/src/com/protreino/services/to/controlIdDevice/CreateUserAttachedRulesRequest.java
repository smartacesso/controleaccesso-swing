package com.protreino.services.to.controlIdDevice;

import java.util.List;

public class CreateUserAttachedRulesRequest {

	private String object;
	private List<AttachedRulesRequest> values;

	public CreateUserAttachedRulesRequest() {
	}

	public CreateUserAttachedRulesRequest(String object, List<AttachedRulesRequest> values) {
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

	public List<AttachedRulesRequest> getUserContent() {
		return values;
	}

	public void setUserContent(List<AttachedRulesRequest> values) {
		this.values = values;
	}

}
