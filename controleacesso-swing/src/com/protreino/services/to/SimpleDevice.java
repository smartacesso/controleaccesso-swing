package com.protreino.services.to;

import java.io.Serializable;

public class SimpleDevice implements Serializable {
	
	private static final long serialVersionUID = -8671862236810566579L;
	
	private String name;
	private String identifier;

	public SimpleDevice() {
	}

	public SimpleDevice(String name, String identifier) {
		this.name = name;
		this.identifier = identifier;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getIdentifier() {
		return identifier;
	}

	public void setIdentifier(String identifier) {
		this.identifier = identifier;
	}
	
}
