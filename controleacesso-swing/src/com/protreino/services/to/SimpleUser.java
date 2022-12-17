package com.protreino.services.to;

import java.io.Serializable;

public class SimpleUser implements Serializable {
	
	private static final long serialVersionUID = -7367565251388765769L;
	
	private Long id;
	private String name;
	private String idClient;
	
	public SimpleUser() {
	}

	public SimpleUser(Long id, String name, String idClient) {
		this.id = id;
		this.name = name;
		this.idClient = idClient;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getIdClient() {
		return idClient;
	}

	public void setIdClient(String idClient) {
		this.idClient = idClient;
	}
	
	

}
