package com.protreino.services.entity;

import com.protreino.services.enumeration.Permissions;


public class PermissaoEntity {

	private Long id;
	
	
	private Permissions permission;

	public PermissaoEntity(Long id, Permissions permission) {
		this.id = id;
		this.permission = permission;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public Permissions getPermission() {
		return permission;
	}

	public void setPermission(Permissions permission) {
		this.permission = permission;
	}
	
}
