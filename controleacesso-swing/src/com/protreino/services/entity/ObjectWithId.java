package com.protreino.services.entity;

import java.io.Serializable;

public interface ObjectWithId extends Serializable {
	
	Long getId();
	void setId(Long id);

}
