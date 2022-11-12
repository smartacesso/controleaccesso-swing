
package com.protreino.services.entity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;


@Entity
@Table(name="DB_CONFIGURACAO")

public class URLEntity {
	
	@Id
	@GeneratedValue(strategy=GenerationType.IDENTITY)
	@Column(name="id", nullable=false, length=4)
	private Long id;
	
	@Column(name="ip", nullable=true, length=100)
	public String ip;

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getIp() {
		return ip;
	}

	public void setIp(String ip) {
		this.ip = ip;
	}
	
	public URLEntity() {
		super();
	}
	
	public URLEntity( String ip) {
		super();
		this.ip = ip;
		
		
	}
	
	
}