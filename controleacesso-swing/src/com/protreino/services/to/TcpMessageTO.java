package com.protreino.services.to;

import java.io.Serializable;
import java.util.HashMap;

import com.google.gson.Gson;
import com.protreino.services.enumeration.TcpMessageType;

public class TcpMessageTO implements Serializable {
	
	private static final long serialVersionUID = -5907399646090897264L;
	
	private String id;
	private TcpMessageType type;
	private String message;
	
	private HashMap<String, Object> parans;
	
	public TcpMessageTO(){
	}
	
	public TcpMessageTO(TcpMessageType type) {
		this.type = type;
		parans = new HashMap<String, Object>();
	}
	
	public TcpMessageTO(String id, TcpMessageType type) {
		this.id = id;
		this.type = type;
	}
	
	public TcpMessageTO(String id, TcpMessageType type, String message) {
		this.id = id;
		this.type = type;
		this.message = message;
	}
	
	public String toString(){
		return new Gson().toJson(this) + "\r\n";
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public TcpMessageType getType() {
		return type;
	}

	public void setType(TcpMessageType type) {
		this.type = type;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public HashMap<String, Object> getParans() {
		return parans;
	}

	public void setParans(HashMap<String, Object> parans) {
		this.parans = parans;
	}

}
