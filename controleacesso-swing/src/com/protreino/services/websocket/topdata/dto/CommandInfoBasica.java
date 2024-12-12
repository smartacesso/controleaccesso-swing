package com.protreino.services.websocket.topdata.dto;

public class CommandInfoBasica {

	private String cmd;
	private String name;
	private long enrollid;
	private long backupnum = 0;
	private String record = "";
	
	public CommandInfoBasica(final long enrollid, final String nome) {
		this.cmd = "setuserinfo";
		this.enrollid = enrollid;
		this.name = nome;
		this.backupnum = 0;
		this.record = "";
	}

	public String getCmd() {
		return cmd;
	}

	public long getEnrollid() {
		return enrollid;
	}

	public long getBackupnum() {
		return backupnum;
	}

	public String getRecord() {
		return record;
	}

	public String getNome() {
		return name;
	}
	
}
