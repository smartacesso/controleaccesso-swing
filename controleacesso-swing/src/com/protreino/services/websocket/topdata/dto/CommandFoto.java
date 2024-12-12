package com.protreino.services.websocket.topdata.dto;

public class CommandFoto {

	private String cmd;
	private long enrollid;
	private long backupnum = 0;
	private String record;
	
	public CommandFoto(final long enrollid, final String record) {
		this.cmd = "setuserinfo";
		this.enrollid = enrollid;
		this.backupnum = 50;
		this.record = record;
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
	
}
