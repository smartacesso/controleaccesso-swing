package com.protreino.services.websocket.topdata.dto;

public class CommandDeleteUser {

	private String cmd;
	private long enrollid;
	private long backupnum = 0;
	
	public CommandDeleteUser(final long enrollid) {
		this.cmd = "deleteuser";
		this.enrollid = enrollid;
		this.backupnum = 13;
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

}
