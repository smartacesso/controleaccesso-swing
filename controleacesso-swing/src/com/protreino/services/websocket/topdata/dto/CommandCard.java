package com.protreino.services.websocket.topdata.dto;

public class CommandCard {
	private String cmd;
	private long enrollid;
	private long backupnum = 0;
	private long record;
	
	public CommandCard(final long enrollid, final long record) {
		this.cmd = "setuserinfo";
		this.enrollid = enrollid;
		this.backupnum = 11; //cartão
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

	public long getRecord() {
		return record;
	}
	

}
