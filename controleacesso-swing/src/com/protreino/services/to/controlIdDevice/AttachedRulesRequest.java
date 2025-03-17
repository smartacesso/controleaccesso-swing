package com.protreino.services.to.controlIdDevice;

public class AttachedRulesRequest {

	private Long user_id;
	private Long access_rule_id;

	public Long getUser_id() {
		return user_id;
	}

	public AttachedRulesRequest(Long user_id, Long access_rule_id) {
		super();
		this.user_id = user_id;
		this.access_rule_id = access_rule_id;
	}

	public void setUser_id(Long user_id) {
		this.user_id = user_id;
	}

	public Long getAccess_rule_id() {
		return access_rule_id;
	}

	public void setAccess_rule_id(Long access_rule_id) {
		this.access_rule_id = access_rule_id;
	}

}
