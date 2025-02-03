package com.protreino.services.to.hikivision;

import java.util.List;

public class PlanoHorarioHikivision {
	
	private final boolean enable;
	private final List<DiaHIkivision> WeekPlanCfg;

	public PlanoHorarioHikivision(boolean enable, List<DiaHIkivision> weekPlanCfg) {
		this.enable = enable;
		this.WeekPlanCfg = weekPlanCfg;
	}

	public boolean isEnable() {
		return enable;
	}

	public List<DiaHIkivision> getWeekPlanCfg() {
		return WeekPlanCfg;
	}

}
