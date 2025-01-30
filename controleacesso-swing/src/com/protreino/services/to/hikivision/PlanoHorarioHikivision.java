package com.protreino.services.to.hikivision;

import java.util.List;

public class PlanoHorarioHikivision {
	 private boolean enable;
	 private List<DiaHIkivision> WeekPlanCfg;

	    public PlanoHorarioHikivision(boolean enable, List<DiaHIkivision> weekPlanCfg) {
	        this.enable = enable;
	        this.WeekPlanCfg = weekPlanCfg;
	    }
	
}
