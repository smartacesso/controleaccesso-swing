package com.protreino.services.to.hikivision;

import java.util.List;

public class UserRightWeekPlanCfgTO {
	 private boolean enable;
	    private List<WeekPlanCfg> WeekPlanCfg;

	    public UserRightWeekPlanCfgTO(boolean enable, List<WeekPlanCfg> weekPlanCfg) {
	        this.enable = enable;
	        this.WeekPlanCfg = weekPlanCfg;
	    }
	
}
