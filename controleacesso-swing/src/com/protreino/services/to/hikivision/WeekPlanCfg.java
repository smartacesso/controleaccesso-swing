package com.protreino.services.to.hikivision;

import java.util.List;

public class WeekPlanCfg {
    private String week;
    private int id;
    private boolean enable;
    private List<TimeSegment> TimeSegment;

    public WeekPlanCfg(String week, int id, boolean enable, List<TimeSegment> timeSegment) {
        this.week = week;
        this.id = id;
        this.enable = enable;
        this.TimeSegment = timeSegment;
    }
}

