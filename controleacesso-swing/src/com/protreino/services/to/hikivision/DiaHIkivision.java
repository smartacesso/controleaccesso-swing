package com.protreino.services.to.hikivision;

import java.util.List;

public class DiaHIkivision {
    private String week;
    private int id;
    private boolean enable;
    private TimeSegment TimeSegment;
    
    public DiaHIkivision(String week, int id, boolean enable, TimeSegment timeSegment) {
        this.week = week;
        this.id = id;
        this.enable = enable;
        this.TimeSegment = timeSegment;
    }
}

