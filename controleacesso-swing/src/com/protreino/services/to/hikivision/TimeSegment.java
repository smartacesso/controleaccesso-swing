package com.protreino.services.to.hikivision;

import java.util.Date;

public class TimeSegment {
    
	private final String beginTime;
    private final String endTime;

    public TimeSegment(String beginTime, String endTime) {
        this.beginTime = beginTime;
        this.endTime = endTime;
    }

	public String getBeginTime() {
		return beginTime;
	}

	public String getEndTime() {
		return endTime;
	}
}
