package com.protreino.services.to.hikivision;

import java.util.Date;

public class TimeSegment {
    
	private final Date beginTime;
    private final Date endTime;

    public TimeSegment(Date beginTime, Date endTime) {
        this.beginTime = beginTime;
        this.endTime = endTime;
    }

	public Date getBeginTime() {
		return beginTime;
	}

	public Date getEndTime() {
		return endTime;
	}
}
