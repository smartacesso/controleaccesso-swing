package com.protreino.services.screens;

public class CachedCount {
	int count;
	long timestamp;
	CachedCount(int count, long timestamp) {
		this.count = count;
		this.timestamp = timestamp;
	}
	boolean isExpired() {
		// Expira após 2 minutos
		return System.currentTimeMillis() - timestamp > 2 * 60 * 1000;
	}
}
