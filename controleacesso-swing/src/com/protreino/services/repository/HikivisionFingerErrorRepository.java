package com.protreino.services.repository;

import java.util.HashMap;
import java.util.List;

import com.protreino.services.entity.HikivisonFingerErrorEntity;

public class HikivisionFingerErrorRepository {
	
	@SuppressWarnings("unchecked")
	public List<HikivisonFingerErrorEntity> findAllLimited(Long limit) {
		 HashMap<String, Object> args = new HashMap<>();
		
		return (List<HikivisonFingerErrorEntity>) HibernateAccessDataFacade
			.getResultListLimited(HikivisonFingerErrorEntity.class, "HikivisonFingerErrorEntity.findAllBiometricWithErrors", limit);
		
	}

}
