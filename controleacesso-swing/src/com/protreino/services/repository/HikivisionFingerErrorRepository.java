package com.protreino.services.repository;

import java.util.List;

import com.protreino.services.entity.HikivisonFingerErrorEntity;

public class HikivisionFingerErrorRepository {
	
	@SuppressWarnings("unchecked")
	public List<HikivisonFingerErrorEntity> findAll() {
		
		return (List<HikivisonFingerErrorEntity>) HibernateAccessDataFacade
				.getResultList(HikivisonFingerErrorEntity.class, "HikivisonFingerErrorEntity.findAllBiometricWithErrors");
		
	}

}
