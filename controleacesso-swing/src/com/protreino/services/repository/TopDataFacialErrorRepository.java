package com.protreino.services.repository;

import java.util.HashMap;
import java.util.List;

import com.protreino.services.entity.TopdataFacialErrorEntity;

public class TopDataFacialErrorRepository {

	@SuppressWarnings("unchecked")
	public List<TopdataFacialErrorEntity> findAllByCardNumber(final String cardNumber) {
		final HashMap<String, Object> args = new HashMap<>();
		args.put("CARD_NUMBER", cardNumber);
		
		return (List<TopdataFacialErrorEntity>) HibernateAccessDataFacade
				.getResultListWithParams(TopdataFacialErrorEntity.class, "TopdataFacialErrorEntity.findAllByCardNumber", args);
	}
	
	@SuppressWarnings("unchecked")
	public List<TopdataFacialErrorEntity> findAll() {
		return (List<TopdataFacialErrorEntity>) HibernateAccessDataFacade
				.getResultList(TopdataFacialErrorEntity.class, "TopdataFacialErrorEntity.findAllOrderByErrorDate");
	}
	
	
	public Integer countFindAll() {
		int resultListCount = HibernateAccessDataFacade
				.getResultListCount(TopdataFacialErrorEntity.class, "TopdataFacialErrorEntity.countAllOrderByErrorDate");
		return resultListCount;
	}
}
