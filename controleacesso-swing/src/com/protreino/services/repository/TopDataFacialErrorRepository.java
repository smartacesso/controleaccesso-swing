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
}
