package com.protreino.services.repository;

import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import com.protreino.services.entity.HikivisionIntegrationErrorEntity;

public class HikivisionIntegrationErrorRepository {

	public void saveAll(final List<HikivisionIntegrationErrorEntity> hikivisonIntegrationErrors) {
		for (HikivisionIntegrationErrorEntity hikivisonIntegrationError : hikivisonIntegrationErrors) {
			save(hikivisonIntegrationError);
		}
	}

	public void save(final HikivisionIntegrationErrorEntity hikivisonIntegrationError) {
		final Optional<HikivisionIntegrationErrorEntity> existentError = findByCardNumberAndDeviceId(hikivisonIntegrationError.getCardNumber(), 
				hikivisonIntegrationError.getDeviceId());
		if(existentError.isPresent()) {
			if(existentError.get().getHikivisionAction() == hikivisonIntegrationError.getHikivisionAction()) {
				return;
			}
			
			remove(existentError.get());
		}
		
		HibernateAccessDataFacade.save(HikivisionIntegrationErrorEntity.class, hikivisonIntegrationError);
	}
	
	public void update(final HikivisionIntegrationErrorEntity hikivisonIntegrationError) {
		HibernateAccessDataFacade.update(HikivisionIntegrationErrorEntity.class, hikivisonIntegrationError);
	}

	@SuppressWarnings("unchecked")
	public List<HikivisionIntegrationErrorEntity> findLatest(Integer limit) {
		return (List<HikivisionIntegrationErrorEntity>) HibernateAccessDataFacade.getResultListWithParams(HikivisionIntegrationErrorEntity.class,
				"HikivisionIntegrationErrorEntity.findByLatest", null, 0, limit);
	}
	
	@SuppressWarnings("unchecked")
	public List<HikivisionIntegrationErrorEntity> findFirts(final Integer limit) {
		return (List<HikivisionIntegrationErrorEntity>) HibernateAccessDataFacade.getResultListWithParams(HikivisionIntegrationErrorEntity.class,
				"HikivisionIntegrationErrorEntity.findAll", null, 0, limit);
	}
	
	@SuppressWarnings("unchecked")
	public List<HikivisionIntegrationErrorEntity> findFirtsWhereMaxRetriesAreLessThan(final int limit, final long maxRetries) {
		final HashMap<String, Object> args = new HashMap<>();
		args.put("MAX_RETRIES", maxRetries);
		
		return (List<HikivisionIntegrationErrorEntity>) HibernateAccessDataFacade
				.getResultListWithParams(HikivisionIntegrationErrorEntity.class, 
						"HikivisionIntegrationErrorEntity.findAllWhereRetriesAreLessThan", args, 0, limit);
	}

	@SuppressWarnings("unchecked")
	public Optional<HikivisionIntegrationErrorEntity> findByCardNumberAndDeviceId(final String cardNumber, final String deviceId) {
		final HashMap<String, Object> args = new HashMap<>();
		args.put("CARD_NUMBER", cardNumber);
		args.put("DEVICE_ID", deviceId);
		
		final List<HikivisionIntegrationErrorEntity> result = (List<HikivisionIntegrationErrorEntity>) HibernateAccessDataFacade
				.getResultListWithParams(HikivisionIntegrationErrorEntity.class, "HikivisionIntegrationErrorEntity.findByCardNumberAndDeviceId", args, 0, 1);
		
		return Objects.nonNull(result) && !result.isEmpty() ? Optional.of(result.get(0)) : Optional.empty();
	}
	
	public void remove(final HikivisionIntegrationErrorEntity hikivisonIntegrationError) {
		HibernateAccessDataFacade.remove(hikivisonIntegrationError);
	}

}
