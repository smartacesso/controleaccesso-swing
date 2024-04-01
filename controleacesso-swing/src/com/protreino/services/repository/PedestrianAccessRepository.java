package com.protreino.services.repository;

import java.util.Objects;
import java.util.Optional;

import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.utils.HibernateUtil;

public class PedestrianAccessRepository {

	public Optional<PedestrianAccessEntity> findByCardNumber(final String cardNumber) {
		final PedestrianAccessEntity pedestre = (PedestrianAccessEntity) HibernateUtil
				.getSingleResultByCardNumber(PedestrianAccessEntity.class, Long.valueOf(cardNumber));
		
		return Objects.nonNull(pedestre) ? Optional.of(pedestre) : Optional.empty();
	}
}
