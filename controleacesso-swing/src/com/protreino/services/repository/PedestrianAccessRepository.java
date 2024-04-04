package com.protreino.services.repository;

import java.util.HashMap;
import java.util.List;
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
	
	public PedestrianAccessEntity save(final PedestrianAccessEntity pedestrianAccessEntity) {
		return (PedestrianAccessEntity) HibernateUtil.save(PedestrianAccessEntity.class, pedestrianAccessEntity)[0];
	}
	
	public PedestrianAccessEntity buscaPedestrePorIdOuIdTemp(final Long idPedestre) {
		PedestrianAccessEntity pedestre = (PedestrianAccessEntity) HibernateUtil
				.getSingleResultById(PedestrianAccessEntity.class, idPedestre);

		if (Objects.nonNull(pedestre) && Objects.nonNull(pedestre.getId())) {
			return pedestre;
		}

		pedestre = (PedestrianAccessEntity) HibernateUtil.getSingleResultByIdTemp(PedestrianAccessEntity.class,
				idPedestre);

		if (Objects.nonNull(pedestre) && Objects.nonNull(pedestre.getId())) {
			return pedestre;
		}

		return null;
	}
	
	@SuppressWarnings("unchecked")
    public PedestrianAccessEntity buscaPedestrePorIdTemp(Long idPedestreTemp) {
        HashMap<String, Object> args = new HashMap<String, Object>();
        args.put("ID_TEMP", idPedestreTemp);

        List<PedestrianAccessEntity> pedestres = (List<PedestrianAccessEntity>)
                HibernateUtil.getResultListWithParams(PedestrianAccessEntity.class, "PedestrianAccessEntity.findByIdTemp", args);

        if (pedestres != null && !pedestres.isEmpty()) {
        	return pedestres.get(0);
        }

        return null;
    }
	
}
