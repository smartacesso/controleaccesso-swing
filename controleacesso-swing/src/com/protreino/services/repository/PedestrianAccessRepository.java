package com.protreino.services.repository;

import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import com.protreino.services.entity.PedestrianAccessEntity;

public class PedestrianAccessRepository {

	public Optional<PedestrianAccessEntity> findByCardNumber(final String cardNumber) {
		final PedestrianAccessEntity pedestre = (PedestrianAccessEntity) HibernateAccessDataFacade
				.getSingleResultByCardNumber(PedestrianAccessEntity.class, Long.valueOf(cardNumber));
		
		return Objects.nonNull(pedestre) ? Optional.of(pedestre) : Optional.empty();
	}
	
	public PedestrianAccessEntity save(final PedestrianAccessEntity pedestrianAccessEntity) {
		return (PedestrianAccessEntity) HibernateAccessDataFacade.save(PedestrianAccessEntity.class, pedestrianAccessEntity)[0];
	}
	
	public PedestrianAccessEntity buscaPedestrePorIdOuIdTemp(final Long idPedestre) {
		PedestrianAccessEntity pedestre = (PedestrianAccessEntity) HibernateAccessDataFacade
				.getSingleResultById(PedestrianAccessEntity.class, idPedestre);

		if (Objects.nonNull(pedestre) && Objects.nonNull(pedestre.getId())) {
			return pedestre;
		}

		pedestre = (PedestrianAccessEntity) HibernateAccessDataFacade.getSingleResultByIdTemp(PedestrianAccessEntity.class,
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
        		HibernateAccessDataFacade.getResultListWithParams(PedestrianAccessEntity.class, "PedestrianAccessEntity.findByIdTemp", args);

        if (pedestres != null && !pedestres.isEmpty()) {
        	return pedestres.get(0);
        }

        return null;
    }
	
	@SuppressWarnings("unchecked")
	public PedestrianAccessEntity buscaPedestrePeloQrCode(String codigoUsuario) {
		HashMap<String, Object> args = new HashMap<>();
		args.put("QR_CODE", codigoUsuario.trim());
		List<PedestrianAccessEntity> pedestres = (List<PedestrianAccessEntity>) HibernateAccessDataFacade
				.getResultListWithParams(PedestrianAccessEntity.class, "PedestrianAccessEntity.findByQRCode", args);

		if (Objects.nonNull(pedestres) && !pedestres.isEmpty()) {
			return pedestres.get(0);
		}

		return null;
	}
	
}
