package com.protreino.services.repository;

import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import com.protreino.services.entity.LocalEntity;
import com.protreino.services.entity.PedestrianAccessEntity;

public class LocalRepository {

	public List<String> getDevicesNameByPedestreLocal(final PedestrianAccessEntity pedestre) {

	    if (pedestre.getUuidLocal() == null) {
	        return null;
	    }

	    Optional<LocalEntity> localOpt = getLocalByUiid(pedestre.getUuidLocal());

	    LocalEntity local = localOpt.orElse(null);

	    if (local == null ||
	        local.getHikivisionDeviceNames() == null ||
	        local.getHikivisionDeviceNames().isEmpty()) {

	        return null;
	    }

	    return local.getHikivisionDeviceNames();
	}
	
	public LocalEntity buscaLocalById(Long idLocal) {
		return (LocalEntity) HibernateAccessDataFacade.getSingleResultById(LocalEntity.class, idLocal);
	}
	
	@SuppressWarnings("unchecked")
	public Optional<LocalEntity> getLocalByUiid(final String uuid) {
		final HashMap<String, Object> args = new HashMap<>();
		args.put("UUID", uuid);
		
		final List<LocalEntity> result = (List<LocalEntity>) HibernateAccessDataFacade.getResultListWithParams(LocalEntity.class, "LocalEntity.findByUuid", args, 0, 1);
		
		return Objects.nonNull(result) && !result.isEmpty() ? 
				Optional.of(result.get(0))
				: Optional.empty();
	}
	
	@SuppressWarnings("unchecked")
	public Optional<LocalEntity> getLocalByName(final String nome) {
		final HashMap<String, Object> args = new HashMap<>();
		args.put("NOME", nome);
		
		final List<LocalEntity> result = (List<LocalEntity>) HibernateAccessDataFacade.getResultListWithParams(LocalEntity.class, "LocalEntity.findByName", args, 0, 1);
		
		return Objects.nonNull(result) && !result.isEmpty() ? 
				Optional.of(result.get(0))
				: Optional.empty();
	}
	
}
