package com.protreino.services.repository;

import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import com.protreino.services.entity.LocalEntity;
import com.protreino.services.entity.PedestrianAccessEntity;

public class LocalRepository {

	public LocalEntity buscaLocalById(Long idLocal) {
		return (LocalEntity) HibernateAccessDataFacade.getSingleResultById(LocalEntity.class, idLocal);
	}
	
	public List<String> getDevicesNameByPedestreLocal(final PedestrianAccessEntity pedestre) {
		System.out.println("id local :" + pedestre.getIdLocal());
		
		if(Objects.isNull(pedestre.getIdLocal())) {
			return null;
		}
		
		final LocalEntity local = buscaLocalById(pedestre.getIdLocal());
		
		if(Objects.isNull(local) 
				|| Objects.isNull(local.getHikivisionDeviceNames())
				|| local.getHikivisionDeviceNames().isEmpty()) {
			return null;
		}
		
		return local.getHikivisionDeviceNames();
	}
	
	@SuppressWarnings("unchecked")
	public Optional<LocalEntity> getLocalByName(final String name) {
		final HashMap<String, Object> args = new HashMap<>();
		args.put("NOME", name);
		
		final List<LocalEntity> result = (List<LocalEntity>) HibernateAccessDataFacade.getResultListWithParams(LocalEntity.class, "LocalEntity.findByName", args, 0, 1);
		
		return Objects.nonNull(result) && !result.isEmpty() ? 
				Optional.of(result.get(0))
				: Optional.empty();
	}
	
}
