package com.protreino.services.repository;

import java.util.List;
import java.util.Objects;

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
	
}
