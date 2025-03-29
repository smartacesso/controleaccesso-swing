package com.protreino.services.repository;

import java.util.HashMap;

import com.protreino.services.entity.PedestreRegraEntity;

public class PedestreRegraRepository {
	
	public static PedestreRegraEntity buscarRegraPedestre(Long idPedestre) {
	    HashMap<String, Object> args = new HashMap<>();
	    args.put("ID_PEDESTRE", idPedestre);

	    @SuppressWarnings("unchecked")
		PedestreRegraEntity pedestreRegra = (PedestreRegraEntity) HibernateAccessDataFacade.getUniqueResultWithParams(
	            PedestreRegraEntity.class, "PedestreRegraEntity.findAllByIdPedestre", args);

	    // Verifica se a regra está presente antes de acessá-la
	    if (pedestreRegra != null) {
	        return pedestreRegra;
	    } else {
	        // Retornar ou lançar exceção, dependendo do comportamento desejado
	        return null;  // Ou pode lançar uma exceção customizada
	    }
	}

}
