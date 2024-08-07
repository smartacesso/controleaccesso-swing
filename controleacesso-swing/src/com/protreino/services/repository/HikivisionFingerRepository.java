package com.protreino.services.repository;

import java.util.HashMap;

import com.protreino.services.entity.HikivisionFingerEntity;
import com.protreino.services.enumeration.Finger;

public class HikivisionFingerRepository {

	
	public HikivisionFingerEntity findByFingerNoAndIdUser(Finger fingerNo, Long idUser) {
		 HashMap<String, Object> args = new HashMap<>();
     //    args.put("FINGER_NO", fingerNo);
         args.put("ID_USER", idUser);
         
		return (HikivisionFingerEntity) HibernateAccessDataFacade
				.getUniqueResultWithParams(HikivisionFingerEntity.class, "HikivisionFingerEntity.findByIdUserAndFingerNo", args);
	}
}
