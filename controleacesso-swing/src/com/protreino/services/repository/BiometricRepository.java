package com.protreino.services.repository;

import java.util.HashMap;
import java.util.List;

import com.protreino.services.entity.BiometricEntity;
import com.protreino.services.utils.HibernateUtil;

public class BiometricRepository {

	@SuppressWarnings("unchecked")
    public List<BiometricEntity> buscaBiometriasVisitante(Long idVisitante) {
        HashMap<String, Object> args = new HashMap<String, Object>();
        args.put("ID_USER", idVisitante);

        List<BiometricEntity> biometriasVisitantesLocais = (List<BiometricEntity>)
                HibernateUtil.getResultListWithParams(BiometricEntity.class,
                        "BiometricEntity.findByIdUser", args);

        return biometriasVisitantesLocais;
    }
}
