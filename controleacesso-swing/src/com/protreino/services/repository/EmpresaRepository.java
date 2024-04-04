package com.protreino.services.repository;

import com.protreino.services.entity.EmpresaEntity;
import com.protreino.services.utils.HibernateUtil;

public class EmpresaRepository {

	public EmpresaEntity buscaEmpresaById(Long idEmpresa) {
		return (EmpresaEntity) HibernateUtil.getSingleResultById(EmpresaEntity.class, idEmpresa);
	}
	
}
