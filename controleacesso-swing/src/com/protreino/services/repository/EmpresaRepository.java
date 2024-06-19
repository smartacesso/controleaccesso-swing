package com.protreino.services.repository;

import com.protreino.services.entity.EmpresaEntity;

public class EmpresaRepository {

	public EmpresaEntity buscaEmpresaById(Long idEmpresa) {
		return (EmpresaEntity) HibernateAccessDataFacade.getSingleResultById(EmpresaEntity.class, idEmpresa);
	}
	
}
