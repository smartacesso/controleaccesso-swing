package com.protreino.services.repository;

import java.util.List;

import com.protreino.services.entity.RegraEntity;

public class RegraRepository {
	
	@SuppressWarnings("unchecked")
	public List<RegraEntity> buscaRegrasComHorario(){
		return (List<RegraEntity>) HibernateAccessDataFacade.getResultList(RegraEntity.class, "RegraEntity.findAllComHorario");
	}
	
	@SuppressWarnings("unchecked")
	public RegraEntity buscaRegraById(Long id){
		return  (RegraEntity) HibernateAccessDataFacade.getSingleResultById(RegraEntity.class, id);
	}
}
