package com.protreino.services.repository;

import java.util.Date;
import java.util.HashMap;
import java.util.List;

import com.protreino.services.constants.Tipo;
import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.main.Main;

public class LogPedestrianAccessRepository {

	public int buscaQuantidadeDeLogsDeAcesso(Long lastSyncLog, Date newLastSyncLog, String namedQuery) {
        HashMap<String, Object> args = new HashMap<String, Object>();
        args.put("LAST_SYNC", new Date(lastSyncLog));
        args.put("NEW_LAST_SYNC", newLastSyncLog);

        return HibernateAccessDataFacade.
                getResultListWithParamsCount(LogPedestrianAccessEntity.class, "LogPedestrianAccessEntity." + namedQuery, args);
    }

    public List<LogPedestrianAccessEntity> buscaLogsAcesso(int offset, int pageSize, Long lastSyncLog, Date newLastSyncLog, String namedQuery) {
        HashMap<String, Object> args = new HashMap<String, Object>();
        args.put("LAST_SYNC", new Date(lastSyncLog));
        args.put("NEW_LAST_SYNC", newLastSyncLog);

        return (List<LogPedestrianAccessEntity>) HibernateAccessDataFacade.
                buscaLogsDeAcessoPaginados("LogPedestrianAccessEntity." + namedQuery, args, offset, pageSize);
    }
    
    public LogPedestrianAccessEntity buscaUltimoAcesso(Long idPedestre, Integer qtdAcessosAntesSinc) {
		HashMap<String, Object> args = new HashMap<String, Object>();
		args.put("ID_PEDESTRE", idPedestre);

		String query = null;
		if (Main.loggedUser != null && Main.loggedUser.getDateNewAccess() != null) {
			query = "LogPedestrianAccessEntity.findByLastAccessbyIdPedestrianAndDate";
			args.put("DATE", Main.loggedUser.getDateNewAccess());
		} else {
			query = "LogPedestrianAccessEntity.findByLastAccessbyIdPedestrian";
		}

		LogPedestrianAccessEntity lastAccess = (LogPedestrianAccessEntity) HibernateAccessDataFacade
				.getUniqueResultWithParams(LogPedestrianAccessEntity.class, query, args);

		if (qtdAcessosAntesSinc != null && qtdAcessosAntesSinc > 0 && lastAccess == null) {
			lastAccess = new LogPedestrianAccessEntity();
			if (qtdAcessosAntesSinc % 2 == 0) {
				lastAccess.setDirection(Tipo.SAIDA);
			} else {
				lastAccess.setDirection(Tipo.ENTRADA);
			}
		}
		
		return lastAccess;
	}
    
    public LogPedestrianAccessEntity buscaUltimoAcessoVisitante(Long idPedestre, Integer qtdAcessosAntesSinc) {
		HashMap<String, Object> args = new HashMap<String, Object>();
		args.put("ID_PEDESTRE", idPedestre);

		String query = null;
		if (Main.loggedUser != null && Main.loggedUser.getDateNewAccess() != null) {
//			query = "LogPedestrianAccessEntity.findByLastAccessbyIdPedestrianAndDate";
			query = "LogPedestrianAccessEntity.findByLastAccessbyIdPedestrian";
//			args.put("DATE", Main.loggedUser.getDateNewAccess());
		} else {
			query = "LogPedestrianAccessEntity.findByLastAccessbyIdPedestrian";
		}

		LogPedestrianAccessEntity lastAccess = (LogPedestrianAccessEntity) HibernateAccessDataFacade
				.getUniqueResultWithParams(LogPedestrianAccessEntity.class, query, args);

		if (qtdAcessosAntesSinc != null && qtdAcessosAntesSinc > 0 && lastAccess == null) {
			lastAccess = new LogPedestrianAccessEntity();
			if (qtdAcessosAntesSinc % 2 == 0) {
				lastAccess.setDirection(Tipo.SAIDA);
			} else {
				lastAccess.setDirection(Tipo.ENTRADA);
			}
		}
		
		return lastAccess;
	}
    
}
