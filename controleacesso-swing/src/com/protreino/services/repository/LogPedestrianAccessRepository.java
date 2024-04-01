package com.protreino.services.repository;

import java.util.Date;
import java.util.HashMap;
import java.util.List;

import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.utils.HibernateUtil;

public class LogPedestrianAccessRepository {

	public int buscaQuantidadeDeLogsDeAcesso(Long lastSyncLog, Date newLastSyncLog, String namedQuery) {
        HashMap<String, Object> args = new HashMap<String, Object>();
        args.put("LAST_SYNC", new Date(lastSyncLog));
        args.put("NEW_LAST_SYNC", newLastSyncLog);

        return HibernateUtil.
                getResultListWithParamsCount(LogPedestrianAccessEntity.class, "LogPedestrianAccessEntity." + namedQuery, args);
    }

    public List<LogPedestrianAccessEntity> buscaLogsAcesso(int offset, int pageSize, Long lastSyncLog, Date newLastSyncLog, String namedQuery) {
        HashMap<String, Object> args = new HashMap<String, Object>();
        args.put("LAST_SYNC", new Date(lastSyncLog));
        args.put("NEW_LAST_SYNC", newLastSyncLog);

        return (List<LogPedestrianAccessEntity>) HibernateUtil.
                buscaLogsDeAcessoPaginados("LogPedestrianAccessEntity." + namedQuery, args, offset, pageSize);
    }
    
}
