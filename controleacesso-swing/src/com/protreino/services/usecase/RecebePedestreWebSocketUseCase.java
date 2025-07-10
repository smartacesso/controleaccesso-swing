package com.protreino.services.usecase;

import java.util.List;
import java.util.Objects;

import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.main.Main;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.repository.LocalRepository;
import com.protreino.services.to.PedestrianAccessTO;
import com.protreino.services.to.hikivision.WebSocketPedestrianAccessTO;
import com.protreino.services.utils.Utils;

public class RecebePedestreWebSocketUseCase {
	
    private final HikivisionUseCases hikivisionUseCases = new HikivisionUseCases();
    private final LocalRepository localRepository = new LocalRepository();

    public void execute(final WebSocketPedestrianAccessTO PedestrianAccessTO) {
    	if (Main.loggedUser == null) {
        	return;
        }

        PedestrianAccessEntity existentAthleteAccess = (PedestrianAccessEntity)
            HibernateAccessDataFacade.getAllPedestresById(PedestrianAccessTO.getId());

        PedestrianAccessEntity pedestre;

        if (Objects.isNull(existentAthleteAccess)) {
        	System.out.println("id : " + PedestrianAccessTO.getId());
        	pedestre = new PedestrianAccessEntity(PedestrianAccessTO);            
            HibernateAccessDataFacade.save(PedestrianAccessEntity.class, pedestre);
            System.out.println("Salvando pedestre/visitante recebido da web : " + pedestre.getName());
        } else {
        	existentAthleteAccess.updateWenSocketPestrianAccess(PedestrianAccessTO);
        	pedestre = existentAthleteAccess;
        }

        if (Utils.isHikivisionConfigValid()) {
        	try {
        		List<String> devicesName = localRepository.getDevicesNameByPedestreLocal(pedestre);
        		hikivisionUseCases.syncronizarUsuarioInDevices(pedestre, null, devicesName);
        	} catch (Exception e) {
        		System.out.println(e.getMessage());
    		}
        }

        // Só atualiza se for um existente (os novos já foram salvos acima)
        if (!Objects.isNull(existentAthleteAccess)) {
        	HibernateAccessDataFacade.update(PedestrianAccessEntity.class, pedestre);
        	System.out.println("Atualizando pedestre/visitante recebido da web : " + pedestre.getName());
        }
    }
}
