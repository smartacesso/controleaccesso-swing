package com.protreino.services.usecase;

import java.util.Date;
import java.util.List;
import java.util.Objects;

import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.exceptions.CartaoInvalidException;
import com.protreino.services.exceptions.InvalidPhotoException;
import com.protreino.services.exceptions.UsuarioInvalidException;
import com.protreino.services.main.Main;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.repository.LocalRepository;
import com.protreino.services.services.WebSocketCadastroClientService;
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
            System.out.println("Salvando pedestre/visitante recebido da web : " + pedestre.getName());
        } else {
        	existentAthleteAccess.updateWenSocketPestrianAccess(PedestrianAccessTO);
        	pedestre = existentAthleteAccess;
        }
        
		if(Objects.isNull(pedestre.getFoto()) || pedestre.getFoto().length == 0) {
			WebSocketCadastroClientService.enviarParaServidor("error");
		}else if (Utils.isHikivisionConfigValid()) {

			try {
				List<String> devicesName = localRepository.getDevicesNameByPedestreLocal(pedestre);
				hikivisionUseCases.syncronizarUsuarioInDevices(pedestre, null, devicesName);
				pedestre.setDataCadastroFotoNaHikivision(new Date());
				if (pedestre.isAtivo() && pedestre.isVisitante()) {
					pedestre.setFotoEnviada(true);
				}
				
				WebSocketCadastroClientService.enviarParaServidor("ok");
			} catch (UsuarioInvalidException ufe) {
				WebSocketCadastroClientService.enviarParaServidor("UsuarioErro");
			} catch (CartaoInvalidException cfe) {
				WebSocketCadastroClientService.enviarParaServidor("CartaoErro");
			} catch (InvalidPhotoException ife) {
				WebSocketCadastroClientService.enviarParaServidor("FotoErro");
			} catch (Exception e) {
				System.out.println(e.getMessage());
			}
		}

        // Só atualiza se for um existente (os novos já foram salvos acima)
        if (!Objects.isNull(existentAthleteAccess)) {
        	HibernateAccessDataFacade.update(PedestrianAccessEntity.class, pedestre);
        	System.out.println("ATUALIZANDO pedestre/visitante recebido da web : " + pedestre.getName());
        }else {
        	System.out.println("SALVANDO pedestre/visitante recebido da web : " + pedestre.getName());
        	 HibernateAccessDataFacade.save(PedestrianAccessEntity.class, pedestre);
        }
    }
}
