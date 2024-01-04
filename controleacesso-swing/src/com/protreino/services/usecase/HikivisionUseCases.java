package com.protreino.services.usecase;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.exceptions.HikivisionIntegrationException;
import com.protreino.services.to.hikivision.HikivisionDeviceTO;
import com.protreino.services.to.hikivision.HikivisionDeviceTO.Device;
import com.protreino.services.to.hikivision.HikivisionDeviceTO.MatchList;
import com.protreino.services.utils.HibernateUtil;
import com.protreino.services.utils.HikiVisionIntegrationService;
import com.protreino.services.utils.Utils;

public class HikivisionUseCases {

	private final HikiVisionIntegrationService hikiVisionIntegrationService;
	
	public HikivisionUseCases(final HikiVisionIntegrationService hikiVisionIntegrationService) {
		this.hikiVisionIntegrationService = hikiVisionIntegrationService;
	}
	
	public boolean getSystemInformation() {
		return hikiVisionIntegrationService.getSystemInformation();
	}
	
	public List<HikivisionDeviceTO.Device> listarDispositivos() {
		final HikivisionDeviceTO hikivisionDeviceTO = hikiVisionIntegrationService.listarDispositivos();
		
		if (hikivisionDeviceTO == null || hikivisionDeviceTO.getSearchResult() == null
				|| hikivisionDeviceTO.getSearchResult().getTotalMatches() == 0) {
			return new ArrayList<>();
		}
		
		final List<HikivisionDeviceTO.Device> devices = new ArrayList<>();
		for (MatchList matchList : hikivisionDeviceTO.getSearchResult().getMatchList()) {
			devices.add(matchList.getDevice());
		}
		
		return devices;
	}
	
	public void syncronizarUsuarioAllDevices(final PedestrianAccessEntity pedestre) {
		final List<Device> devices = listarDispositivos();
		
		devices.forEach(device -> {
			try {
				syncronizaUsuario(device.getDevIndex(), pedestre);

			} catch (Exception e) {
				System.out.println(e.getMessage());
			}
		});
	}
	
	public void syncronizaUsuario(final String deviceId, final PedestrianAccessEntity pedestre) {
		if (Boolean.TRUE.equals(pedestre.getRemovido()) || pedestre.getFoto() == null || "INATIVO".equals(pedestre.getStatus())) {
			hikiVisionIntegrationService.apagarUsuario(deviceId, pedestre.getCardNumber());
			apagaCampoDataCadastroDeFotoNaHikivision(pedestre.getId());
		
		} else {
			boolean isUsuarioCadastrado = true;
			if (!hikiVisionIntegrationService.isUsuarioJaCadastrado(deviceId, pedestre.getCardNumber())) {
				isUsuarioCadastrado = hikiVisionIntegrationService.adicionarUsuario(deviceId, pedestre.getCardNumber(), pedestre.getName());

				if(Boolean.FALSE.equals(isUsuarioCadastrado)) {
					final String message = String.format("Erro ao cadastrar pedestre %s no device %s", pedestre.getCardNumber(), deviceId);
					logAndThrowException(message);
				}
			}
			
			if (hikiVisionIntegrationService.isFotoUsuarioJaCadastrada(deviceId, pedestre.getCardNumber())) {
				final boolean isFotoApagada = hikiVisionIntegrationService.apagarFotoUsuario(deviceId, pedestre.getCardNumber());
				if(Boolean.FALSE.equals(isFotoApagada)) {
					final String message = String.format("Erro ao apagar foto do pedestre %s no device %s", pedestre.getCardNumber(), deviceId);
					logAndThrowException(message);
				}
			}

			boolean isFotoAdicionada = hikiVisionIntegrationService.adicionarFotoUsuario(deviceId, pedestre.getCardNumber(), pedestre.getFoto());
			if(Boolean.FALSE.equals(isFotoAdicionada)) {
				final String message = String.format("Erro ao adicioar foto do usuario %s na camera %s", pedestre.getCardNumber(), deviceId);
				logAndThrowException(message);
			}
			
			final boolean isCartaoJaCadastrado = hikiVisionIntegrationService.isCartaoJaCadastrado(deviceId, pedestre.getCardNumber());
            
            if(!isCartaoJaCadastrado) {
            	final boolean isCartaoAdicionado = hikiVisionIntegrationService.adicionarCartaoDePedestre(deviceId, pedestre.getCardNumber());
            	if(Boolean.FALSE.equals(isCartaoAdicionado)) {
    				final String message = String.format("Erro ao adicioar cartão do usuario %s na camera %s", pedestre.getCardNumber(), deviceId);
    				logAndThrowException(message);
    			}
            }
		}
	}
	
	private void logAndThrowException(final String message) {
		System.out.println(message);
		throw new HikivisionIntegrationException(message);
	}
	
	private void apagaCampoDataCadastroDeFotoNaHikivision(final Long idPedestre) {
		final PedestrianAccessEntity pedestre = (PedestrianAccessEntity) HibernateUtil.getSingleResultById(PedestrianAccessEntity.class, idPedestre);
		pedestre.setDataCadastroFotoNaHikivision(null);
		pedestre.setLastAccessHikiVision(null);
		HibernateUtil.save(PedestrianAccessEntity.class, pedestre);
	}

	public void adicionarDispositivoAndListener(final String ipAddress, final Integer port, final String userName, final String password,
			final String deviceName) {
		hikiVisionIntegrationService.adicionarDispositivo(ipAddress, port, userName, password, deviceName);
		Utils.sleep(300);
		final Device device = getDeviceByName(deviceName);

		if(Objects.nonNull(device)) {
			adicionarListnerParaCamera(device.getDevIndex());
		} else {
			System.out.println("Listener não adicionado para camera: " + deviceName);
		}
	}
	
	public void adicionarListnerParaCamera(final String deviceId) {
		hikiVisionIntegrationService.adicionarListenerParaEventosCamera(deviceId, Utils.getLocalIpAddress(), 
				Integer.valueOf(Utils.getPreference("tcpServerHikivisionSocketPort")));
	}
	
	private HikivisionDeviceTO.Device getDeviceByName(final String deviceName) {
		final HikivisionDeviceTO devices = hikiVisionIntegrationService.listarDispositivos();
		
		if (devices == null || devices.getSearchResult().getTotalMatches() == 0) {
            return null;
        }

        for (MatchList matchList : devices.getSearchResult().getMatchList()) {
        	if(deviceName.equalsIgnoreCase(matchList.getDevice().getDevName())) {
        		return matchList.getDevice();
        	}
        }
        
        return null;
	}
	
	public void apagarUsuario(final PedestrianAccessEntity pedestrianAccessEntity, final String deviceId ) {
		hikiVisionIntegrationService.apagarUsuario(pedestrianAccessEntity.getCardNumber(), deviceId);
		apagaCampoDataCadastroDeFotoNaHikivision(pedestrianAccessEntity.getId());
	}
	
	public void apagarFotosUsuario(final String cardNumber) {
		final List<Device> devices = listarDispositivos();
		
		devices.forEach(device -> {
			final boolean isFotoUsuarioJaCadastrada = hikiVisionIntegrationService.isFotoUsuarioJaCadastrada(device.getDevIndex(), cardNumber);
			
			if(isFotoUsuarioJaCadastrada) {
				hikiVisionIntegrationService.apagarFotoUsuario(device.getDevIndex(), cardNumber);				
			}
		});
	}
	
	public void capturaFoto() {
		hikiVisionIntegrationService.capturePicture();
	}
	

}
