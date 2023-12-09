package com.protreino.services.usecase;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.protreino.services.entity.PedestrianAccessEntity;
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
	
	public void syncronizaUsuario(final String deviceId, final PedestrianAccessEntity pedestre) {
		if (pedestre.getRemovido() || pedestre.getFoto() == null) {
			hikiVisionIntegrationService.apagarUsuario(deviceId, pedestre.getCardNumber());
			apagaCampoDataCadastroDeFotoNaHikivision(pedestre.getId());
		} else {
			boolean isUsuarioCadastrado = true;
			if (!hikiVisionIntegrationService.isUsuarioJaCadastrado(deviceId, pedestre.getCardNumber())) {
				isUsuarioCadastrado = hikiVisionIntegrationService.adicionarUsuario(deviceId, pedestre.getCardNumber(), pedestre.getName());
			}
			
			if (hikiVisionIntegrationService.isFotoUsuarioJaCadastrada(deviceId, pedestre.getCardNumber())) {
				hikiVisionIntegrationService.apagarFotoUsuario(deviceId, pedestre.getCardNumber());
			}

			if(isUsuarioCadastrado) {
				hikiVisionIntegrationService.adicionarFotoUsuario(deviceId, pedestre.getCardNumber(), pedestre.getFoto());
				
				final boolean isCartaoJaCadastrado = hikiVisionIntegrationService.isCartaoJaCadastrado(deviceId, pedestre.getCardNumber());
	            
	            if(!isCartaoJaCadastrado) {
	            	hikiVisionIntegrationService.adicionarCartaoDePedestre(deviceId, pedestre.getCardNumber());
	            }
			}
		}
	}
	
	private void apagaCampoDataCadastroDeFotoNaHikivision(Long id) {
		PedestrianAccessEntity pedestre = (PedestrianAccessEntity) HibernateUtil.getSingleResultById(PedestrianAccessEntity.class, id);
		pedestre.setDataCadastroFotoNaHikivision(null);
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
}
