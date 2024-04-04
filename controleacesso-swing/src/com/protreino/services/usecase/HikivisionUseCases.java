package com.protreino.services.usecase;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import com.protreino.services.entity.HikivisionIntegrationErrorEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.enumeration.HikivisionAction;
import com.protreino.services.exceptions.HikivisionIntegrationException;
import com.protreino.services.repository.HikivisionIntegrationErrorRepository;
import com.protreino.services.to.hikivision.HikivisionDeviceTO;
import com.protreino.services.to.hikivision.HikivisionDeviceTO.Device;
import com.protreino.services.to.hikivision.HikivisionDeviceTO.MatchList;
import com.protreino.services.utils.HikiVisionIntegrationService;
import com.protreino.services.utils.Utils;

public class HikivisionUseCases {

	private final HikiVisionIntegrationService hikiVisionIntegrationService;
	
	private final HikivisionIntegrationErrorRepository hikivisionIntegrationErrorRepository = new HikivisionIntegrationErrorRepository();
	
	public HikivisionUseCases() {
		this.hikiVisionIntegrationService = HikiVisionIntegrationService.getInstace();
	}
	
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
	
	public void syncronizarUsuarioInDevices(final PedestrianAccessEntity pedestre) {
		syncronizarUsuarioInDevices(pedestre, null);
	}
	
	public void syncronizarUsuarioInDevices(final PedestrianAccessEntity pedestre, List<String> devicesToSync) {
		if(pedestre.isRemovido() || Objects.isNull(pedestre.getFoto()) || pedestre.isInativo()) {
			removerUsuarioFromDevices(pedestre, devicesToSync);
		
		} else {
			cadastrarUsuarioInDevices(pedestre, devicesToSync);
		}
	}
	
	public void removerUsuarioFromDevices(final PedestrianAccessEntity pedestre) {
		removerUsuarioFromDevices(pedestre, null);
	}
	
	public void removerUsuarioFromDevices(final PedestrianAccessEntity pedestre, List<String> devicesToSync) {
		if(Objects.isNull(devicesToSync) || devicesToSync.isEmpty()) {
			final List<Device> devices = listarDispositivos();
			if(Objects.isNull(devices) || devices.isEmpty()) {
				return;
			}
			
			devicesToSync = devices.stream()
								.map(Device::getDevIndex)
								.collect(Collectors.toList());
		}
		
		final List<HikivisionIntegrationErrorEntity> integrationErrors = new ArrayList<>();
		
		devicesToSync.forEach(deviceId -> {
			try {
				final boolean isApagadoComSucesso = hikiVisionIntegrationService.apagarUsuario(deviceId, pedestre.getCardNumber());
				if(!isApagadoComSucesso) {
					final String message = String.format("Erro ao apagar pedestre %s no device %s", pedestre.getCardNumber(), deviceId);
					logAndThrowException(message, pedestre.getCardNumber(), deviceId, HikivisionAction.REMOVE);
				}
				
			} catch(HikivisionIntegrationException ex) {
				integrationErrors.add(new HikivisionIntegrationErrorEntity(ex.getMessage(), ex.getCardNumber(), ex.getDeviceId(), ex.getHikivisionAction()));
				
			} catch (Exception e) {
				System.out.println(e.getMessage());
			}
		});
		
		pedestre.setDataCadastroFotoNaHikivision(null);
		pedestre.setLastAccessHikiVision(null);
		
		if(!integrationErrors.isEmpty()) {
			hikivisionIntegrationErrorRepository.saveAll(integrationErrors);
		}
	}
	
	public boolean reprocessarRemocaoInDevice(final PedestrianAccessEntity pedestre, String deviceId) {
		final boolean isApagadoComSucesso = hikiVisionIntegrationService.apagarUsuario(deviceId, pedestre.getCardNumber());
		
		if(isApagadoComSucesso) {
			pedestre.setDataCadastroFotoNaHikivision(null);
		}

		return isApagadoComSucesso;
	}
	
	public void cadastrarUsuarioInDevices(final PedestrianAccessEntity pedestre) {
		cadastrarUsuarioInDevices(pedestre, null);
	}
	
	public void cadastrarUsuarioInDevices(final PedestrianAccessEntity pedestre, List<String> devicesToSync) {
		if(Objects.isNull(devicesToSync) || devicesToSync.isEmpty()) {
			final List<Device> devices = listarDispositivos();
			if(Objects.isNull(devices) || devices.isEmpty()) {
				return;
			}
			
			devicesToSync = devices.stream()
								.map(Device::getDevIndex)
								.collect(Collectors.toList());
		}
		
		final List<HikivisionIntegrationErrorEntity> integrationErrors = new ArrayList<>();
		
		devicesToSync.forEach(deviceId -> {
			try {
				if (!hikiVisionIntegrationService.isUsuarioJaCadastrado(deviceId, pedestre.getCardNumber())) {
					final boolean isUsuarioCadastrado = hikiVisionIntegrationService.adicionarUsuario(deviceId, pedestre.getCardNumber(), pedestre.getName());

					if(Boolean.FALSE.equals(isUsuarioCadastrado)) {
						final String message = String.format("Erro ao cadastrar pedestre %s no device %s", pedestre.getCardNumber(), deviceId);
						logAndThrowException(message, pedestre.getCardNumber(), deviceId, HikivisionAction.CREATE);
					}
				}
				
				if (hikiVisionIntegrationService.isFotoUsuarioJaCadastrada(deviceId, pedestre.getCardNumber())) {
					final boolean isFotoApagada = hikiVisionIntegrationService.apagarFotoUsuario(deviceId, pedestre.getCardNumber());
					if(Boolean.FALSE.equals(isFotoApagada)) {
						final String message = String.format("Erro ao apagar foto do pedestre %s no device %s", pedestre.getCardNumber(), deviceId);
						logAndThrowException(message, pedestre.getCardNumber(), deviceId, HikivisionAction.CREATE);
					}
				}

				boolean isFotoAdicionada = hikiVisionIntegrationService.adicionarFotoUsuario(deviceId, pedestre.getCardNumber(), pedestre.getFoto());
				if(Boolean.FALSE.equals(isFotoAdicionada)) {
					final String message = String.format("Erro ao adicionar foto do usuario %s na camera %s", pedestre.getCardNumber(), deviceId);
					logAndThrowException(message, pedestre.getCardNumber(), deviceId, HikivisionAction.CREATE);
				}
				
				final boolean isCartaoJaCadastrado = hikiVisionIntegrationService.isCartaoJaCadastrado(deviceId, pedestre.getCardNumber());
		        
		        if(!isCartaoJaCadastrado) {
		        	final boolean isCartaoAdicionado = hikiVisionIntegrationService.adicionarCartaoDePedestre(deviceId, pedestre.getCardNumber());
		        	if(Boolean.FALSE.equals(isCartaoAdicionado)) {
						final String message = String.format("Erro ao adicioar cartão do usuario %s na camera %s", pedestre.getCardNumber(), deviceId);
						logAndThrowException(message, pedestre.getCardNumber(), deviceId, HikivisionAction.CREATE);
					}
		        }
		        
			} catch(HikivisionIntegrationException ex) {
				integrationErrors.add(new HikivisionIntegrationErrorEntity(ex.getMessage(), ex.getCardNumber(), ex.getDeviceId(), ex.getHikivisionAction()));
				
			} catch (Exception e) {
				System.out.println(e.getMessage());
			}
		});
		
		pedestre.setDataCadastroFotoNaHikivision(new Date());
		
		if(!integrationErrors.isEmpty()) {
			hikivisionIntegrationErrorRepository.saveAll(integrationErrors);
		}
	}
	
	public boolean reprocessarCadastroInDevice(final PedestrianAccessEntity pedestre, String deviceId) {
		if (!hikiVisionIntegrationService.isUsuarioJaCadastrado(deviceId, pedestre.getCardNumber())) {
			final boolean isUsuarioCadastrado = hikiVisionIntegrationService.adicionarUsuario(deviceId, pedestre.getCardNumber(), pedestre.getName());

			if(Boolean.FALSE.equals(isUsuarioCadastrado)) {
				return false;
			}
		}
		
		if (hikiVisionIntegrationService.isFotoUsuarioJaCadastrada(deviceId, pedestre.getCardNumber())) {
			final boolean isFotoApagada = hikiVisionIntegrationService.apagarFotoUsuario(deviceId, pedestre.getCardNumber());
			if(Boolean.FALSE.equals(isFotoApagada)) {
				return false;
			}
		}

		boolean isFotoAdicionada = hikiVisionIntegrationService.adicionarFotoUsuario(deviceId, pedestre.getCardNumber(), pedestre.getFoto());
		if(Boolean.FALSE.equals(isFotoAdicionada)) {
			return false;
		}
		
		final boolean isCartaoJaCadastrado = hikiVisionIntegrationService.isCartaoJaCadastrado(deviceId, pedestre.getCardNumber());
        if(!isCartaoJaCadastrado) {
        	final boolean isCartaoAdicionado = hikiVisionIntegrationService.adicionarCartaoDePedestre(deviceId, pedestre.getCardNumber());
        	if(Boolean.FALSE.equals(isCartaoAdicionado)) {
				return false;
			}
        }
        
        pedestre.setDataCadastroFotoNaHikivision(new Date());
        return true;
	}
	
	private void logAndThrowException(final String message, final String cardNumber, final String deviceId, HikivisionAction hikivisionAction) {
		System.out.println(message);
		throw new HikivisionIntegrationException(message, cardNumber, deviceId, hikivisionAction);
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
	
	public void apagarFotosUsuario(final PedestrianAccessEntity pedestre) {
		final List<Device> devices = listarDispositivos();
		
		devices.forEach(device -> {
			final boolean isFotoUsuarioJaCadastrada = hikiVisionIntegrationService.isFotoUsuarioJaCadastrada(device.getDevIndex(), pedestre.getCardNumber());
			
			if(isFotoUsuarioJaCadastrada) {
				hikiVisionIntegrationService.apagarFotoUsuario(device.getDevIndex(), pedestre.getCardNumber());
			}
		});
		
		pedestre.setDataCadastroFotoNaHikivision(null);
		pedestre.setLastAccessHikiVision(null);
	}

}
