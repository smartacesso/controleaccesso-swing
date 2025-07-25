package com.protreino.services.usecase;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.protreino.services.entity.HikivisionFingerEntity;
import com.protreino.services.entity.HikivisionIntegrationErrorEntity;
import com.protreino.services.entity.HikivisonFingerErrorEntity;
import com.protreino.services.entity.PedestreRegraEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.entity.RegraEntity;
import com.protreino.services.enumeration.Finger;
import com.protreino.services.enumeration.HikivisionAction;
import com.protreino.services.exceptions.HikivisionIntegrationException;
import com.protreino.services.exceptions.InvalidPhotoException;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.repository.HikivisionFingerErrorRepository;
import com.protreino.services.repository.HikivisionIntegrationErrorRepository;
import com.protreino.services.repository.PedestreRegraRepository;
import com.protreino.services.repository.RegraRepository;
import com.protreino.services.to.hikivision.CaptureFingerPrintTO.CaptureFingerPrint;
import com.protreino.services.to.hikivision.HikivisionDeviceTO;
import com.protreino.services.to.hikivision.PlanoHorarioHikivision;
import com.protreino.services.to.hikivision.HikivisionDeviceTO.Device;
import com.protreino.services.to.hikivision.HikivisionDeviceTO.MatchList;
import com.protreino.services.utils.HikiVisionIntegrationService;
import com.protreino.services.utils.Utils;

public class HikivisionUseCases {

	private final HikiVisionIntegrationService hikiVisionIntegrationService;

	private final HikivisionIntegrationErrorRepository hikivisionIntegrationErrorRepository = new HikivisionIntegrationErrorRepository();
	private final HikivisionFingerErrorRepository hikivisionFingerErrors = new HikivisionFingerErrorRepository();
	public final RegraRepository regraRepository = new RegraRepository();
	
	public HikivisionUseCases() {
		this.hikiVisionIntegrationService = HikiVisionIntegrationService.getInstace();
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

	public void syncronizarUsuarioInDevices(final PedestrianAccessEntity pedestre, List<String> devicesById, List<String> devicesByName) {
		
		if (pedestre.isRemovido() || Objects.isNull(pedestre.getFoto()) || pedestre.isInativo()) {
			System.out.println("removendo foto");
			removerUsuarioFromDevices(pedestre, devicesById);

		} else {
			System.out.println("cadastrando foto");
			cadastrarUsuarioInDevices(pedestre, devicesById, devicesByName);
		}
	}

	public void removerUsuarioFromDevices(final PedestrianAccessEntity pedestre) {
		removerUsuarioFromDevices(pedestre, null);
	}

	public void removerUsuarioFromDevices(final PedestrianAccessEntity pedestre, List<String> devicesToSync) {
		if (Objects.isNull(devicesToSync) || devicesToSync.isEmpty()) {
			final List<Device> devices = listarDispositivos();
			if (Objects.isNull(devices) || devices.isEmpty()) {
				return;
			}

			devicesToSync = devices.stream().map(Device::getDevIndex).collect(Collectors.toList());
		}

		final List<HikivisionIntegrationErrorEntity> integrationErrors = new ArrayList<>();

		devicesToSync.forEach(deviceId -> {
			try {
				final boolean isApagadoComSucesso = hikiVisionIntegrationService.apagarUsuario(deviceId,
						pedestre.getCardNumber());
				if (!isApagadoComSucesso) {
					final String message = String.format("Erro ao apagar pedestre %s no device %s",
							pedestre.getCardNumber(), deviceId);
					logAndThrowException(message, pedestre.getCardNumber(), deviceId, HikivisionAction.REMOVE);
				}

			} catch (HikivisionIntegrationException ex) {
				integrationErrors.add(new HikivisionIntegrationErrorEntity(ex.getMessage(), ex.getCardNumber(),
						ex.getDeviceId(), ex.getHikivisionAction()));

			} catch (Exception e) {
				System.out.println(e.getMessage());
			}
		});

		pedestre.setDataCadastroFotoNaHikivision(null);
		pedestre.setFotoEnviada(null);
		pedestre.setLastAccessHikiVision(null);

		if (!integrationErrors.isEmpty()) {
			hikivisionIntegrationErrorRepository.saveAll(integrationErrors);
		}
	}

	public boolean reprocessarRemocaoInDevice(final PedestrianAccessEntity pedestre, String deviceId) {
		final boolean isApagadoComSucesso = hikiVisionIntegrationService.apagarUsuario(deviceId,
				pedestre.getCardNumber());

		if (isApagadoComSucesso) {
			pedestre.setDataCadastroFotoNaHikivision(null);
		}

		return isApagadoComSucesso;
	}
	
	private List<String> getDevicesToSync(List<String> devicesById, List<String> devicesByName) {
		if(Objects.nonNull(devicesById) && !devicesById.isEmpty()) {
			return devicesById;
		}
		
		if(Objects.nonNull(devicesByName) && !devicesByName.isEmpty()) {
			final List<Device> devices = listarDispositivos();
			if (Objects.isNull(devices) || devices.isEmpty()) {
				return new ArrayList<String>();
			}

			return devices.stream()
					.filter(device -> devicesByName.contains(device.getDevName()))
					.map(Device::getDevIndex).collect(Collectors.toList());
		}
		
		final List<Device> devices = listarDispositivos();
		if (Objects.isNull(devices) || devices.isEmpty()) {
			return new ArrayList<String>();
		}

		return devices.stream().map(Device::getDevIndex).collect(Collectors.toList());
	}

	public void cadastrarUsuarioInDevices(final PedestrianAccessEntity pedestre, List<String> devicesById, List<String> devicesByName) {
		final List<String> devicesToSync = getDevicesToSync(devicesById, devicesByName);

		final List<HikivisionIntegrationErrorEntity> integrationErrors = new ArrayList<>();

		devicesToSync.forEach(deviceId -> {
			try {
				if (!hikiVisionIntegrationService.isUsuarioJaCadastrado(deviceId, pedestre.getCardNumber())) {
					final boolean isUsuarioCadastrado = hikiVisionIntegrationService.adicionarUsuario(deviceId,
							pedestre.getCardNumber(), pedestre.getName());

					if (Boolean.FALSE.equals(isUsuarioCadastrado)) {
						final String message = String.format("Erro ao cadastrar pedestre %s no device %s",
								pedestre.getCardNumber(), deviceId);
						logAndThrowException(message, pedestre.getCardNumber(), deviceId, HikivisionAction.CREATE);
					}
				}

				if (hikiVisionIntegrationService.isFotoUsuarioJaCadastrada(deviceId, pedestre.getCardNumber())) {
					final boolean isFotoApagada = hikiVisionIntegrationService.apagarFotoUsuario(deviceId,
							pedestre.getCardNumber());
					if (Boolean.FALSE.equals(isFotoApagada)) {
						final String message = String.format("Erro ao apagar foto do pedestre %s no device %s",
								pedestre.getCardNumber(), deviceId);
						logAndThrowException(message, pedestre.getCardNumber(), deviceId, HikivisionAction.CREATE);
					}
				}

				boolean isFotoAdicionada = hikiVisionIntegrationService.adicionarFotoUsuario(deviceId,
						pedestre.getCardNumber(), pedestre.getFoto());
				if (Boolean.FALSE.equals(isFotoAdicionada)) {
					final String message = String.format("Erro ao adicionar foto do usuario %s na camera %s",
							pedestre.getCardNumber(), deviceId);
					logAndThrowException(message, pedestre.getCardNumber(), deviceId, HikivisionAction.CREATE);
				}

				final boolean isCartaoJaCadastrado = hikiVisionIntegrationService.isCartaoJaCadastrado(deviceId,
						pedestre.getCardNumber());

				if (!isCartaoJaCadastrado) {
					final boolean isCartaoAdicionado = hikiVisionIntegrationService.adicionarCartaoDePedestre(deviceId,
							pedestre.getCardNumber());
					if (Boolean.FALSE.equals(isCartaoAdicionado)) {
						final String message = String.format("Erro ao adicioar cartao do usuario %s na camera %s",
								pedestre.getCardNumber(), deviceId);
						logAndThrowException(message, pedestre.getCardNumber(), deviceId, HikivisionAction.CREATE);
					}
				}
				
			} catch (HikivisionIntegrationException ex) {
				integrationErrors.add(new HikivisionIntegrationErrorEntity(ex.getMessage(), ex.getCardNumber(),
						ex.getDeviceId(), ex.getHikivisionAction()));

			} catch (InvalidPhotoException ife) {
				throw ife;

			} catch (Exception e) {
				System.out.println(e.getMessage());
			}
		
		});
		
		
		vincularPedestreaoTemplate(pedestre, devicesById);

		pedestre.setDataCadastroFotoNaHikivision(new Date());

		if (!integrationErrors.isEmpty()) {
			hikivisionIntegrationErrorRepository.saveAll(integrationErrors);
		}
	}

	public boolean reprocessarCadastroInDevice(final PedestrianAccessEntity pedestre, String deviceId) {
		if (!hikiVisionIntegrationService.isUsuarioJaCadastrado(deviceId, pedestre.getCardNumber())) {
			final boolean isUsuarioCadastrado = hikiVisionIntegrationService.adicionarUsuario(deviceId,
					pedestre.getCardNumber(), pedestre.getName());

			if (Boolean.FALSE.equals(isUsuarioCadastrado)) {
				return false;
			}
		}

		if (hikiVisionIntegrationService.isFotoUsuarioJaCadastrada(deviceId, pedestre.getCardNumber())) {
			final boolean isFotoApagada = hikiVisionIntegrationService.apagarFotoUsuario(deviceId,
					pedestre.getCardNumber());
			if (Boolean.FALSE.equals(isFotoApagada)) {
				return false;
			}
		}

		try {
			boolean isFotoAdicionada = hikiVisionIntegrationService.adicionarFotoUsuario(deviceId,
					pedestre.getCardNumber(), pedestre.getFoto());
			if (Boolean.FALSE.equals(isFotoAdicionada)) {
				return false;
			}

		} catch (Exception e) {
			System.out.println(e.getMessage());
			return false;
		}

		final boolean isCartaoJaCadastrado = hikiVisionIntegrationService.isCartaoJaCadastrado(deviceId,
				pedestre.getCardNumber());
		if (!isCartaoJaCadastrado) {
			final boolean isCartaoAdicionado = hikiVisionIntegrationService.adicionarCartaoDePedestre(deviceId,
					pedestre.getCardNumber());
			if (Boolean.FALSE.equals(isCartaoAdicionado)) {
				return false;
			}
		}
		
		//vincularPedestreaoTemplate(pedestre, Arrays.asList(deviceId));

		pedestre.setDataCadastroFotoNaHikivision(new Date());
		return true;
	}

	private void logAndThrowException(final String message, final String cardNumber, final String deviceId,
			HikivisionAction hikivisionAction) {
		System.out.println(message);
		throw new HikivisionIntegrationException(message, cardNumber, deviceId, hikivisionAction);
	}

	public void adicionarDispositivoAndListener(final String ipAddress, final Integer port, final String userName,
			final String password, final String deviceName) {
		hikiVisionIntegrationService.adicionarDispositivo(ipAddress, port, userName, password, deviceName);
		Utils.sleep(300);
		final Device device = getDeviceByName(deviceName);

		if (Objects.nonNull(device)) {
			adicionarListnerParaCamera(device.getDevIndex());
		} else {
			System.out.println("Listener nao adicionado para camera: " + deviceName);
		}
	}

	public void adicionarListnerParaCamera(final String deviceId) {
		hikiVisionIntegrationService.adicionarListenerParaEventosCamera(deviceId, Utils.getLocalIpAddress(),
				Integer.valueOf(Utils.getPreference("tcpServerHikivisionSocketPort")));
	}

	public void adicionarDigitalNoDevice(final Finger fingerNo, final Long idUser, final String fingerData, HikivisionFingerEntity hikivisionSaved) {
		List<HikivisionDeviceTO.Device> devices = listarDispositivos();
		
		if(Objects.isNull(devices) || devices.isEmpty()) {
			return;
		}
		
		devices.forEach(device -> {
			apagarDigitalUsuario(device.getDevIndex(), idUser, fingerNo);

			final boolean vincularBiometria = vinculaDigitalUsuario(device.getDevIndex(), fingerNo, idUser, fingerData);
			if(!vincularBiometria) {
				HibernateAccessDataFacade.save(HikivisonFingerErrorEntity.class,
						new HikivisonFingerErrorEntity(idUser, "Digital Nao vinculada", device.getDevIndex(), fingerNo, hikivisionSaved));
			}
		});
	}

	public Optional<CaptureFingerPrint> coletarBiometriabiometria(final String deviceId, final Finger fingerNo) {
		final Optional<CaptureFingerPrint> digitalCadastrada = hikiVisionIntegrationService
				.capturaDigitalUsuario(deviceId, fingerNo);

		if (!digitalCadastrada.isPresent()) {
			System.out.println(String.format("Nao foi possivel cadastrar a digital cadastrada %s", fingerNo.name()));
			return Optional.empty();
		}

		return digitalCadastrada;
	}

	private boolean vinculaDigitalUsuario(final String deviceId, final Finger fingerNo, final Long idUser, final String fingerData) {
		return hikiVisionIntegrationService.vinculaDigitalUsuario(deviceId ,idUser, fingerNo, fingerData);

	}
	
	public void processarBiometriasComErros() {
		Long limit  = 100l;
		List<HikivisonFingerErrorEntity> biometricsWithErros = hikivisionFingerErrors.findAllLimited(limit);
		//TODO PAGINAR
		
		biometricsWithErros.forEach(biometric -> {		
			final boolean vincularBiometria = vinculaDigitalUsuario(biometric.getDeviceId(),
					biometric.getHikisionFingerEntity().getFingerNo(), biometric.getIdUser(),
					biometric.getHikisionFingerEntity().getFingerData());

			if (vincularBiometria) {
				//apagar biometria
				HibernateAccessDataFacade.remove(biometric);
			} else {
				System.out.println(String.format("Nao foi possivel reprocessar a biometria do usuario %d",
						biometric.getIdUser(), null));
			}
		});

	}

	private boolean apagarDigitalUsuario(final String deviceId, Long idUser, Finger fingerNo) {
		return hikiVisionIntegrationService.apagarDigitalUsuario(deviceId, idUser, fingerNo);
	}

	private HikivisionDeviceTO.Device getDeviceByName(final String deviceName) {
		final HikivisionDeviceTO devices = hikiVisionIntegrationService.listarDispositivos();

		if (devices == null || devices.getSearchResult().getTotalMatches() == 0) {
			return null;
		}

		for (MatchList matchList : devices.getSearchResult().getMatchList()) {
			if (deviceName.equalsIgnoreCase(matchList.getDevice().getDevName())) {
				return matchList.getDevice();
			}
		}

		return null;
	}
	
	public void liberaCameraRemoto(final String deviceId) {
		hikiVisionIntegrationService.liberaRemotoCamera(deviceId);
	}

	public void apagarFotosUsuario(final PedestrianAccessEntity pedestre) {
		final List<Device> devices = listarDispositivos();

		devices.forEach(device -> {
			final boolean isFotoUsuarioJaCadastrada = hikiVisionIntegrationService
					.isFotoUsuarioJaCadastrada(device.getDevIndex(), pedestre.getCardNumber());

			if (isFotoUsuarioJaCadastrada) {
				hikiVisionIntegrationService.apagarFotoUsuario(device.getDevIndex(), pedestre.getCardNumber());
			}
		});

		pedestre.setDataCadastroFotoNaHikivision(null);
		pedestre.setLastAccessHikiVision(null);
	}
	
	public void sincronizarHorarioHIkivision(Integer idPlan, PlanoHorarioHikivision config) {
		final List<Device> devices = listarDispositivos();
		
		devices.forEach(device -> {
			String nomeNormalizado =  device.getDevName().trim().toLowerCase();
			if(!nomeNormalizado.contains("refeitorio") && !nomeNormalizado.contains("refeitório")) {
				hikiVisionIntegrationService.criarPlanoDeHorario(device.getDevIndex(), idPlan, config);
			}else {
				System.out.println("Horario não enviado para device, pois nao faz parte");
			}
		});

	}
	
	
	public void sincronizarTemplateHIkivision(Integer idPlan, Integer idTemplate, String nomeTemplate) {
		final List<Device> devices = listarDispositivos();

		devices.forEach(device -> {
			String nomeNormalizado =  device.getDevName().trim().toLowerCase();
			if(!nomeNormalizado.contains("refeitorio") && !nomeNormalizado.contains("refeitório")) {
				hikiVisionIntegrationService.criarTemplateComHorario(device.getDevIndex(),idTemplate , idPlan, nomeTemplate);
			}else {
				System.out.println("Horario não enviado para device, pois nao faz parte");
			}
			
		});

	}
	
//	public void vincularPedestreaoTemplate(final PedestrianAccessEntity pedestre, List<String> devicesToSync) {
//		if(Utils.getPreferenceAsBoolean("hikiVisionPlanHorario")) {
//			if (Objects.isNull(devicesToSync) || devicesToSync.isEmpty()) {
//				final List<Device> devices = listarDispositivos();
//				if (Objects.isNull(devices) || devices.isEmpty()) {
//					return;
//				}				
//				
//				devicesToSync = devices.stream().filter(device -> !device.getDevName().toLowerCase().trim().contains("refeitorio") ||
//						!device.getDevName().toLowerCase().trim().contains("refeitorio"))
//						.map(Device::getDevIndex).collect(Collectors.toList());
//			}
//			
//			final List<Device> devices = listarDispositivos();
//			if (Objects.isNull(devices) || devices.isEmpty()) {
//				return;
//			}				
//			
//			System.out.println("devices to sync : " + devicesToSync.size());
//			//nao quero sync os devices do refeitorio como fazer isso? posso pegar a lista dos que tem nome refeitorio e remover da lista tosync
//			
//		    Optional<PedestreRegraEntity> regraAtiva = pedestre.getRegraAtiva();
//		    
//
//		    if (!regraAtiva.isPresent() || Objects.isNull(regraAtiva.get().getRegra().getHorarios())) {
//		        System.out.println("sem regras de horarios");
//		        devicesToSync.forEach(device -> {
//
//			        hikiVisionIntegrationService.vincularTemplateNoUsuario(
//			            device,
//			            pedestre.getCardNumber(),
//			            1
//			        );
//			    });
//		        return;
//		    }
//
//		    // Pegamos a regra inicial
//		    RegraEntity regra = regraAtiva.get().getRegra();
//
//		    if (Objects.isNull(regra.getIdTemplate())) {
//		        SincronismoHorariosHikivision sincronismoHorariosHikivision = new SincronismoHorariosHikivision();
//		        sincronismoHorariosHikivision.execute();
//
//		        // Buscar a regra atualizada do banco
//		        regra = regraRepository.buscaRegraById(regra.getId());
//		    }
//
//		    // A regra usada nos dispositivos será sempre a mais atual
//		    final RegraEntity regraFinal = regra;
//
//		    devicesToSync.forEach(device -> {
//		        hikiVisionIntegrationService.vincularTemplateNoUsuario(
//		            device,
//		            pedestre.getCardNumber(),
//		            regraFinal.getIdTemplate()
//		        );
//		    });
//
//		    System.out.println("regra atualizada com sucesso");
//		}else {
//			 System.out.println("Horario hikivision desabilitado");
//		}
//	}
	
	public void vincularPedestreaoTemplate(final PedestrianAccessEntity pedestre, List<String> devicesToSync) {
	    if (Utils.getPreferenceAsBoolean("hikiVisionPlanHorario")) {
	        final List<Device> devices = listarDispositivos();
	        if (Objects.isNull(devices) || devices.isEmpty()) {
	            return;
	        }

	        // Se devicesToSync vier nulo ou vazio, inicializamos com todos os dispositivos disponíveis
	        if (Objects.isNull(devicesToSync) || devicesToSync.isEmpty()) {
	            devicesToSync = devices.stream()
	                .map(Device::getDevIndex)
	                .collect(Collectors.toList());
	        }

	        // Removendo dispositivos que contenham "refeitório" no nome
	        Set<String> devicesRefeitorio = devices.stream()
	            .filter(device -> device.getDevName().toLowerCase().trim().contains("refeitorio")
	            		|| device.getDevName().toLowerCase().trim().contains("refeitório"))
	            .map(Device::getDevIndex)
	            .collect(Collectors.toSet());

	        devicesToSync.removeAll(devicesRefeitorio);
	        
	        if(devicesToSync.size() == 0) {
	        	System.out.println("Sem devices para sincronizar");
	        	return;
	        }

	        System.out.println("Devices para vincular horarios: " + devicesToSync.size());

	        Optional<PedestreRegraEntity> regraAtiva = pedestre.getRegraAtiva();

	        if (!regraAtiva.isPresent() || Objects.isNull(regraAtiva.get().getRegra().getHorarios())) {
	            System.out.println("Sem regras de horários");

	            // Buscar regra associada ao pedestre, caso ela exista
	            PedestreRegraEntity pedestreRegra = PedestreRegraRepository.buscarRegraPedestre(pedestre.getId());
	            
	            if (pedestreRegra != null) {
	            	regraAtiva = Optional.of(pedestreRegra);
	                System.out.println("Regra carregada do banco");
	            } else {
	                System.out.println("Nenhuma regra encontrada no banco");
		            devicesToSync.forEach(device -> {
		                hikiVisionIntegrationService.vincularTemplateNoUsuario(
		                    device,
		                    pedestre.getCardNumber(),
		                    1 // ID padrão quando não há regra
		                );
		            });
		            return;
	            }
	           
	        }

	        // Pegamos a regra inicial
	        RegraEntity regra = regraAtiva.get().getRegra();

	        if (Objects.isNull(regra.getIdTemplate())) {
	            SincronismoHorariosHikivision sincronismoHorariosHikivision = new SincronismoHorariosHikivision();
	            sincronismoHorariosHikivision.execute();

	            // Buscar a regra atualizada do banco
	            regra = regraRepository.buscaRegraById(regra.getId());
	        }

	        // A regra usada nos dispositivos será sempre a mais atual
	        final RegraEntity regraFinal = regra;

	        devicesToSync.forEach(device -> {
	            hikiVisionIntegrationService.vincularTemplateNoUsuario(
	                device,
	                pedestre.getCardNumber(),
	                regraFinal.getIdTemplate()
	            );
	        });

	        System.out.println("Regra atualizada com sucesso");
	    } else {
	        System.out.println("Horário Hikivision desabilitado");
	    }
	}


	
	public void vincularPedestreaoTemplate(final PedestrianAccessEntity pedestre) {
		vincularPedestreaoTemplate(pedestre, null);
	}

}
