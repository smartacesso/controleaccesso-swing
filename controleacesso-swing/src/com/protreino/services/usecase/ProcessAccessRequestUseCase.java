package com.protreino.services.usecase;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import com.protreino.services.constants.Origens;
import com.protreino.services.constants.Tipo;
import com.protreino.services.devices.ControlIdDevice;
import com.protreino.services.devices.Device;
import com.protreino.services.devices.FacialDevice;
import com.protreino.services.devices.TopDataAcessoDevice;
import com.protreino.services.devices.TopDataDevice;
import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.entity.PedestreRegraEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.entity.PedestrianEquipamentEntity;
import com.protreino.services.enumeration.BroadcastMessageType;
import com.protreino.services.enumeration.NotificationType;
import com.protreino.services.enumeration.TipoEscala;
import com.protreino.services.enumeration.VerificationResult;
import com.protreino.services.exceptions.QrcodeVencidoException;
import com.protreino.services.main.Main;
import com.protreino.services.repository.DeviceRepository;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.repository.LogPedestrianAccessRepository;
import com.protreino.services.repository.PedestrianAccessRepository;
import com.protreino.services.to.AttachedTO;
import com.protreino.services.to.BroadcastMessageTO;
import com.protreino.services.utils.Utils;
import com.topdata.easyInner.enumeradores.Enumeradores;

public class ProcessAccessRequestUseCase {

	private final LogPedestrianAccessRepository logPedestrianAccessRepository = new LogPedestrianAccessRepository();
	private final PedestrianAccessRepository pedestrianAccessRepository = new PedestrianAccessRepository();
	
	/**
	 * @param codigo
	 * @return Object[] { resultadoVerificacao, userName, matchedPedestrianAccess }
	 */
	public Object[] processAccessRequest(String codigo, String location, boolean createNotification, boolean ignoraRegras) {
		return processAccessRequest(codigo, location, null, null, false, createNotification, null, false, ignoraRegras);
	}

	/**
	 * @param codigo
	 * @return Object[] { resultadoVerificacao, userName, matchedPedestrianAccess }
	 */
	public Object[] processAccessRequest(String codigo, String equipamento, Integer origem,
			String location, boolean usaUrna, boolean createNotification, Date data, Boolean digitaisCatraca, Boolean ignoraRegras) {
		return processAccessRequest(codigo, location, null, equipamento, origem, usaUrna, createNotification, data, digitaisCatraca, ignoraRegras);
	}

	/**
	 * @param codigo
	 * @return Object[] { resultadoVerificacao, userName, matchedPedestrianAccess }
	 */
	public Object[] processAccessRequest(String codigo, String equipamento, Integer origem,
			String location, boolean usaUrna, boolean createNotification, boolean ignoraRegras) {
		return processAccessRequest(codigo, location, null, equipamento, origem, usaUrna, createNotification, null, false, ignoraRegras);
	}

	/**
	 * @param codigo
	 * @return Object[] { resultadoVerificacao, userName, matchedPedestrianAccess }
	 */
	@SuppressWarnings("unchecked")
	private Object[] processAccessRequest(String codigo, String location, String direction, String equipament,
			Integer origem, Boolean usaUrna, boolean createNotification, Date data, Boolean digitaisCatraca, Boolean ignoraRegras) {
		VerificationResult resultadoVerificacao = VerificationResult.NOT_FOUND;
		String userName = "";
		String motivo = "";
		byte[] foto = null;
		PedestrianAccessEntity matchedPedestrianAccess = null;

		try {
			if (Utils.isNullOrEmpty(codigo)) {
				return new Object[] { resultadoVerificacao, userName, matchedPedestrianAccess };
			}

			// processa dados do qrcode
			if (codigo.contains("_")) {
				try {
					matchedPedestrianAccess = trataPedestreQRCode(codigo);
				} catch (QrcodeVencidoException e) {
					if (createNotification) {
						Utils.createNotification("QRCode do usu·rio expirado.", NotificationType.BAD, foto);
					}
					
					return new Object[] { VerificationResult.NOT_FOUND, userName, matchedPedestrianAccess };
				}

			} else {
				Long codigoUsuario = Long.valueOf(codigo.trim());

				if (Utils.isNullOrZero(codigoUsuario)) {
					return new Object[] { resultadoVerificacao, userName, matchedPedestrianAccess };					
				}

				if (Objects.nonNull(origem)
						&& origem.equals(Enumeradores.VIA_TECLADO)) {
					matchedPedestrianAccess = (PedestrianAccessEntity) HibernateAccessDataFacade
							.getSingleResultByRegistration(PedestrianAccessEntity.class, codigoUsuario);

				} else if (Objects.nonNull(origem) 
						&& (origem == Origens.ORIGEM_LEITOR_1 || origem == Origens.ORIGEM_LEITOR_2)) {
					matchedPedestrianAccess = (PedestrianAccessEntity) HibernateAccessDataFacade
							.getSingleResultByCardNumber(PedestrianAccessEntity.class, codigoUsuario);
				} else {
					matchedPedestrianAccess = (PedestrianAccessEntity) HibernateAccessDataFacade
							.getSingleResultById(PedestrianAccessEntity.class, codigoUsuario);
				}

				if (Objects.isNull(matchedPedestrianAccess)) {
					matchedPedestrianAccess = (PedestrianAccessEntity) HibernateAccessDataFacade
							.getSingleResultByCardNumber(PedestrianAccessEntity.class, codigoUsuario);
				}

				if (Objects.isNull(matchedPedestrianAccess) 
						&& Boolean.TRUE.equals(digitaisCatraca)
						&& origem == Origens.ORIGEM_LEITOR_1) {
					matchedPedestrianAccess = (PedestrianAccessEntity) HibernateAccessDataFacade
							.getSingleResultById(PedestrianAccessEntity.class, codigoUsuario);
				}

			}

			if (Objects.isNull(matchedPedestrianAccess)) {
				resultadoVerificacao = VerificationResult.NOT_FOUND;
				if (createNotification) {
					Utils.createNotification("Usu·rio de CÛdigo " + codigo + " n„o encontrado.", NotificationType.BAD, foto);					
				}

				return new Object[] { resultadoVerificacao, userName, matchedPedestrianAccess };
			}

			userName = matchedPedestrianAccess.getFirstName().toUpperCase();
			foto = matchedPedestrianAccess.getFoto();
			Boolean acessoRestrito = Boolean.valueOf(Utils.getPreference("restrictAccess"));
			Boolean permitidoHoje = true;
			Boolean permitidoSensor = true;
			Boolean permitido = true;
			Boolean permitidoRetornar = true;

			// achou o pedestre, pula regras e envia acesso liberado
			// verifica somente se no for pra pular
			if (!Boolean.TRUE.equals(ignoraRegras)) {
				if (isObrigatorioPassagemViaLeitorFacial(matchedPedestrianAccess, origem)) {
					if (createNotification) {
						Utils.createNotification(userName + " deve cadastrar sua face.", NotificationType.BAD, foto);
					}
					
					return new Object[] { VerificationResult.NOT_ALLOWED_FACE_REQUIRED, userName, matchedPedestrianAccess };
				}

				if (isPedestreNaoPossuiRegras(matchedPedestrianAccess)) {
					if (createNotification) {
						Utils.createNotification(userName + " n„o possui regras.", NotificationType.BAD, foto);
					}
					return new Object[] { VerificationResult.NOT_ALLOWED, userName, matchedPedestrianAccess };
				}
			}

			matchedPedestrianAccess.setOrigemCatraca(origem);

			if ("INATIVO".equals(matchedPedestrianAccess.getStatus())) {
				Utils.createNotification(" Acesso Negado, usu·rio: " + userName + "Inativo", NotificationType.BAD, foto);
				motivo = "Usu·rio inativo.";
				return new Object[] { VerificationResult.NOT_ALLOWED, userName, matchedPedestrianAccess };
			}
			
			if(isNaoPermitidoEquipamentoRestrito(equipament, matchedPedestrianAccess.getEquipamentos())) {
				if (createNotification) {
					Utils.createNotification(userName + " n„o permitido nesse equipamento restrito.", NotificationType.BAD, foto);
				}

				return new Object[] { VerificationResult.NOT_ALLOWED_ORIGEM, userName, matchedPedestrianAccess };
			}

			if (Objects.isNull(origem) || (Boolean.TRUE.equals(matchedPedestrianAccess.getSempreLiberado())
					|| Boolean.TRUE.equals(ignoraRegras)) && origem != Origens.ORIGEM_LEITOR_2) {

				criaLogDeAcessoSempreLiberado(ignoraRegras, origem, matchedPedestrianAccess, location, direction, data,
						codigo, createNotification, equipament, foto, userName);

				return new Object[] { VerificationResult.ALLOWED, userName, matchedPedestrianAccess };
			}

			if (matchedPedestrianAccess.getTipo().equals("PEDESTRE") && origem == Origens.ORIGEM_LEITOR_2) {
				permitidoSensor = false;
			}
			
			if (isNaoPermitidoNoEquipamento(equipament, matchedPedestrianAccess.getEquipamentos())) {
				if (createNotification) {
					Utils.createNotification(userName + " n„o permitido nesse equipamento.", NotificationType.BAD, foto);
				}

				return new Object[] { VerificationResult.NOT_ALLOWED_ORIGEM, userName, matchedPedestrianAccess };
			}

			LogPedestrianAccessEntity ultimoAcesso = logPedestrianAccessRepository.buscaUltimoAcesso(matchedPedestrianAccess.getId(),
					matchedPedestrianAccess.getQtdAcessoAntesSinc());

			if (Integer.valueOf(Enumeradores.VIA_TECLADO).equals(origem)
					&& Boolean.FALSE.equals(matchedPedestrianAccess.getHabilitarTeclado())) {
				permitido = false;

			} else if (Objects.nonNull(origem) 
					&& Objects.nonNull(equipament)
					&& !verificaUltimaPassagemEmCatracaVinculada(equipament, matchedPedestrianAccess.getId())) {
				permitido = false;

			} else if (Objects.nonNull(ultimoAcesso) 
					&& ultimoAcesso.isSaida()
					&& !isPedestrePermitidoRetornar(matchedPedestrianAccess)) {
				permitidoRetornar = true;

			} else if (matchedPedestrianAccess.isVisitante()) {
				if (!Integer.valueOf(Origens.ORIGEM_LEITOR_2).equals(origem)) {
					if (matchedPedestrianAccess.temCreditos()
							|| isPermitidoPedestreRegra(matchedPedestrianAccess)) {
						permitido = matchedPedestrianAccess.temCreditosValidos();

						// fazer um for validando se existe regra livre ou com quantidade veazia, s√≥
						// assim libero

						if ((matchedPedestrianAccess.isUltimoCredito()
								|| (matchedPedestrianAccess.getRegraAtiva().isPresent() 
										&& matchedPedestrianAccess.getRegraAtiva().get().isUltimoCredito()))
								&& !Integer.valueOf(Origens.ORIGEM_BIOMETRIA).equals(origem) 
								&& usaUrna) {
							permitidoSensor = isPermitidoNoSensor(ultimoAcesso, origem, matchedPedestrianAccess);
						}

						if (isPermitidoPedestreRegra(matchedPedestrianAccess)) {
							permitido = true;
						}

					} else if (matchedPedestrianAccess.temRegraDeAcessoPorPeriodoValido()) {
						permitido = true;
					} else {
						if (Integer.valueOf(FacialDevice.ORIGEM_FACIAL).equals(origem)
								&& matchedPedestrianAccess.getCardNumber() == null) {
							permitido = false;
						} else if (usaUrna) {
							permitidoSensor = isPermitidoNoSensor(ultimoAcesso, origem, matchedPedestrianAccess);
						}
					}

				} else {
					if (usaUrna) {
						permitidoSensor = isPermitidoNoSensor(ultimoAcesso, origem, matchedPedestrianAccess);
					}
				}

			} else {

				/*
				 * regras de pedestres
				 */

				if (matchedPedestrianAccess.temCreditos()) {

					/*
					 * verificar creditos
					 */

					// verifica se tem cr√©ditos para passar

					permitido = matchedPedestrianAccess.temCreditosValidos(data)
							&& !isPermitidoPedestreRegra(matchedPedestrianAccess)
							&& Objects.nonNull(matchedPedestrianAccess.getCardNumber());

				} else if (matchedPedestrianAccess.temTipoTurno()) {
					/*
					 * verificar turno/escala
					 */

					TipoEscala tipo = TipoEscala.valueOf(matchedPedestrianAccess.getTipoTurno());
					int tipoAdicao = TipoEscala.ESCALA_12_36.equals(tipo) || TipoEscala.ESCALA_24_04.equals(tipo)
							? Calendar.HOUR
							: Calendar.DATE;
					Calendar dataAcesso = Calendar.getInstance();
					String[] escala = tipo.name().replace("ESCALA_", "").split("_");

					// data de partida
					Date dataInicial = calculaDataInicialEscala(matchedPedestrianAccess, escala, tipoAdicao);
					System.out.println("Data calculada: " + new SimpleDateFormat("dd/MM/yyyy HH:mm").format(dataInicial));

					Calendar periodoPermitidoIni = Calendar.getInstance();
					periodoPermitidoIni.setTime(dataInicial);

					Calendar periodoPermitidoFim = Calendar.getInstance();
					periodoPermitidoFim.setTime(dataInicial);
					periodoPermitidoFim.add(tipoAdicao, Integer.parseInt(escala[0]));

					permitido = dataAcesso.after(periodoPermitidoIni) && dataAcesso.before(periodoPermitidoFim);

				} else if (acessoRestrito) {
					// verifica se h√° algum log de acesso para este aluno hoje
					HashMap<String, Object> args = new HashMap<String, Object>();
					args.put("ID_ATLETA", matchedPedestrianAccess.getId());
					List<LogPedestrianAccessEntity> acessosHoje = (List<LogPedestrianAccessEntity>) HibernateAccessDataFacade
							.getResultListWithParams(LogPedestrianAccessEntity.class,
									"LogPedestrianAccessEntity.findTodayByAthlete", args);

					Integer limiteAcessos = Integer.valueOf(Utils.getPreference("restrictAccessDays"));

					if (acessosHoje != null && acessosHoje.size() >= limiteAcessos) {
						permitidoHoje = false;
					}
				}

			}

			if (!permitidoSensor) {
				if (origem != Origens.ORIGEM_LEITOR_2) {
					resultadoVerificacao = VerificationResult.NOT_ALLOWED_SENSOR;
				} else {
					resultadoVerificacao = VerificationResult.NOT_ALLOWED_BOX;
				}

				if (createNotification) {
					if (origem != Origens.ORIGEM_LEITOR_2) {
						Utils.createNotification(userName + " deve depositar cart„o na urna.", NotificationType.BAD, foto);
						motivo = "Deve depositar cart„o na urna.";

					} else {
						Utils.createNotification(userName + " n„o deve depositar na urna", NotificationType.BAD, foto);
						motivo = "N„o deve depositar cart„o na urna.";
					}
				}

			} else if (!permitido) {
				resultadoVerificacao = VerificationResult.NOT_ALLOWED;
				if (createNotification) {
					Utils.createNotification(userName + " n„o permitido.", NotificationType.BAD, foto);
					motivo = "N„o permitido.";
				}

			} else if (!permitidoRetornar) {
				resultadoVerificacao = VerificationResult.NOT_ALLOWED_NOW;
				if (createNotification) {
					Utils.createNotification(userName + " n„o pode retornar agora.", NotificationType.BAD, foto);
					motivo = "N„o pode retornar agora.";
				}

			} else if (permitidoHoje) {

				// para l√≥gicas do pedestre:
				// - hor√°rio
				// - periodo
				// - escala
				LogPedestrianAccessEntity logAccess = new LogPedestrianAccessEntity(Main.loggedUser.getId(),
						matchedPedestrianAccess.getId(), matchedPedestrianAccess.getStatus(), location, motivo, direction,
						equipament);
				if (data != null) {
					logAccess.setAccessDate(data);
				}

				if (matchedPedestrianAccess.isAtivo()) {
					resultadoVerificacao = validaDiasHorarios(createNotification, userName, matchedPedestrianAccess,
							logAccess, VerificationResult.ALLOWED, foto, origem, data);

					// indefine acesso
//					if ("ATIVO".equals(logAccess.getStatus()))
//						logAccess.setStatus("INDEFINIDO");

				} else {
					resultadoVerificacao = VerificationResult.NOT_ALLOWED;
					logAccess.setStatus("INATIVO");
					if (createNotification) {
						Utils.createNotification(userName + " n„o permitido.", NotificationType.BAD, foto);
						motivo = "N„o permitido.";
					}
				}

				logAccess.setReason(motivo);

				if (deveGravarCartaoRecebidoNoLog(origem)) {
					logAccess.setCartaoAcessoRecebido(codigo);
				}

				if (Main.broadcastServer != null) {
					Main.broadcastServer.sendMessage(new BroadcastMessageTO(BroadcastMessageType.LOG_ACCESS, logAccess));
				}

				if ((Objects.nonNull(origem) && !origem.equals(Origens.ORIGEM_LIBERADO_SISTEMA))
						|| !VerificationResult.ALLOWED.equals(resultadoVerificacao)) {
					HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);
				}

			} else {
				resultadoVerificacao = VerificationResult.ALLOWED_ONLY_ONCE;
				if (createNotification) {
					Utils.createNotification(userName + " j· registrado hoje.", NotificationType.BAD, foto);
				}
			}

		} catch (Exception e) {
			e.printStackTrace();
			resultadoVerificacao = VerificationResult.ERROR;
			if (createNotification) {
				Utils.createNotification("Falha ao processar requisi√ß√£o de acesso. " + e.getMessage(),
						NotificationType.BAD, foto);
			}
		}

		return new Object[] { resultadoVerificacao, userName, matchedPedestrianAccess };
	}
	
	private PedestrianAccessEntity trataPedestreQRCode(String codigo) throws ParseException {
		String qrCode = "";
		String tempo = null;

		if (codigo.startsWith("T_")) {
			String[] parts = codigo.split("_");
			tempo = parts[parts.length - 1];
			qrCode = parts[0] + "_" + parts[1];
		} else {
			qrCode = codigo;
		}

		PedestrianAccessEntity pedestre = pedestrianAccessRepository.buscaPedestrePeloQrCode(qrCode);

		// verifica se QRCode valido ainda
		if (Objects.nonNull(pedestre) && Objects.nonNull(tempo)) {
			SimpleDateFormat sdf = new SimpleDateFormat("ddMMyyyyHHmmss");
			Date tempoQrCode = sdf.parse(new SimpleDateFormat("ddMMyyyy").format(new Date()) + tempo);

			Long agora = new Date().getTime();
			if (tempoQrCode.getTime() <= agora) {
				throw new QrcodeVencidoException();
			}
		}

		return pedestre;
	}
	
	private boolean isObrigatorioPassagemViaLeitorFacial(final PedestrianAccessEntity pedestre, final Integer origem) {
		return Boolean.TRUE.equals(pedestre.getCadastroFacialObrigatorio())
				&& !Integer.valueOf(FacialDevice.ORIGEM_FACIAL).equals(origem);
	}
	
	private boolean isPedestreNaoPossuiRegras(final PedestrianAccessEntity pedestre) {
		return Boolean.FALSE.equals(pedestre.getSempreLiberado())
				&& (Objects.isNull(pedestre.getPedestreRegra()) || pedestre.getPedestreRegra().isEmpty());
	}
	
	private boolean isNaoPermitidoEquipamentoRestrito(String equipament, List<PedestrianEquipamentEntity> equipamentosPedestre) {
		final Device device = DeviceRepository.getDeviceByFullIdentifier(equipament);
		
		if(Objects.isNull(device)) {
			return false;
		}
		
		if(device instanceof TopDataDevice && ((TopDataDevice) device).isDeviceRestrito()) {
			boolean naoTemAcessoEquipamentoRestrito = true;

			if(Objects.isNull(equipamentosPedestre) || equipamentosPedestre.isEmpty()) {
				return naoTemAcessoEquipamentoRestrito;
			}
			
			for(PedestrianEquipamentEntity equipamentoPedestre : equipamentosPedestre) {
				if(equipamentoPedestre.getNomeEquipamento().equals(device.getName())) {
					naoTemAcessoEquipamentoRestrito = false;
					break;
				}
			}
			
			return naoTemAcessoEquipamentoRestrito;
		}
		
		return false;
	}
	
	private void criaLogDeAcessoSempreLiberado(Boolean ignoraRegras, Integer origem,
			PedestrianAccessEntity matchedPedestrianAccess, String location, String direction, Date data, String codigo,
			boolean createNotification, String equipament, byte[] foto, String userName) {
		String motivo = ignoraRegras ? "Regras ignoradas" : "Sempre liberado";

		if (Origens.ORIGEM_LIBERADO_SISTEMA.equals(origem)) {
			return;
		}

		LogPedestrianAccessEntity logAccess = new LogPedestrianAccessEntity(Main.loggedUser.getId(),
				matchedPedestrianAccess.getId(), matchedPedestrianAccess.getStatus(), location, motivo, direction,
				equipament);
		logAccess.setStatus("INDEFINIDO");
		if (data != null) {
			logAccess.setAccessDate(data);
		}

		if (deveGravarCartaoRecebidoNoLog(origem)) {
			logAccess.setCartaoAcessoRecebido(codigo);
		}

		if (createNotification) {
			Utils.createNotification(userName + " liberado.", NotificationType.GOOD, foto);
		}

		if (Main.broadcastServer != null) {
			Main.broadcastServer.sendMessage(new BroadcastMessageTO(BroadcastMessageType.LOG_ACCESS, logAccess));
		}

		HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);
	}
	
	private boolean deveGravarCartaoRecebidoNoLog(Integer origem) {
		return Objects.nonNull(origem) 
				&& origem != Origens.ORIGEM_LIBERADO_SISTEMA 
				&& origem != 18;
	}
	
	private boolean isNaoPermitidoNoEquipamento(String equipament, List<PedestrianEquipamentEntity> equipamentos) {
		/*
		final Device device = getDeviceByIdentifier(equipament);
		
		if(Objects.isNull(device)) {
			return false;
		}
		
		if(device instanceof TopDataDevice && ((TopDataDevice) device).isDeviceRestrito()) {
			return naoPossuiEquipamentoVinculado(equipament, equipamentos);
		}
		*/

		// n„o tem bloqueo por equipamento
		if (Objects.isNull(equipamentos) || equipamentos.isEmpty()) {
			return false;
		}
		
		if(equipamentos.get(0).getPedestrianAccess().hasOnlyRestrictedEquipaments()) {
			return false;
		}

		return naoPossuiEquipamentoVinculado(equipament, equipamentos);
	}
	
	private boolean naoPossuiEquipamentoVinculado(final String identifier, final List<PedestrianEquipamentEntity> equipamentos) {
		if (Objects.isNull(equipamentos) || equipamentos.isEmpty()) {
			return true;
		}
		
		boolean naoPossuiEquipamentoVinculado = true;
		for (PedestrianEquipamentEntity e : equipamentos) {
			String idEquipament = identifier.replace("Inner ", "").replace("Inner Acesso ", "").replace("Control ", "");

			if (e.getIdEquipamento().equals(idEquipament) || idEquipament.contains(e.getIdEquipamento())) {
				naoPossuiEquipamentoVinculado = false;
				break;
			}
		}
		
		return naoPossuiEquipamentoVinculado;
	}
	
	private boolean verificaUltimaPassagemEmCatracaVinculada(String equipamento, Long idPedestre) {
		Device deviceAtual = null;
		boolean permitido = true;

		for (Device device : Main.devicesList) {
			String idDevice = device.getIdentifier().split(";")[0];
			String idEquipamento = equipamento.replace("Inner ", "").replace("Inner Acesso ", "").replace("Control ", "");

			if (idDevice != null && equipamento != null && idDevice.equals(idEquipamento)) {
				deviceAtual = device;
				break;
			}
		}

		if (deviceAtual != null 
				&& deviceAtual.getAttachedDevices() != null
				&& !deviceAtual.getAttachedDevices().isEmpty()) {
			for (AttachedTO attachedTO : deviceAtual.getAttachedDevices()) {
				String idDevice = attachedTO.getIdDevice().split(";")[0];

				if (deviceAtual instanceof TopDataDevice) {
					idDevice = "Inner " + idDevice;
				} else if (deviceAtual instanceof TopDataAcessoDevice) {
					idDevice = "Inner Acesso " + idDevice;
				} else if (deviceAtual instanceof ControlIdDevice) {
					idDevice = "Control " + idDevice;
				}

				permitido = HibernateAccessDataFacade.verificaSePossuiEntradaNoEquipamento(idDevice, idPedestre);

				if (permitido) {
					return true;
				}
			}

			permitido = false;
		}

		return permitido;
	}
	
	private boolean isPedestrePermitidoRetornar(PedestrianAccessEntity matchedPedestrianAccess) {
		Date ultimoAcesso = HibernateAccessDataFacade.buscaUltimoAcessoAtivoPedestre(matchedPedestrianAccess.getId());

		if (Objects.isNull(ultimoAcesso)) {
			return true;
		}

		return Utils.isPodeEntrarNovamente(ultimoAcesso);
	}
	
	private boolean isPermitidoPedestreRegra(PedestrianAccessEntity pedestre) {
		final Optional<PedestreRegraEntity> regraAtiva = pedestre.getRegraAtiva();
		
		return regraAtiva.isPresent() && regraAtiva.get().temCreditos();
	}
	
	private boolean isPermitidoNoSensor(LogPedestrianAccessEntity ultimoAcesso, Integer origem, PedestrianAccessEntity pedestre) {
		Boolean permitidoSensor = true;

		if (pedestre.getQrCodeParaAcesso() != null 
				&& pedestre.getQrCodeParaAcesso().contains("_")
				&& pedestre.temRegraDeAcessoPorPeriodoValido()) {
			return true;
		}

		if (origem == Origens.ORIGEM_LEITOR_1 
				&& ultimoAcesso != null
				&& Tipo.ENTRADA.equals(ultimoAcesso.getDirection())) {
			return false;
		}

		if (origem == Origens.ORIGEM_LEITOR_2
				&& (ultimoAcesso == null || Tipo.SAIDA.equals(ultimoAcesso.getDirection()))) {
			return false;
		}

		return permitidoSensor;
	}
	
	private Date calculaDataInicialEscala(PedestrianAccessEntity pedestre, String[] escala, int tipoAdicao) {
		HashMap<String, Object> args = new HashMap<String, Object>();
		args.put("ID_PEDESTRE", pedestre.getId());
		LogPedestrianAccessEntity ultimoAcesso = (LogPedestrianAccessEntity) HibernateAccessDataFacade.getUniqueResultWithParams(
				LogPedestrianAccessEntity.class, "LogPedestrianAccessEntity.findByPedestreEntrada", args);

		Calendar c = Calendar.getInstance();
		c.setTime(ultimoAcesso != null ? ultimoAcesso.getAccessDate() : pedestre.getDataInicioPeriodo());

		Calendar h = Calendar.getInstance();
		h.setTime(pedestre.getInicioTurno());

		c.set(Calendar.HOUR_OF_DAY, h.get(Calendar.HOUR_OF_DAY));
		c.set(Calendar.MINUTE, h.get(Calendar.MINUTE));
		c.set(Calendar.SECOND, 0);

		if (ultimoAcesso != null) {
			Calendar ajuste = Calendar.getInstance();
			ajuste.setTime(c.getTime());
			ajuste.add(tipoAdicao, Integer.parseInt(escala[0]));

			Calendar agora = Calendar.getInstance();
			if (agora.after(ajuste)) {
				c.add(tipoAdicao, Integer.parseInt(escala[1]));
			} else {
				c.add(tipoAdicao, Integer.parseInt(escala[0]) * -1);
			}
		}

		return c.getTime();
	}
	
	private static VerificationResult validaDiasHorarios(boolean createNotification, String userName,
			PedestrianAccessEntity matchedAthleteAccess, LogPedestrianAccessEntity logAccess,
			VerificationResult validado, byte[] foto, Integer origem, Date data) {
		VerificationResult resultadoVerificacao;

		if (Utils.isDiaPermitido(matchedAthleteAccess, data)) {
			if (Utils.isDentroDoHorario(matchedAthleteAccess, data)) {
				resultadoVerificacao = validado;
				logAccess.setStatus("ATIVO");

				if (createNotification && Origens.ORIGEM_LIBERADO_SISTEMA.equals(origem)) {
					Utils.createNotification(
							userName + " permitido"
									+ (VerificationResult.TOLERANCE_PERIOD.equals(validado) ? " pela toler‚ncia."
											: "."), NotificationType.GOOD, foto);
				}

			} else {
				resultadoVerificacao = VerificationResult.NOT_ALLOWED_NOW;
				logAccess.setStatus("INATIVO");
				if (createNotification) {
					Utils.createNotification(userName + " fora do hor·rio.", NotificationType.BAD, foto);
				}
			}

		} else {
			resultadoVerificacao = VerificationResult.NOT_ALLOWED_TODAY;
			logAccess.setStatus("INATIVO");
			if (createNotification) {
				Utils.createNotification(userName + " n„o permitido hoje.", NotificationType.BAD, foto);
			}
		}

		return resultadoVerificacao;
	}
	
}
