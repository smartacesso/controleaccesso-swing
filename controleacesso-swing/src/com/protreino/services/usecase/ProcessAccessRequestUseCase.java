package com.protreino.services.usecase;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.Calendar;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Random;

import com.protreino.services.constants.Origens;
import com.protreino.services.constants.Tipo;
import com.protreino.services.devices.ControlIdDevice;
import com.protreino.services.devices.Device;
import com.protreino.services.devices.FacialDevice;
import com.protreino.services.devices.TopDataAcessoDevice;
import com.protreino.services.devices.TopDataDevice;
import com.protreino.services.entity.HorarioEntity;
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
import com.protreino.services.repository.HibernateLocalAccessData;
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
	public Object[] processAccessRequest(String codigo, String location, boolean createNotification,
			boolean ignoraRegras) {
		return processAccessRequest(codigo, location, null, null, false, createNotification, null, false, ignoraRegras);
	}

	/**
	 * @param codigo
	 * @return Object[] { resultadoVerificacao, userName, matchedPedestrianAccess }
	 */
	public Object[] processAccessRequest(String codigo, String equipamento, Integer origem, String location,
			boolean usaUrna, boolean createNotification, Date data, Boolean digitaisCatraca, Boolean ignoraRegras) {
		return processAccessRequest(codigo, location, null, equipamento, origem, usaUrna, createNotification, data,
				digitaisCatraca, ignoraRegras, true);
	}

	/**
	 * @param codigo
	 * @return Object[] { resultadoVerificacao, userName, matchedPedestrianAccess }
	 */
	public Object[] processAccessRequest(String codigo, String equipamento, Integer origem, String location,
			boolean usaUrna, boolean createNotification, boolean ignoraRegras) {
		return processAccessRequest(codigo, location, null, equipamento, origem, usaUrna, createNotification, null,
				false, ignoraRegras, true);
	}
	
	public Object[] processAccessRequest(String codigo, Integer origem, Date date) {
		return processAccessRequest(codigo, null, null, "", origem, false, false, date,
				false, false, false);
	}

	private Object[] processAccessRequest(String codigo, String location, String direction, String equipament,
			Integer origem, Boolean usaUrna, boolean createNotification, Date data, Boolean digitaisCatraca,
			Boolean ignoraRegras, Boolean registerLog) {

		VerificationResult resultadoVerificacao = VerificationResult.NOT_FOUND;
		String userName = "";
		byte[] foto = null;
		PedestrianAccessEntity matchedPedestrianAccess = null;

		try {
			if (Utils.isNullOrEmpty(codigo)) {
				return new Object[] { resultadoVerificacao, userName, null };
			}
			// para tratar QrCode
			if (codigo.contains("_")) {
				try {
					matchedPedestrianAccess = trataPedestreQRCode(codigo);
				} catch (QrcodeVencidoException e) {
					if (createNotification) {
						Utils.createNotification("QRCode do usuario expirado.", NotificationType.BAD, foto);
					}
					return new Object[] { VerificationResult.NOT_FOUND, userName, null };
				}

			} else { // para codigo normal
				Long codigoUsuario = Long.valueOf(codigo.trim());
				if (Utils.isNullOrZero(codigoUsuario)) {
					return new Object[] { resultadoVerificacao, userName, null };
				}
				matchedPedestrianAccess = buscaPedestrePeloCodigo(codigoUsuario, origem, digitaisCatraca);
			}

			// se nao for encontrado
			if (matchedPedestrianAccess == null) {
				if (createNotification) {
					Utils.createNotification("Usuário de Código " + codigo + " não encontrado.", NotificationType.BAD, foto);
				}

				return new Object[] { VerificationResult.NOT_FOUND, userName, null };
			}

			// se a catraca estiver marcada como ignorarRegras
			if (!Boolean.TRUE.equals(ignoraRegras)) {
				if (isObrigatorioPassagemViaLeitorFacial(matchedPedestrianAccess, origem)) {
					if (createNotification) {
						Utils.createNotification(userName + " deve cadastrar sua face.", NotificationType.BAD, foto);
					}
					return new Object[] { VerificationResult.NOT_ALLOWED_FACE_REQUIRED, userName,
							matchedPedestrianAccess };
				}

				if (isPedestreNaoPossuiRegras(matchedPedestrianAccess)) {
					if (createNotification) {
						Utils.createNotification(userName + " não possui regras.", NotificationType.BAD, foto);
					}
					return new Object[] { VerificationResult.NOT_ALLOWED, userName, matchedPedestrianAccess };
				}
			}

			// --- SEPARAÇÃO AQUI ---
			boolean isVisitante = matchedPedestrianAccess.isVisitante(); // ou qualquer outra verificação
			if (isVisitante) {
				return processVisitanteAccess(ignoraRegras, origem, matchedPedestrianAccess, location, data, direction,
						createNotification, equipament, codigo, usaUrna, registerLog);
			} else {
				return processaVerificacoesBasicasPedestre(ignoraRegras, origem, matchedPedestrianAccess, location,
						data, direction, createNotification, equipament, codigo, registerLog);
			}

		} catch (Exception e) {
			e.printStackTrace(); // ou log
			return new Object[] { VerificationResult.ERROR, userName, null };
		}
	}

	private Object[] processVisitanteAccess(Boolean ignoraRegras, Integer origem, PedestrianAccessEntity visitante,
			String location, Date data, String direction, boolean createNotification, String equipament,
			String codigoCartao, Boolean usaUrna, boolean registerLog) {
		String userName = visitante.getFirstName().toUpperCase();
		byte[] foto = visitante.getFoto();
		String motivo = "";
		visitante.setOrigemCatraca(origem);
		if (data == null) {
			data = new Date();
		}

		LogPedestrianAccessEntity ultimoAcesso = logPedestrianAccessRepository.buscaUltimoAcesso(visitante.getId(),
				visitante.getQtdAcessoAntesSinc());

		Boolean acessoUnico = isAcessoUnico(visitante);
		Boolean regraHorario = isRegraHorario(visitante);
		Boolean regraPeriodo = isRegraPeriodo(visitante);
		Boolean permitidoSensor = isPermitidoNoSensor(ultimoAcesso, origem, visitante);

		if (isPedestreInativo(visitante)) {
			if(createNotification) {
				Utils.createNotification("Usuário inativo.", NotificationType.BAD, foto);
			}
			motivo = "ACESSO NEGADO - INATIVO";
			return resultadoNegado(VerificationResult.NOT_ALLOWED, userName, visitante, motivo);
		}

		if (isNaoPermitidoEquipamentoRestrito(equipament, visitante.getEquipamentos())) {
			if(createNotification) {
				Utils.createNotification("Equipamento não autorizado.", NotificationType.BAD, foto);				
			}
			motivo = "ACESSO NEGADO - EQUIPAMENTO NAO AUTORIZADO";
			return resultadoNegado(VerificationResult.NOT_ALLOWED, userName, visitante, motivo);
		}

		if (regraPeriodo && !visitante.temRegraDeAcessoPorPeriodoValido()) {
			if(createNotification) {
				Utils.createNotification("Fora do periodo.", NotificationType.BAD, foto);				
			}
			motivo = "ACESSO NEGADO - Fora do periodo";
			return resultadoNegado(VerificationResult.NOT_ALLOWED, userName, visitante, motivo);
		}

		System.out.println("Usa urna : " + usaUrna);
		if (usaUrna) {
			if (!permitidoSensor && acessoUnico) {
				if(createNotification) {
					Utils.createNotification("Não permitido no sensor.", NotificationType.BAD, foto);					
				}
				motivo = "ACESSO NEGADO - Deposite o carao na urna";
				return resultadoNegado(VerificationResult.NOT_ALLOWED_SENSOR, userName, visitante, motivo);
			}
		}

		// cria log de acesso
		LogPedestrianAccessEntity logAccess = new LogPedestrianAccessEntity(Main.loggedUser.getId(), visitante.getId(),
				false, location, motivo, direction, equipament, codigoCartao, data);

		logAccess.setStatus("INDEFINIDO");

		if (data != null) {
			logAccess.setAccessDate(data);
		}

		if (deveGravarCartaoRecebidoNoLog(origem)) {
			logAccess.setCartaoAcessoRecebido(codigoCartao);
		}

		if (regraHorario) {
			VerificationResult resultadoFinal = validaDiasHorarios(createNotification, userName, visitante, logAccess,
					VerificationResult.ALLOWED, foto, origem, data);

			if (!VerificationResult.ALLOWED.equals(resultadoFinal)
					&& !VerificationResult.TOLERANCE_PERIOD.equals(resultadoFinal)) {
				return resultadoNegado(resultadoFinal, userName, visitante, motivo);
			}
		}

		if(registerLog) {
			HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);
		}

		return resultadoSucesso(userName, visitante, foto, motivo, createNotification);
	}

	private Boolean isRegraHorario(PedestrianAccessEntity visitante) {
		return visitante.getRegraAtiva().get().isRegraComHorarios()
				|| visitante.getRegraAtiva().get().temRegraDeHorariosComCredito();
	}

	private Boolean isRegraPeriodo(PedestrianAccessEntity visitante) {
		return visitante.getRegraAtiva().get().isRegraComPeriodo();
	}

	private Boolean isAcessoUnico(PedestrianAccessEntity visitante) {
		if (visitante == null || visitante.getRegraAtiva() == null || visitante.getRegraAtiva().get() == null
				|| visitante.getRegraAtiva().get().getRegra() == null
				|| visitante.getRegraAtiva().get().getRegra().getNome() == null) {
			return true;
		}

		return visitante.getRegraAtiva().get().getRegra().getNome().toUpperCase().contains("UNICO");
	}

	private Object[] processaVerificacoesBasicasPedestre(Boolean ignoraRegras, Integer origem,
			PedestrianAccessEntity pedestre, String location, Date data, String direction, boolean createNotification,
			String equipament, String codigoCartao, boolean registerLog) {

		String userName = pedestre.getFirstName().toUpperCase();
		byte[] foto = pedestre.getFoto();
		String motivo = "";
		pedestre.setOrigemCatraca(origem);
		if (data == null) {
			data = new Date();
		}
		
		LogPedestrianAccessEntity ultimoAcesso = logPedestrianAccessRepository.buscaUltimoAcesso(pedestre.getId(),
				pedestre.getQtdAcessoAntesSinc());

		LogPedestrianAccessEntity logAccess = new LogPedestrianAccessEntity(Main.loggedUser.getId(), pedestre.getId(),
				pedestre.getStatus(), location, motivo, direction, equipament);

		logAccess.setStatus("INDEFINIDO");
		logAccess.setAccessDate(data);

		if (deveGravarCartaoRecebidoNoLog(origem)) {
			logAccess.setCartaoAcessoRecebido(codigoCartao);
		}

		String liberado = null;

		if (isSempreLiberado(pedestre)) {
			liberado = "Sempre liberado";
		} else if (isSaidaLiberada(ultimoAcesso)) {
			liberado = "Saida liberada";
		} else if (isAcessoLivre(pedestre)) {
			liberado = "Acesso livre";
		}else if(isHorarioIgnorado(equipament)) {
			liberado = "horario liberado";
		}

		if (isPedestreInativo(pedestre)) {
			System.out.println("LOG - Parou: pedestre inativo.");
			if(createNotification) {
				Utils.createNotification("Usuário inativo.", NotificationType.BAD, foto);				
			}
			motivo = "ACESSO NEGADO - INATIVO";
			logAccess.setReason(motivo);
			logAccess.setStatus("INATIVO");
			if(Boolean.TRUE.equals(registerLog)) {
				HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);				
			}
			return resultadoNegado(VerificationResult.NOT_ALLOWED, userName, pedestre, motivo);
		}

		if (isTecladoBloqueado(pedestre, origem)) {
			System.out.println("LOG - Parou: teclado bloqueado.");
			if(createNotification) {
				Utils.createNotification("Teclado bloqueado.", NotificationType.BAD, foto);
			}
			motivo = "ACESSO NEGADO - TECLADO BLOQUEADO";
			logAccess.setReason(motivo);
			logAccess.setStatus("INATIVO");
			if(Boolean.TRUE.equals(registerLog)) {
				HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);				
			}
			return resultadoNegado(VerificationResult.NOT_ALLOWED, userName, pedestre, motivo);
		}

		if (precisaRevista(ultimoAcesso)) {
			System.out.println("LOG - Parou: revista obrigatória.");
			if(createNotification) {
				Utils.createNotification("Revista obrigatória.", NotificationType.BAD, foto);
			}
			motivo = "ACESSO NEGADO - REVISTA";
			logAccess.setReason(motivo);
			logAccess.setStatus("INATIVO");
			if(registerLog) {
				HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);				
			}
			return resultadoNegado(VerificationResult.REVISTA_REQUIRED, userName, pedestre, motivo);
		}

		if (isNaoPermitidoEquipamentoRestrito(equipament, pedestre.getEquipamentos())) {
			System.out.println("LOG - Parou: equipamento não autorizado.");
			if(createNotification) {
				Utils.createNotification("Equipamento não autorizado.", NotificationType.BAD, foto);				
			}
			motivo = "ACESSO NEGADO - EQUIPAMENTO NAO AUTORIZADO";
			logAccess.setReason(motivo);
			logAccess.setStatus("INATIVO");
			if(registerLog) {
				HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);
			}
			return resultadoNegado(VerificationResult.NOT_ALLOWED, userName, pedestre, motivo);
		}

		if (Utils.isAcessoRestrito() && !isPermitidoHoje(pedestre)) {
			System.out.println("LOG - Parou: acesso restrito.");
			if(createNotification) {
				Utils.createNotification("Acesso restrito", NotificationType.BAD, foto);				
			}
			motivo = "ACESSO NEGADO - DIA NÃO PERMITIDO";
			logAccess.setReason(motivo);
			logAccess.setStatus("INATIVO");
			if(registerLog) {
				HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);
			}
			return resultadoNegado(VerificationResult.NOT_ALLOWED, userName, pedestre, motivo);
		}

		if (liberado != null) {
			System.out.println("LOG - Parou: " + liberado);
			if(registerLog) {
				criaLogDeAcessoSempreLiberado(ignoraRegras, origem, pedestre, location, direction, data, codigoCartao,
						createNotification, equipament, foto, userName);				
			}
			return resultadoSucesso(userName, pedestre, foto, motivo, createNotification);
		}
		
		if(isCatracaRefeitorio(equipament)) {
			if (isHorarioRefeitorio(equipament)) {
				if (!pedestre.temTipoHorarioCredito()) {
					System.out.println(">> Ignorando regra de créditos (não se aplica ao pedestre)");
				} else {
					Optional<PedestreRegraEntity> regraAtivaPedestre = pedestre.getRegraAtivaPedestre();

					LocalTime agora = toLocalTime(new Date());

					Optional<HorarioEntity> horarioValidoComCredito = regraAtivaPedestre.get().getHorarios().stream()
							.filter(h -> h.getHorarioInicio() != null).min(Comparator.comparingLong(h -> {
								LocalTime horario = toLocalTime(h.getHorarioInicio());
								return Math.abs(Duration.between(horario, agora).toMinutes());
							}));

					if (horarioValidoComCredito.map(HorarioEntity::temCreditos).orElse(false)) {
						if(registerLog) {
							HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);
						}
						return resultadoSucesso(userName, pedestre, foto, motivo, createNotification);
					}else {
						logAccess.setStatus("INATIVO");
						logAccess.setReason("Não permitido, sem créditos");
						if (createNotification) {
							Utils.createNotification(userName + " sem créditos.", NotificationType.BAD, foto);
						}
						if(registerLog) {
							HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);
						}
						return resultadoNegado(VerificationResult.NOT_ALLOWED_NO_CREDITS, userName, pedestre, motivo);
					}
				}

			}else if(Utils.isBloqueadoForaHorarioRefeitorio()) {
				if (createNotification) {
					Utils.createNotification(userName + " refeitorio fechado.", NotificationType.BAD, foto);
				}
				HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);
				return resultadoNegado(VerificationResult.NOT_ALLOWED, userName, pedestre, motivo);
			}

			if (pedestre.temTipoCredito() && !pedestre.temCreditosValidos(data)) {
				System.out.println("LOG - Parou: sem créditos.");
				if(createNotification) {
					Utils.createNotification("Sem creditos.", NotificationType.BAD, foto);
				}
				motivo = "ACESSO NEGADO - SEM CREDITOS";
				logAccess.setReason(motivo);
				logAccess.setStatus("INATIVO");
				if(registerLog) {
					HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);
				}
				return resultadoNegado(VerificationResult.NOT_ALLOWED_NO_CREDITS, userName, pedestre, motivo);
			}
		}

		if (pedestre.temTipoEscala3x3()) {
			if(pedestre.temEscala3x3Fixa() && !isPermitidoPorEscala3x3Fixo(pedestre)) {
				System.out.println("LOG - Parou: fora da escala.");
				if(createNotification) {
					Utils.createNotification("Fora da escala", NotificationType.BAD, foto);
				}
				motivo = "ACESSO NEGADO - FORA DA ESCALA";
				logAccess.setReason(motivo);
				logAccess.setStatus("INATIVO");
				if(registerLog) {
					HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);
				}
				return resultadoNegado(VerificationResult.NOT_ALLOWED_TODAY, userName, pedestre, motivo);
			}else {
				if(!isPermitidoPorEscala3x3(pedestre, data)) {

					System.out.println("LOG - Parou: fora da escala.");
					if(createNotification) {
						Utils.createNotification("Fora da escala", NotificationType.BAD, foto);
					}
					motivo = "ACESSO NEGADO - FORA DA ESCALA";
					logAccess.setReason(motivo);
					logAccess.setStatus("INATIVO");
					if(registerLog) {
						HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);
					}
					return resultadoNegado(VerificationResult.NOT_ALLOWED_TODAY, userName, pedestre, motivo);
				
				}
			}
		}

		if (pedestre.temTipoTurno() && !isPermitidoPorTurno(pedestre)) {
			System.out.println("LOG - Parou: fora do turno.");
			if(createNotification) {
				Utils.createNotification("Fora do turno", NotificationType.BAD, foto);
			}
			motivo = "ACESSO NEGADO - FORA DO TURNO";
			logAccess.setReason(motivo);
			logAccess.setStatus("INATIVO");
			if(registerLog) {
				HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);
			}
			return resultadoNegado(VerificationResult.NOT_ALLOWED_TODAY, userName, pedestre, motivo);
		}

		if (pedestre.temTipoPeriodo() && !pedestre.temRegraDeAcessoPorPeriodoValido()) {
			System.out.println("LOG - Parou: fora do periodo.");
			if(createNotification) {
				Utils.createNotification("Fora do periodo", NotificationType.BAD, foto);
			}
			motivo = "ACESSO NEGADO - FORA DO PERIODO";
			logAccess.setReason(motivo);
			logAccess.setStatus("INATIVO");
			if(registerLog) {
				HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);
			}
			return resultadoNegado(VerificationResult.NOT_ALLOWED_TODAY, userName, pedestre, motivo);
		}

		if (pedestre.temTipoHorarioCredito()) {
			System.out.println("LOG - Validando dias e horários.");

			VerificationResult resultadoFinal = validaDiasHorarios(createNotification, userName, pedestre, logAccess,
					VerificationResult.ALLOWED, foto, origem, data);

			System.out.println("LOG - Resultado final da validação: " + resultadoFinal);
			if(registerLog) {
				HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);
			}

			if (!VerificationResult.ALLOWED.equals(resultadoFinal)
					&& !VerificationResult.TOLERANCE_PERIOD.equals(resultadoFinal)) {
				System.out.println("LOG - Parou: não permitido após validação de dias e horários.");
				return resultadoNegado(resultadoFinal, userName, pedestre, motivo);
			} else {
				return resultadoSucesso(userName, pedestre, foto, motivo, createNotification);
			}
		}

		if(registerLog) {
			HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);
		}

		return resultadoSucesso(userName, pedestre, foto, motivo, createNotification);
	}
	
	private LocalTime toLocalTime(Date date) {
	    return date.toInstant().atZone(ZoneId.systemDefault()).toLocalTime();
	}


	private boolean isAcessoLivre(PedestrianAccessEntity pedestre) {
		if (Objects.nonNull(pedestre.getAcessoLivre())) {
			return Boolean.TRUE.equals(pedestre.getAcessoLivre());
		}

		return false;
	}

	private boolean isPermitidoPorCredito(PedestrianAccessEntity pedestre, Date data) {
		return pedestre.temCreditos() && pedestre.temCreditosValidos(data) && !isPermitidoPedestreRegra(pedestre)
				&& Objects.nonNull(pedestre.getCardNumber());
	}
	
	private boolean isPermitidoPorEscala3x3(PedestrianAccessEntity pedestre, Date data) {

		System.out.println("Tipo de regra - Escala 3x3");
		LocalDateTime dataAcesso = LocalDateTime.now();
		LocalDate dataInicioEscala = new java.util.Date(
				pedestre.getRegraAtivaPedestre().get().getDataInicioPeriodo().getTime()).toInstant()
				.atZone(ZoneId.systemDefault()).toLocalDate();

		LocalTime turnoBase = pedestre.getRegraAtivaPedestre().get().getRegra().getHorarioInicioTurno().toInstant()
				.atZone(ZoneId.systemDefault()).toLocalTime();

		// Calcular o dia do ciclo (1 a 12)
		long diasEntre = ChronoUnit.DAYS.between(dataInicioEscala, dataAcesso.toLocalDate());
		int diaDaEscala = (int) (diasEntre % 12) + 1;

		// Alternância de turno: a cada 6 dias o turno inverte
		boolean cicloPar = (diasEntre / 6) % 2 == 0;

		// Turno atual baseado no ciclo (inverte 12h)
		LocalTime horarioInicioTurno = cicloPar ? turnoBase
				: turnoBase.plusHours(12).withHour((turnoBase.getHour() + 12) % 24);

		// Antecipação de 3h (se necessário)
		LocalTime horarioInicio = horarioInicioTurno.minusHours(3);
		LocalTime fimTurno = horarioInicio.plusHours(12);
		boolean atravessaMeiaNoite = fimTurno.isBefore(horarioInicio);

		LocalTime horaAtual = dataAcesso.toLocalTime();

		System.out.println("Data inicio escala : " + dataInicioEscala);
		System.out.println("Hora do acesso     : " + dataAcesso);
		System.out.println("Dia da escala      : " + diaDaEscala);
		System.out.println("Turno base inicial : " + turnoBase);
		System.out.println("Turno atual ciclo  : " + horarioInicioTurno);
		System.out.println("Inicio do turno    : " + horarioInicio);
		System.out.println("Fim do turno       : " + fimTurno);
		System.out.println("Atravessa meia-noite: " + atravessaMeiaNoite);

// Verifica se é dia de trabalho
		boolean diaTrabalho = diaDaEscala == 1 || diaDaEscala == 2 || diaDaEscala == 3 || diaDaEscala == 7
				|| diaDaEscala == 8 || diaDaEscala == 9;

		if (diaTrabalho) {
			if (atravessaMeiaNoite) {
				return horaAtual.isAfter(horarioInicio) || horaAtual.isBefore(fimTurno);
			} else {
				return horaAtual.isAfter(horarioInicio) && horaAtual.isBefore(fimTurno);
			}
		} else {
			// Dias 4-6 e 10-12: folga
			return false;
		}
	}
	
	private boolean isPermitidoPorEscala3x3Fixo(PedestrianAccessEntity pedestre) {
		System.out.println("Tipo de regra - Escala 3x3 FIXA");

		LocalDateTime dataAcesso = LocalDateTime.now();
		LocalDate dataInicioEscala = new java.util.Date(
				pedestre.getRegraAtivaPedestre().get().getDataInicioPeriodo().getTime())
				.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();

		LocalTime turnoBase = pedestre.getRegraAtivaPedestre().get().getRegra().getHorarioInicioTurno()
				.toInstant().atZone(ZoneId.systemDefault()).toLocalTime();

		// Calcular o dia do ciclo (1 a 6)
		long diasEntre = ChronoUnit.DAYS.between(dataInicioEscala, dataAcesso.toLocalDate());
		int diaDaEscala = (int) (diasEntre % 6) + 1;

		// Janela de acesso
		LocalTime horarioInicio = turnoBase.minusHours(3);
		LocalTime fimTurno = turnoBase.plusHours(12);
		boolean atravessaMeiaNoite = fimTurno.isBefore(horarioInicio);

		LocalTime horaAtual = dataAcesso.toLocalTime();

		System.out.println("Data inicio escala : " + dataInicioEscala);
		System.out.println("Hora do acesso     : " + dataAcesso);
		System.out.println("Dia da escala      : " + diaDaEscala);
		System.out.println("Turno base fixo    : " + turnoBase);
		System.out.println("Inicio do turno    : " + horarioInicio);
		System.out.println("Fim do turno       : " + fimTurno);
		System.out.println("Atravessa meia-noite: " + atravessaMeiaNoite);

		// Dias 1, 2, 3 = trabalho | Dias 4, 5, 6 = folga
		boolean diaTrabalho = diaDaEscala == 1 || diaDaEscala == 2 || diaDaEscala == 3;

		if (diaTrabalho) {
			boolean permitido = atravessaMeiaNoite
					? horaAtual.isAfter(horarioInicio) || horaAtual.isBefore(fimTurno)
					: horaAtual.isAfter(horarioInicio) && horaAtual.isBefore(fimTurno);

			System.out.println("Acesso permitido?   : " + permitido);
			return permitido;
		} else {
			return false;
		}
	}


	private boolean isPermitidoPorTurno(PedestrianAccessEntity pedestre) {
		System.out.println("Tipo escala - Turno ");
		TipoEscala tipo = TipoEscala.valueOf(pedestre.getTipoTurno());
		int tipoAdicao = TipoEscala.ESCALA_12_36.equals(tipo) || TipoEscala.ESCALA_24_04.equals(tipo) ? Calendar.HOUR
				: Calendar.DATE;
		Calendar dataAcesso = Calendar.getInstance();
		String[] escala = tipo.name().replace("ESCALA_", "").split("_");

		// data de partida
		Date dataInicial = calculaDataInicialEscala(pedestre, escala, tipoAdicao);
		System.out.println("Data calculada: " + new SimpleDateFormat("dd/MM/yyyy HH:mm").format(dataInicial));

		Calendar periodoPermitidoIni = Calendar.getInstance();
		periodoPermitidoIni.setTime(dataInicial);

		Calendar periodoPermitidoFim = Calendar.getInstance();
		periodoPermitidoFim.setTime(dataInicial);
		periodoPermitidoFim.add(tipoAdicao, Integer.parseInt(escala[0]));

		System.out.println("Periodo Permitido Inicio: "
				+ new SimpleDateFormat("dd/MM/yyyy HH:mm:ss").format(periodoPermitidoIni.getTime()));
		System.out.println("Periodo Permitido Fim: "
				+ new SimpleDateFormat("dd/MM/yyyy HH:mm:ss").format(periodoPermitidoFim.getTime()));
		System.out.println("Data Acesso: " + new SimpleDateFormat("dd/MM/yyyy HH:mm:ss").format(dataAcesso.getTime()));

//		permitido = dataAcesso.after(periodoPermitidoIni) && dataAcesso.before(periodoPermitidoFim);

		Calendar dataInicioTurno = Calendar.getInstance();
		dataInicioTurno.setTime(pedestre.getInicioTurno()); // Data base do inicio do turno, ex.:
															// 04/10/2024 �s 10:00

		// Calcular a diferen�a em milissegundos entre agora e o in�cio do turno
		long diffMillis = Calendar.getInstance().getTimeInMillis() - dataInicioTurno.getTimeInMillis();
		long diffHours = diffMillis / (1000 * 60 * 60); // Convertendo para horas

		// Cada ciclo tem 48 horas (12 horas de trabalho + 36 horas de folga)
		long cicloAtual = diffHours % 48;

		// Se está nas primeiras 12 horas do ciclo, ou entre 24 e 36 horas (segundo
		// periodo de trabalho)
		if ((cicloAtual >= 0 && cicloAtual < 12) || (cicloAtual >= 24 && cicloAtual < 36)) {
			// Est� no periodo de trabalho
			return true;
		} else {
			// Est� no periodo de folga
			return false;
		}
	}

	private boolean isPermitidoHoje(PedestrianAccessEntity pedestre) {
		HashMap<String, Object> args = new HashMap<>();
		args.put("ID_ATLETA", pedestre.getId());

		@SuppressWarnings("unchecked")
		List<LogPedestrianAccessEntity> acessosHoje = (List<LogPedestrianAccessEntity>) HibernateAccessDataFacade
				.getResultListWithParams(LogPedestrianAccessEntity.class,
						"LogPedestrianAccessEntity.findTodayByAthlete", args);

		int limiteAcessos = Integer.parseInt(Utils.getPreference("restrictAccessDays"));
		return acessosHoje == null || acessosHoje.size() < limiteAcessos;
	}

	private boolean isSempreLiberado(PedestrianAccessEntity pedestre) {
		return Boolean.TRUE.equals(pedestre.getSempreLiberado());
	}

	private boolean isTecladoBloqueado(PedestrianAccessEntity pedestre, Integer origem) {
		if (Objects.nonNull(origem) && origem.equals(Origens.ORIGEM_TECLADO) && tecladoBloqueado(pedestre)) {
			return true;
		}
		return false;
	}

	private boolean precisaRevista(LogPedestrianAccessEntity ultimoAcesso) {
		// realiza revista apenas na saida
		if (Objects.nonNull(ultimoAcesso) && ultimoAcesso.isEntrada()) {
			if (realizarRevista()) {
				return true;
			}
		}
		return false;
	}

	private boolean isSaidaLiberada(LogPedestrianAccessEntity ultimoAcesso) {
		if(Utils.isSaidaSemVerificar()) {
			if (Objects.nonNull(ultimoAcesso)) {
				if (ultimoAcesso.isEntrada()) {
					System.out.println("Último acesso foi ENTRADA. Liberar SAÍDA.");
					return true; // Último foi entrada → pode sair
				} else {
					System.out.println("Último acesso foi SAÍDA. Liberar ENTRADA.");
					return false; // Já saiu → não pode sair de novo
				}
			} else {
				System.out.println("Sem registro de último acesso. Não liberar SAÍDA.");
				return false; // Sem histórico → não libera
			}
		}
		return false;
	}

	private boolean isPedestreInativo(PedestrianAccessEntity pedestre) {
		return !pedestre.isAtivo();
	}

	private boolean realizarRevista() {
		int porcentagem = Utils.getPreferenceAsInteger("porcentagemRevista");
		double porcentagemConvertida = porcentagem;

		System.out.println("entrou na revista");
		Random random = new Random();
		double chance = random.nextDouble(); // Valor entre 0.0 e 1.0
		System.out.println("porcentagem : " + porcentagem + "% ");

		if (porcentagemConvertida != 0) {
			porcentagemConvertida = porcentagemConvertida / 100.00;
		}

		System.out.println("porcentagem calculada : " + porcentagemConvertida); // Corrigido para exibir
																				// porcentagemConvertida
		System.out.println("chance gerada : " + chance);

		// Verifica se a chance gerada é menor ou igual à porcentagem convertida
		if (chance <= porcentagemConvertida) {
			return true;
		}

		return false;

	}

	private Boolean tecladoBloqueado(PedestrianAccessEntity pedestre) {
		return Boolean.FALSE.equals(pedestre.getHabilitarTeclado());
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
		Boolean semRegras = Boolean.FALSE.equals(pedestre.getSempreLiberado())
				&& (Objects.isNull(pedestre.getPedestreRegra()) || pedestre.getPedestreRegra().isEmpty());
		return semRegras;
	}

	private boolean isNaoPermitidoEquipamentoRestrito(String equipament,
			List<PedestrianEquipamentEntity> equipamentosPedestre) {
		Device device = DeviceRepository.getDeviceByFullIdentifier(equipament);

		if (Objects.isNull(device)) {

			device = DeviceRepository.getDeviceByIdentifierNomeAlterado(equipament);

			if (Objects.isNull(device)) {
				System.out.println("Device não encontrado");
				return false;
			}

		}

		if (device instanceof TopDataDevice && ((TopDataDevice) device).isDeviceRestrito()) {

			if (Objects.isNull(equipamentosPedestre) || equipamentosPedestre.isEmpty()) {
				System.out.println("Pedestre não possui permissão para equipamento restrito: " + device.getName());
				return true;
			}

			for (PedestrianEquipamentEntity equipamentoPedestre : equipamentosPedestre) {
				if (equipamentoPedestre.getNomeEquipamento().equals(device.getName())) {
					System.out.println("Pedestre possui permissão para equipamento restrito: " + device.getName());
					return false;
				}
			}

			System.out.println("Pedestre não possui permissão para equipamento restrito: " + device.getName());
			return true;
		}

		return false;
	}
	
	private Boolean isCatracaRefeitorio(String equipament) {
		if(equipament.isEmpty()) {
			return false;
		}
		Device device = (TopDataDevice) DeviceRepository.getDeviceByIdentifierNomeAlterado(equipament);

		if (Objects.isNull(device)) {
			System.out.println("Device não encontrado");
			return false;
		}

		return ((TopDataDevice) device).isCatracaRefeitorio();
	}

	private Boolean isHorarioRefeitorio(String equipament) {
		Device device = (TopDataDevice) DeviceRepository.getDeviceByIdentifierNomeAlterado(equipament);

		if (Objects.isNull(device)) {
			System.out.println("Device não encontrado");
			return false;
		}

		return ((TopDataDevice) device).isDentroHorarioRefeitorio();
	}

	private Boolean isHorarioIgnorado(String equipament) {
		Device device = (TopDataDevice) DeviceRepository.getDeviceByIdentifierNomeAlterado(equipament);

		if (Objects.isNull(device)) {
			System.out.println("Device não encontrado");
			return false;
		}

		return ((TopDataDevice) device).ignorarAcesso();
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

		if (Main.broadcastServer != null) {
			Main.broadcastServer.sendMessage(new BroadcastMessageTO(BroadcastMessageType.LOG_ACCESS, logAccess));
		}

		HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);
	}

	private boolean deveGravarCartaoRecebidoNoLog(Integer origem) {
		return Objects.nonNull(origem) && origem != Origens.ORIGEM_LIBERADO_SISTEMA && origem != 18;
	}

	private boolean verificaUltimaPassagemEmCatracaVinculada(String equipamento, Long idPedestre) {
		Device deviceAtual = null;
		boolean permitido = true;

		for (Device device : Main.devicesList) {
			String idDevice = device.getIdentifier().split(";")[0];
			String idEquipamento = equipamento.replace("Inner ", "").replace("Inner Acesso ", "").replace("Control ",
					"");

			if (idDevice != null && equipamento != null && idDevice.equals(idEquipamento)) {
				deviceAtual = device;
				break;
			}
		}

		if (deviceAtual != null && deviceAtual.getAttachedDevices() != null
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
	
	private boolean isPermitidoNoSensor(LogPedestrianAccessEntity ultimoAcesso, Integer origem,
			PedestrianAccessEntity pedestre) {
		if (pedestre.getQrCodeParaAcesso() != null && pedestre.getQrCodeParaAcesso().contains("_")
				&& pedestre.temRegraDeAcessoPorPeriodoValido()) {
			return true;
		}

		if (origem == Origens.ORIGEM_LEITOR_1 && ultimoAcesso != null
				&& Tipo.ENTRADA.equals(ultimoAcesso.getDirection())) {
			return false;
		}

		if (origem == Origens.ORIGEM_LEITOR_2
				&& (ultimoAcesso == null || Tipo.SAIDA.equals(ultimoAcesso.getDirection()))) {
			return false;
		}

		return true;
	}

	private Date calculaDataInicialEscala(PedestrianAccessEntity pedestre, String[] escala, int tipoAdicao) {
		HashMap<String, Object> args = new HashMap<String, Object>();
		args.put("ID_PEDESTRE", pedestre.getId());
		LogPedestrianAccessEntity ultimoAcesso = (LogPedestrianAccessEntity) HibernateAccessDataFacade
				.getUniqueResultWithParams(LogPedestrianAccessEntity.class,
						"LogPedestrianAccessEntity.findByPedestreEntrada", args);

		Calendar c = Calendar.getInstance();
		c.setTime(ultimoAcesso != null ? ultimoAcesso.getAccessDate() : pedestre.getDataInicioPeriodo());

		Calendar h = Calendar.getInstance();
		h.setTime(pedestre.getInicioTurno());

		c.set(Calendar.HOUR_OF_DAY, h.get(Calendar.HOUR_OF_DAY));
		c.set(Calendar.MINUTE, h.get(Calendar.MINUTE));
		c.set(Calendar.SECOND, 0);

		if (ultimoAcesso != null) {
			Calendar ajuste = Calendar.getInstance();
			ajuste.setTime(c.getTime()); // c ja cont�m o in�cio do turno
			ajuste.add(tipoAdicao, Integer.parseInt(escala[0])); // Adiciona 12 horas de trabalho

			Calendar agora = Calendar.getInstance();

			// Se o acesso for ap�s o fim do turno atual
			if (agora.after(ajuste)) {
				// Adiciona 36 horas de folga e move para o pr�ximo ciclo de trabalho
				c.add(tipoAdicao, Integer.parseInt(escala[1])); // Adiciona 36 horas de folga
			}
		}
		return c.getTime();
	}

	private static VerificationResult validaDiasHorarios(boolean createNotification, String userName,
			PedestrianAccessEntity matchedAthleteAccess, LogPedestrianAccessEntity logAccess,
			VerificationResult validado, byte[] foto, Integer origem, Date data) {
		VerificationResult resultadoVerificacao;

		Optional<PedestreRegraEntity> regraAtivaPedestre = matchedAthleteAccess.getRegraAtivaPedestre();

		if (regraAtivaPedestre.isPresent() && Objects.nonNull(regraAtivaPedestre.get().getHorarios())) {
			Optional<HorarioEntity> diaValido = regraAtivaPedestre.get().getHorarios().stream()
					.filter(horario -> horario.isDiaPermitido(data)).findFirst();

			if (!diaValido.isPresent()) {
				resultadoVerificacao = VerificationResult.NOT_ALLOWED_TODAY;
				logAccess.setStatus("INATIVO");
				logAccess.setReason("Não permitido hoje");
				if (createNotification) {
					Utils.createNotification(userName + " nao permitido hoje.", NotificationType.BAD, foto);
				}

				return resultadoVerificacao;
			}

			Optional<HorarioEntity> horarioValido = regraAtivaPedestre.get().getHorarios().stream()
					.filter(horario -> horario.isDentroDoHorarioPermitido(data))
					.findFirst();
			if (!horarioValido.isPresent()) {
				resultadoVerificacao = VerificationResult.NOT_ALLOWED_NOW;
				logAccess.setStatus("INATIVO");
				logAccess.setReason("Não permitido agora");
				if (createNotification) {
					Utils.createNotification(userName + " fora do horario.", NotificationType.BAD, foto);
				}

				return resultadoVerificacao;
			}

			Optional<HorarioEntity> horarioValidoComCredito = regraAtivaPedestre.get().getHorarios().stream()
					.filter(horario -> horario.temCreditos())
					.findFirst();

			if (!horarioValidoComCredito.isPresent()) {
				resultadoVerificacao = VerificationResult.NOT_ALLOWED_NO_CREDITS;
				logAccess.setStatus("INATIVO");
				logAccess.setReason("Não permitido, sem creditos");
				if (createNotification) {
					Utils.createNotification(userName + " sem creditos.", NotificationType.BAD, foto);
				}

				return resultadoVerificacao;
			}
		}

		resultadoVerificacao = validado;
//		logAccess.setStatus("INDEFINIDO");

		if (createNotification && Origens.ORIGEM_LIBERADO_SISTEMA.equals(origem)) {
			Utils.createNotification(
					userName + " permitido"
							+ (VerificationResult.TOLERANCE_PERIOD.equals(validado) ? " pela tolerancia." : "."),
					NotificationType.GOOD, foto);
		}

		return resultadoVerificacao;
	}

	private PedestrianAccessEntity buscaPedestrePeloCodigo(final Long codigoUsuario, final Integer origem,
			final Boolean digitaisCatraca) {
		PedestrianAccessEntity matched = null;

		if (Objects.equals(origem, Enumeradores.VIA_TECLADO)) {
			matched = (PedestrianAccessEntity) HibernateAccessDataFacade
					.getSingleResultByRegistration(PedestrianAccessEntity.class, codigoUsuario);

		} else if (Objects.equals(origem, Origens.ORIGEM_LEITOR_1) || Objects.equals(origem, Origens.ORIGEM_LEITOR_2) ||  Objects.equals(origem, Origens.ORIGEM_TESTE_PEDESTRE_REGRA)) {
			matched = (PedestrianAccessEntity) HibernateLocalAccessData
					.getSingleResultByCardNumberString(PedestrianAccessEntity.class, codigoUsuario.toString());
			if (matched == null) {
				matched = (PedestrianAccessEntity) HibernateAccessDataFacade
						.getSingleResultByCardNumber(PedestrianAccessEntity.class, codigoUsuario);
			}

		} else {
			matched = (PedestrianAccessEntity) HibernateAccessDataFacade
					.getSingleResultById(PedestrianAccessEntity.class, codigoUsuario);
		}

		// Tentativa de fallback com número do cartão
		if (matched == null) {
			matched = (PedestrianAccessEntity) HibernateLocalAccessData
					.getSingleResultByCardNumberString(PedestrianAccessEntity.class, codigoUsuario.toString());
			if (matched == null) {
				matched = (PedestrianAccessEntity) HibernateAccessDataFacade
						.getSingleResultByCardNumber(PedestrianAccessEntity.class, codigoUsuario);
			}
		}

		// Tentativa de fallback com ID, caso leitor 1 com digitais ativadas
		if (matched == null && Boolean.TRUE.equals(digitaisCatraca)
				&& Objects.equals(origem, Origens.ORIGEM_LEITOR_1)) {
			matched = (PedestrianAccessEntity) HibernateAccessDataFacade
					.getSingleResultById(PedestrianAccessEntity.class, codigoUsuario);
		}

		return matched;

	}

	private Object[] resultadoNegado(VerificationResult status, String user, PedestrianAccessEntity pedestre,
			String motivo) {
		return new Object[] { status, user, pedestre };
	}

	private Object[] resultadoSucesso(String user, PedestrianAccessEntity pedestre, byte[] foto, String motivo, boolean createNotification) {
		if(createNotification) {
			Utils.createNotification(String.format("Liberado %s", user), NotificationType.GOOD, foto);
		}
		return new Object[] { VerificationResult.ALLOWED, user, pedestre };
	}
}
