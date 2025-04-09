package com.protreino.services.usecase;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.Calendar;
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
import com.protreino.services.repository.LogPedestrianAccessRepository;
import com.protreino.services.repository.PedestrianAccessRepository;
import com.protreino.services.to.AttachedTO;
import com.protreino.services.to.BroadcastMessageTO;
import com.protreino.services.utils.Utils;
import com.topdata.EasyInner;
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
						Utils.createNotification("QRCode do usuario expirado.", NotificationType.BAD, foto);
					}
					
					return new Object[] { VerificationResult.NOT_FOUND, userName, matchedPedestrianAccess };
				}

			} else {
				Long codigoUsuario = Long.valueOf(codigo.trim());

				if (Utils.isNullOrZero(codigoUsuario)) {
					return new Object[] { resultadoVerificacao, userName, matchedPedestrianAccess };					
				}

				matchedPedestrianAccess = buscaPedestrePeloCodigo(codigoUsuario, origem, digitaisCatraca);
			}

			if (Objects.isNull(matchedPedestrianAccess)) {
				System.out.println("Não encontou o pedestre");
				resultadoVerificacao = VerificationResult.NOT_FOUND;
				if (createNotification) {
					Utils.createNotification("Usuario de Codigo " + codigo + " nao encontrado.", NotificationType.BAD, foto);					
				}

				return new Object[] { resultadoVerificacao, userName, matchedPedestrianAccess };
			}
			System.out.println("Encontrou o pedestre");
			
			
			userName = matchedPedestrianAccess.getFirstName().toUpperCase();
			foto = matchedPedestrianAccess.getFoto();
			
			Boolean acessoRestrito = Boolean.valueOf(Utils.getPreference("restrictAccess"));
			Boolean permitidoHoje = true;
			Boolean permitidoSensor = true;
			Boolean permitido = true;
			Boolean permitidoRetornar = true;

			// achou o pedestre, pula regras e envia acesso liberado
			// verifica somente se no for pra pular
			
			final LogPedestrianAccessEntity ultimoAcesso = logPedestrianAccessRepository.buscaUltimoAcesso(matchedPedestrianAccess.getId(),
					matchedPedestrianAccess.getQtdAcessoAntesSinc());
			if(!Objects.isNull(ultimoAcesso))
			{
				System.out.println("ULTIMO ACESSO : " + ultimoAcesso.getDirection());
			    if (ultimoAcesso.isEntrada()) {
			    	
			    	boolean revista = realizarRevista(); // Método para aplicar a lógica de revista

			        if (revista) {
			            if (createNotification) {
			                Utils.createNotification(userName + " não passou pela revista obrigatória.", NotificationType.BAD, foto);
			            }
			            int numeroInner =  Integer.parseInt(equipament.replaceAll("[^0-9]", "")); 
			            EasyInner.AcionarRele2(numeroInner);
			            return new Object[] { VerificationResult.REVISTA_REQUIRED, userName, matchedPedestrianAccess };
			        }
			        
			        
			        boolean saidaSemVerificacao =  Utils.getPreferenceAsBoolean("saidaSemVerificar");
					if(saidaSemVerificacao) {
						System.out.println("SAIDA LIBERADO - TRUE: ");
						permitido = true;
			        
			        }
			    }
			}
			
			if (!Boolean.TRUE.equals(ignoraRegras)) {
				if (isObrigatorioPassagemViaLeitorFacial(matchedPedestrianAccess, origem)) {
					if (createNotification) {
						Utils.createNotification(userName + " deve cadastrar sua face.", NotificationType.BAD, foto);
					}
					
					return new Object[] { VerificationResult.NOT_ALLOWED_FACE_REQUIRED, userName, matchedPedestrianAccess };
				}

				if (isPedestreNaoPossuiRegras(matchedPedestrianAccess)) {
					if (createNotification) {
						Utils.createNotification(userName + " nao possui regras.", NotificationType.BAD, foto);
					}
					return new Object[] { VerificationResult.NOT_ALLOWED, userName, matchedPedestrianAccess };
				}
			}

			matchedPedestrianAccess.setOrigemCatraca(origem);

			if ("INATIVO".equals(matchedPedestrianAccess.getStatus())) {
				Utils.createNotification(" Acesso Negado, usuario: " + userName + " Inativo", NotificationType.BAD, foto);
				motivo = "Usuario inativo.";
				return new Object[] { VerificationResult.NOT_ALLOWED, userName, matchedPedestrianAccess };
			}
			
			if(isNaoPermitidoEquipamentoRestrito(equipament, matchedPedestrianAccess.getEquipamentos())) {
				if (createNotification) {
					Utils.createNotification(userName + " nao permitido nesse equipamento restrito.", NotificationType.BAD, foto);
				}

				return new Object[] { VerificationResult.NOT_ALLOWED_ORIGEM, userName, matchedPedestrianAccess };
			}else {
				System.out.println("Sem equipamento restrito");
			}

			if ((Objects.isNull(origem)
					|| (Boolean.TRUE.equals(matchedPedestrianAccess.getSempreLiberado()) || Boolean.TRUE.equals(ignoraRegras))
					&& origem != Origens.ORIGEM_LEITOR_2)
			&& !matchedPedestrianAccess.isVisitante()) {

				criaLogDeAcessoSempreLiberado(ignoraRegras, origem, matchedPedestrianAccess, location, direction, data,
						codigo, createNotification, equipament, foto, userName);

				return new Object[] { VerificationResult.ALLOWED, userName, matchedPedestrianAccess };
			}

			if (matchedPedestrianAccess.getTipo().equals("PEDESTRE") && origem == Origens.ORIGEM_LEITOR_2) {
				System.out.println("Pedestre permitido no sensor");
				permitidoSensor = false;
			}
			
//			if (isNaoPermitidoNoEquipamento(equipament, matchedPedestrianAccess.getEquipamentos())) {
//				if (createNotification) {
//					Utils.createNotification(userName + " nao permitido nesse equipamento.", NotificationType.BAD, foto);
//				}
//
//				return new Object[] { VerificationResult.NOT_ALLOWED_ORIGEM, userName, matchedPedestrianAccess };
//			}

			
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

			}else if (matchedPedestrianAccess.isVisitante()) {
				System.out.println("Acesso de visitante");
				
				if (!Integer.valueOf(Origens.ORIGEM_LEITOR_2).equals(origem)) {
					if (matchedPedestrianAccess.temCreditos()
							|| isPermitidoPedestreRegra(matchedPedestrianAccess)) {
						permitido = matchedPedestrianAccess.temCreditosValidos();

						// fazer um for validando se existe regra livre ou com quantidade veazia, só
						// assim libero
						System.out.println("usua urna : " + usaUrna);
						
						if ((matchedPedestrianAccess.isUltimoCredito()
								|| (matchedPedestrianAccess.getRegraAtiva().isPresent() 
										&& matchedPedestrianAccess.getRegraAtiva().get().isUltimoCredito()))
								&& !Integer.valueOf(Origens.ORIGEM_BIOMETRIA).equals(origem) 
								&& usaUrna) {
							permitidoSensor = isPermitidoNoSensor(ultimoAcesso, origem, matchedPedestrianAccess);
							System.out.println("permitido no sensor : " + permitidoSensor);
							resultadoVerificacao = VerificationResult.ALLOWED;
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
						System.out.println("Usou urna");
						permitidoSensor = isPermitidoNoSensor(ultimoAcesso, origem, matchedPedestrianAccess);
						System.out.println("permitido no sensor : " + permitidoSensor);
						resultadoVerificacao = VerificationResult.ALLOWED;
					}
				}

			} else {
				if (matchedPedestrianAccess.temCreditos()) {
					permitido = matchedPedestrianAccess.temCreditosValidos(data)
							&& !isPermitidoPedestreRegra(matchedPedestrianAccess)
							&& Objects.nonNull(matchedPedestrianAccess.getCardNumber());

				} else if(matchedPedestrianAccess.temTipoEscala3x3()) {
					System.out.println("Tipo de regra - Escala 3x3");

					LocalDateTime dataAcesso = LocalDateTime.now();
					LocalDate dataInicioEscala = new java.util.Date(
					    matchedPedestrianAccess.getRegraAtivaPedestre().get().getDataInicioPeriodo().getTime()
					).toInstant()
					 .atZone(ZoneId.systemDefault())
					 .toLocalDate();

					LocalTime horarioInicio = matchedPedestrianAccess.getRegraAtivaPedestre().get().getRegra().getHorarioInicioTurno().toInstant()
	                        .atZone(ZoneId.systemDefault())
	                        .toLocalTime();
					
					horarioInicio = horarioInicio.minusHours(3);
					System.out.println("Horario inicio turno : " + horarioInicio);
					
					// Calcular o número de dias entre a data de início da escala e a data de acesso
					long diasEntre = ChronoUnit.DAYS.between(dataInicioEscala, dataAcesso.toLocalDate());

					// Aplicar o ciclo de 12 dias
					int diaDaEscala = (int) (diasEntre % 12) + 1; // +1 para garantir que o ciclo comece do 1


					// Ajustar os intervalos com base no horário de início dinâmico
					LocalTime fimDia = horarioInicio.plusHours(12);
					// 12 horas após o início
					LocalTime fimNoite = LocalTime.of(23, 59, 59);

					// Caso o horário de fim do turno passe para o dia seguinte
					boolean atravessaMeiaNoite = fimDia.isBefore(horarioInicio);
					System.out.println("Data inicio escala : " + dataInicioEscala);
					System.out.println("hora passagem : " + dataAcesso);
					System.out.println("diaDaEscala : " + diaDaEscala);
					System.out.println("horario inicio : " + horarioInicio);
					System.out.println("fim do dia : " + fimDia);
					System.out.println("fim da noite : " + fimNoite);

					if(diaDaEscala == 1 || diaDaEscala == 2 || diaDaEscala == 3) {
					    if (atravessaMeiaNoite) {
					        permitido = dataAcesso.toLocalTime().isAfter(horarioInicio) || dataAcesso.toLocalTime().isBefore(fimDia);
					    } else {
					        permitido = dataAcesso.toLocalTime().isAfter(horarioInicio) && dataAcesso.toLocalTime().isBefore(fimDia);
					    }

					} else if(diaDaEscala == 7) {
					    permitido = dataAcesso.toLocalTime().isAfter(fimDia) && dataAcesso.toLocalTime().isBefore(fimNoite);

					} else if(diaDaEscala == 8 || diaDaEscala == 9) {
					    if (atravessaMeiaNoite) {
					        permitido = (dataAcesso.toLocalTime().isAfter(horarioInicio) || dataAcesso.toLocalTime().isBefore(fimDia))
					                    || (dataAcesso.toLocalTime().isAfter(fimDia) && dataAcesso.toLocalTime().isBefore(fimNoite));
					    } else {
					        permitido = (dataAcesso.toLocalTime().isAfter(horarioInicio) && dataAcesso.toLocalTime().isBefore(fimDia))
					                    || (dataAcesso.toLocalTime().isAfter(fimDia) && dataAcesso.toLocalTime().isBefore(fimNoite));
					    }

					} else if(diaDaEscala == 10) {
					    permitido = dataAcesso.toLocalTime().isAfter(LocalTime.MIDNIGHT) && dataAcesso.toLocalTime().isBefore(horarioInicio);

					} else {
					    permitido = false;
					}

   
				}else if (matchedPedestrianAccess.temTipoTurno()) {
					System.out.println("Tipo escala - Turno ");
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

					System.out.println("Periodo Permitido Inicio: " + new SimpleDateFormat("dd/MM/yyyy HH:mm:ss").format(periodoPermitidoIni.getTime()));
					System.out.println("Periodo Permitido Fim: " + new SimpleDateFormat("dd/MM/yyyy HH:mm:ss").format(periodoPermitidoFim.getTime()));
					System.out.println("Data Acesso: " + new SimpleDateFormat("dd/MM/yyyy HH:mm:ss").format(dataAcesso.getTime()));

//					permitido = dataAcesso.after(periodoPermitidoIni) && dataAcesso.before(periodoPermitidoFim);
					
					Calendar dataInicioTurno = Calendar.getInstance();
					dataInicioTurno.setTime(matchedPedestrianAccess.getInicioTurno());  // Data base do inicio do turno, ex.: 04/10/2024 �s 10:00

					// Calcular a diferen�a em milissegundos entre agora e o in�cio do turno
					long diffMillis = Calendar.getInstance().getTimeInMillis() - dataInicioTurno.getTimeInMillis();
					long diffHours = diffMillis / (1000 * 60 * 60);  // Convertendo para horas

					// Cada ciclo tem 48 horas (12 horas de trabalho + 36 horas de folga)
					long cicloAtual = diffHours % 48;

					// Se está nas primeiras 12 horas do ciclo, ou entre 24 e 36 horas (segundo periodo de trabalho)
					if ((cicloAtual >= 0 && cicloAtual < 12) || (cicloAtual >= 24 && cicloAtual < 36)) {
					    // Est� no periodo de trabalho
					    permitido = true;
					} else {
					    // Est� no periodo de folga
					    permitido = false;
					}

//					permitido = !dataAcesso.before(periodoPermitidoIni) && !dataAcesso.after(periodoPermitidoFim);


				} else if (acessoRestrito) {
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
						Utils.createNotification(userName + " deve depositar cartao na urna.", NotificationType.BAD, foto);
						motivo = "Deve depositar cartao na urna.";

					} else {
						Utils.createNotification(userName + " nao deve depositar na urna", NotificationType.BAD, foto);
						motivo = "Nao deve depositar cartao na urna.";
					}
				}

			} else if (!permitido) {
				resultadoVerificacao = VerificationResult.NOT_ALLOWED;
				if (createNotification) {
					Utils.createNotification(userName + " nao permitido.", NotificationType.BAD, foto);
					motivo = "Nao permitido.";
				}

			} else if (!permitidoRetornar) {
				resultadoVerificacao = VerificationResult.NOT_ALLOWED_NOW;
				if (createNotification) {
					Utils.createNotification(userName + " nao pode retornar agora.", NotificationType.BAD, foto);
					motivo = "Nao pode retornar agora.";
				}

			} else if (permitidoHoje) {
				LogPedestrianAccessEntity logAccess = new LogPedestrianAccessEntity(Main.loggedUser.getId(),
						matchedPedestrianAccess.getId(), matchedPedestrianAccess.getStatus(), location, motivo, direction, equipament);
				if (data != null) {
					logAccess.setAccessDate(data);
				}

				if (matchedPedestrianAccess.isAtivo()) {
					
					if(!matchedPedestrianAccess.isVisitante()) {
						resultadoVerificacao = validaDiasHorarios(createNotification, userName, matchedPedestrianAccess,
								logAccess, VerificationResult.ALLOWED, foto, origem, data);
					}
				} else {
					resultadoVerificacao = VerificationResult.NOT_ALLOWED;
					logAccess.setStatus("INATIVO");
					if (createNotification) {
						Utils.createNotification(userName + " nao permitido.", NotificationType.BAD, foto);
						motivo = "Nao permitido.";
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
					Utils.createNotification(userName + " ja registrado hoje.", NotificationType.BAD, foto);
				}
			}

		} catch (Exception e) {
			e.printStackTrace();
			resultadoVerificacao = VerificationResult.ERROR;
			if (createNotification) {
				Utils.createNotification("Falha ao processar requisição de acesso. " + e.getMessage(),
						NotificationType.BAD, foto);
			}
		}
		System.out.println("Resultado da verificação : " + resultadoVerificacao);
		
		return new Object[] { resultadoVerificacao, userName, matchedPedestrianAccess };
	}
	
	private boolean realizarRevista() {
		int porcentagem = Utils.getPreferenceAsInteger("porcentagemRevista");
		double porcentagemConvertida = porcentagem;
		// TODO Auto-generated method stub
		// Gerador de número aleatório
		System.out.println("entrou na revista");
		Random random = new Random();
		double chance = random.nextDouble(); // Valor entre 0.0 e 1.0
		System.out.println("porcentagem : " + porcentagem + "% ");

		if (porcentagemConvertida != 0) {
		    porcentagemConvertida = porcentagemConvertida / 100.00;
		}

		System.out.println("porcentagem calculada : " + porcentagemConvertida); // Corrigido para exibir porcentagemConvertida
		System.out.println("chance gerada : " + chance);

		// Verifica se a chance gerada é menor ou igual à porcentagem convertida
		if (chance <= porcentagemConvertida) {
		    return true;
		}

		return false;

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

		// nao tem bloqueo por equipamento
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

//		if (ultimoAcesso != null) {
//			Calendar ajuste = Calendar.getInstance();
//			ajuste.setTime(c.getTime());
//			ajuste.add(tipoAdicao, Integer.parseInt(escala[0]));
//
//			Calendar agora = Calendar.getInstance();
//			if (agora.after(ajuste)) {
//				c.add(tipoAdicao, Integer.parseInt(escala[1]));
//			} else {
//				c.add(tipoAdicao, Integer.parseInt(escala[0]) * -1);
//			}
//		}
		
		if (ultimoAcesso != null) {
		    Calendar ajuste = Calendar.getInstance();
		    ajuste.setTime(c.getTime());  // c ja cont�m o in�cio do turno
		    ajuste.add(tipoAdicao, Integer.parseInt(escala[0]));  // Adiciona 12 horas de trabalho

		    Calendar agora = Calendar.getInstance();
		    
		    // Se o acesso for ap�s o fim do turno atual
		    if (agora.after(ajuste)) {
		        // Adiciona 36 horas de folga e move para o pr�ximo ciclo de trabalho
		        c.add(tipoAdicao, Integer.parseInt(escala[1]));  // Adiciona 36 horas de folga
		    }
		}


		return c.getTime();
	}
	
	private static VerificationResult validaDiasHorarios(boolean createNotification, String userName,
			PedestrianAccessEntity matchedAthleteAccess, LogPedestrianAccessEntity logAccess,
			VerificationResult validado, byte[] foto, Integer origem, Date data) {
		VerificationResult resultadoVerificacao;

		Optional<PedestreRegraEntity> regraAtivaPedestre = matchedAthleteAccess.getRegraAtivaPedestre();
		
		if(regraAtivaPedestre.isPresent() 
				&& Objects.nonNull(regraAtivaPedestre.get().getHorarios())) {
			Optional<HorarioEntity> diaValido = regraAtivaPedestre.get().getHorarios()
			.stream()
			.filter(horario -> horario.isDiaPermitido(data))
			.findFirst();
			
			if(!diaValido.isPresent()) {
				resultadoVerificacao = VerificationResult.NOT_ALLOWED_TODAY;
				logAccess.setStatus("INATIVO");
				if (createNotification) {
					Utils.createNotification(userName + " nao permitido hoje.", NotificationType.BAD, foto);
				}
				
				return resultadoVerificacao;
			}
			
			Optional<HorarioEntity> horarioValido = regraAtivaPedestre.get().getHorarios()
					.stream()
					.filter(horario -> horario.isDiaPermitido(data) && horario.isDentroDoHorarioPermitido(data))
					.findFirst();
			if(!horarioValido.isPresent()) {
				resultadoVerificacao = VerificationResult.NOT_ALLOWED_NOW;
				logAccess.setStatus("INATIVO");
				if (createNotification) {
					Utils.createNotification(userName + " fora do horario.", NotificationType.BAD, foto);
				}
				
				return resultadoVerificacao;
			}
			
			Optional<HorarioEntity> horarioValidoComCredito = regraAtivaPedestre.get().getHorarios()
					.stream()
					.filter(horario -> horario.isDiaPermitido(data) && horario.isDentroDoHorarioPermitido(data) && horario.temCreditos())
					.findFirst();
			
			if(!horarioValidoComCredito.isPresent()) {
				resultadoVerificacao = VerificationResult.NOT_ALLOWED_NO_CREDITS;
				logAccess.setStatus("INATIVO");
				if (createNotification) {
					Utils.createNotification(userName + " sem creditos.", NotificationType.BAD, foto);
				}
				
				return resultadoVerificacao;
			}
		}
		
		resultadoVerificacao = validado;
		logAccess.setStatus("ATIVO");

		if (createNotification && Origens.ORIGEM_LIBERADO_SISTEMA.equals(origem)) {
			Utils.createNotification(
					userName + " permitido"
							+ (VerificationResult.TOLERANCE_PERIOD.equals(validado) ? " pela toler�ncia."
									: "."), NotificationType.GOOD, foto);
		}

		return resultadoVerificacao;
	}
	
	private PedestrianAccessEntity buscaPedestrePeloCodigo(final Long codigoUsuario, final Integer origem, final Boolean digitaisCatraca) {
		PedestrianAccessEntity matchedPedestrianAccess = null;
		System.out.println("codigo usuario : " + codigoUsuario);
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
		
		return matchedPedestrianAccess;
	}
}
