package com.protreino.services.devices;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

import javax.swing.SwingWorker;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;
import com.protreino.services.entity.CartaoComandaEntity;
import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.entity.LogCartaoComandaEntity;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.enumeration.FieldType;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.enumeration.StatusCard;
import com.protreino.services.enumeration.VerificationResult;
import com.protreino.services.main.Main;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.to.AttachedTO;
import com.protreino.services.to.ConfigurationGroupTO;
import com.protreino.services.to.ConfigurationTO;
import com.protreino.services.utils.Utils;
import com.topdata.EasyInner;
import com.topdata.easyInner.enumeradores.Enumeradores;
import com.topdata.easyInner.enumeradores.Enumeradores.EstadosInner;

import static com.protreino.services.constants.TopDataExpedidoraDevice.*;

@SuppressWarnings("serial")
public class TopDataExpedidoraDevice extends TopDataDevice {

	public static final int EXPEDIDORA_ENTRADA_COM_CARTAO = 10;
	public static final int EXPEDIDORA_TIMEOUT_COM_CARTAO = 12;
	public static final int EXPEDIDORA_CARTAO_SEM_VALIDACAO = 13;
	public static final int EXPEDIDORA_ENTRADA_COM_BOTAO = 0;
	public static final int EXPEDIDORA_TIMEOUT_COM_BOTAO = 2;
	public static final int EXPEDIDORA_MECANISMO_COM_CARTAO = 3;
	public static final int EXPEDIDORA_MECANISMO_POUCO_CARTAO = 4;
	public static final int EXPEDIDORA_MECANISMO_SEM_CARTAO = 5;

	private CartaoComandaEntity matched;
	private Date dataGiro;
	private int origem = 0;

	public TopDataExpedidoraDevice(DeviceEntity deviceEntity) {
		this(deviceEntity.getIdentifier(), deviceEntity.getConfigurationGroupsTO());
		this.deviceEntity = deviceEntity;
		this.name = deviceEntity.getName();
		this.location = deviceEntity.getLocation();
		this.desiredStatus = deviceEntity.getDesiredStatus();
		this.defaultDevice = deviceEntity.getDefaultDevice();
		this.athleteScreenConfig = deviceEntity.getAthleteScreenConfig();

		Gson gson = new GsonBuilder().create();
		List<AttachedTO> attachedDevices = gson.fromJson(deviceEntity.getAttachedDevices(),
				new TypeToken<List<AttachedTO>>() {
				}.getType());
		List<AttachedTO> attachedHikivisionCameras = gson.fromJson(deviceEntity.getAttachedHikivisionCameras(),
				new TypeToken<List<AttachedTO>>() {
				}.getType());

		this.setAttachedDevices(attachedDevices);
		this.setAttachedHikivisionCameras(attachedHikivisionCameras);
	}

	public TopDataExpedidoraDevice(String identifier) {
		super(identifier);
		this.manufacturer = Manufacturer.TOP_DATA_EXPEDIDORA;
		this.name = "TopData Expedidora (" + this.tipo + ") " + this.innerNumber;
	}

	public TopDataExpedidoraDevice(String identifier, List<ConfigurationGroupTO> configurationGroups) {
		super(identifier, configurationGroups);
		this.manufacturer = Manufacturer.TOP_DATA_EXPEDIDORA;
		this.name = "TopData Expedidora (" + this.tipo + ") " + this.innerNumber;
	}

	@Override
	public void connect(String... args) throws Exception {
		if ("SAIDA".equals(this.tipo)) {
			super.connect(args);
		} else {

			// faz conexÃ£o propria
			easyInner = new EasyInner();
			int ret = 0;
			if (!portaAberta) {
				EasyInner.DefinirTipoConexao(2);
				ret = EasyInner.AbrirPortaComunicacao(port);
				if (ret != Enumeradores.RET_COMANDO_OK && ret != Enumeradores.RET_PORTA_JAABERTA)
					throw new Exception("Erro ao abrir a porta de comunica��o: " + ret);
				portaAberta = true;
			}
			ret = Enumeradores.Limpar;
			Long inicio = System.currentTimeMillis();
			Long tempoDeEspera = getConfigurationValueAsLong(TEMPO_ESPERA_PARA_CONECTAR) * 1000;
			while (ret != Enumeradores.RET_COMANDO_OK && (System.currentTimeMillis() - inicio) < tempoDeEspera) {
				ret = testarConexaoInner(inner.Numero);
				Utils.sleep(50);
			}
			if (ret != Enumeradores.RET_COMANDO_OK) {
				throw new Exception("N�o foi poss�vel conectar.");
			}

			sendConfiguration();

			setStatus(DeviceStatus.CONNECTED);
			watchDogEnabled = true;
			workerEnabled = true;

			worker = new SwingWorker<Void, Void>() {
				@Override
				protected Void doInBackground() throws Exception {
					while (workerEnabled) {
						try {
							while (sendingConfiguration) {
								// aguarda configuracoes serem enviadas
								Utils.sleep(50);
							}

							coletarBilhetesOffLine();

							Long tempo = getConfigurationValueAsLong(TEMPO_COLETAR_CARTAO_COMANDA);
							if (tempo == null) {
								tempo = 3l;
							}

							Utils.sleep(tempo * 1000);

						} catch (Exception e) {
							e.printStackTrace();
						}
					}
					return null;
				}
			};
			worker.execute();

			watchDog = new SwingWorker<Void, Void>() {
				@Override
				protected Void doInBackground() throws Exception {
					while (watchDogEnabled) {
						Long sleepTime = null;
						try {
							if (!busy)
								ping();
							sleepTime = getConfigurationValueAsLong(TEMPO_DE_PING) * 1000;

							if (DeviceStatus.DISCONNECTED.equals(lastStatus)
									&& DeviceStatus.CONNECTED.equals(getStatus())) {
								sendConfiguration();
							}

						} catch (Exception e) {
							e.printStackTrace();
							setStatus(DeviceStatus.DISCONNECTED);
						} finally {
							Utils.sleep(sleepTime != null ? sleepTime : 5000);
						}
					}
					return null;
				}
			};
			watchDog.execute();

		}
	}

	@Override
	protected Integer testarConexaoInner(Integer Inner) {
		return "SAIDA".equals(this.tipo) ? super.testarConexaoInner(Inner) : 0;
	}

	@Override
	public void sendConfiguration() throws Exception {
		if ("SAIDA".equals(this.tipo)) {
			super.sendConfiguration();
		} else {
			sendingConfiguration = true;
			configureInner();

			enviarConfiguracoesOffline();
			enviarMensagensOffline();
			enviarConfiguracaoMudancaOnlineOffline();

			coletarBilhetesOffLine();

			sendingConfiguration = false;
		}
	}

	@Override
	protected void registraGiro(int sentido, Date data) {

		// volta cartão para o status de AGUARDANDO
		if (matched != null) {
			
			
			// verifica sentido
			if ("ENTRADA".equals(this.tipo) || 
					Objects.equals(getConfigurationValueAsString(LEITOR_1), "Somente entrada_1")) {
				
				// bloqueia se entrada
				matched.setDataAlteracao(new Date());
				matched.setStatus(
						getConfigurationValueAsBoolean(CARTAO_EXPEDIDO_STATUS_LIBERADO) ? StatusCard.LIBERADO
								: StatusCard.BLOQUEADO);
			} else {
				// aguardando se saida
				matched.setDataAlteracao(new Date());
				matched.setStatus(StatusCard.AGUARDANDO);
			}
			
			System.out.println("status do cartao: " + matched.getStatus());

			HibernateAccessDataFacade.save(CartaoComandaEntity.class, matched);

			// cria log de liberação (sem sincronizaÃ§Ã£o com web)
			LogCartaoComandaEntity log = new LogCartaoComandaEntity(matched);
			log.setUsuario(Main.internoLoggedUser);
			log.setTipoLiberacao(this.tipo + "_" + matched.getStatus().name());
			log.setOrigem("CATRACA");
			log.setData(new Date());
			HibernateAccessDataFacade.save(LogCartaoComandaEntity.class, log);

			matched = null;
			allowedUserName = null;
		}

	}

	@SuppressWarnings("unchecked")
	@Override
	public void processAccessRequest(Object obj) {
		if (this.dataGiro == null) {
			dataGiro = new Date();
		}

		String cartao = obj.toString();
		if ("ENTRADA".equals(this.tipo) && !Objects.equals(cartao, "0000000000000000")) {
			cartao = Utils.toHEX(obj.toString().replaceAll("[^a-zA-Z0-9]+", ""));
			cartao = cartao.substring(2);
			// System.out.println("Numero do cartão: " +cartao);
		}

		// procura cartão por cÃ³digo real
		HashMap<String, Object> args = new HashMap<>();
		args.put("numeroReal", cartao);
		args.put("removido", false);
		List<CartaoComandaEntity> cartoes = (List<CartaoComandaEntity>) HibernateAccessDataFacade
				.getResultListWithDynamicParams(CartaoComandaEntity.class, null, args);

		if (cartoes != null && !cartoes.isEmpty()) {
			if (cartoes.size() > 1) {
				for (CartaoComandaEntity c : cartoes) {
					if (c.getRemovido() == null || Boolean.FALSE.equals(c.getRemovido())) {
						matched = c;
						break;
					}
				}
			} else {
				matched = cartoes.get(0);
			}
		}

		if (matched != null) {

			allowedUserName = matched.getNumeroAlternativo();

			if ("ENTRADA".equals(this.tipo)) {
				// entrada sempre liberada
				verificationResult = VerificationResult.ALLOWED;
			} else {
				origem = inner.BilheteInner.Origem;

				// verificar se vai validar a origem
				if (inner.BilheteInner.Origem != 3)
					verificationResult = VerificationResult.NOT_ALLOWED_SENSOR;
				else {
					// verifica se cartão pode sair
					// esta no status de LIBERADO
					if (StatusCard.LIBERADO.equals(matched.getStatus())) {
						verificationResult = VerificationResult.ALLOWED;
						// catraca com urna
						// boolean usaUrna = getConfigurationValueAsBoolean("Lógica da catraca com
						// urna");
						// if(usaUrna)
						// EasyInner.AcionarRele2(inner.Numero);
					} else {
						verificationResult = VerificationResult.NOT_ALLOWED;
					}
				}
			}
		} else {
			// cartão nÃ£o encontrado
			verificationResult = VerificationResult.NOT_FOUND;
		}

		this.dataGiro = null;
	}

	@Override
	public void processAccessRequest(Object obj, Date data) {

		// logica propria
		this.dataGiro = data;

		processAccessRequest(obj);
	}

	@Override
	protected void validarAcesso() {
		try {
			validandoAcesso = true;
			System.out.print("\n" + sdf.format(new Date()) + "  VALIDAR ACESSO SA�DA: ");
			System.out.print(" Origem: " + inner.BilheteInner.Origem);
			System.out.println("   Cartao: " + inner.BilheteInner.Cartao);

			if (inner.BilheteInner.Origem == 1 || inner.BilheteInner.Origem == 2 || inner.BilheteInner.Origem == 3
					|| inner.BilheteInner.Origem == 12) { // Teclado, sensor de proximidade ou biometrico(cartao)
				processAccessRequest(inner.BilheteInner.Cartao.toString());

			}

			if (athleteScreen != null)
				athleteScreen.requisicaoPorDigital(null, verificationResult, allowedUserName, matchedAthleteAccess);

			if (VerificationResult.ALLOWED.equals(verificationResult)
					|| VerificationResult.TOLERANCE_PERIOD.equals(verificationResult)) {
				// catraca com urna
				boolean usaUrna = getConfigurationValueAsBoolean(LOGICA_DE_CATRACA_COM_URNA);
				if (usaUrna && origem == 3) {
					EasyInner.AcionarRele2(inner.Numero);
				} else {
					allowAccess();
				}
			} else {
				denyAccess();
			}

		} catch (Exception e) {
			e.printStackTrace();
			inner.EstadoAtual = EstadosInner.ESTADO_CONECTAR;
		}
		validandoAcesso = false;
	}

	@Override
	public void createDefaultConfiguration() {
		if ("SAIDA".equals(this.tipo)) {
			super.createDefaultConfiguration();
		} else {

			List<ConfigurationTO> geralConfigurations = new ArrayList<ConfigurationTO>();
			geralConfigurations.add(new ConfigurationTO(MODO_DE_TRABALHO, "Digitais no servidor_noServidor",
					FieldType.COMBOBOX, "Digitais na catraca_naCatraca;Digitais no servidor_noServidor"));
			geralConfigurations.add(new ConfigurationTO(ENVIA_DIGITAIS_PARA_CATRACA, "false", FieldType.CHECKBOX));
			geralConfigurations.add(new ConfigurationTO("Sentido da catraca", "Hor�rio_clockwise", FieldType.COMBOBOX,
					"Hor�rio_clockwise;Antihor�rio_anticlockwise"));
			geralConfigurations.add(new ConfigurationTO(TEMPO_DE_LIBERADO, "7", FieldType.NUMERIC_LIST, "5;1;15"));
			geralConfigurations
					.add(new ConfigurationTO(TEMPO_DE_MENSAGEM_NEGADO, "5", FieldType.NUMERIC_LIST, "1;1;15"));
			geralConfigurations.add(new ConfigurationTO(BLOQUEAR_SAIDA, "true", FieldType.CHECKBOX));
			geralConfigurations.add(new ConfigurationTO(HABILITAR_TECLADO, "true", FieldType.CHECKBOX));
			geralConfigurations.add(new ConfigurationTO(ECOAR_ASTERISCOS, "false", FieldType.CHECKBOX));
			geralConfigurations.add(new ConfigurationTO(NIVEL_RECONHECIMENTO, "6", FieldType.NUMERIC_LIST, "1;1;9"));
			geralConfigurations.add(new ConfigurationTO(TEMPO_TECLADO, "10", FieldType.NUMERIC_LIST, "5;1;20"));
			geralConfigurations
					.add(new ConfigurationTO(TEMPO_MUDANCA_ONLINE_OFFLINE, "10", FieldType.NUMERIC_LIST, "6;1;20"));
			geralConfigurations.add(new ConfigurationTO(TEMPO_DE_PING, "5", FieldType.NUMERIC_LIST, "2;1;10"));
			geralConfigurations
					.add(new ConfigurationTO(TEMPO_ESPERA_PARA_CONECTAR, "10", FieldType.NUMERIC_LIST, "5;1;20"));
			geralConfigurations.add(new ConfigurationTO(TIPO_LEITOR, "Proximidade AbaTrack2_2", FieldType.COMBOBOX,
					"Codigo de barras_0;Magn�tico_1;Proximidade AbaTrack2_2;Proximidade Wiegand_3;Proximidade Wiegand FC_33;"
							+ "Proximidade Wiegand FC Sem Separador_6;Proximidade Smart Card_4;QRCode_7;",
					240));
			geralConfigurations
					.add(new ConfigurationTO(QUANTIDADE_DIGITOS_CARTAO, "14", FieldType.NUMERIC_LIST, "4;1;16"));
			geralConfigurations.add(new ConfigurationTO(MODELO_BIOMETRICO, "false", FieldType.CHECKBOX));
			geralConfigurations
					.add(new ConfigurationTO(TIPO_BIOMETRICO, "LFD_lfd", FieldType.COMBOBOX, "LFD_lfd;LC_lc"));
			geralConfigurations.add(new ConfigurationTO(DOIS_LEITORES, "true", FieldType.CHECKBOX,
					"(usa para catracas com urna)", true));
			geralConfigurations.add(new ConfigurationTO(LEITOR_1, "Somente entrada_1", FieldType.COMBOBOX,
					"Desativado_0;Somente entrada_1;Somente sa�da_2;Entrada e sa�da_3;Sa�da e entrada_4"));
			geralConfigurations.add(new ConfigurationTO(LEITOR_2, "Desativado_0", FieldType.COMBOBOX,
					"Desativado_0;Somente entrada_1;Somente sa�da_2;Entrada e sa�da_3;Sa�da e entrada_4"));
			geralConfigurations
					.add(new ConfigurationTO(IDENTIFICACAO_BIOMETRICA, "N�o_1", FieldType.COMBOBOX, "Sim_1;N�o_0"));
			geralConfigurations
					.add(new ConfigurationTO(VERIFICACAO_BIOMETRICA, "N�o_0", FieldType.COMBOBOX, "Sim_1;N�o_0"));
			geralConfigurations.add(new ConfigurationTO(PADRAO_DE_CARTAO, "Padr�o livre_1", FieldType.COMBOBOX,
					"Padr�o livre_1;Padr�o TopData_0"));
			geralConfigurations.add(new ConfigurationTO(LOGICA_DE_CATRACA_COM_URNA, "false", FieldType.CHECKBOX));

			geralConfigurations
					.add(new ConfigurationTO(TEMPO_COLETAR_CARTAO_COMANDA, "3", FieldType.NUMERIC_LIST, "3;1;15"));
			geralConfigurations.add(new ConfigurationTO(CARTAO_EXPEDIDO_STATUS_LIBERADO, "false", FieldType.CHECKBOX));

			String nomeEmpresa = "SmartPonto;Controle Acesso";
			if (Main.loggedUser != null)
				nomeEmpresa = Utils.formatAcademyName(Main.loggedUser.getName());
			if (nomeEmpresa.length() > 16)
				nomeEmpresa = nomeEmpresa.substring(0, 16).trim() + ";" + nomeEmpresa.substring(16, 32).trim();

			List<ConfigurationTO> customConfigurations = new ArrayList<ConfigurationTO>();
			customConfigurations.add(new ConfigurationTO(MENSAGEM_ONLINE, nomeEmpresa, FieldType.MESSAGE_LINES));

			configurationGroups = new ArrayList<ConfigurationGroupTO>();
			configurationGroups.add(new ConfigurationGroupTO("Geral", geralConfigurations));
			configurationGroups.add(new ConfigurationGroupTO("Personaliza��o", customConfigurations));
		}

	}

	protected int ping() {
		if ("SAIDA".equals(this.tipo)) {
			final int ret = super.ping();
			
			if (ret == easyInner.RET_COMANDO_OK) {
				inner.TempoInicialPingOnLine = System.currentTimeMillis();
				if(!coletandoDadosOffLine) {
					setStatus(DeviceStatus.CONNECTED);
				}
				inner.CountRepeatPingOnline = 0;
			
			} else {
				System.out.println("a catraca caiu " + inner.Numero);
				setStatus(DeviceStatus.DISCONNECTED);
			}
			
			return ret;
		} else {
			int ret = 0;
			try {
				int countTentativasEnvioComando = 0;
				ret = EasyInner.PingOnLine(inner.Numero);
				while (ret != easyInner.RET_COMANDO_OK && countTentativasEnvioComando < 3) {
					Utils.sleep(300);
					ret = testarConexaoInner(inner.Numero);
					countTentativasEnvioComando++;
				}
				if (ret == easyInner.RET_COMANDO_OK) {
					inner.TempoInicialPingOnLine = System.currentTimeMillis();
					if (!coletandoDadosOffLine)
						setStatus(DeviceStatus.CONNECTED);
					inner.CountRepeatPingOnline = 0;

				} else
					setStatus(DeviceStatus.DISCONNECTED);
			} catch (Exception ex) {
				ex.printStackTrace();
				setStatus(DeviceStatus.DISCONNECTED);
			}
			return ret;
		}
	}

	public boolean isTheSame(Object obj) {
		Device other = (Device) obj;
		if (!this.manufacturer.equals(other.manufacturer))
			return false;

		String[] partsThis = this.identifier.split(";");
		String[] partsOther = other.identifier.split(";");
		if (!partsThis[0].equals(partsOther[0]))
			return false;
		return true;
	}

}
