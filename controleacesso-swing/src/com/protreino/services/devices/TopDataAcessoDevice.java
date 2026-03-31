package com.protreino.services.devices;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;
import com.protreino.services.constants.Origens;
import com.protreino.services.constants.Tipo;
import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.enumeration.FieldType;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.enumeration.VerificationResult;
import com.protreino.services.main.Main;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.repository.HibernateLocalAccessData;
import com.protreino.services.to.AttachedTO;
import com.protreino.services.to.ConfigurationGroupTO;
import com.protreino.services.to.ConfigurationTO;
import com.protreino.services.usecase.ProcessAccessRequestUseCase;
import com.protreino.services.utils.Utils;
import com.topdata.EasyInner;
import com.topdata.easyInner.enumeradores.Enumeradores.EstadosInner;

import static com.protreino.services.constants.TopDataAcessoDeviceConstatns.*;
import static com.protreino.services.constants.TopDataDeviceConstants.IGNORAR_REGRAS_DE_ACESSO;
import static com.protreino.services.constants.TopDataDeviceConstants.ONLY_ENABLED_MODE;
import static com.protreino.services.constants.TopDataDeviceConstants.SENTIDO_DA_CATRACA;

@SuppressWarnings("serial")
public class TopDataAcessoDevice extends TopDataDevice {
	
	public TopDataAcessoDevice(DeviceEntity deviceEntity){
		this(deviceEntity.getIdentifier(), deviceEntity.getConfigurationGroupsTO());
		this.deviceEntity = deviceEntity;
		this.name = deviceEntity.getName();
		this.location = deviceEntity.getLocation();
		this.desiredStatus = deviceEntity.getDesiredStatus();
		this.defaultDevice = deviceEntity.getDefaultDevice();
		this.athleteScreenConfig = deviceEntity.getAthleteScreenConfig();
		
		Gson gson = new GsonBuilder().create();
		List<AttachedTO> attachedDevices = gson.fromJson(deviceEntity.getAttachedDevices(), new TypeToken<List<AttachedTO>>() {}.getType());
		List<AttachedTO> attachedHikivisionCameras = gson.fromJson(deviceEntity.getAttachedHikivisionCameras(), new TypeToken<List<AttachedTO>>() {}.getType());

		this.setAttachedDevices(attachedDevices);
		this.setAttachedHikivisionCameras(attachedHikivisionCameras);
	}
	
	public TopDataAcessoDevice(String identifier){
		super(identifier);
		this.manufacturer = Manufacturer.TOP_DATA_ACESSO;
		this.name = "TopData Acesso Inner " + this.innerNumber;
	}
	
	public TopDataAcessoDevice(String identifier, List<ConfigurationGroupTO> configurationGroups){
		super(identifier, configurationGroups);
		this.manufacturer = Manufacturer.TOP_DATA_ACESSO;
		this.name = "TopData Acesso Inner " + this.innerNumber;
	}
	
	
	public void validaAcessoHikivision(final String cardNumber, String cameraNome) {
		try {
			while (sendingConfiguration) {
				Utils.sleep(50);
			}
			
			Main.validandoAcesso = true;
			if(Main.temServidor()) {
				HibernateAccessDataFacade.enviaInicioVerificandoAcesso();
			}
			
			validandoAcesso = true;
			System.out.print("\n" + sdf.format(new Date()) + "  VALIDAR ACESSO HIKIVISION: ");
			System.out.println("\tCartao: " + cardNumber);
			System.out.print(" Origem: " + inner.BilheteInner.Origem);
			
			if(Objects.isNull(inner.BilheteInner.Origem)) {
				inner.BilheteInner.Origem = Origens.ORIGEM_LEITOR_1;
			}
			
			
			inner.BilheteInner.Cartao.setLength(0);
			inner.BilheteInner.Cartao = new StringBuilder(cardNumber);
			
			System.out.println(">>> PASSAGEM NA CAMERA - " + cameraNome);
			
			if (this.ignorarAcesso() || Utils.isAcessoLiberado()) {

				setVerificationResult(VerificationResult.ALLOWED);

				PedestrianAccessEntity pedestre = (PedestrianAccessEntity) HibernateLocalAccessData
						.getSingleResultByCardNumberString(PedestrianAccessEntity.class, cardNumber);
				if (pedestre == null) {
					pedestre = (PedestrianAccessEntity) HibernateAccessDataFacade
							.getSingleResultByCardNumber(PedestrianAccessEntity.class, Long.valueOf(cardNumber));
				}

				if (Objects.isNull(pedestre)) {
					System.out.println("pedestre não encontrado — sempre liberado mesmo assim");
				} else {
					System.out.println("Acesso de : " + pedestre.getName());

					LogPedestrianAccessEntity logAccess = new LogPedestrianAccessEntity(Main.loggedUser.getId(),
							pedestre.getId(), false, location, "", null, Objects.nonNull(this) ? this.getName() : "",
							cardNumber, new Date());

					logAccess.setStatus("INDEFINIDO");
					HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);

					System.out.println(">>>>>> ACESSO SEMPRE LIBERADO");
				}

			} else {
			    // Processa normalmente
			    processAccessRequest(cardNumber, false);
			}

				
			if (athleteScreen != null) {
				athleteScreen.requisicaoPorDigital(null, verificationResult, allowedUserName, matchedAthleteAccess);
			}
			
			if (VerificationResult.ALLOWED.equals(getVerificationResult())
					|| VerificationResult.TOLERANCE_PERIOD.equals(getVerificationResult())) {
	            allowAccess(cameraNome);
			} else {
				denyAccess();
			}
			
		} catch (Exception e) {
			e.printStackTrace();
			inner.EstadoAtual = EstadosInner.ESTADO_CONECTAR;
		} finally {
			validandoAcesso = false;
			Main.validandoAcesso = false;
			if(Main.temServidor()) {
				HibernateAccessDataFacade.enviaFimVerificandoAcesso();										
			}
		}
		
	}
	
	@Override
	public void allowAccess() {
	    // fluxo antigo, intacto
	    allowAccess((String) null );
	}
	
	
	public void allowAccess(String sentidoCamera) {
		procuraSeExisteMensagemParaPedestre();

		if (mensagemPermitido == null || mensagemPermitido.isEmpty()) {
			mensagemPermitido = formatMessage(VerificationResult.AUTHORIZED.getMessage());
		}

		if (allowedUserName != null) {
			mensagemPermitido = formatMessage(mensagemPermitido + ";" + allowedUserName);
		}

		EasyInner.LigarLedVerde(inner.Numero);
		EasyInner.EnviarMensagemPadraoOnLine(inner.Numero, 0, mensagemPermitido);

		if (Objects.nonNull(sentidoCamera)) {
			decideLadoEntrada(sentidoCamera);
		}else {
			System.out.println("Inner origem : " + inner.BilheteInner.Origem);
			
			Integer sentido = (inner.BilheteInner.Origem == 2 )
					? getConfigurationValueAsInteger(LEITOR_1)
					: getConfigurationValueAsInteger(LEITOR_2);

			if (sentido == 1) {
				System.out.println("Libera entrada");
				AcionaRele1();
			} else if (sentido == 2) {
				System.out.println("Libera saida");
				AcionaRele2();
			}
		}

		Boolean usaTorniquete = getConfigurationValueAsBoolean(USA_TORNIQUETE);
		// rturn pois vai validar o giro na logica do registra giro
		if (Boolean.TRUE.equals(usaTorniquete)) {
			return;
		}

		LogPedestrianAccessEntity ultimo = buscaUltimoAcesso();

		if (ultimo == null) {
			return;
		}

		// registar giro manualmente
		Integer sentido = (inner.BilheteInner.Complemento == 1 || inner.BilheteInner.Complemento == 38)
				? getConfigurationValueAsInteger(LEITOR_2)
				: getConfigurationValueAsInteger(LEITOR_1);

		if (sentido == 1) {
			ultimo.setDirection(Tipo.ENTRADA);
		} else if (sentido == 2) {
			ultimo.setDirection(Tipo.SAIDA);
		}

		ultimo.setStatus("ATIVO");
		ultimo.setDataCriacao(new Date());
		registraGiro(sentido, null);

		HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, ultimo);

		PedestrianAccessEntity pedestre = (PedestrianAccessEntity) HibernateAccessDataFacade
				.getSingleResultById(PedestrianAccessEntity.class, ultimo.getIdPedestrian());

		if (pedestre == null) {
			return;
		}

		boolean ignoraRegras = getConfigurationValueAsBoolean(IGNORAR_REGRAS_DE_ACESSO);
		if (!ignoraRegras) {
			if (pedestre.temMensagens()) {
				pedestre.decrementaMensagens();
			}

			if (getConfigurationValueAsBoolean(BLOQUEAR_SAIDA) && Tipo.SAIDA.equals(ultimo.getDirection())) {
				pedestre.decrementaCreditos();
			}

			if ("DINAMICO_USO".equals(pedestre.getTipoQRCode())) {
				pedestre.decrementaQRCodeUso();
			}

			HibernateAccessDataFacade.save(PedestrianAccessEntity.class, pedestre);
		}

		try {
			enviarMensagemPadrao();
			configurarEntradasOnline();

		} catch (Exception e) {
			e.printStackTrace();
		}

	}
	
	
	private LogPedestrianAccessEntity buscaUltimoAcesso() {
		String query = "";

		HashMap<String, Object> args = new HashMap<>();

		// ===== TRATAMENTO 1: DEVICE =====
		Device device = equipamentoPassado(getFullIdentifier());
		if (device == null) {
			System.out.println("ERRO: Equipamento não encontrado");
			return null;
		}

		System.out.println("EQUIPAMENTO QUE REGISTROU GIRO: " + device.getName());
		args.put("EQUIPAMENTO", device.getName());

		// ===== TRATAMENTO 2: INNER =====
		if (inner == null || inner.BilheteInner == null) {
			System.out.println("ERRO: inner ou BilheteInner nulo");
			return null;
		}

		// DEBUG
		System.out.println(">>> Cartao bruto: " + inner.BilheteInner.Cartao);
		System.out.println(">>> FacialId: " + matchedFacialId);

		String cartaoStr = (inner.BilheteInner.Cartao != null) ? inner.BilheteInner.Cartao.toString() : null;

		boolean cartaoEhValido = cartaoStr != null && !cartaoStr.isEmpty() && !cartaoStr.replace("0", "").isEmpty();

		if (cartaoEhValido) {

			if (cartaoStr.startsWith("05000") || cartaoStr.startsWith("005000") || cartaoStr.startsWith("00100")) {
				cartaoStr = cartaoStr.substring(5);
			}

			System.out.println(">>> Registra giro com CARTAO válido: " + cartaoStr);
			args.put("NUMERO_CARTAO_RECEBIDO", cartaoStr);
			query = "LogPedestrianAccessEntity.findByEquipamentSemDirectionAndComCartaoRecebido";

		} else if (matchedFacialId != null) {

			System.out.println(">>> Registra giro com FACIAL ID: " + matchedFacialId);
			args.put("NUMERO_CARTAO_RECEBIDO", matchedFacialId.toString());
			query = "LogPedestrianAccessEntity.findByEquipamentSemDirectionAndComCartaoRecebido";
			matchedFacialId = null;

		} else if (cartaoStr != null && cartaoStr.isEmpty()) {

			System.out.println(">>> Registra giro da LIBERAÇÃO MANUAL");
			inner.BilheteInner.Cartao = new StringBuilder();
			return null;

		} else if (cartaoStr != null && cartaoStr.replace("0", "").isEmpty()) {

			System.out.println(">>> Registra giro SEM CARTÃO (zerado)");
			query = "LogPedestrianAccessEntity.findByEquipamentSemDirectionAndSemCartaoRecebido";

		} else {

			System.out.println(">>> Registra giro da LIBERAÇÃO MANUAL (fallback)");
			inner.BilheteInner.Cartao = new StringBuilder();
			return null;
		}

		// ===== LIMPA CARTÃO =====
		inner.BilheteInner.Cartao = new StringBuilder();

		// ===== TRATAMENTO 3: QUERY =====
		if (query == null || query.isEmpty()) {
			System.out.println("ERRO: query não definida");
			return null;
		}

		return (LogPedestrianAccessEntity) HibernateAccessDataFacade
				.getUniqueResultWithParams(LogPedestrianAccessEntity.class, query, args);

	}

	private void decideLadoEntrada( String sentidoCamera) {

		// nova implementação (mínima)
		if (sentidoCamera != null && !sentidoCamera.trim().isEmpty()) {
			System.out.println("Sentido da camera recebido : " + sentidoCamera);

			String cameraUpper = sentidoCamera.toUpperCase();

			boolean cameraEntrada = cameraUpper.contains("ENTRADA");
			boolean cameraSaida = cameraUpper.contains("SAIDA") || cameraUpper.contains("SAÍDA");
			final Boolean doisSentidosLiberado = Utils.getPreferenceAsBoolean("doisSentidos");
			
			System.out.println("Convertido, entrada : " + cameraEntrada + ", camera saida : " + cameraSaida);

			// força ENTRADA
			if (cameraEntrada && !cameraSaida) {
				AcionaRele1();
			}

			// força SAÍDA
			if (cameraSaida && !cameraEntrada) {
				AcionaRele2();
			}
		}else {
			// força ENTRADA
				AcionaRele1();
		}

	}
	
	private void AcionaRele1() {
		EasyInner.AcionarRele1(inner.Numero);

		EasyInner.ManterRele1Acionado(inner.Numero);

		EasyInner.AcionarBipCurto(inner.Numero);

		Integer tempoAcionamentoRele = getConfigurationValueAsInteger(TEMPO_ACIONAMENTO_DO_RELE);
		if (tempoAcionamentoRele == null) {
			tempoAcionamentoRele = 3;
		}

		tempoAcionamentoRele *= 1000;

		Long inicio = System.currentTimeMillis();
		while ((System.currentTimeMillis() - inicio) < tempoAcionamentoRele) {
			Utils.sleep(1000);
		}

		EasyInner.DesabilitarRele1(inner.Numero);
	}
	
	private void AcionaRele2() {
		EasyInner.AcionarRele2(inner.Numero);

		EasyInner.ManterRele2Acionado(inner.Numero);

		EasyInner.AcionarBipCurto(inner.Numero);

		Integer tempoAcionamentoRele = getConfigurationValueAsInteger(TEMPO_ACIONAMENTO_DO_RELE);
		if (tempoAcionamentoRele == null) {
			tempoAcionamentoRele = 3;
		}

		tempoAcionamentoRele *= 1000;

		Long inicio = System.currentTimeMillis();
		while ((System.currentTimeMillis() - inicio) < tempoAcionamentoRele) {
			Utils.sleep(1000);
		}

		EasyInner.DesabilitarRele2(inner.Numero);
	}
	
	
	@Override
	public void processAccessRequest(Object obj) {
		try {
			final ProcessAccessRequestUseCase processAccessRequestUseCase = new ProcessAccessRequestUseCase();
			Object[] retorno = processAccessRequestUseCase.processAccessRequest((String) obj, "Inner Acesso " + inner.Numero, 
					inner.BilheteInner.Origem, location, getConfigurationValueAsBoolean(LOGICA_DE_CATRACA_COM_URNA), true, 
					getConfigurationValueAsBoolean(IGNORAR_REGRAS_DE_ACESSO));
			verificationResult = (VerificationResult) retorno[0];
			allowedUserName = (String) retorno[1];
			matchedAthleteAccess = (PedestrianAccessEntity) retorno[2];
		
		} catch (Exception e) {
			e.printStackTrace();
			verificationResult = VerificationResult.ERROR;
		}
	}
	
	@Override
	public void createDefaultConfiguration() {
		List<ConfigurationTO> geralConfigurations = new ArrayList<ConfigurationTO>();
		geralConfigurations.add(new ConfigurationTO(MODO_DE_TRABALHO, "Digitais no servidor_noServidor", FieldType.COMBOBOX, 
				"Digitais na catraca_naCatraca;Digitais no servidor_noServidor"));
		geralConfigurations.add(new ConfigurationTO(ENVIA_DIGITAIS_PARA_CATRACA, "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(SENTIDO_DA_CATRACA, "Horario_clockwise", FieldType.COMBOBOX, 
				"Horario_clockwise;AntiHorario_anticlockwise"));
		geralConfigurations.add(new ConfigurationTO(TEMPO_DE_LIBERADO, "7", FieldType.NUMERIC_LIST, "5;1;15"));
		geralConfigurations.add(new ConfigurationTO(BLOQUEAR_SAIDA, "true", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(HABILITAR_TECLADO, "true", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(ECOAR_ASTERISCOS, "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(NIVEL_RECONHECIMENTO, "6", FieldType.NUMERIC_LIST, "1;1;9"));
		geralConfigurations.add(new ConfigurationTO(TEMPO_TECLADO, "10", FieldType.NUMERIC_LIST, "5;1;20"));
		geralConfigurations.add(new ConfigurationTO(TEMPO_MUDANCA_ONLINE_OFFLINE, "10", FieldType.NUMERIC_LIST, "6;1;20"));
		geralConfigurations.add(new ConfigurationTO(TEMPO_DE_PING, "5", FieldType.NUMERIC_LIST, "2;1;10"));
		geralConfigurations.add(new ConfigurationTO(TEMPO_ESPERA_PARA_CONECTAR, "10", FieldType.NUMERIC_LIST, "5;1;20"));
		geralConfigurations.add(new ConfigurationTO(TIPO_LEITOR, "Proximidade Wiegand_3", FieldType.COMBOBOX,
				"Codigo de barras_0;Magnetico_1;Proximidade AbaTrack2_2;Proximidade Wiegand_3;Proximidade Wiegand FC_33;"
                        + "Proximidade Wiegand FC Sem Separador_6;Proximidade Smart Card_4;QRCode_7;", 240));
		geralConfigurations.add(new ConfigurationTO(QUANTIDADE_DIGITOS_CARTAO, "5", FieldType.NUMERIC_LIST, "4;1;16"));
		geralConfigurations.add(new ConfigurationTO(MODELO_BIOMETRICO, "true", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(TIPO_BIOMETRICO, "LFD_lfd", FieldType.COMBOBOX, "LFD_lfd;LC_lc"));
		geralConfigurations.add(new ConfigurationTO(DOIS_LEITORES, "true", FieldType.CHECKBOX, "(usa para catracas com urna)", true));
		geralConfigurations.add(new ConfigurationTO(LEITOR_1, "Somente entrada_1", FieldType.COMBOBOX,
				"Desativado_0;Somente entrada_1;Somente Saida_2"));
		geralConfigurations.add(new ConfigurationTO(LEITOR_2, "Somente Saida_2", FieldType.COMBOBOX,
				"Desativado_0;Somente entrada_1;Somente Saida_2"));
		geralConfigurations.add(new ConfigurationTO(IDENTIFICACAO_BIOMETRICA, "Sim_1", FieldType.COMBOBOX, "Sim_1;Nao_0"));
		geralConfigurations.add(new ConfigurationTO(VERIFICACAO_BIOMETRICA, "Nao_0", FieldType.COMBOBOX, "Sim_1;Nao_0"));
		geralConfigurations.add(new ConfigurationTO(PADRAO_DE_CARTAO, "Padrao livre_1", FieldType.COMBOBOX, "Padrao livre_1;Padrao TopData_0"));
		geralConfigurations.add(new ConfigurationTO(LOGICA_DE_CATRACA_COM_URNA, "true", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(USA_TORNIQUETE, "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(ACIONA_RELE_2, "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(TEMPO_ACIONAMENTO_DO_RELE, "3", FieldType.NUMERIC_LIST, "0;1;10"));
		geralConfigurations.add(new ConfigurationTO(COLETA_CARTOES_OFFLINE, "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(IGNORAR_REGRAS_DE_ACESSO, "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(ONLY_ENABLED_MODE, "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO(BOTAO_EXTERNO, "true", FieldType.CHECKBOX));
		
		
		String nomeEmpresa = "SmartPonto;Controle Acesso";
    	if (Main.loggedUser != null)
    		nomeEmpresa = Utils.formatAcademyName(Main.loggedUser.getName());
    	if (nomeEmpresa.length() > 16)
    		nomeEmpresa = nomeEmpresa.substring(0, 16).trim() + ";" + nomeEmpresa.substring(16, 32).trim();
    	
		List<ConfigurationTO> customConfigurations = new ArrayList<ConfigurationTO>();
    	customConfigurations.add(new ConfigurationTO(MENSAGEM_ONLINE, nomeEmpresa, FieldType.MESSAGE_LINES));

		configurationGroups = new ArrayList<ConfigurationGroupTO>();
		configurationGroups.add(new ConfigurationGroupTO("Geral", geralConfigurations));
		configurationGroups.add(new ConfigurationGroupTO("Personalizacao", customConfigurations));
	}

}
