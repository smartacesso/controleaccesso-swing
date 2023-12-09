package com.protreino.services.utils;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Component;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.MouseInfo;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.geom.RoundRectangle2D;
import java.awt.image.BufferedImage;
import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.math.BigInteger;
import java.net.ConnectException;
import java.net.HttpURLConnection;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.UnknownHostException;
import java.security.Key;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.text.Normalizer;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.TimerTask;

import javax.imageio.ImageIO;
import javax.persistence.Query;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.SwingWorker;
import javax.swing.UIManager;
import javax.swing.text.MaskFormatter;

import org.apache.commons.codec.binary.Base64;
import org.hibernate.Session;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.reflect.TypeToken;
import com.protreino.services.constants.Configurations;
import com.protreino.services.devices.Device;
import com.protreino.services.entity.AllowedTimeEntity;
import com.protreino.services.entity.ConfigurationEntity;
import com.protreino.services.entity.ConfigurationGroupEntity;
import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.entity.EmpresaEntity;
import com.protreino.services.entity.PedestreRegraEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.entity.PedestrianMessagesEntity;
import com.protreino.services.entity.PreferenceEntity;
import com.protreino.services.entity.UserEntity;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.enumeration.FieldType;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.enumeration.NotificationType;
import com.protreino.services.enumeration.OperationalSystem;
import com.protreino.services.enumeration.PreferenceGroup;
import com.protreino.services.main.Main;
import com.protreino.services.services.LuxandService;
import com.protreino.services.to.AttachedTO;
import com.protreino.services.to.PreferenceTO; 

import javazoom.jl.player.Player;

public class Utils {

	// As preferencias sao armazenadas no registro do Windows, na pasta
	// HKU\Software\JavaSoft\Prefs\myapplication\myclass\ (Pesquise por JavaSoft)
	// public static Preferences preferences =
	// Preferences.userNodeForPackage(Main.class.getClass());

	private static int dialogWidth = 370;
	private static int dialogHeight = 67;
	private static List<JDialog> notifications = new ArrayList<JDialog>();
	private static List<PreferenceTO> defaultPreferencesList;

	public static boolean isHikivisionConfigValid() {
		final String hikivisionServerRecognizerURL = getPreference("hikivisionServerRecognizerURL");

		return hikivisionServerRecognizerURL != null && !hikivisionServerRecognizerURL.isEmpty();
	}

	public static void sleep(long tempo) {
		try {
			Thread.sleep(tempo);
		} catch (InterruptedException e) {
		}
	}

	public static class SoundPlayer extends SwingWorker<Void, Void> {
		@Override
		public Void doInBackground() {
			InputStream inputStream = null;
			BufferedInputStream bufferedInputStream = null;
			try {
				inputStream = Main.class.getClass()
						.getResourceAsStream("/com/protreino/services/resources/sounds/new_notifications.mp3");
				bufferedInputStream = new BufferedInputStream(inputStream);
				Player player = new Player(bufferedInputStream);
				player.play();
			} catch (Exception e) {
				if (Main.desenvolvimento)
					e.printStackTrace();
			} finally {
				if (inputStream != null) {
					try {
						inputStream.close();
					} catch (IOException e) {
						// ignore
					}
				}
				if (bufferedInputStream != null) {
					try {
						bufferedInputStream.close();
					} catch (IOException e) {
						// ignore
					}
				}
			}
			return null;
		}
	}

	public static void setJavaLibraryPath() {
		String libpath = System.getProperty("java.library.path");
		System.out.println("java.library.path: " + libpath);
		if (!libpath.contains("System32")) {
			System.out.println("Incluindo caminho do System32");
			libpath = "C:\\Windows\\System32;" + libpath;
			System.setProperty("java.library.path", libpath);
			System.out.println("java.library.path: " + System.getProperty("java.library.path"));
		}
		if (!libpath.contains("SysWOW64")) {
			System.out.println("Incluindo caminho do SysWOW64");
			libpath = "C:\\Windows\\SysWOW64;" + libpath;
			System.setProperty("java.library.path", libpath);
			System.out.println("java.library.path: " + System.getProperty("java.library.path"));
		}
	}

	public static void loadDllTopData() {
		String libpath = System.getProperty("java.library.path");
		System.out.println("java.library.path: " + libpath);
		if (!libpath.contains("System32")) {
			System.out.println("Incluindo caminho do System32");
			String separador = "";
			if (libpath.length() > 0)
				separador = ";";
			libpath = libpath + separador + "C:/Windows/System32";
			System.setProperty("java.library.path", libpath);
			System.out.println("java.library.path: " + System.getProperty("java.library.path"));
		}
		System.out.println("Carregando dll...");
		try {
			System.load("C:/Windows/System32/EasyInner");
		} catch (Throwable e) {
			e.printStackTrace();
		}
		try {
			System.load("C:/Windows/System32/EasyInner.dll");
		} catch (Throwable e) {
			e.printStackTrace();
		}
	}

	/**
	 * Recupera uma preferencia. Caso a preferencia nao exista, entao retorna o
	 * valor padrao
	 * 
	 * @param key
	 * @return
	 */
	public static String getPreference(String key) {
		String value = null;
		try {
			HashMap<String, Object> args = new HashMap<String, Object>();
			args.put("CHAVE", key);
			PreferenceEntity preferenceEntity = (PreferenceEntity) HibernateUtil
					.getUniqueResultWithParams(PreferenceEntity.class, "PreferenceEntity.findByKey", args);
			if (preferenceEntity != null) {
				if (FieldType.IMAGE.equals(preferenceEntity.getFieldType())) {
					value = preferenceEntity.getImageValue() != null
							? Base64.encodeBase64String(preferenceEntity.getImageValue())
							: null;
				} else
					value = preferenceEntity.getValue();
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		// caso nao tenha preferencia definida, retorna valor padrao
		if (value == null) {
			value = getDefaultPreference(key);			
		}
		
		return value;
	}

	public static Integer getPreferenceAsInteger(String key) {
		try {
			String stringValue = getPreference(key);
			return Integer.valueOf(stringValue);
		} catch (Exception e) {
		}
		return 0;
	}

	public static Long getPreferenceAsLong(String key) {
		try {
			String stringValue = getPreference(key);
			return Long.valueOf(stringValue);
		} catch (Exception e) {
		}
		return 0l;
	}

	public static Boolean getPreferenceAsBoolean(String key) {
		try {
			String stringValue = getPreference(key);
			return Boolean.valueOf(stringValue);
		} catch (Exception e) {
		}
		return false;
	}

	/**
	 * Recupera uma preferencia. Caso a preferencia nao exista, entao retorna null
	 * 
	 * @param key
	 * @return
	 */
	public static String getPreferenceWithNull(String key) {
		String value = null;
		try {
			HashMap<String, Object> args = new HashMap<String, Object>();
			args.put("CHAVE", key);
			PreferenceEntity preferenceEntity = (PreferenceEntity) HibernateUtil
					.getUniqueResultWithParams(PreferenceEntity.class, "PreferenceEntity.findByKey", args);
			if (preferenceEntity != null) {
				if (FieldType.IMAGE.equals(preferenceEntity.getFieldType())) {
					value = preferenceEntity.getImageValue() != null
							? Base64.encodeBase64String(preferenceEntity.getImageValue())
							: null;
				} else
					value = preferenceEntity.getValue();
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return value;
	}

	/**
	 * Salva uma preferencia no sistema, nos registros do Windows
	 * 
	 * @param key
	 * @param value
	 */
	public static void setPreference(String key, String value) {
		try {
			HashMap<String, Object> args = new HashMap<String, Object>();
			args.put("CHAVE", key);
			PreferenceEntity preferenceEntity = (PreferenceEntity) HibernateUtil
					.getUniqueResultWithParams(PreferenceEntity.class, "PreferenceEntity.findByKey", args);
			boolean novo = preferenceEntity == null;
			if (novo) {
				for (PreferenceTO p : defaultPreferencesList) {
					if (p.getKey().equals(key)) {
						preferenceEntity = new PreferenceEntity(p);
						break;
					}
				}
				if (preferenceEntity == null)
					return;
			}
			if (FieldType.IMAGE.equals(preferenceEntity.getFieldType())) {
				if (value != null) {
					byte[] bytes = Base64.decodeBase64(value);
					preferenceEntity.setImageValue(bytes);

				} else
					preferenceEntity.setImageValue(null);
			} else
				preferenceEntity.setValue(value);

			if (novo)
				HibernateUtil.save(PreferenceEntity.class, preferenceEntity);
			else
				HibernateUtil.update(PreferenceEntity.class, preferenceEntity);

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public static void defineDefaultPreferences() {

		defaultPreferencesList = new ArrayList<PreferenceTO>();
		defaultPreferencesList.add(
				new PreferenceTO(PreferenceGroup.GENERAL, "blockSounds", "Bloquear sons", FieldType.CHECKBOX, "false"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "scrollSpeed", "Velocidade de rolagem",
				FieldType.TEXT, "5", true, 12));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "restrictAccess",
				"Limitar a quantidade de acessos por pedestre por dia", FieldType.CHECKBOX, "false"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "restrictAccessDays", "Limite de acessos",
				FieldType.NUMERIC_LIST, "1", "1;1;5"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "toleranceAccess",
				"TolerÃ¢ncia de entrada e saída (em minutos)", FieldType.NUMERIC_LIST, "0", "0;1;20"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "minTimeBetweenAccess",
				"Tempo mÃ­nimo entre entradas (em minutos)", FieldType.NUMERIC_LIST, "0", "0;1;20"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "timeAccessList",
				"Tempo de atualização da lista de acesso (em minutos)", FieldType.TEXT, "2", true, 10));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "timeUserAccessList",
				"Tempo de atualização das lista de usuários (em minutos)", FieldType.TEXT, "2", true, 10));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "printLog", "Imprimir log com o servidor",
				FieldType.CHECKBOX, "false"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "releaseAccessRequiresPassword",
				"Exigir senha de administrador para liberação de acesso", FieldType.CHECKBOX, "false"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "releaseAccessReason",
				"Motivos para liberação de acesso (separados por virgula)", FieldType.TEXT, "", false, 25));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "timeReconectDevices",
				"Tempo de aguardo para reconectar dispositivos (em segundos)", FieldType.TEXT, "5", true, 10));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "enableTCPServer",
				"Habilitar servidor TCP", FieldType.CHECKBOX, "false"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "tcpServerSocketPort",
				"Porta do servidor TCP", FieldType.TEXT, "2020", true, 10));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "enableBroadcastServer",
				"Habilitar servidor broadcast", FieldType.CHECKBOX, "false"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "broadcastServerSocketPort",
				"Porta do servidor de broadcast", FieldType.TEXT, "2019", true, 10));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "warningPaymentDueDate",
				"Dias para avisar sobre vencimento do pagamento", FieldType.TEXT, "0", true, 10));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "importExportDevices",
				"Importar/Exportar dispositivos do servidor", FieldType.CHECKBOX, "true"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "imageSizeRequestServer",
				"Tamanho das fotos recebidas do servidor (dimensão em px)", FieldType.TEXT, "48", true, 10));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "registerAccessWithoutConnectedDevices",
				"Registrar acesso mesmo que não haja dispositivos conectados", FieldType.CHECKBOX, "false"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "pedestrianAlwaysOpen",
				"Cadastro de pedestre/visitante em série", FieldType.CHECKBOX, "false"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "hourAutomaticRoutines",
				"Hora para execução das rotinas automáticas", FieldType.NUMERIC_LIST, "00", "0;1;23"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "enableCardAcessClear",
				"Habilita 'baixa' automática de cartões", FieldType.CHECKBOX, "false"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "enableDirectionClear",
				"Habilita reset de direções registradas pelos pedestres", FieldType.CHECKBOX, "false"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "enableCardReset",
				"Habilita reset status de cartão/comanda", FieldType.CHECKBOX, "false"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "enableOfflineCard",
				"Enviar cartões para Catraca Offline", FieldType.CHECKBOX, "false"));

		// TODO NOVAS PREFERENCIAS SAO INSERIDAS AQUI

		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageEnrollment",
				"Mensagem de cadastro de digital", FieldType.MESSAGE_LINES, "POSICIONE O DEDO;NO LEITOR"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageEnrollmentFinished",
				"Mensagem de cadastro de digital finalizado", FieldType.MESSAGE_LINES, "COLETA;FINALIZADA"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageEnrollmentCancelled",
				"Mensagem de cadastro de digital cancelado", FieldType.MESSAGE_LINES, "COLETA;CANCELADA"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageWait", "Mensagem de aguarde",
				FieldType.MESSAGE_LINES, "PROCURANDO...; "));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageAccessAuthorized",
				"Mensagem de acesso autorizado", FieldType.TEXT, "AUTORIZADO"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageAllowed",
				"Mensagem de acesso permitido", FieldType.TEXT, "GIRE A CATRACA"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageEntryAllowed",
				"Mensagem de entrada permitida", FieldType.TEXT, "ENTRE"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageExitAllowed",
				"Mensagem de saída permitida", FieldType.TEXT, "SAIA"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageNotAllowed",
				"Mensagem de acesso negado", FieldType.MESSAGE_LINES, "PEDESTRE;NAO PERMITIDO"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageNotFound",
				"Mensagem de pedestre não encontrado", FieldType.MESSAGE_LINES, "PEDESTRE;NAO ENCONTRADO"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageAllowedOnlyOnce",
				"Mensagem de pedestre que já acessou no dia", FieldType.MESSAGE_LINES, "PEDESTRE JA;REGISTRADO HOJE"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageNotAllowedToday",
				"Mensagem de pedestre fora do dia permitido", FieldType.MESSAGE_LINES, "PEDESTRE NAO;PERMITIDO HOJE"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageNotAllowedNow",
				"Mensagem de pedestre fora do horÃ¡rio permitido", FieldType.MESSAGE_LINES,
				"PEDESTRE NAO;PERMITIDO AGORA"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageError",
				"Mensagem de erro na verificação", FieldType.MESSAGE_LINES, "ERRO NA;VERIFICACAO"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageTolerancePeriod",
				"Mensagem de pedestre no perÃ­odo de tolerÃ¢ncia", FieldType.MESSAGE_LINES, "BEM-VINDO;PLANO VENCIDO"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageAllowedAthleteScreen",
				"(Tela do pedestre) Mensagem de acesso permitido", FieldType.TEXT, "Liberado!"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageNotAllowedAthleteScreen",
				"(Tela do pedestre) Mensagem de acesso negado", FieldType.TEXT,
				"Não permitido. Procure a secretaria."));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageNotAllowedFaceRequired",
				"(Tela do pedestre) Mensagem de cadastro de face obrigatório", FieldType.TEXT,
				"ObrigatÃ³rio cadastro facial"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageAllowedOnlyOnceAthleteScreen",
				"(Tela do pedestre) Mensagem pedestre que já acessou no dia", FieldType.TEXT,
				"Não permitido mais hoje."));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageNotAllowedTodayAthleteScreen",
				"(Tela do pedestre) Mensagem fora do dia permitido", FieldType.TEXT, "Fora do dia permitido."));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageNotAllowedNowAthleteScreen",
				"(Tela do pedestre) Mensagem fora do horÃ¡rio permitido", FieldType.TEXT,
				"Fora do horÃ¡rio permitido."));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageNotAllowedOrigem",
				"Mensagem para pedestre não permitido nesse equipamento", FieldType.TEXT,
				"Não permitido;no equipamento."));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageNotAllowedSensor",
				"Mensagem para pedestre que não depositou cartão na urna", FieldType.TEXT,
				"Deposite;o cartao na urna."));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageSMSAfterPassInDevice",
				"Mensagem SMS apÃ³s passagem na catraca", FieldType.TEXT, "Acabou de passar na catraca"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, "messageNotAllowedBox",
				"Mensagem de cartão não permitido na urna", FieldType.TEXT, "Não permitido;na urna."));

		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.ATHLETE_SCREEN, "athleteScreenBackgroundImage",
				"Imagem de fundo da tela do pedestre", FieldType.IMAGE, ""));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.ATHLETE_SCREEN, "athleteScreenFirstColor",
				"Cor primária da tela do pedestre", FieldType.COLOR_CHOOSER,
				(Main.firstColor.getRed() + ";" + Main.firstColor.getGreen() + ";" + Main.firstColor.getBlue())));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.ATHLETE_SCREEN, "athleteScreenSecondColor",
				"Cor secundária da tela do pedestre", FieldType.COLOR_CHOOSER,
				(Main.secondColor.getRed() + ";" + Main.secondColor.getGreen() + ";" + Main.secondColor.getBlue())));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.ATHLETE_SCREEN, "athleteScreenTimeout",
				"Tempo limite para apresentaÃ§Ã£o dos dados (segundos)", FieldType.NUMERIC_LIST, "5", "5;5;60"));

		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.FACE_RECOGNIZER, "samplesNumberForTraining",
				"Número de amostras para treinamento", FieldType.NUMERIC_LIST, "1", "1;1;10"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.FACE_RECOGNIZER,
				"intervalBetweenCapturesForTraining", "Intervalo entre capturas para treinamento (em ms)",
				FieldType.NUMERIC_LIST, "250", "100;50;500"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.FACE_RECOGNIZER,
				"intervalBetweenCapturesForRecognition", "Intervalo entre capturas para reconhecimento (em ms)",
				FieldType.NUMERIC_LIST, "50", "30;10;200"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.FACE_RECOGNIZER, "waitTimeAfterRecognizer",
				"Tempo de espera apÃ³s reconhecimento (em ms)", FieldType.NUMERIC_LIST, "4000", "1000;500;10000"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.FACE_RECOGNIZER, "maxTimeForFaceCapturing",
				"Tempo máximo para captura de faces (em seg)", FieldType.NUMERIC_LIST, "20", "1;1;40"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.FACE_RECOGNIZER, "serverRecognizerIP",
				"Ip do servidor de reconhecimento", FieldType.TEXT, "localhost:8080", false, 10));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, "cardMaster",
				"Definir número do cartão Master", FieldType.TEXT, "", true, 12));
		
		// Preferencias do nova Integração HIKIVISION
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.HIKIVISION_FACE_RECOGONIZER, "hikivisionServerRecognizerURL",
				"URL do servidor Device Gateway", FieldType.TEXT, "http://localhost:8082", false, 15));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.HIKIVISION_FACE_RECOGONIZER, "hikivisionUserServerConnection",
				"Usuário para conexão ao Servidor", FieldType.TEXT, "admin", false, 10));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.HIKIVISION_FACE_RECOGONIZER, "hikivisionPasswordServerConnection",
				"Senha para conexão ao Servidor", FieldType.TEXT, "123456", false, 10));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.HIKIVISION_FACE_RECOGONIZER, "tcpServerHikivisionSocketPort",
				"Porta do servidor TCP Hikivision para receber os eventos", FieldType.TEXT, "2025", true, 10));

		for (PreferenceTO preferenceTO : defaultPreferencesList) {
			if (getPreferenceWithNull(preferenceTO.getKey()) == null)
				setPreference(preferenceTO.getKey(), preferenceTO.getValue());
		}
	}

	public static String getDefaultPreference(String key) {
		for (PreferenceTO preferenceTO : defaultPreferencesList) {
			if (preferenceTO.getKey().equals(key))
				return preferenceTO.getValue();
		}
		return null;
	}

	public static void resetDefaultPreferences() {
		for (PreferenceTO preferenceTO : defaultPreferencesList) {
			setPreference(preferenceTO.getKey(), preferenceTO.getValue());
		}
	}

	public static void importPreferences() {
		String preferences = Main.loggedUser.getBackupPreferences();
		if (!isNullOrEmpty(preferences)) {
			String[] preferencias = preferences.split("\\$");
			if (preferencias != null && preferencias.length > 0) {
				for (String preferencia : preferencias) {
					String[] partes = preferencia.split("_");
					if (partes != null && partes.length > 0) {
						String key = partes[0];
						String value = partes.length > 1 ? partes[1] : getDefaultPreference(key);
						setPreference(key, value);
					}
				}
			}
		}
	}

	public static String exportPreferences() {
		StringBuilder stringBuilder = new StringBuilder();
		for (PreferenceTO preferenceTO : defaultPreferencesList) {
			if (FieldType.IMAGE.equals(preferenceTO.getFieldType()))
				continue;
			stringBuilder.append(preferenceTO.getKey() + "_" + getPreference(preferenceTO.getKey()));
			stringBuilder.append("$");
		}
		Main.loggedUser.setBackupPreferences(stringBuilder.toString());
		sendBackupToServer();
		return stringBuilder.toString();
	}

	@SuppressWarnings("unchecked")
	public static void importDevices() {
		if (getPreferenceAsBoolean("importExportDevices")) {
			String json = Main.loggedUser.getBackupDevices();
			if (isNullOrEmpty(json)) {
				return;
			}

			JsonParser parser = new JsonParser();
			JsonArray deviceArray = (JsonArray) parser.parse(json);
			for (JsonElement elementDevice : deviceArray) {
				JsonObject deviceObj = (JsonObject) elementDevice;
				DeviceEntity deviceEntity = new DeviceEntity();
				deviceEntity.setManufacturer(Manufacturer.valueFromImport(deviceObj.get("manufacturer").getAsString()));
				deviceEntity.setIdentifier(deviceObj.get("identifier").getAsString());
				deviceEntity.setName(deviceObj.get("name").isJsonNull() ? null : deviceObj.get("name").getAsString());
				deviceEntity
						.setLogin(deviceObj.get("login").isJsonNull() ? null : deviceObj.get("login").getAsString());
				deviceEntity.setPassword(
						deviceObj.get("password").isJsonNull() ? null : deviceObj.get("password").getAsString());
				deviceEntity.setLocation(
						deviceObj.get("location").isJsonNull() ? null : deviceObj.get("location").getAsString());
				deviceEntity
						.setDesiredStatus(DeviceStatus.valueFromImport(deviceObj.get("desiredStatus").getAsString()));
				deviceEntity.setDefaultDevice(deviceObj.get("defaultDevice").isJsonNull() ? false
						: deviceObj.get("defaultDevice").getAsBoolean());
				deviceEntity.setMirrorDevice(deviceObj.get("mirrorDevice").isJsonNull() ? false
						: deviceObj.get("mirrorDevice").getAsBoolean());
				deviceEntity.setAthleteScreenConfig(deviceObj.get("athleteScreenConfig").isJsonNull() ? null
						: deviceObj.get("athleteScreenConfig").getAsString());
				List<ConfigurationGroupEntity> configurationGroupList = new ArrayList<ConfigurationGroupEntity>();
				JsonArray configGroupArray = deviceObj.get("configurationGroups").getAsJsonArray();

				for (JsonElement elementConfigGroup : configGroupArray) {
					JsonObject configGroupObj = (JsonObject) elementConfigGroup;
					ConfigurationGroupEntity configGroupEntity = new ConfigurationGroupEntity();
					configGroupEntity.setDeviceEntity(deviceEntity);
					configGroupEntity.setName(configGroupObj.get("name").getAsString());
					List<ConfigurationEntity> configurationList = new ArrayList<ConfigurationEntity>();
					JsonArray configArray = configGroupObj.get("configurations") != null
							? configGroupObj.get("configurations").getAsJsonArray()
							: new JsonArray();

					for (JsonElement elementConfig : configArray) {
						JsonObject configObj = (JsonObject) elementConfig;
						ConfigurationEntity configEntity = new ConfigurationEntity();
						configEntity.setGroup(configGroupEntity);
						configEntity.setName(configObj.get("name").getAsString());
						configEntity.setValue(
								configObj.get("value").isJsonNull() ? null : configObj.get("value").getAsString());
						configEntity.setType(FieldType.valueOf(configObj.get("type").getAsString()));
						configEntity.setComboboxValues(configObj.get("comboboxValues").isJsonNull() ? null
								: configObj.get("comboboxValues").getAsString());
						configEntity.setMaxCharacteres(configObj.get("maxCharacteres").isJsonNull() ? null
								: configObj.get("maxCharacteres").getAsInt());
						configEntity.setMinCharacteres(configObj.get("minCharacteres").isJsonNull() ? null
								: configObj.get("minCharacteres").getAsInt());
						configEntity.setNumeric(
								configObj.get("numeric").isJsonNull() ? null : configObj.get("numeric").getAsBoolean());
						configEntity.setRequired(configObj.get("required").isJsonNull() ? null
								: configObj.get("required").getAsBoolean());
						configurationList.add(configEntity);
					}
					configGroupEntity.setConfigurations(configurationList);
					configurationGroupList.add(configGroupEntity);
				}

				deviceEntity.setConfigurationGroups(configurationGroupList);
				JsonArray attachedDevicesArray = deviceObj.get("attachedDevices").getAsJsonArray();

				if (attachedDevicesArray != null && attachedDevicesArray.size() > 0) {
					deviceEntity.setAttachedDevices(attachedDevicesArray.toString());
				}

				HibernateUtil.save(DeviceEntity.class, deviceEntity);
			}

			List<DeviceEntity> lista = (List<DeviceEntity>) HibernateUtil.getResultList(DeviceEntity.class,
					"DeviceEntity.findAll");
			if (lista != null && !lista.isEmpty()) {
				for (DeviceEntity deviceEntity : lista)
					Main.devicesList.add(deviceEntity.recoverDevice());
			}
			boolean haveDefaultDevice = false;
			for (Device device : Main.devicesList) {
				if (device.isDefaultDevice()) {
					haveDefaultDevice = true;
					break;
				}
			}
			if (!haveDefaultDevice && !Main.devicesList.isEmpty())
				Main.devicesList.get(0).setDefaultDevice(true);
		}
	}

	@SuppressWarnings("unchecked")
	public static String exportDevices() {
		if (getPreferenceAsBoolean("importExportDevices")) {
			JsonArray deviceArray = new JsonArray();
			List<DeviceEntity> lista = (List<DeviceEntity>) HibernateUtil.getResultList(DeviceEntity.class,
					"DeviceEntity.findAll");
			if (!isNullOrEmpty(lista)) {
				for (DeviceEntity deviceEntity : lista) {
					JsonObject deviceObj = new JsonObject();
					deviceObj.addProperty("manufacturer", deviceEntity.getManufacturer().toString());
					deviceObj.addProperty("identifier", deviceEntity.getIdentifier());
					deviceObj.addProperty("name", deviceEntity.getName());
					deviceObj.addProperty("login", deviceEntity.getLogin());
					deviceObj.addProperty("password", deviceEntity.getPassword());
					deviceObj.addProperty("location", deviceEntity.getLocation());
					deviceObj.addProperty("desiredStatus", deviceEntity.getDesiredStatus().toString());
					deviceObj.addProperty("defaultDevice", deviceEntity.getDefaultDevice());
					deviceObj.addProperty("mirrorDevice", deviceEntity.getMirrorDevice());
					deviceObj.addProperty("athleteScreenConfig", deviceEntity.getAthleteScreenConfig());

					JsonArray configGroupArray = new JsonArray();
					if (deviceEntity.getConfigurationGroups() != null) {
						for (ConfigurationGroupEntity configGroupEntity : deviceEntity.getConfigurationGroups()) {
							JsonObject configGroupObj = new JsonObject();
							configGroupObj.addProperty("name", configGroupEntity.getName());
							JsonArray configArray = new JsonArray();
							if (configGroupEntity != null) {
								for (ConfigurationEntity configEntity : configGroupEntity.getConfigurations()) {
									// Nao salva configuracoes do tipo imagem para reduzir tamanho dos dados
									if (!FieldType.IMAGE.equals(configEntity.getType())) {
										JsonObject configObj = new JsonObject();
										configObj.addProperty("name", configEntity.getName());
										configObj.addProperty("value", configEntity.getValue());
										configObj.addProperty("type", configEntity.getType().toString());
										configObj.addProperty("comboboxValues", configEntity.getComboboxValues());
										configObj.addProperty("maxCharacteres", configEntity.getMaxCharacteres());
										configObj.addProperty("minCharacteres", configEntity.getMinCharacteres());
										configObj.addProperty("numeric", configEntity.getNumeric());
										configObj.addProperty("required", configEntity.getRequired());
										configArray.add(configObj);
									}
								}
							}
							configGroupObj.add("configurations", configArray);
							configGroupArray.add(configGroupObj);
						}
					}
					deviceObj.add("configurationGroups", configGroupArray);

					JsonArray attachedDevicesArray = new JsonArray();
					if (deviceEntity.getAttachedDevices() != null && !deviceEntity.getAttachedDevices().isEmpty()) {
						String attachedDevices = deviceEntity.getAttachedDevices();
						Gson gson = new GsonBuilder().create();
						List<AttachedTO> list = gson.fromJson(attachedDevices, new TypeToken<List<AttachedTO>>() {
						}.getType());

						for (AttachedTO attachedTO : list) {
							JsonObject attachedDevicesObj = new JsonObject();
							attachedDevicesObj.addProperty("nomeDevice", attachedTO.getNomeDevice());
							attachedDevicesObj.addProperty("localDevice", attachedTO.getLocalDevice());
							attachedDevicesObj.addProperty("idDevice", attachedTO.getIdDevice());
							attachedDevicesArray.add(attachedDevicesObj);
						}
					}
					deviceObj.add("attachedDevices", attachedDevicesArray);
					deviceArray.add(deviceObj);
					
					JsonArray hikivisionAttachedCamerasArray = new JsonArray();
					if (deviceEntity.getAttachedHikivisionCameras() != null && !deviceEntity.getAttachedHikivisionCameras().isEmpty()) {
						String hikivisionAttachedCameras = deviceEntity.getAttachedHikivisionCameras();
						Gson gson = new GsonBuilder().create();
						List<AttachedTO> list = gson.fromJson(hikivisionAttachedCameras, new TypeToken<List<AttachedTO>>() {
						}.getType());

						for (AttachedTO attachedTO : list) {
							JsonObject attachedHikivisionCamerasObj = new JsonObject();
							attachedHikivisionCamerasObj.addProperty("nomeDevice", attachedTO.getNomeDevice());
							attachedHikivisionCamerasObj.addProperty("idDevice", attachedTO.getIdDevice());
							hikivisionAttachedCamerasArray.add(attachedHikivisionCamerasObj);
						}
					}
					deviceObj.add("hikivisionAttachedCameras", hikivisionAttachedCamerasArray);
					deviceArray.add(deviceObj);
				}
			}
			Main.loggedUser.setBackupDevices(deviceArray.toString());
			sendBackupToServer();
			return deviceArray.toString();
		}
		return "";
	}

	/**
	 * Tenta enviar o backup para o servidor. Caso falhe, entao seta um flag
	 * indicando que o backup precisa ser enviado. Com isso, haveria nova tentativa
	 * juntamente com o envio dos logs de acesso
	 */
	private static void sendBackupToServer() {
		SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
			@Override
			public Void doInBackground() {
				try {
					Main.loggedUser.setBackupChanged(false);
					System.out.println("\nENVIANDO BACKUP...");
					JsonObject responseObj = new JsonObject();
					responseObj.addProperty("backupPreferences", Main.loggedUser.getBackupPreferences());
					responseObj.addProperty("backupDevices", Main.loggedUser.getBackupDevices());
					HttpConnection con = new HttpConnection(Main.urlApplication
							+ "/restful-services/access/saveBackupByUser?idUser=" + Main.loggedUser.getId().toString());
					int responseCode = con.sendResponse(responseObj.toString());
					if (responseCode == 200) { // OK
						System.out.println("BACKUP ENVIADO!");

					} else {
						System.out.println("FALHA AO ENVIAR BACKUP. Response code: " + responseCode);
						Main.loggedUser.setBackupChanged(true);
					}
				} catch (ConnectException ce) {
					System.out.println("FALHA AO ENVIAR BACKUP: ConnectException: " + ce.getMessage());
					Main.loggedUser.setBackupChanged(true);
				} catch (Exception e) {
					e.printStackTrace();
					System.out.println("FALHA AO ENVIAR BACKUP: Exception: " + e.getMessage());
					Main.loggedUser.setBackupChanged(true);
				} finally {
					Main.loggedUser = (UserEntity) HibernateUtil.saveUser(UserEntity.class, Main.loggedUser)[0];
				}
				return null;
			}
		};
		worker.execute();
	}

	public static void createNotification(String message, NotificationType type) {
		createNotification(message, type, null, 5000);
	}

	public static void createNotification(String message, NotificationType type, byte[] photo) {
		createNotification(message, type, photo, 5000);
	}

	public static void createNotification(String message, NotificationType type, byte[] photo, int duration) {

		List<String> mensagens = new ArrayList<String>();
		String[] palavras = message.split(" ");
		String frase = new String();
		for (String palavra : palavras) {
			if ((frase.length() + palavra.length()) < 50) {
				frase = frase + palavra + " ";
			} else {
				mensagens.add(frase);
				frase = new String();
			}
		}
		mensagens.add(frase);

		final JDialog dialog = new JDialog();
		dialog.setLocationRelativeTo(null);
		dialog.setUndecorated(true);
		dialog.setSize(dialogWidth, dialogHeight);
		dialog.setOpacity(0.99f);
		dialog.setShape(new java.awt.geom.RoundRectangle2D.Double(0, 0, dialogWidth, dialogHeight, 5, 5));
		dialog.setAlwaysOnTop(true);
		dialog.setBackground(Main.firstColor);

		JLabel proTreinoLabel = new JLabel(Main.nomeAplicacao);
		proTreinoLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		proTreinoLabel.setForeground(Color.WHITE);
		Font customFont = new Font(proTreinoLabel.getFont().getFontName(), Font.BOLD,
				proTreinoLabel.getFont().getSize() + 1);
		proTreinoLabel.setFont(customFont);

		JPanel messageContainer = new JPanel();
		messageContainer.setLayout(new BoxLayout(messageContainer, BoxLayout.Y_AXIS));
		messageContainer.setPreferredSize(new Dimension(dialogWidth - 65, dialogHeight - 10));
		messageContainer.setOpaque(false);
		messageContainer.add(Box.createVerticalGlue());
		messageContainer.add(proTreinoLabel);
		messageContainer.add(Box.createRigidArea(new Dimension(0, 3)));

		if (NotificationType.BIRTHDAY.equals(type)) {
			JLabel infoMessage = new JLabel("ANIVERSARIANTE!");
			infoMessage.setAlignmentX(Component.LEFT_ALIGNMENT);
			infoMessage.setForeground(Color.WHITE);
			messageContainer.add(infoMessage);
			messageContainer.add(Box.createRigidArea(new Dimension(0, 1)));
		}

		for (String mensagem : mensagens) {
			JLabel infoMessage = new JLabel(mensagem);
			infoMessage.setAlignmentX(Component.LEFT_ALIGNMENT);
			infoMessage.setForeground(Color.WHITE);
			messageContainer.add(infoMessage);
			messageContainer.add(Box.createRigidArea(new Dimension(0, 2)));
		}

		messageContainer.add(Box.createVerticalGlue());

		ImageIcon logoImageIcon = new ImageIcon(Toolkit.getDefaultToolkit()
				.getImage(Main.class.getResource(Configurations.IMAGE_FOLDER + "comuns/" + type.getIconName())));
		JPanel notificationContainer = new JPanel(new FlowLayout(FlowLayout.LEFT));
		notificationContainer.setAlignmentY(Component.BOTTOM_ALIGNMENT);
		notificationContainer.setOpaque(false);
		if (photo != null)
			notificationContainer.add(new JLabel(new ImageIcon(createRoundImageWithIcon(photo, type))));
		else
			notificationContainer.add(new JLabel(logoImageIcon));
		notificationContainer.add(messageContainer);

		dialog.setContentPane(notificationContainer);

		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Rectangle winSize = GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();
		int taskBarSize = screenSize.height - winSize.height;

		dialog.addMouseListener(new MouseListener() {
			public void mouseReleased(MouseEvent e) {
			}

			public void mousePressed(MouseEvent e) {
			}

			public void mouseExited(MouseEvent e) {
			}

			public void mouseEntered(MouseEvent e) {
			}

			public void mouseClicked(MouseEvent e) {
				if (dialog.isVisible()) {
					dialog.setVisible(false);
					notifications.remove(dialog);
					updateNotificationsPosition();
				}
			}
		});

		updateNotificationsPosition();
		dialog.setLocation(screenSize.width - dialogWidth - 10,
				screenSize.height - taskBarSize - dialogHeight - 10 - notifications.size() * (dialogHeight + 10));
		notifications.add(dialog);
		dialog.setVisible(true);

		if (!Boolean.parseBoolean(getPreference("blockSounds"))) {
			new Utils.SoundPlayer().execute();
		}

		new java.util.Timer().schedule(new FaderOut(dialog), duration, 5);
	}

	private static void updateNotificationsPosition() {
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Rectangle winSize = GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();
		int taskBarSize = screenSize.height - winSize.height;
		for (int i = 0; i < notifications.size(); i++) {
			JDialog d = notifications.get(i);
			d.setLocation((int) d.getLocation().getX(),
					(screenSize.height - taskBarSize - dialogHeight - 10 - i * (dialogHeight + 10)));
		}
	}

	private static class FaderOut extends TimerTask {
		private JDialog jDialog;

		public FaderOut(JDialog jDialog) {
			this.jDialog = jDialog;
		}

		@Override
		public void run() {
			if (jDialog.isVisible()) {
				while (jDialog.getOpacity() > 0.01f) {
					jDialog.setOpacity(jDialog.getOpacity() - 0.01f);
					try {
						Thread.sleep(3);
					} catch (InterruptedException e) {
						// ignore
					}
				}
				jDialog.setVisible(false);
				notifications.remove(jDialog);
				for (JDialog dialog : notifications) {
					dialog.setLocation((int) dialog.getLocation().getX(),
							(int) dialog.getLocation().getY() + dialog.getHeight() + 10);
				}
				this.cancel();
			} else
				this.cancel();
		}
	}

	/**
	 * Returns a string representing the version number
	 */
	public static String checkVersion() {
		String version = "";
		try {
			String bufferedString;
			URL url = new URL(Configurations.URL_APPLICATION + "/restful-services/biometric/version");
			HttpURLConnection con = (HttpURLConnection) url.openConnection();
			if (con.getResponseCode() == 200) {
				BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(con.getInputStream()));
				StringBuilder stringBuilder = new StringBuilder();
				while (null != (bufferedString = bufferedReader.readLine()))
					stringBuilder.append(bufferedString);
				version = stringBuilder.toString();
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return version;
	}

	public static void openWebLink(String title, String link) {
		try {
			URL url = new URL(link);
			Desktop desktop = Desktop.isDesktopSupported() ? Desktop.getDesktop() : null;
			if (desktop != null && desktop.isSupported(Desktop.Action.BROWSE))
				desktop.browse(url.toURI());
			else
				JOptionPane.showConfirmDialog(null,
						"Não foi possível abrir a pÃ¡gina de download.\nTente abri-la manualmente atravÃ©s do link:\n"
								+ link,
						title, JOptionPane.OK_CANCEL_OPTION);
		} catch (Exception e) {
			e.printStackTrace();
			JOptionPane.showConfirmDialog(null,
					"Não foi possível abrir a pÃ¡gina de download.\nTente abri-la manualmente atravÃ©s do link:\n"
							+ link,
					title, JOptionPane.OK_CANCEL_OPTION);
		}
	}

	public static boolean isMouseWithinComponent(Component c) {
		Rectangle bounds = c.getBounds();
		bounds.setLocation(c.getLocationOnScreen());
		return bounds.contains(MouseInfo.getPointerInfo().getLocation());
	}

	public static String whatIsMyIp() {
		String ip = "";
		try {
			URL whatismyip = new URL(Configurations.WIP_SITE);
			BufferedReader in = new BufferedReader(new InputStreamReader(whatismyip.openStream()));
			ip = in.readLine();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return ip;
	}
	
	public static String getLocalIpAddress() {
		try {
			InetAddress IP = InetAddress.getLocalHost();
			return IP.getHostAddress();

		} catch (Exception e) {
			e.printStackTrace();
		}
		
		return "";
	}

	public static List<InetAddress> getAllLocalHostLANAddress() throws UnknownHostException {
		try {
			List<InetAddress> addressesList = new ArrayList<>();
			for (Enumeration<NetworkInterface> ifaces = NetworkInterface.getNetworkInterfaces(); ifaces
					.hasMoreElements();) {
				NetworkInterface iface = (NetworkInterface) ifaces.nextElement();
				for (Enumeration<InetAddress> inetAddrs = iface.getInetAddresses(); inetAddrs.hasMoreElements();) {
					InetAddress inetAddr = (InetAddress) inetAddrs.nextElement();
					if (!inetAddr.isLoopbackAddress()) {
						addressesList.add(inetAddr);
					}
				}
			}
			return addressesList;
		} catch (Exception e) {
			UnknownHostException unknownHostException = new UnknownHostException(
					"Failed to determine LAN address: " + e);
			unknownHostException.initCause(e);
			throw unknownHostException;
		}
	}

	public static String[] getAllLocalIps() {
		List<String> local = new ArrayList<String>();
		try {
			List<InetAddress> addressesList = Utils.getAllLocalHostLANAddress();
			if (addressesList != null && !addressesList.isEmpty()) {
				for (InetAddress inetAddress : addressesList) {
					if (inetAddress.isSiteLocalAddress())
						local.add(inetAddress.getHostAddress());
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		String[] localString = new String[local.size()];
		localString = local.toArray(localString);
		return localString;
	}

	public static String[] getAvailableCameras() {
		try {
			String[] cameras = LuxandService.getInstance().getCameraDescriptionList();
			return cameras;
		} catch (Throwable e) {
			e.printStackTrace();
			return new String[0];
		}
	}

	public static String getInstallationPath() {
		String path = "";
		try {
			path = Main.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath();
			path = path.substring(1, path.lastIndexOf("/") + 1);
		} catch (URISyntaxException e) {
			e.printStackTrace();
		}
		return path;
	}

	public static String getJvmArchitecture() {
		return System.getProperty("sun.arch.data.model");
	}

	public static String getCLRVersion() {
		String version = "20";
		String sysRoot = System.getenv("SystemRoot");
		if (sysRoot == null || sysRoot.equals("")) {
			sysRoot = "C:/Windows";
		}
		File d = new File(sysRoot, "Microsoft.NET/Framework/");
		final String[] vers = d.list(new FilenameFilter() {
			public boolean accept(File dir, String name) {
				return name.startsWith("v4.0.");
			}
		});
		if (vers != null && vers.length > 0) {
			version = "40";
		}
		return version;
	}

	public static void addLibraryPath(String pathToAdd) throws Exception {
		final Field usrPathsField = ClassLoader.class.getDeclaredField("usr_paths");
		usrPathsField.setAccessible(true);
		final String[] paths = (String[]) usrPathsField.get(null);
		for (String path : paths) {
			if (path.equals(pathToAdd)) {
				return;
			}
		}
		final String[] newPaths = Arrays.copyOf(paths, paths.length + 1);
		newPaths[newPaths.length - 1] = pathToAdd;
		usrPathsField.set(null, newPaths);
	}

	public static OperationalSystem getOperationalSystem() {
		String os = System.getProperty("os.name", "generic").toLowerCase(Locale.ENGLISH);
		if ((os.indexOf("mac") >= 0) || (os.indexOf("darwin") >= 0))
			return OperationalSystem.MAC_OS;
		else if (os.indexOf("nux") >= 0)
			return OperationalSystem.LINUX;
		else
			return OperationalSystem.WINDOWS;
	}

	public static boolean isWindows64bits() {
		boolean is64bits = false;
		if (System.getProperty("os.name").contains("Windows")) {
			is64bits = (System.getenv("ProgramFiles(x86)") != null);
		} else {
			is64bits = (System.getProperty("os.arch").indexOf("64") != -1);
		}
		return is64bits;
	}

	public static String getAppDataFolder() {
		return Utils.getOperationalSystem().equals(OperationalSystem.LINUX)
				? System.getProperty("user.home") + "/.appData/Local/SmartAcesso"
				: System.getProperty("user.home") + "/AppData/Local/SmartAcesso";
//		return Utils.getOperationalSystem().equals(OperationalSystem.LINUX)
//				? System.getProperty("user.home") + "/.appData/Local/SmartAcesso"
//				: "c:/SmartAcesso";
	}

	public static String removerAcentos(String str) {
		return Normalizer.normalize(str, Normalizer.Form.NFD).replaceAll("[^\\p{ASCII}]", "");
	}

	public static String formatString16(String string) {
		String newString = "";
		if (string.length() >= 16) {
			newString = string.substring(0, 16);
		} else {
			int diferenca = 16 - string.length();
			for (int i = 0; i < diferenca / 2; i++)
				newString = newString + " ";
			newString = newString + string;
			while (newString.length() < 16)
				newString = newString + " ";
		}
		return newString;
	}

	public static String formatAcademyName(String name) {
		String newString = "";
		String part1 = "";
		String part2 = "";
		if (name.length() > 16) {
			int cutIndex = 0;
			int index = -1;
			do {
				index = name.indexOf(" ", index + 1);
				if (index < 16 && index != -1)
					cutIndex = index;
			} while (index < 16 && index != -1);
			if (cutIndex == 0) {
				part1 = name.substring(0, 15);
				part2 = name.substring(16, name.length() > 32 ? 32 : name.length());
				newString = part1 + formatString16(part2);
			} else {
				part1 = name.substring(0, cutIndex);
				part2 = name.substring(cutIndex + 1, name.length() > cutIndex + 16 ? cutIndex + 16 : name.length());
				newString = formatString16(part1) + formatString16(part2);
			}
			return newString;
		}
		return formatString16(name);
	}

	public static String formatMessage16(String message) {
		String[] partes = message.split(";");
		String parte1 = partes.length > 0 ? partes[0] : "";
		String parte2 = partes.length > 1 ? partes[1] : "";
		return formatString16(parte1) + ";" + formatString16(parte2);
	}

	public static void correctIcons() {
		try {
			String[][] icons = { { "OptionPane.informationIcon", "65587" }, { "OptionPane.warningIcon", "65581" },
					{ "OptionPane.errorIcon", "65585" }, { "OptionPane.questionIcon", "65583" } };
			Method getIconBits = Class.forName("sun.awt.shell.Win32ShellFolder2").getDeclaredMethod("getIconBits",
					new Class[] { long.class, int.class });
			getIconBits.setAccessible(true);
			double dpiScalingFactor = Toolkit.getDefaultToolkit().getScreenResolution() / 96.0;
			int icon32Size = (dpiScalingFactor == 1) ? (32)
					: ((dpiScalingFactor == 1.25) ? (40)
							: ((dpiScalingFactor == 1.5) ? (45) : ((int) (32 * dpiScalingFactor))));
			Object[] arguments = { null, icon32Size };
			for (String[] s : icons) {
				if (UIManager.get(s[0]) instanceof ImageIcon) {
					arguments[0] = Long.valueOf(s[1]);
					int[] iconBits = (int[]) getIconBits.invoke(null, arguments);
					if (iconBits != null) {
						BufferedImage img = new BufferedImage(icon32Size, icon32Size, BufferedImage.TYPE_INT_ARGB);
						img.setRGB(0, 0, icon32Size, icon32Size, iconBits, 0, icon32Size);
						ImageIcon newIcon = new ImageIcon(img);
						UIManager.put(s[0], newIcon);
					}
				}
			}

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public static Date getZeroHora(Calendar calendar) {
		calendar.set(Calendar.MILLISECOND, 0);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MINUTE, 0);
		calendar.set(Calendar.HOUR_OF_DAY, 0);
		return calendar.getTime();
	}

	public static Calendar getZeroHora(Date date) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);
		calendar.set(Calendar.MILLISECOND, 0);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MINUTE, 0);
		calendar.set(Calendar.HOUR_OF_DAY, 0);
		return calendar;
	}

	public static Date zerarHora(Date date) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);
		calendar.set(Calendar.MILLISECOND, 0);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MINUTE, 0);
		calendar.set(Calendar.HOUR_OF_DAY, 0);
		return calendar.getTime();
	}

	/**
	 * Identifica se o dia de hoje se encontra entre os dias livres para acesso.
	 * 
	 * @param acesso
	 * @return
	 */
	public static boolean isDiaLivre(String diasLivres) {
		if (diasLivres != null && !diasLivres.isEmpty()) {
			Integer hoje = Calendar.getInstance().get(Calendar.DAY_OF_WEEK);
			hoje--; // ajusta a numeracao dos dias para coincidir com a numeracao do pro-treino
			if (hoje == 0)
				hoje = 7;
			if (diasLivres.contains(hoje.toString()))
				return true;
		}
		return false;
	}

	/**
	 * Identifica se o dia de hoje se encontra entre os dias permitidos para acesso.
	 * 
	 * @param acesso
	 * @return
	 */
	public static boolean isDiaPermitido(PedestrianAccessEntity acesso, Date data) {
		if (acesso.getHorariosPermitidos() == null || acesso.getHorariosPermitidos().isEmpty())
			return true;
		Calendar cHoje = Calendar.getInstance();
		if (data != null)
			cHoje.setTime(data);
		Integer hoje = cHoje.get(Calendar.DAY_OF_WEEK);
		hoje--; // ajusta a numeracao dos dias para coincidir com a numeracao do pro-treino
		if (hoje == 0)
			hoje = 7;
		for (AllowedTimeEntity horario : acesso.getHorariosPermitidos()) {
			if (horario.getDiasPermitidos() != null && horario.getDiasPermitidos().contains(hoje.toString()))
				return true;
		}
		return false;
	}

	/**
	 * Identifica se a hora atual se encontra dentro dos limites de horario
	 * permitidos pelo acesso no dia de hoje
	 * 
	 * @param acesso
	 * @return
	 */
	public static boolean isDentroDoHorario(PedestrianAccessEntity acesso, Date data) {
		if (acesso.getHorariosPermitidos() == null || acesso.getHorariosPermitidos().isEmpty())
			return true;
		Calendar cHoje = Calendar.getInstance();
		if (data != null)
			cHoje.setTime(data);
		Integer hoje = cHoje.get(Calendar.DAY_OF_WEEK);
		hoje--; // ajusta a numeracao dos dias para coincidir com a numeracao do pro-treino
		if (hoje == 0)
			hoje = 7;
		for (AllowedTimeEntity horario : acesso.getHorariosPermitidos()) {
			if (horario.getDiasPermitidos() != null && horario.getDiasPermitidos().contains(hoje.toString())
					&& isDentroDoHorario(horario.getInicio(), horario.getFim(), data))
				return true;
		}
		return false;
	}

	/**
	 * Identifica se a hora atual se encontra dentro dos limites de horario
	 * fornecidos. Parametros nulos serao desconsiderados na comparacao
	 * 
	 * @param inicio - String no formato HH:mm
	 * @param fim    - String no formato HH:mm
	 * @return True se a hora atual esta dentro dos limites enviados
	 */
	public static boolean isDentroDoHorario(String inicio, String fim, Date data) {

		Calendar agora = Calendar.getInstance();
		if (data != null)
			agora.setTime(data);
		agora.set(Calendar.SECOND, 0);

		if (inicio != null) {
			String[] parts = inicio.split(":");
			Calendar horaInicio = Calendar.getInstance();
			horaInicio.set(Calendar.HOUR_OF_DAY, Integer.parseInt(parts[0]));
			horaInicio.set(Calendar.MINUTE, Integer.parseInt(parts[1]));
			horaInicio.set(Calendar.SECOND, 0);

			// verifica se esta dentro da tolerancia
			Long tolerancia = getPreferenceAsLong("toleranceAccess");
			if (tolerancia != null && tolerancia != 0)
				horaInicio.add(Calendar.MINUTE, tolerancia.intValue() * -1);

			if (agora.before(horaInicio))
				return false;
		}

		if (fim != null) {
			String[] parts = fim.split(":");
			Calendar horaFim = Calendar.getInstance();
			horaFim.set(Calendar.HOUR_OF_DAY, Integer.parseInt(parts[0]));
			horaFim.set(Calendar.MINUTE, Integer.parseInt(parts[1]));
			horaFim.set(Calendar.SECOND, 0);

			Long tolerancia = getPreferenceAsLong("toleranceAccess");
			if (tolerancia != null && tolerancia != 0)
				horaFim.add(Calendar.MINUTE, tolerancia.intValue());

			if (agora.after(horaFim))
				return false;
		}
		return true;
	}

	public static boolean isPodeEntrarNovamente(Date ultimoAcesso) {
		boolean allowed = true;

		LocalDateTime lastAccess = new java.sql.Timestamp(ultimoAcesso.getTime()).toLocalDateTime();
		LocalDateTime now = LocalDateTime.now();

		Long interval = getPreferenceAsLong("minTimeBetweenAccess");

		if (interval > 0 && now.isBefore(lastAccess.plusMinutes(interval))) {
			allowed = false;
		}

		return allowed;
	}

	/**
	 * CODIGO PARA LISTAR TODAS AS CONSTRAINTS DO BANCO DE DADOS
	 */
	@SuppressWarnings("unchecked")
	public static void listarConstraints() {
		Session session = HibernateUtil.getSessionFactory().getCurrentSession();
		session.beginTransaction();
		Query nativeQuery = session.createNativeQuery(" SELECT "
				+ "  'ALTER TABLE '||S.SCHEMANAME||'.'||T.TABLENAME||' DROP CONSTRAINT '||C.CONSTRAINTNAME||';'"
				+ " FROM " + "  SYS.SYSCONSTRAINTS C, " + "  SYS.SYSSCHEMAS S," + "  SYS.SYSTABLES T" + " WHERE"
				+ "  C.SCHEMAID = S.SCHEMAID" + " AND" + "  C.TABLEID = T.TABLEID" + " AND"
				+ "  S.SCHEMANAME = 'SMARTACESSO'" + " UNION"
				+ " SELECT 'DROP TABLE ' || schemaname ||'.' || tablename || ';'" + " FROM SYS.SYSTABLES"
				+ " INNER JOIN SYS.SYSSCHEMAS ON SYS.SYSTABLES.SCHEMAID = SYS.SYSSCHEMAS.SCHEMAID"
				+ " where schemaname='SMARTACESSO'");
		List<String> queries = nativeQuery.getResultList();
		for (String query : queries)
			System.out.println(query);
		session.getTransaction().commit();
		session.close();
	}

	public static String getPublicKey() {
		String publicKey = null;
		try {
			KeyPairGenerator kpg = KeyPairGenerator.getInstance("RSA");
			kpg.initialize(1024);
			KeyPair kp = kpg.generateKeyPair();
			Key pub = kp.getPublic();
			java.util.Base64.Encoder encoder = java.util.Base64.getEncoder();
			publicKey = encoder.encodeToString(pub.getEncoded());
		} catch (NoSuchAlgorithmException e1) {
			e1.printStackTrace();
		}
		return publicKey;
	}

	public static boolean isNullOrEmpty(List<?> list) {
		if (list == null || list.isEmpty())
			return true;
		return false;
	}

	public static boolean isNullOrEmpty(String string) {
		if (string == null || string.trim().isEmpty()) {
			return true;
		}
		return false;
	}

	public static boolean isNullOrZero(Number number) {
		if (number == null || number.doubleValue() == 0d)
			return true;
		return false;
	}

	public static List<PreferenceTO> getDefaultPreferencesList() {
		return defaultPreferencesList;
	}

	public static boolean isBirthday(PedestrianAccessEntity athleteAccess, Date data) {
		if (athleteAccess.getDataNascimento() != null) {
			SimpleDateFormat sdf = new SimpleDateFormat("dd/MM");
			if (sdf.format(data != null ? data : new Date()).equals(sdf.format(athleteAccess.getDataNascimento())))
				return true;
		}
		return false;
	}

	private static byte[] createRoundImageWithIcon(byte[] original, NotificationType type) {
		try {
			BufferedImage originalImage = ImageIO.read(new ByteArrayInputStream(original));
			int sizeImage = 48; // em px
			BufferedImage clipedImage = new BufferedImage(sizeImage, sizeImage, BufferedImage.TYPE_INT_ARGB);
			Graphics2D g1 = clipedImage.createGraphics();
			g1.setClip(new RoundRectangle2D.Double(0, 0, sizeImage, sizeImage, 5, 5));
			g1.drawImage(originalImage, 0, 0, sizeImage, sizeImage, null);
			g1.dispose();
			BufferedImage icon = ImageIO
					.read(Main.class.getResource(Configurations.IMAGE_FOLDER + "comuns/" + type.getSmallIconName()));
			int sizeIcon = icon.getWidth();
			Graphics2D g2 = clipedImage.createGraphics();
			g2.drawImage(clipedImage, 0, 0, sizeImage, sizeImage, null);
			if (NotificationType.BIRTHDAY.equals(type))
				g2.drawImage(icon, 0, 0, sizeIcon, sizeIcon, null);
			else
				g2.drawImage(icon, 0, (sizeImage - sizeIcon), sizeIcon, sizeIcon, null);
			g2.dispose();
			ByteArrayOutputStream bos = new ByteArrayOutputStream();
			ImageIO.write(clipedImage, "png", bos);
			return bos.toByteArray();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return original;
	}

	public static Image paintImage(BufferedImage buffMask, Color cor, int largura, int altura) {
		int color = getIntFromColor(cor);

		// cria um retangulo transparente
		BufferedImage imgColorida = new BufferedImage(largura, altura, BufferedImage.TYPE_INT_ARGB);
		Graphics2D graphics2 = imgColorida.createGraphics();
		graphics2.setComposite(AlphaComposite.Clear);
		graphics2.fillRect(0, 0, largura, altura);
		graphics2.dispose();

		int[] maskData = new int[largura];
		int[] imgData = new int[largura];

		// percorre a mascara. onde nao for transparente, entao colore com a cor
		for (int y = 0; y < altura; y++) {
			buffMask.getRGB(0, y, largura, 1, maskData, 0, 1);
			imgColorida.getRGB(0, y, largura, 1, imgData, 0, 1);
			for (int x = 0; x < largura; x++) {
				Color maskColor = new Color(maskData[x], true);
				if (maskColor.getAlpha() != 0)
					imgData[x] = color;
			}
			// replace the data
			imgColorida.setRGB(0, y, largura, 1, imgData, 0, 1);
		}
		return imgColorida;
	}

	public static JFormattedTextField getNewJFormattedTextField(int columns) {
		JFormattedTextField formattedTextField = new JFormattedTextField();
		formattedTextField.setColumns(columns);
		formattedTextField.setAlignmentX(Component.CENTER_ALIGNMENT);

		return formattedTextField;
	}

	public static String maxlength(String str, int lenght) {
		String valor = "";
		if (str.length() > lenght) {
			valor = str.substring(0, lenght);
			str = valor;
		}
		return str;
	}

	public static MaskFormatter getNewMaskFormatter(String mask) {
		MaskFormatter mascara = null;
		try {
			mascara = new MaskFormatter(mask);
		} catch (ParseException e) {
			e.printStackTrace();
		}

		return mascara;
	}

	public static int getIntFromColor(Color cor) {
		int Red = (cor.getRed() << 16) & 0x00FF0000; // Shift red 16-bits and mask out other stuff
		int Green = (cor.getGreen() << 8) & 0x0000FF00; // Shift Green 8-bits and mask out other stuff
		int Blue = cor.getGreen() & 0x000000FF; // Mask out anything not blue.
		int Alpha = (cor.getAlpha() << 24) & 0xFF000000;
		return Alpha | Red | Green | Blue;
	}

	public static String getRandomName() {
		int i = (int) (Math.random() * 10000000);

		return String.valueOf(i);
	}

	public static Long getRandomNumber() {
		Long idTemp = new Date().getTime();

		idTemp = (long) (idTemp * (Math.random() * 10000));

		return idTemp;
	}

	public static void escondeColunaFromTable(JTable table, int columnNumero) {
		table.getColumnModel().getColumn(columnNumero).setMinWidth(0);
		table.getColumnModel().getColumn(columnNumero).setMaxWidth(0);
		table.getColumnModel().getColumn(columnNumero).setWidth(0);
	}

	public static void decrementaCreditos(PedestrianAccessEntity pedestre) {
		if (pedestreTemRegraDeAcessoPorPeriodoValido(pedestre)) {
			return;
		}

		if ("VISITANTE".equals(pedestre.getTipo())) {
			if (pedestre.getQuantidadeCreditos() != null && pedestre.getQuantidadeCreditos() > 0) {
				pedestre.setQuantidadeCreditos(pedestre.getQuantidadeCreditos() - 1);
				if (pedestre.getQuantidadeCreditos() <= 0)
					pedestre.setCardNumber(null);
			} else {
				decrementaCreditosPedestreRegra(pedestre);
				pedestre.setQuantidadeCreditos(null);
				pedestre.setCardNumber(null);
			}

		} else {
			if (pedestre.getQuantidadeCreditos() != null && pedestre.getQuantidadeCreditos() > 0)
				pedestre.setQuantidadeCreditos(pedestre.getQuantidadeCreditos() - 1);

		}
	}

	private static void decrementaCreditosPedestreRegra(PedestrianAccessEntity pedestre) {
		if (pedestre.getPedestreRegra() == null) {
			return;
		}
		for (PedestreRegraEntity pedestreRegra : pedestre.getPedestreRegra()) {
			if (pedestreRegra.getQtdeDeCreditos() != null) {
				pedestreRegra.setQtdeDeCreditos(pedestreRegra.getQtdeDeCreditos() - 1);
			}
		}

	}

	public static boolean pedestreTemRegraDeAcessoPorPeriodoValido(PedestrianAccessEntity pedestre) {
		if (pedestre.getPedestreRegra() == null || pedestre.getPedestreRegra().isEmpty()) {
			return false;
		}

		for (PedestreRegraEntity pedestreRegra : pedestre.getPedestreRegra()) {
			if (pedestreRegra.getRemovidoNoDesktop()) {
				continue;
			}

			if (isRegraDeAcessoValida(pedestreRegra.getDataInicioPeriodo(), pedestreRegra.getDataFimPeriodo(),
					pedestreRegra.getValidade())) {
				return true;
			}
		}

		return false;
	}

	private static boolean isRegraDeAcessoValida(Date dataInicioPeriodo, Date dataFimPeriodo, Date validade) {
		if (dataInicioPeriodo == null || dataFimPeriodo == null) {
			return false;
		}

		Date dataAtual = new Date();
		return dataInicioPeriodo.compareTo(dataAtual) >= 0 && dataFimPeriodo.compareTo(dataAtual) <= 0
				&& validade != null ? validade.compareTo(dataAtual) >= 0 : true;
	}

	public static void decrementaMensagens(List<PedestrianMessagesEntity> messages) {
		for (PedestrianMessagesEntity message : messages) {
			if (message.getQuantidade() > 0) {
				message.setQuantidade(message.getQuantidade() - 1);
			}
		}
	}

	public static void decrementaQRCodeUso(PedestrianAccessEntity pedestre) {

		if (pedestre.getQrCodeParaAcesso() != null && pedestre.getQrCodeParaAcesso().startsWith("U_")) {
			String[] parts = pedestre.getQrCodeParaAcesso().split("_");
			Long usos = Long.valueOf(parts[parts.length - 1]);
			usos++;

			pedestre.setQrCodeParaAcesso("U_" + EncryptionUtils.getRandomString(4) + "_" + usos);
			pedestre.setEditadoNoDesktop(true);
			pedestre.setDataAlteracao(new Date());

		}

	}

	public static void enviaSmsDeRegistro(PedestrianAccessEntity pedestre) {

		if (Main.loggedUser.getChaveIntegracaoComtele() == null
				|| Main.loggedUser.getChaveIntegracaoComtele().isEmpty())
			return;

		if (!pedestre.getEnviaSmsAoPassarNaCatraca() || pedestre.getIdEmpresa() == null)
			return;

		EmpresaEntity empresa = buscaEmpresaDoPedestre(pedestre.getIdEmpresa());

		if (empresa == null || empresa.getTelefone() == null || empresa.getTelefone().isEmpty())
			return;

		try {
			SMSUtils sms = new SMSUtils(Main.loggedUser.getChaveIntegracaoComtele());
			String message = pedestre.getName() + " " + getPreference("messageSMSAfterPassInDevice");

			String numeroTelefone = empresa.getCelular().replace(" ", "").replace("-", "").replace("(", "").replace(")",
					"");

			sms.enviaSMS(numeroTelefone, message);

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private static EmpresaEntity buscaEmpresaDoPedestre(Long idEmpresa) {
		EmpresaEntity empresa = null;
		empresa = (EmpresaEntity) HibernateUtil.getSingleResultById(EmpresaEntity.class, idEmpresa);
		return empresa;
	}

	public static String verificaMensagemAvisoVencimento(PedestrianAccessEntity athleteAccess) {
		try {

			// TODO : verifica mensagens para mostrar para o pedestre

//			Integer diasAviso = getPreferenceAsInteger("warningPaymentDueDate");
//			if (diasAviso > 0) {
//				Date hoje = Utils.zerarHora(new Date());
//				Date dataVencimento = Utils.zerarHora(athleteAccess.getDataPermitido());
//			    Long diff = TimeUnit.DAYS.convert(dataVencimento.getTime() - hoje.getTime(), TimeUnit.MILLISECONDS);
//			    if (diff.intValue() <= diasAviso) {
//			    	if (diff.intValue() == 1)
//			    		return "VENC EM 1 DIA";
//			    	return "VENC EM " + diff + " DIAS";
//			    }
//			}
		} catch (Exception e) {
		}
		return null;
	}

	public static Date convertDataJson(JsonElement element) throws ParseException {
		try {
			return new Date(element.getAsLong());
		} catch (Exception e) {
			try {
				SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z['z']'");
				return sdf.parse(element.getAsString());
			} catch (Exception ex) {

				try {
					SimpleDateFormat sdf2 = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z['z']'");
					return sdf2.parse(element.getAsString());

				} catch (Exception exe) {
					String data = null;
					SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
					if(element.getAsString().contains(".000+0000")) {
						data = element.getAsString().replace(".000+0000", "");
					}
					return sdf.parse(data);
				}
			}
		}
	}

	public static PedestrianAccessEntity buscaPedestrePorIdOuIdTemp(Long idPedestre) {

		PedestrianAccessEntity pedestre = (PedestrianAccessEntity) HibernateUtil
				.getSingleResultById(PedestrianAccessEntity.class, idPedestre);

		if (pedestre != null && pedestre.getId() != null)
			return pedestre;

		pedestre = (PedestrianAccessEntity) HibernateUtil.getSingleResultByIdTemp(PedestrianAccessEntity.class,
				idPedestre);

		if (pedestre != null && pedestre.getId() != null)
			return pedestre;

		return null;
	}

	// colocar conversor de ABATRACK para WIGAN aqui como estÃ¡tico

	public static String toHEX(String Cartao) {

		try {

			long longAbatrack = Long.parseLong(Cartao);
			long fclong = Long.parseLong(Cartao);
			String hexAbatrack = Long.toHexString(longAbatrack);
			String hexWigan = hexAbatrack.substring(hexAbatrack.length() - 4);
//			olhar a posiÃ§Ã£o pegar exatamente o mesmo if
			String fcWiegand = "";
			String temp = "";
			fcWiegand = hexAbatrack.substring(hexAbatrack.length() - 6);
			temp += fcWiegand.charAt(0);
			temp += fcWiegand.charAt(1);
			longAbatrack = new BigInteger(hexWigan, 16).longValue();
			fclong = new BigInteger(temp, 16).longValue();
			String wiegand = String.valueOf(longAbatrack);
			if (wiegand.length() < 4) {
				wiegand = "00" + wiegand;
			}
			if (wiegand.length() < 5) {
				wiegand = "0" + wiegand;
			}

			String fcwiegand = String.valueOf(fclong);
			String fcwiegandtg = fcwiegand + wiegand;
			return fcwiegandtg;

		} catch (Exception e) {
			System.out.println("Não foi possível converter cartão " + Cartao + "\n motivo " + e.getMessage());
			return Cartao;
		}

	}
	
	public static String getFirstJsonFromString(String input) {
        List<Character> stack = new ArrayList<Character>();

        String temp = "";
        
        for(char eachChar: input.toCharArray()) {
            if(stack.isEmpty() && eachChar == '{') {
                stack.add(eachChar);
                temp += eachChar;
            } else if(!stack.isEmpty()) {
                temp += eachChar;
                if(stack.get(stack.size()-1).equals('{') && eachChar == '}') {
                    stack.remove(stack.size()-1);
                    if(stack.isEmpty()) {
                    	return temp;
                    }
                } else if(eachChar == '{' || eachChar == '}') {
                	stack.add(eachChar);
                }
            } else if(temp.length() > 0 && stack.isEmpty()) {
            	return temp;
            }
        }
        
        return "";
    }

	public static void main(String[] args) {
		final String te = "POST /event/notification HTTP/1.1\r\n"
				+ "Host: 192.168.15.4:8090\r\n"
				+ "Accept: */*\r\n"
				+ "Content-Type: multipart/form-data; boundary=MIME_boundary\r\n"
				+ "Connection: keep-alive\r\n"
				+ "Content-Length: 64308\r\n"
				+ "\r\n"
				+ "--MIME_boundary\r\n"
				+ "Content-Disposition: form-data; name=\"event_log\"\r\n"
				+ "Content-Type: application/json\r\n"
				+ "Content-Length: 956\r\n"
				+ "\r\n"
				+ "{\r\n"
				+ "	\"ipAddress\":	\"192.168.15.133\",\r\n"
				+ "	\"ipv6Address\":	\"2804:1b2:1001:3e3:be5e:33ff:fe5c:3adb\",\r\n"
				+ "	\"portNo\":	8090,\r\n"
				+ "	\"protocol\":	\"HTTP\",\r\n"
				+ "	\"macAddress\":	\"bc:5e:33:5c:3a:db\",\r\n"
				+ "	\"channelID\":	1,\r\n"
				+ "	\"dateTime\":	\"2023-12-01T02:48:43+08:00\",\r\n"
				+ "	\"activePostCount\":	1,\r\n"
				+ "	\"eventType\":	\"AccessControllerEvent\",\r\n"
				+ "	\"eventState\":	\"active\",\r\n"
				+ "	\"eventDescription\":	\"Access Controller Event\",\r\n"
				+ "	\"AccessControllerEvent\":	{\r\n"
				+ "		\"deviceName\":	\"cam 3\",\r\n"
				+ "		\"majorEventType\":	5,\r\n"
				+ "		\"subEventType\":	75,\r\n"
				+ "		\"cardNo\":	\"1768\",\r\n"
				+ "		\"cardType\":	1,\r\n"
				+ "		\"name\":	\"Boss\",\r\n"
				+ "		\"cardReaderKind\":	1,\r\n"
				+ "		\"cardReaderNo\":	1,\r\n"
				+ "		\"verifyNo\":	140,\r\n"
				+ "		\"employeeNoString\":	\"3\",\r\n"
				+ "		\"serialNo\":	42,\r\n"
				+ "		\"userType\":	\"normal\",\r\n"
				+ "		\"currentVerifyMode\":	\"cardOrFace\",\r\n"
				+ "		\"frontSerialNo\":	0,\r\n"
				+ "		\"attendanceStatus\":	\"undefined\",\r\n"
				+ "		\"label\":	\"\",\r\n"
				+ "		\"statusValue\":	0,\r\n"
				+ "		\"mask\":	\"no\",\r\n"
				+ "		\"helmet\":	\"unknown\",\r\n"
				+ "		\"picturesNumber\":	1,\r\n"
				+ "		\"purePwdVerifyEnable\":	true,\r\n"
				+ "		\"FaceRect\":	{\r\n"
				+ "			\"height\":	0.304,\r\n"
				+ "			\"width\":	0.496,\r\n"
				+ "			\"x\":	0.5,\r\n"
				+ "			\"y\":	0.487\r\n"
				+ "		}\r\n"
				+ "	}\r\n"
				+ "}\r\n"
				+ "--MIME_boundary\r\n"
				+ "Content-Disposition: form-data; name=\"Picture\"; filename=\"Picture.jpg\"\r\n"
				+ "Content-Type: image/jpeg\r\n"
				+ "Content-Length: 63041\r\n"
				+ "Content-ID: pictureImage\r\n"
				+ "\r\n"
				+ "ÿØÿà>JPEGæ S    INFô                                                                                                                        2023-12-1 2:48:43                  F1x                   `              *\r\n"
				+ "                           -   -   Z                  áõ áõ                                                                   œNü\bTç,\r\n"
				+ "öFèÐ !2@±`x§r¼M‡}Ä {ÊHþx5–ú0ŽQ                                            è                   pµ       \r\n"
				+ "           P'ò 0Q                                                                                      .                                                              ­\r\n"
				+ "                                                         FR700006       ê(¼\r\n"
				+ "ÑÏãåVy@8#Þ†,¼Æ›mîV4Þ÷Lÿ sm2ÿ ×IÃÌ(¨¤–Ýÿ ½:Õ$/ÜiÞx=*\r\n"
				+ "wW,“RÔ£Ê=jºO(| wæ•¥˜¯2 Aš5ØN%…\r\n"
				+ "yùÓv¯Rß@g›oË'žcPO);tÀÁüi0FÒø¨Ä„ò´ÖÃçüŠdò’<Gï\r\n"
				+ "ŽHUó‘íG)äožär2tÐŽk8ÊaEWžÉ\r\n"
				+ "qW’U²S¯ñU\\‰@È“DŠ6wPIs–÷5ÏjÞ’\"Ý>ñ®ÌÄ­ÿ -9ëŠ­=›Is“EÙŒ©itŽOê†T„sƒœóÔ?!Y÷zM›î¹\f$_˜°+¿–ÜcîtíÚ³ït,¤éßëÚ©´ÕÇõ(¬Œ²Jã¾}k˜Ö'†-^F”ÆöÎ+©Û¶Bcã9åxïÿ êýk”ñLhº‹;Žá—åëÁçÛµy´ÝÙèI~ïB\r\n"
				+ "‰Li•—¶îÞ¿Ê¢2`g°õíQ<ï)&SÆþžÜõé¾wúB¨ëÆ:žýþ¿¥vÇ¡Èì·/A8C¹›vz\f}+cÂ2@÷\r\n"
				+ "ûÎ£8ÇN+Ÿ™ö‘žO×îûVÏ„â\"ï(ç,9÷ÿ <þu2B	Ö$Þl¢5L\fuÝO»t‘\"I÷zŸóôýiÖšmÌ™§*FáŽŸçÓÞ´&ÒWåRÛy®{¨³Ÿ–¥­b‹,ŒÙ\r\n"
				+ "»üôþu\f±ÝHO•o–9°- Pª£§9©¸:{\r\n"
				+ "ÑP£'-L¸t	î~Ñòäç§J¿aá­:#µþb*ý¬7>~>µ*E\fs|ÏùSöºQ ž£`µ³…ó\f#9«ð+¹ÀK\r\n"
				+ "…É«++ç*(rêU1ì¤6•V2yj™¹4Õ'vsK­Ê5Ô„+ÏZ –~p }j6—\r\n"
				+ "œÔx‘Îìþµ>õµ6åHšI•Nwþ b¢y7Ê§ñ4»BœäzúR° Îiî_.š –E<w¥>d€ï4ãÇ­ec’O<ÐØÔ;ˆêPr>œÓYFsÅJ!‘º®iæÊác'g7ê_!Q•¾öÓJ!cÔ{Õ¥ðšD‰÷äŠ».åR„¶HïÒ»†ú­¸Òµ?\fjq†Žê2Ñ’{íÚGò?…skmó\fœZÞøs ßkÞ0³Ñ4ø·Ïrì‘F?ˆí'¡ªR¶ÄÎ‰Ê½™Š_,ŽPã>´ôŒç%k¤ñß„5O	x²ïÃÚµ“Á,lkŒeXdËò¬t¶ÙÉZ­z—K]J/Ý»¾\\Wä·Ù˜ð9?wô¨léå(ÉÕâ¡#ò«­y\"š¶Ù;€ªLÂPl¬–¥†Hêh{bË´øÕ¸ápÙSDð0â´R9¥O±›4Œ‚Ó$³ùw¬˜õUóÜK/ãLxÔS­UÌœ\r\n"
				+ "ÒLáˆÇó¤TA&ÚºcÊôï“QÍn¹ÝŠW)@®QçæL§5:EÇÍÓÞ”GÇJ\r\n"
				+ "å¹WËão#šd&ñüªe\\¶w  VÀ´\"Ä*\r\n"
				+ "ŒÒ2äg?Z˜ bšêÝ •W%Ä…R6šFÝ÷A©\fCvi’)ƒÞÈkAÎ7nãÔšxÆ‘†\bïEÈwê1œ	ÉP¹;ÔŽ •úS\b%xâ®æv¹ ƒUåbÄ\r\n"
				+ "¼çš³¼íÜã·zŽêH¢]îq“MkêsñÁq,r äuü?ÏZæüwnlî#Ï;ðsøŒJìYƒíê~÷§zÉñî›öø­Ù˜+\f.py¯•oßYž\"tô80í,‡ÉCÈóÅ]Óô=[S›e­£·¡Øqÿ Ö­Ãºd;KBÄðà€\r\n"
				+ "wo•gòUFWTã¨—Jîž%CTŽ9SîsWÂ»¹\r\n"
				+ "É«]¤KÔ¤|°ö®»IðÎ‰¤mû%¢î–a’i·­¥²šéGãP'Ší·•¶‡w©Ïé\\’¯Vª ”o±ºctc™$HÄo<ôëYM­4èÑ9#pãüÿ ž´A¨Ÿ$©lsP¥>åû6Ú4‘~`3I÷ñÇj«óËâ:tç­<¬¥·g5pææÐÙÁ[¹bK… cÓ4”aÕ¨†=¹ÀÁïïHmNÏ½ÍZ•Íy4¹vÕÇFjÄyfÊzÔ60ÈÄ§Š¼‘dóV5®Ä[\br\r\n"
				+ "# Þ&¬˜sÎi$‡q«EÜîA\"2¶ô©»Y×\r\n"
				+ "VV3Œ0§\r\n"
				+ "aØÔîõfŠEHƒ+#¿5fxÅ‚;Ô«h|Ã¯Zr‰$€‚Ÿ=M9-¹Ø0 ¡<ŒÕ«KQåò*Å´M$k¸}jÌvjƒh©rî8Âú•>Ì£…õ©D“«ÙOJ–;vÛÒ³rêl¡©ö<±j6µ).Ö_z×†Ó2`¯zY´Ï2Mõ¢•µfÙ‘ö}ÌJþ5«à¿j ñv™ãm:\r\n"
				+ "óiWÑÝ\"aË'ü	r?–](ò£¨ã³ í\r\n"
				+ "Þ‹÷)SÓSéø(—ÃòÏÂ?´G‚)4}zÅm¥ž7/nIÇ÷“ê˜¯˜§²?tšûoöLÐö©ýˆ|aû<M2Ï¬xbG“Hþú#–¸´aì&Y£ú2ŒWÇrY0’æØÅ,nÑÈ’\fÝI\f¤z‚\b­[ÚFtbãx>†3éÍéM{·¥n½¢•ÜEA5žFTVNNçj§îÜÆk ‹’”‘Ø÷µþÉ…ÆÊdV£y=*ÓfU!±•ö&È¤›Opü­k½ iíš|–jã V‰œÓ¦a=ƒœTM`Ywb·ÚÇŒÕw±gãm;™¸˜mfP}Þj6µ,>ílKe†À¨ZÛ‚\bæ™†_Ár˜ÊUúÕy¡(x‹péSqòY“ü)\r\n"
				+ "aöU–ˆnÎ{Ôr¡VÜE¹½ÊòFPð:Ò2Ž­S\\!ëQ¾YwbªîäÊ$mÏQøÔnªÜ©Jg¶‘U}51’\"uÏ\r\n"
				+ "I´\r\n"
				+ "šqBAõÌú\r\n"
				+ "w3å¹$òÜÿ Zg åOÖ¤`«ü<Ð2¿xU\\‡£\r\n"
				+ "|»Vº‚Gjê7\fÕ¦ðOÒ¢ —Ç¥R–¤¸„aeóIÀôª~(ÑZòËÊœ“œb¯hÚŠÞY¬ÞnáŽOjuÎ¹¥ù-L®åxE<×ÍÝÃTvÆmÓÔã'°’Ð4S?ûKŽ3ùÿ žjªkW-@Óc?Ý=kcY²’æÌ˜Ãwÿ ž•ÎÃE’íü8çŸ­wÂÓ…Ìog©*N’>%ÝçÏùâ¯Å$m!Ž==GåYVŸ$›CëŽÞ•©§@òI¿7qëèÆ¶”S*èhiöòHã?/ãý*Ì‰wá²8Ûž•jÆÂI ß\r\n"
				+ "t?×¥kZi\bÜ>c×Þ°ç‚º.éôËgTØWŽ¼Õô¶Á\fÂŸ·“1CÜÕÉ1ÜsPäú!)ib½´²Ui^2óSÚ•C´RÈ¬š¨ÊãÝjOb˜Œ|¼÷«Ü8¨ôä,y«Š\b} uëMèÊŽ±+í\r\n"
				+ "Å\r\n"
				+ "Ûš°öùn”¦”üµqz#e\r\n"
				+ "½JÂ0@#š”Û°ÇbÚÉä\"ÖŒ:9˜(“Š¦ìtF:hfÛDå\r\n"
				+ "§Eoò+[_Ø†Ý°óß4°éA²qMrÞ:Úm³ã§N*êZeúU«8	Hutç\r\n"
				+ "Âÿ õê^@Œ{™bÀƒµ46L[Ík¦œ›0Wš–\r\n"
				+ "?k}ÊÉ³¢0ÙñÙ2HIZœÙîÇJ×}1÷/P;Ñýšãæ\r\n"
				+ "N.å¨Jû«`­Á<Qý|ªsë[I¦6Ýá9ïš·”Ó`l?•[}M;Äï?aŒ«û<þÓÞñ]õéƒFÖ'7ˆOamp@Iýs˜Bÿ @Õ»ÿ #ø§|(ý¤5\r\n"
				+ "oÃ–k“âäþÕµ…3¶œâê?ûùócý¾‚¼¦ÿ CimdŽÚ?ŸaòÏ÷[  óþUõ‡í¿ ÞøßöeðÅ¹d3Æëg(¸+Ô\\Û'_øõ«Œÿ vÌ%I,LmÕ[î>&ky_ÌSf°$goÒº¯ì¶wcääb«6»ª¶ik¹ÕM_C›]<‘œt¨&±e—!k¨::ª©ÝiŒ¯´­8n*‘÷nÎ}ìÛ!”bžÖ¿.\b­y´Ö	µµ\r\n"
				+ "áÒµ9e,Ú’Ù+PKnGU­¶¶ÏµEu§ 9îsò3\r\n"
				+ "klüÕ]íÈlâ·ZËrggN*ŒöedÀOÂ›¹š‡VaÝ[ílT\r\n"
				+ "mákjïN%òS{R§a\\TßCNIO\r\n"
				+ "ÍEq-–5§-¢«g^âÝWæÛÞ‚%\r\n"
				+ "2Ä ÃÇáP˜)À­·ÝHªÂ&ÁUÑ”âQpÇ¯¥G&2 5nX™‚{ÔBÙéUs	D®ìIäTXù±ùÕ‡ˆ‘øÔ)ÎqLÉÄŽPÙã­7%ˆ'\r\n"
				+ "8:“Œ÷£Íˆüúý½m¥¶øÍÀh[>øoþ¿ë^\r\n"
				+ "2í$Èzq_JÁDôàŸ!º1ýü®~¹¯šu c!ç­~³;ài3êð½(²•üÇ:ÌžfI9=jKéÈ}êœ²N+õŠ:Â'ÒÓØÌàòzÒµÀÛ’jåWÔe˜É°šÙ4iÍÕ“í3¾1šÖÓ<?s>ÝÜ/Ri4M.7äk©µH Œ*žqMÛq¦ú·‰tOìèÄðköúe„†“ b»_5ƒñÎ+Íõ\bn’f;#Ò•ôÔ9í_Fñ&§r.aÕ¤‰‡Ý\r\n"
				+ "Ä*ì¼-­ê©`¶:¤¾d‘ü¢SüuJÊaä39çµIl$™ƒÆzÎ‰6ãd‰ÚW¹ÑG,|îÇ­W½¾,Û¥sÍg=ôñFK½bkRKª¡\\ínœÖ)­ÙÒ4‰(ÊŸþ½6d`Ó5Éx.ëS²ºk[ÙÙ—' öæºÉŸrníŽkU'Íddù·ÂFRA=êë˜cT¬Ê±,9çò«ÛÄRSè\r\n"
				+ " á:´|j9%!3Q+!\r\n"
				+ "Ó4LãsT¾\"î42Ï =*IãnóQG°ÊF)Òa=½i«\\—a,¬êõmTs#b¾Ûý›ü&žð”;— Ê#üûWÊŸ¼,Þ)ñÕ´1ÆÛÜŠû‡Â:lzF…\r\n"
				+ "´qí;lõÎ+ç8Ž¿.Q]O+Sì#àŸ‹,üFÕ5ÍÀ¤×c#ºŽ}‡ûþÑíñ Âgá·‰î³«hñ)¶‘ºÜÁÓ<õ õ¯…$a+ûõÉúÖÿ Ã¯ë¿<_aãÜ”¹±œ:®x‘sÊb?Æ½S–ve8sÆÇèOÄoÙ[á¿õ‹Ÿ‰Z'ƒ|J<Ooñ?ƒ5{‹[©ðmÈgíÆ¾Pño‹|]à5¯ˆt­SOÖ\"”¼ñë\r\n"
				+ "éq #oÁ'Ô÷¯´~|j³ø‰á]7â/†.6Hèh·sŸÄ‡ñ®?öàøM'Ä½>Óã†—¥ý«SÑgI/­ÔŸô›pÙ`G>ßyøì§ûÄôíÛÎæ5jÎPŒCÍÿ fÚ\r\n"
				+ "[ñ§õ/‡^Šk›KëO:óI’y$…H]§`,B€3ß8ç\"¾‚ø¯èþ»¹ørºë:#–µ²¸\\IhIÿ TOFpxàŠø\r\n"
				+ "ö‹øÕ®§ÆKüñæ‹¨K¦yW'K¹1>p|§ÛÄ‹•ÎÖöâ»OÙ‹ãÞ¥qâè$×5G[›‰U.D­.lžF~ê’OÔWF4Ô_Þr¿{¡ú ­Þ›Üã§5çòÛy×M\"\fl×M«êï£Û³ÉI<`æ&È#k!P(ÁZøþ(¯Æ”V»œZØ¤Ð´oÃSâK‚*GsäR*Û‰¯Èñ“¶—*8¥bQ€qOBA$ð* wn§‡-Þ¸½ç¹nË\"t:‘%Ürj”ŽUx,ñÒ±³Ü‹u/ÆêNþ˜$wñòŠ¨²„æ«As!lJÖ-Í´»LåKVýæk2ÝšNwt«ð1TäÔòõ´-y™Ç<lvn&©FçŒš™&9Á¢ÝÉérØç9ïS$¤Ró‘RÅ&îsSËÐ”‹jØZ’'#½UW9æžq¥Ë}‚Å¥`ÀÓ¼Ò ÊZ¬ã–ïNWÊäÊ¨–‹JËÜóRy¤ŒU5õÍIäaš‹iäÓÑ½MV´õ‘»ñG-÷>Rÿ ‚‹ømÏØu•‹+æ[8a_kxBÀu¯Ðÿ Û{Ãëÿ 	..Ñ3,\b]žà?­~wø‚—\fZL÷¯Ò¸^¯´À¥Øú®W¥©Îß8.[_r“»šžïjä“š¬¹`[¥~»†“tbü­§Ñ‰,€ž´ûT¶Z«Hÿ >*kyü³šéó+›±µg¨´\r\n"
				+ "ÜUûm]Cå›õ®j[†eÊš’Ù¥uûõ§™Ÿ9Ó\\^‹… žÕÉêšD‘]—-òk^ÊÚöVÃ±û/äÿ H ÑhËprm€¶A	ÖfV,ÇÒ·µ%¤«Ëýë(æ•­¹/mJÚ½ØH‰?Ê¹é¯¯bº%T#åãšè5{a$8\"°¤ŒG&òÙ'Þ·„\"ö%É§fX´Ô\bL18ÿ >õ¯g©=Èò€ù{µa…1Ë¹¢'žßçÞµô¹¢Ù±@Ër*Ô-¨½£h×Ó×Õ¹IàzÕk\\'\r\n"
				+ "Ø©$˜8#5›ZÜÑ=Kˆ£m¬{Õ[û–’l«qEÜŠ\r\n"
				+ "®îÎØ=i­%¨ù•®Y†ì‚{TÏ8b6ÕHcpqŠÝðw‡®<E®ÛiÆI–e¨­=Õ˜Ýõ>ƒýŽ<*Zÿ m]ÀA›æät¿ZúAÔ¢pk˜ø=áH¼3á˜ H¶þì*ý ýzé¥$63Í~q›âþ³‹”¯¦ßqóõçÏ7#ó=	ÞŽOÔ°?ïrÆ³,5MF:Öí%VcpqùUÛf$\fÿ }uNç­d{‡ì}û@]|!ñ¸Ò5{¶þÄÕG:çå†BFÙ>}÷¡ë–—¶Æ)\r\n"
				+ "Mku=Õá_”–ûYpkìØ“ö„>%Òá‡Š¯‡Û¬”>Yæš.›}Êð?ztg§+9ñßÄy7ÆOÙòÇ@øå¯_Xi>]•Ó	­pÃä$e°;rkÆïõKï‡ž02LCJÒ„•Xó×ƒíƒ_x~ÑÞ\b7Úcx³K¶/q\r\n"
				+ "aÔ6 	æ¾øåookª~?1n$}²ÀßÂ¯ÿ ^”SçåOc•NÛŸ ?²7Æk?‰Þ\r\n"
				+ "M*öàý¿NŒ# <ºc\r\n"
				+ "/å^»,LW>•ù£û3üu¿ð/Šm5”Ê›Fzƒþ²#Á‡øz×è¿ƒ~!øcÇµÖô­VûLAÕòsßòO–Ô¨ž\"—mÌæÅS•ù‘y²É¦oçæ¢Iã?ÅÚ¡3g¿zü‚½Ò¨á#Í’¶¬»Ö…vSÖ¢óNqš<ÅS“õ5ÏÊÒ'Ôœ¶îZûÔC2£yc«ãøÕÿ \r\n"
				+ "ø'Æ>2ŸÈð¿†/¯ˆëäÛ1ŸJöÙKÅþ/ð—î| âí\"âÒ2Ø6—pàcÓ ¨ëù×¯•äµ³IÛá{\r\n"
				+ "2›²<6;˜ÊnÞ>¹¦ÅÓz}\b5÷­ïìûðî÷WoøkÂÚU¾¡(ÌÞe¨1ÉžOËÑO¾+'\\øð‡ÇRÇ^±´¼_õwºT\"	ý´Œ\fþ5ïÃÃÌâµ:m>Æ¿S­kŸYJûÈn*ìA”çu}©aû#|\r\n"
				+ "ÐìÇÙüá—½žIK~mŠƒRý“~xŠÐý›ÀkkpÞµÔ'‹“cô©ÿ ˆsÄJ7åýà\r\n"
				+ "ê8ž¨øÑ'}ÛEL“zWÕw³ìóáû1¥ø‡Á÷(íò™fÕe,O´ŠËŸÊ¼ãÇÿ ±·Ž¢Õ…çÂ[$Õ4©†ä:Œi$~Ùlnæâøˆ°°öŽ7”ufRÁ×Š»GÇ)'­H’a°­ÞºMwöøíá¤/ª|)ÕYTüÍ`èä\"Çô®NiM¸6š¤ZMžb¼…¢qÿ p\r\n"
				+ "|þ+/ÆàÝ±¥æ¬sû6·.	›8ÍH“Ù5§à?‡Þ'ø©7Âöªÿ ßžLùiõ W[©þÉŸ4ÉÊ'…lîâê.-µh‚þ\"BŒ?*TòüehsS¦ÚòM‹‘½lpi.ãiVS“Ç½t÷~*Ø³éúNñÖ5ñ aø4‹UGÁÿ ŒC	ÿ \r\n"
				+ "ÏT}ßuí£YÐÿ Àãf_Ö­å˜èüT¥÷18Ê×hÇI29=©Ë ãŸÖºë?ÙËãâyø¦z}¦þ\bÏäÍš–OÙ«ãŒ4þ³¿ñ<¶þ­N9^=«ª2û˜½”ÚØãüÏ—ïsOY·k©oÙ÷ãJŒùƒ³[ê–®?I*	þ\r\n"
				+ "|_µm²|=½b{G,.ñ×5/-Æ­éKÿ cå’ÜóŒ|zÇÃNÊTàÛ±>ÜWåÏm_LÖ.¬	ÃC3¡Ï±¯×ï|ø·wáÛÈdøi¬mkv ÁÏOöI5ù7ûChZ‡‡¾&ëzF¥§ÜZÌ—¬Z\r\n"
				+ "˜Ln™äeO5õü+Ôœá8Û®Ö=l©?yw~v\fûæ ¹\b_J.YÂífïP‰ãUá«ö,¾\\ØTþGÖÐ»†£XØ ó¡¥U9-PM)ÝòöªwsËoÃzú×d{3F×CrÉÑï:U·¼²µ\f+‰“ÅRDÅ>bztÿ >õRëÄ7Ó”mÍoI»Ø—£Ôî¦ñ`÷jÿ Nj\r\n"
				+ "ÞÍò¤?\\W¹NZû¨û¼sWlõËK¸°·çî(tä‚/]7:«ÉnvÞ32“ÎEO}¨é3ÇöÈÜ_jåÒc&C8ÇlŽ¿ãL“;ð>‡Š^›š½w7“P²½ÿ GWÇõRm%78ÃdûÖOï¢c,giíZznªÍ¶	Xî8ÏzÒ7‰„’z4UÔU-î„äwïRXªºïLŒtmÒ	›3ž{¯shð1`ÜOJÑI=\fìã©bÆà«s/^•¡Úí¬X¤Œ¬pzçðÍ[†c°“(aëš*Fî÷$ÑvV×sqP…%²§­@e2‚OÍªÝ”\fÒmŠQìÙRe«(ZC–ëÜe¯†ÒkÐÖn\"+ÎØÏo¯é^càï\bÞø“X·Òìã$ÈàvöŸÁ‡Öž\r\n"
				+ "nÍ÷ZjzdÆ9ýG8=ÇCÜ•î\r\n"
				+ "?nýKN½‡Áÿ µ6„¶¥ñ>5ÑíO“!ÁÁ¹·^ÐÉ9*y<HÛ²z»s\r\n"
				+ "¦¡oö;Ût–'+\"ƒ]x|eJNÛ£:¸HT[b]Aa«èpêú]åž£¦ßEæZ^ÙÌ³Aq\f®¼0ï^7ñGá²X¬º¾•BN\r\n"
				+ "\f‘Óô¯ð‹¾.~ÏÒêut¸Ò§É©xCTÌ¶W'”^±¹Ðƒë»¥{×Âÿ ¿\fÿ hÛÒ4¨äÒ<D‹ºëÂú«1=Ì\r\n"
				+ "ÿ -ÓÜ\r\n"
				+ "Ã¨¯j–\"–‡‹_\r\n"
				+ ":[7©é°Ü7ïFÙ ?61P/ŠµíMþÀ¼‘¦±´P¹ÊÄ}Wû¿…u¼;.ƒ®K–î€öuÅrwR$€«(#ßµlr¦ö(ÞAo|Ÿi³˜Œ‘Yóì‘ÞtÏnj[„–ÊO6ÎO—ºƒÒ]A¨¯mâ©=l+\\Á¹·¸Ñn\r\n"
				+ "í¬‡fîÇùÕ¹<Cý­oåÌáO¾‹Ì\r\n"
				+ "Žµ‡5´–2nCQ®¥/2†£i÷>r’yÍMi|e•e…þpi5'ßó9È¬{†‚ùUŸø»šCóg±h’n_v ?ZèmeY;½º×¢_¶IÏ¯½tV³9SÏ½Kwz”¼Šº•é7kž{Öö“9«?uÅs7®^çZÕÓn µiêo2ÄÌ|ØçŠÏÖd\bH~ic»|æ«j²3Œ÷âÓvbZ3KDòDK°õý*õÌqç\r\n"
				+ "Ö°ô	Ê«€{w­n 'ssÞŸ™lÜÈ±˜Q³íŠ’ÒÙ|£óªkOZ¸“n‡9¥b®hÂ……:pÊ¹Væ¡°¸]§-RÜIº2GZ%pZ™÷Ëç¶\bïŠœØGŒcqÉÏzn‚zëVi\f`0éQ~Ã+]\"¬9\r\n"
				+ "ÐRY06Ì7T“Ë·$©J©g#)uÇr*lÞÃÝLvHBŒš­¨Dnm\f`÷«L«>IïQÎø„±³KÈçžxûÁº4ÇÐõýÐH™^Dè›úŠùëâ/Ãoü;¸ÿ ‰õšØ¶\"Ôm†øeÿ »ô>¾Õôö©Ý»àÖ£\fÐ£E:îB>dp\b#Ü‰+î\\fâ|¦×`ó#9õ¦©	Ïq^ó¬ü-ø}©ï2xr;vc“%‹yG>¸gð¬¿€~vÝ¥ªÇþÏœ‡?øíO/‘²¨(K†åI¤ŽöQ¸—<ñŠõXþ x-~wÖ5Cœÿ ËXÇòZ¹iðSáÔxÙÝHW©7®7}qT Çí¢y\r\n"
				+ "\\60I?$Ö† kúôt:I2~ðákÚ,¼àM8iá»|ç;¤]çój×‚HSe½º'l\"€*¹…íQÃø;à}¥¼Ë©x…¼Ù \">vƒô¯D²´†ÆÜAmE0)JýcQ5ðûª+XC”Æs”‡_NV=ªÜ÷ªqÄÒ8ËRœÈÜž>´ø‚ ë[%c;õ4¢(¶ªóYÒ»É&3Þ¦»¸Ë`†0ZMÌ8íU~Ä+’†Cš¢×1¼ß¾n*ÅÜÿ .g\\L³¿“Š–Ã^„›æóbv\r\n"
				+ "Ô{Ôð«IóS|¨¼‚Ýù©£aç ©W,°óÈÑ¬G9$ {Æ‰j>ü*Å@žxÆXu.Üñ^+ðûM÷Œlìä“ÌÞÊO]£?Ò½Gâçˆ•î-´T|,hû“À¡ùØçåIDRKûõ&¦ñ>»†|04»o¾àƒ·¯=MSÓnsrŒçˆþoÄt®cÇ³^êæ!&á õ4’W¹{‚r¼f­E9r2k Lß{uOÙ^\r\n"
				+ "×Ër\\ú~klnG2€Ôâã#iæ±!½åêq©m9Þiò¢\\u¸ã­`øÇÃZûÅªÛÍ%®£k –ÎþÙöK\fŠA\frEI>²‘Gæ¬ûŸ»nWn;V´ùéûËC\r\n"
				+ "œ³Ýg‡ÿ j-CPÑ“áßí\bŸmhÐ.™âØá|l8Ûq·×ý°2;Ž¤rGðÜ+¡å]\b!‡±¯1ñ,ö÷Ö¬Œ£=«'Âž<Õ<söW‘®4ã÷­	ù£÷Œöÿ w¥zÔ±$yðÚû§ªOtÅIQ†ìk.øÌÍ€ysuàü­þý;_Òõë£Ý	˜7\f‡û¬;©ytÏÕØšzœÖ°û]rÓPÿ D¿Ìrô÷4ÍJÎD‹'æˆæ°õ(¤vó{ö£Lñ­Ö—'Øµ8÷ÂÍßœSõ%¦Æjr	P9ƒ§LÓëb9®¾é4R/µØ\\\f·%	éÅaiZ$‡Ä­r\bÛßëI¡«Î<  \r\n"
				+ "ô®žÆåÌyõÌÚ¤™+ Ód2ÇœsYîì‹Ób\r\n"
				+ "¶a>æ©§91|§šÌ¼mÒîrÅSƒZÇÌƒAž\\üÔÉå6±úÑ¿ oi“¡VÎ)­I$Ò¦h®1ä¾õ¡æª·GIŠC.ø{u«w	µ÷\r\n"
				+ "µ&}£½h‚cT-,÷IæÈýªØ˜²y#®=iù“n¨·m–ëV^Póüêž!\r\n"
				+ "GÇzšyXGµOj›ýÆ¤^jÔ²ycq9¬á+#äõ«s:1¹±š´âÍ2KÝªjH‘Žx'?ãSí&KV~§x`Ãyyçü?Æ“cØ–ø\\¯ÿ ® \fÍÖäS¼ÿ 6»P$Ì8 \f­nÏoÏ®jîö\\2J2=ë°Ôœõ®{YÓUd^½êXï©ÊÞmo“8ª²ÿ =:UûË|p¢¨ÍÖÅ8ïr™,Ü–âœ0Þ¤ÙÆÜÿ õé3»h5¢3’êM\r\n"
				+ "+ƒÍ<¿–8õ¨YÊ®¤.HËÓ¤þk¹ÈÍ!-ÔÔfão Pò– @='O»š|“¡jõª£po™qõ¦¼žiŠ÷#€Ä“Iö‘aY¿Õd9Í$€+Äw°\r\n"
				+ "¹ó%R‘j±¤'s±-üªU™—œóL•Ý&¥¨#ræœÓ7JŒ‚ `u¥³º-Îçà„Å­\\jÒ¯ú¨ö©ô'¯éSø·UmKÄS]H \r\n"
				+ "´c¸*‡‚u¥h2Í¸\r\n"
				+ "ÌßÊ³$¾’YËHÙ,sSê_K&—ºM>[Ò}qø\r\n"
				+ "ãu	BÉ%Ôœ’k¶¿4ß	\";a¤p=Ï5ÅjqÇ4>_©§Ðd’Í!s×¥C=ìQ/Í ¬;x°ùÈªwZ¸“†?|úG¿*ŠÇB|Ep¯œqU¤ñT™9“¹YïÀ“rÉQK©±Ž7dcÏ7¹ÒËâ6<3üµVç^ÉÞŠçòi ÞõÅÑ\r\n"
				+ "´J:zÕ$úîÍ»íDÍ˜¬kæá]·g½9®¤ífíÚ©¹WLÀ µ\\SìZ±Öõ?Þ\r\n"
				+ "CGº(øù×ødÝ?çŠî<9ã½3ÄËöiA·»÷·Cþé=~•çD‚Û•¹ïP´Ûq lÙVSýkªYGDsN\r\n"
				+ "Z³Õ®O?5f]Ã€‰‡¹-'âmíœëe®qÝYSï/áüUÑ&«g«[‰tû‘('/­wFI«˜N.;•-ìõYõao ÎÀÿ  ®»FÒ.ì%ÍÕï˜O_—/„,¬4; ¿¹ˆµÌÈ@#¶jè„ÀÌ0OZáxµ,B…7§S±a0Îu½\r\n"
				+ "+ü[ëcO>bü+$Ù9íW´yaË{âº#.i]sVÑ–&SæpÝëCOˆºŒ‚~ƒ59ù·µièî\bÅt-Lùz	m¬:w§I \fõ!]ƒ'­>DVLšÅ-\r\n"
				+ "ú|Ëmq°¼zÕû˜wž\r\n"
				+ "P™Ì«°#²â_Nôú\\^„öªÆ=¦Ÿ2ù?:}i°wÒ¥û,“†Ãr;S°±˜°&CRÈÇnsÉ¨-ášÄ©Š|ìx<ô©`ö y	¸\r\n"
				+ "Ã¿5zâOJHmPEæJ –¤`Š¥S<sš^…\fbì*­â$Ãl•3MÁ«NàåZL6ØŒJÑ|ªÞÕ^iH%·õê\r\n"
				+ "™åŠó,ß#qøÒÈÿ 7Ì{÷©ò˜’:Î~oJ§urf<ñÚ¤œ´Ž\r\n"
				+ "f¢’9¹cWm	}ÌM_Mû;nP0ZÀ»UV\"»+ëU¿¶1£§=ëÕôëûy¶Ü@Ê3ò¶84’6†{^M5ËcƒV<‡nëOMbrÇ¯4ÕÁ¾…Hã–B	«?c’AŠ·ohm5(D\\‚Ý)ÜwfH·–9\fn?tPÙ5rc’qU.dÚ›Q¹ö§C®Åy%mäç=é«38 T„°;œsMi?î×òªQsÉ¨.dËmSúÔ¬åAÜ*>?LÐ@ü½;úÑî|³ýiÒ+‘ëFTr[ëR;s’8 ¦¢gÈ=»Ñ¸mÒ~€Þ·­ô‘n§ƒÍ.•bn¯UÏBpsNÑl%¿µUzÝ³Òc°{uÅ+^%-È<k|±ÛGh³n\r\n"
				+ "3\\MÖ«$—$Åjø¾û}ÞÐÜç¨éº]¾\r\n"
				+ "Íä õ#4Öƒo¹ÁÞj‘JjØ9ãÞ©6¥æ6<Îgñ¬Ëëù‹y˜R§®ßá>žôØ.®ÔÃ1=?þµx\\·Õž«jö4ä¼2·Þþy¦-â8Àn+=nŒ’ùØÆœgŠRáNîÂŸ/‘<Ëré˜ÿ  ¯Ò	ßv7¼ªÉ&ì¶î  #õ Mµ¹Sƒß*­©7Ô±%Ã±§9Ç ô§;FÃaCë»üÿ žjœÓº¿Êù=ÏzD¹-òóÏ®9Çò­;u±+ð¿,Ðp* vÎp0?Ò4­¿zŒžØ¨¤‘•²ãëïZ®†oÈ »»¹ŠÚÆßÍšWXà‰G.Ç Í}Kðsö ÕƒbñßŠ ´Õ§]ÓÚÞÙ±ˆ7<,‰– ŽP÷5å?²Ž¿ð?Â^3þÛøµ©\\[Ü–ÂWµ/.2rG9'>Ÿ}Ñá?ø7Ç)sà?XêVÁ Aa8r£§*9•m:>ÒŸ#ü\f½¼éÏ™#çMWö}ø±£ê(·~šîÕ_2ÞiN·Pªú“J÷€®#Ä~ Ó,¼y&wr°8á¿(nHÀ'¿µ}©\fÆ&Þ‚ AÆ8¯Œ>%Ã§x·ÇúÄZõºN¦_/÷N;är=s^t2¯a>jr;ªfo×´ŠÓ±©!+p{qRhÓ·­qèþ,ðž®I§tkù2ñT~ÿ C]_‡®ÚtYvOQé^\r\n"
				+ "rŒlÏ>nî÷6n¥vš¯h· ô<Õ $Œ–ÊM‚»žÒ:	/£û’jtÃ&àsõ¬D%¤óóéZNÅ>cU±:=mæŽÔù¬2j_µ%Ä[Ñ¿:Ë»›1òzRÙ_íˆDªÖ… µ7,ÕY2®A¹zýzÈ±¸•xSŒÖ”7DŒ¥ÔW×BY$Þß8Í	³F] ¨g”¹[Ö¤³œù$oþ*,ú÷\r\n"
				+ "ÉWšCò»ëQ[ß}ª1»çŠ­¨Ü‚I’,ŸZ§§^˜¥0“zJ†4hÉûÇ-žžõ^pq“N[¯-Øçó¨§6ÿ žhfëf·Ý=yX^J‘Ÿ¶0jåÕÖ3Þ³.¦ó¾AÇ½Kî=KQÝÆàúÓ&‘ä—r·Ëõª\feŠ^[åëI%ò«d·5[ƒî[ûLeÊƒúÔÊ¶—#eÜa—Ò°ÍñsÍ\\·¿Ü1XWðÅ³IçiQ¶?ˆÕ9~VÀ\"º5¿täšÁñ\r\n"
				+ "¨—ý\".|Sô'Ì¨× É’«Ét¹%Z¨K;“µš£óÙIíNÅl¬[¸ºÀÁj¦âRþ`n>µ—FRFiŸku\b«ù	ìZ–ä‘°ŠŽ&füö¨D…¾cCNü¢¥Û ½I&˜ÔÔr;úé¾gË¹G,Á¸•Ê%ó\r\n"
				+ ".\r\n"
				+ "4Ìˆvâ¢VcÉ4­ÁóÔ‡¨²;· qõ«Uœ—W\r\n"
				+ "ñšª…çm°©cè£9­¯	L·´£ÇÃ+‚®?‡v3ªÑàHc]Â¦×/¢·´gtTÊb\\«W/ñ\r\n"
				+ "[¸‡H•V}Ÿ!Ë½(ŠÐlçuÏ\r\n"
				+ "­Q¢¶ùÜ¶ZÖÒ|?}~‚mFrŸìæ¸¿‡šlð]5Ä—u!rÂFä×¡ZÌdÀ½¼XW¾;Q¸õZGã?	ßø+S6WŒZ6ŠLð}«\r\n"
				+ "°ÒŽ:×Ð_<kâíš1æ(-ZùóQ±»ÓuI¬ïËd?ÄzûŸZñ¢Ô·ÜõªsB×%.êwíœö4æ™GÝ—’}j”“I¸!8Àõö>¿Jp¹7CZò˜&ËÞsHÜäö£Î‘[õéT$¼\"PªÙôæœ.Ø¡ŽH©éëÒšR¶¨\\Ë¡pÜÿ õ»Tfy>ñïÆ=:UdšX\r\n"
				+ "Œ“žÔéffæ7äuÍh‘».Ç4lwL~ƒ¥G1„¬9ÏµBnÜv\r\n"
				+ "øgÿ ­I\f»ÛÌrNÒÙƒvÐ™£fx?×ü“Si7×Ú%òj.©qcpŒn,çhÜc¦\r\n"
				+ "šÎ7†ÞFQ““ùSÚu Ë»g\"ªö9äÑì^\fý¶þ>ø4‹\r\n"
				+ "ÏÃ¯Úº1k–áÝâT*üdõ&¬éZäÞ'Õ'×ç‰bk©<ÇUä{W‹Û^Dì§ƒÎ8¯Tð#°Ó#f dŸZÚœžÌ‡Üêî·ElÁsPxG_µ¼º–Ê)öÍ2Bx`=qéïMž`èNk:óÃìÑÝ-ÜÖwqa¼´“l‰ížãÚµ½‹[ØïÉMÁ¿Z’É™Î­p©¯üRð’â\r\n"
				+ "\r\n"
				+ "5û%ûv›„ ûQ÷ú[žø“àÏÌ¶º>·ÜžMÉòåÏpýïÃ5piì)AïÐë¡b¼müjÌSƒž*²#¡ÚGçRÂWñÓšd±÷AÙp^ ´Þ|½j[‰p6íQÙ¨õªVz²dkÛLxoÒ®Áp6ÇšÍVØ0*äÀÆj´èNÄ²Ü±N,È\r\n"
				+ "ÜPò±BÅEB³'-Š^A¾âÞ•\bnõA xäÜ£šº×Q…¨Ü.0zÔ¿\"º•¤»¸#ïüÕ]KåeÛ&w\f‰ûÅ=j m±åŸœÔv#–vlïüê£Ü…óÓ¥O?-ŸZÍÔ¦1#F½ûÒe\r\n"
				+ "ŸUiÔ<â¢[ŽXUbTÓÒR¯– ¶š–u§Õ‹WùóžÕ@ÈÎÛŠqNŠbZbÝhhÍ;Œj–¡pL[ÿ ®„›p;Úª_^C¸éN\"õ25 <âÇŒ÷ª2Êá©uDO'\r\n"
				+ "®&Ü»³Þ´Ø”/˜G4ä!ùcQ3ŒñJ†ëRÛ\r\n"
				+ ":“æšIn3Q]^Gòj—ü$©‘3°}ïÊ€±£# ›sQnAýkŸÓuÍcT¹š	FÕVÂæ<q_®kcN‚çiÄÿ *ŽnƒB¦£;ê\r\n"
				+ "hmFÅ@|Àýý1MÑì|E>ºï$\r\n"
				+ "Ö¯€ª&ÉÏ®1Å^†ÙQ÷ºc=}ëCO¸´Óæ[›ËˆãûÌàTÞû\r\n"
				+ "èmè>·Ó7O#gvOÒ­®—ei6ø\r\n"
				+ "n<åß5…ñ‹Â–ëö-áõ+ž‹’ïçÜöª+ñ#Å§>|zD¹ùwJGò¦OÚ½Íïx§HÐâ-{{;x_0gò¯%ñÏÅ;}E•fÏ\r\n"
				+ "œäd×Q«x;Ã~³{ë¹f»œä½ÅÌ›˜ñúWš4‘ø£_UŽ±næ ½OçIšÆÛ×ƒ/õmfÞ&Ó­Þü{yÅw¾ð‘½‘U÷JØüë'Âz]½¼pR4\r\n"
				+ "æ b½—á¾µð“Â-ö·«O}sŒ‹}>ÔÊß_J5¹6g¨Ê€\r\n"
				+ "Ç½yÇ?\r\n"
				+ "F„x‚Æ,ï??Šõ;Ð×üÇ‘Y:•¥¦³a&›}`êG>µãGÝg·RïCæô»UlI’ß^¿ç­*Ã>f?ýz»ãÝxS\\–ÖT>ZœÆHç¬†vwÂÊ:zõõë]–Ù£‚WÙ–7´N65*JCeÉéÇ½gùÞhÏL¾•6Vyy\r\n"
				+ "ŽsZ­Q7ì[†i%Pž=iÂ\\Žìñþ{ÕIee™¢‡•ë¸w÷¤óR{0aÆyÿ ?…CµÌœû–üÄÁ=?½ŽK{Õ*zóÏj­,»†r)\\ž >µ\r\n"
				+ "™ÝßBàrÿ :·¹ö¨®¦Ü69ãuFóH¯´@üê7˜ÈpäƒR»±›}‰-Z8.Òa÷C®~õz† ×ô{$½ÐÇÚ­äù¦±•ðÃÔÆO~¿)àæ¼®=èÊÜz€?Ïùæ½{Á·ý·\\`ýkHKPNÎÆ¾—â];ZGŠÚY#ž/õö—1˜åˆÿ ´§ùô«ö7[$÷¬ýGH²½)w, OÜF{g¸ö5%…Ê± ut£M7;mE¸€1<ç×¥7Ä\r\n"
				+ "|ãDòüO C3q‹…ù%ð%äþ9‡gR˜µ¿ùò=+K)tÚwLä›á÷Ä\r\n"
				+ "â‡?\r\n"
				+ "Ì1¯WˆÓÌ_÷Dª7cðßø\\Z×„Ú8~*|7¿ÑóÃjV‡íVŽyèÊ>_¡&»••1éÞ–9\\œoûÝG¯Ö¶{êÑŸáïxcÆVÂïÃ¾!´¼ù’9@u>…Ì?*¿\f\r\n"
				+ "ç5ÏxàßÃ\\}ºûÃËgvä¿Òdû4È}rœÄÍ0ü`ø [;³ã\r\n"
				+ ",g÷wcÔ\"F&ïî}©«õ”^Ï_3ÐpÎ¬Û–'“\\>ñãá–ªæÃU×ÿ ±¯T|öZäfÙÔ÷å¾_×ò®¾ÂúÊþ;N¼†á;<3+)ú08«Ò×„–æ“8“ÅU1Ë<¥”ŒŠ¬uG9B¸ö§EzÑ.ìÒº&Ì²\"‘W­;¤\fçŠ‡í$|ùÓ#¼flÿ ×£aYt%º·.ÃiíT§ÓûÈ*´óz ’è¿ì-\r\n"
				+ "“Û9JòÁd¯áZ3HOSÖ«I¸1ëPÑiœõý©…€Q÷ª¼qÉælzÚ¾„H¥‰úVd°<d°©êÔåªevêeÝÀ)ªWšŠÄ0W¨ï©n[µŒ-ùÖ§~f™Šô¨¯u v#uUó7eœS±;ì&A|±¡å+Àèá–òQ+’jÔ^ÔY¶´_ž*Ÿ–š•#lœæ¥fUo™{V½¿ƒ¦òw¹#ßÍx—_Ñ¼7¸ê·„a¶€‹¸“øTu\r\n"
				+ "ôD·6qÞ)Žcò·½Ak£Úié¶ßyôÑ·ÌÖ#|C¼ºrºƒïî”p%‘|µ>ã?ýjHî~)j­¾;-?Oþš9þB©Ù\r\n"
				+ "«$1AneÇÖ¨^x®(eû.‹lnçÎ6Æx_©ªÿ ïoÏÚ|Oâ««£ÿ <býÚWA¤èZ~†ÂÐ \fkÊÑjcÁ¢|P×ÜIq®Ã¥ÆOÝ…DÇ¥tÞø%áf›íúì×šœÇ’onK/©ù~ïé[&–Ïóº÷®ŠÚ!aUi¥©-¾„:v‡£èÖâßNÓ¡… @‘ŠŽöxáŒËÒ®ÎÑíùqþ=ñZfŸ#4» sïWk‚<ÿ ãŒ¯ Î—¦2’ÿ /Br{ÿ Jó++ÍjÞõn-îž7à,ãj÷Š5¦Õ59.$ŽI³×ÞO[ÝFÆì»*ñÛ þDþU\r\n"
				+ "õ5Žº§ð“@¾×®£«^Í<c…99Å{”Özd+X@ô®KÁu¯†ô(¢ÜãW®5ryG¯6¬å%©ÙN6èXšW\r\n"
				+ "ƒYÎÐÜSÆkYØ8Ãþu—ª¦	>üVrBkK˜ü ŒôCq	ÙsÍŒdt>ÕóåÝ•Þ™<¶·ÈÈñÈTƒÉ¯¨´‹¯9|–=«Ë¾=|:Þ/Š4ð6âá@êÝ¿Ãñ®Š2³µÎ±ûG–©d\\°À>ù©Ò\\E»pôëÔTr$‘“)#§¯jDh•0ãðÝßüãó­®‘Ç)&<Hàù ýî:Ô	a¤–Î3Ï¯ZŒ|ø#©íK¶'‹Í ç×'zÖ.÷9›¾ ’d˜›†ÆsR£,@.ï›n*0ÓnÇ”y^[çÐyóŽ8?\\ƒÒ•‡ÐRDÍŽøÈÇóýiI8;Ž}j4e\fÎÏËdîÏ§oóÒ‘dÜqŸÖ‚T“,#&ÀNzû\r\n"
				+ "ô_‡Ò]Û­¹Cò&	=Èà×š’æ`ò:cëŠ×ð–­>›xgùI<çÛÿ ÕOáw§²}¡Z,)ÍfOs.“?Û<¶xHùÂu_z­¦x’Êøý”Ê«0þýïqëW¼ÍÃ¹â»\"ôÐÑy/…uˆgQ5¤Ë\"·Ýu9»K)Ä‘ƒšñØ ¾Ò.ÿ ´¼7*E1}ÒÃ'ú¹½÷OûCñ®«Eø¿¡[Nšw‹á¸Ñ®$ Ë7QæÞ\\u+2åHç¾:ÖÑŽõ¦líÅ9&a Ö}ž¯§ê0¬öð\\FÃ+%¼«\"ŸÅMN³¸ëÏ4ý	Ñš~nåéQ1ËncøS!•¹#4®¹;óT‘.ì5-#D×¢ëú-¥ò‚òÙ%ÛôÜ+•¾ýŸ¾Êæ}L»ÑfÎ|ýQš†â¿¥u«+u4ÿ 8ìçëIÂïSHÎQVLà§øgãýmÞøÛ¬\"»­wcónß…,Wß´V’\r\n"
				+ "Í7„u„ÛÆó%£þaq]´Ò`àÕy¶`ç<¥©2Oúò±ÈÅñ_âÇˆ>ê2(<É¢ê1\\ã½iÑþÑ^´r<I¤øƒFnë©h®?T$WM#ƒÀ§%Äßß8÷9£ÞèOºþÏâfèÿ þëÁ›ñNË—ù$õþþ+£´¿³Ô¢ó´»Èn•¿ŠÚUÅs\\î±àÿ ëñ2kžÓn·}ó-’?Ž3úÖïÀ?„7s}¦×Âï§Iýí.úh3ø+bEh&wÓ<™*ñG<Š†Y3ÍpàÕ•Î‘ñ3ÆáO\r\n"
				+ "5³ ƒƒTæð'´ü®•ñã\\Á?*ßZG9Éí“Jö{ *{3Ðgeq‚k>úu†\"IÈ¸I£øÙ§î¿4Û¾3‹Í\fÈïƒYWþ øÖ\\Ãs­xm¿Ý´_óš.‡ÊÖ¨ëu=L9;+êåúæ¹™æø™x\r\n"
				+ "Iâ\bGý2´ÉŸõ¥µðÖ¥<‚çYñÅÈê6áüRdJ6Ô¹¬ëwö±çOÑå»ôT`?SYÑk?¯TCgàÛ4'îý¢÷‘õÅnDŠÃ=ñ[¾Ò™¥ó¦\\PöÔzt1t-\r\n"
				+ "ã		puÐãœÛ¼Ä~ÎµÏ‚¾!jç~±ñ)à Ì.Á#ÏýöZºeD@ ©%”¬\\QæC½Ò±Ç_ü0ÓÂ¹¾ñ·ˆn‰vmL¨ÏÑ1U­¼#§XF\r\n"
				+ "bØû¯!Ü3]5Ñ{‰v–«PYÇ2d¦îúœ£Z•áSô©í´ÛÉV1]LZš¾öJ»\r\n"
				+ "¬&ÕˆTÊä«]Øä Ðîî%Øc8­Ý?ÃñB ‘kAQR]ÁHá³Ò¡Eî2HãŽß\r\n"
				+ "£š™îây®[ñ¦ƒ¡Ç¿QÕcCœmêJæï>-\\ê™Â~¸»Çü¼Hv!üúÕZÃµŽÃ[Öb¶·f2`ú“Ò¼CâŸŽÖþñìaŸ~Î¿7½\\ñ–¯âÝB)#×õ˜£¶Öˆ~aŽç¯ÿ ª¼ÞîÍ^ïs.w7}¿•ìZÛA†ëÎGQÉëôë]×Â»Ôµïnag<¨®>ÚÚ8Ë‡Ž¾½?ýUê,bÓ´¸˜'ÌGÏõ¬+»BÌ¸|Zãj*#©éP}¯y8=ë+íe$ž*EºÙÓ­ys–‡¡ÿÙ\r\n"
				+ "--MIME_boundary--\r\n"
				+ "\r\n"
				+ "";
		
		System.out.println(getFirstJsonFromString(te));
		final String text = "POST /65457-5456-5457-987456 HTTP/1.1"; 
		final String extract = text.substring(text.indexOf("/") + 1, text.indexOf(" HTTP"));
		
		System.out.println("extract: " + extract);
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'Z'");
		String aux = "8591641950";
		String temp = "";

		System.out.println(" o que Ã© cartão " + toHEX(aux));

		String n5 = "1237";
		if (n5.matches("^(?=\\d{3}$)(?:(.)\\1*|0?1?2?3?4?5?6?7?8?9?|9?8?7?6?5?4?3?2?1?0?)$")) {
			System.out.println("true");
		} else {
			System.out.println("false");
		}

	}

//	public static String conversorHexaDeciimal(String Cartao) {
//		try {
//			if(Cartao.startsWith("00")) {
//				return Cartao;
//			}
//			if(Cartao.length()< 8) {
//				return Cartao;
//			}
//			long longAbatrack = Long.parseLong(Cartao);
//			long fclong = Long.parseLong(Cartao);
//			String hexAbatrack = Long.toHexString(longAbatrack);
//			String hexWigan = hexAbatrack.substring(hexAbatrack.length()-4);
////			olhar a posiÃ§Ã£o pegar exatamente o mesmo if
//			String fcWiegand = "";
//			String temp = "";
//			fcWiegand = hexAbatrack.substring(0, 4);
//			temp += fcWiegand.charAt(0);
//			temp += fcWiegand.charAt(1);
//			temp += fcWiegand.charAt(2);
//			temp += fcWiegand.charAt(3);
//			
//			longAbatrack  = new BigInteger(hexWigan, 16).longValue();
//			fclong  = new BigInteger(temp, 16).longValue();
//			String wiegand = String.valueOf(longAbatrack);
//			if(hexWigan.startsWith("0")) {
//				wiegand = "0"+wiegand;
//			}
//			String fcwiegand = String.valueOf(fclong);
//			String fcwiegandtg = fcwiegand+wiegand;
//	        return fcwiegandtg;
//	       
//		}catch (Exception e) {
//			System.out.println("Não foi possível converter cartão " + Cartao + "\n motivo " + e.getMessage());
//			return Cartao;
//		}
//	
//	}

}
