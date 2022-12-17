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
	
	// As preferencias sao armazenadas no registro do Windows, na pasta HKU\Software\JavaSoft\Prefs\myapplication\myclass\ (Pesquise por JavaSoft)
	//public static Preferences preferences = Preferences.userNodeForPackage(Main.class.getClass());
	
	private static int dialogWidth = 370;
	private static int dialogHeight = 67;
	private static List<JDialog> notifications = new ArrayList<JDialog>();
	private static List<PreferenceTO> defaultPreferencesList;
	
	public static void sleep(long tempo){
		try { Thread.sleep(tempo); } catch (InterruptedException e) {}
	}
	
	public static class SoundPlayer extends SwingWorker<Void, Void> {
		@Override
		public Void doInBackground() {
			InputStream inputStream = null;
			BufferedInputStream bufferedInputStream = null;
			try{
				inputStream = Main.class.getClass().
						getResourceAsStream("/com/protreino/services/resources/sounds/new_notifications.mp3");
				bufferedInputStream = new BufferedInputStream(inputStream);
				Player player = new Player(bufferedInputStream);
				player.play();
			}
			catch (Exception e){
				if (Main.desenvolvimento)
					e.printStackTrace();
			}
			finally{
				if (inputStream != null){
					try {
						inputStream.close();
					} catch (IOException e) {
						// ignore
					}
				}
				if (bufferedInputStream != null){
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
	
	
	public static void setJavaLibraryPath(){
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
	
	
	public static void loadDllTopData(){
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
		}
		catch (Throwable e) {
			e.printStackTrace();
		}
		try {
			System.load("C:/Windows/System32/EasyInner.dll");
		}
		catch (Throwable e){
			e.printStackTrace();
		}
	}
	
	
	/**
	 * Recupera uma preferencia. Caso a preferencia nao exista, entao retorna o valor padrao
	 * @param key
	 * @return
	 */
	public static String getPreference(String key){
		String value = null;
		try{
			HashMap<String, Object> args = new HashMap<String, Object>();
			args.put("CHAVE", key);
			PreferenceEntity preferenceEntity = (PreferenceEntity) HibernateUtil
						.getUniqueResultWithParams(PreferenceEntity.class, "PreferenceEntity.findByKey", args);
			if (preferenceEntity != null) {
				if (FieldType.IMAGE.equals(preferenceEntity.getFieldType())) {
					value = preferenceEntity.getImageValue() != null ? Base64.encodeBase64String(preferenceEntity.getImageValue()) : null;
				}
				else 
					value = preferenceEntity.getValue();
			}
		}
		catch (Exception e){
			e.printStackTrace();
		}
		// caso nao tenha preferencia definida, retorna valor padrao
		if (value == null)
			value = getDefaultPreference(key);
		return value;
	}
	
	public static Integer getPreferenceAsInteger(String key) {
		try {
			String stringValue = getPreference(key);
			return Integer.valueOf(stringValue);
		}
		catch (Exception e){}
		return 0;
	}
	
	public static Long getPreferenceAsLong(String key) {
		try {
			String stringValue = getPreference(key);
			return Long.valueOf(stringValue);
		}
		catch (Exception e){}
		return 0l;
	}
	
	public static Boolean getPreferenceAsBoolean(String key) {
		try {
			String stringValue = getPreference(key);
			return Boolean.valueOf(stringValue);
		}
		catch (Exception e){}
		return false;
	}
	
	/**
	 * Recupera uma preferencia. Caso a preferencia nao exista, entao retorna null
	 * @param key
	 * @return
	 */
	public static String getPreferenceWithNull(String key){
		String value = null;
		try{
			HashMap<String, Object> args = new HashMap<String, Object>();
			args.put("CHAVE", key);
			PreferenceEntity preferenceEntity = (PreferenceEntity) HibernateUtil.getUniqueResultWithParams(PreferenceEntity.class, "PreferenceEntity.findByKey", args);
			if (preferenceEntity != null) {
				if (FieldType.IMAGE.equals(preferenceEntity.getFieldType())) {
					value = preferenceEntity.getImageValue() != null ? Base64.encodeBase64String(preferenceEntity.getImageValue()) : null;
				} else 
					value = preferenceEntity.getValue();
			}
		} catch (Exception e){
			e.printStackTrace();
		}
		return value;
	}
	
	
	/**
	 * Salva uma preferencia no sistema, nos registros do Windows
	 * @param key
	 * @param value
	 */
	public static void setPreference(String key, String value){
		try {
			HashMap<String, Object> args = new HashMap<String, Object>();
			args.put("CHAVE", key);
			PreferenceEntity preferenceEntity = (PreferenceEntity) HibernateUtil.getUniqueResultWithParams(PreferenceEntity.class, "PreferenceEntity.findByKey", args);
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
			
		} catch (Exception e){
			e.printStackTrace();
		}
	}
	
	public static void defineDefaultPreferences(){
		
		defaultPreferencesList = new ArrayList<PreferenceTO>();
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"blockSounds", "Bloquear sons", FieldType.CHECKBOX, "false"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"scrollSpeed", "Velocidade de rolagem", FieldType.TEXT, "5", true, 10));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"restrictAccess", "Limitar a quantidade de acessos por pedestre por dia", FieldType.CHECKBOX, "false"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"restrictAccessDays", "Limite de acessos", FieldType.NUMERIC_LIST, "1", "1;1;5"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"toleranceAccess", "Toler‚ncia de entrada e saÌda (em minutos)", FieldType.NUMERIC_LIST, "0", "0;1;20"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL,
				"minTimeBetweenAccess", "Tempo mÌnimo entre entradas (em minutos)", FieldType.NUMERIC_LIST, "0", "0;1;20"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"timeAccessList", "Tempo de atualizaÁ„oo da lista de acesso (em minutos)", FieldType.TEXT, "2", true, 10));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL,
				"timeUserAccessList", "Tempo de atualizaÁ„o das lista de usu·rios (em minutos)", FieldType.TEXT, "2", true, 10));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"printLog", "Imprimir log com o servidor", FieldType.CHECKBOX, "false"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"releaseAccessRequiresPassword", "Exigir senha de administrador para liberaÁ„o de acesso", FieldType.CHECKBOX, "false"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"releaseAccessReason", "Motivos para liberaÁ„o de acesso (separados por virgula)", FieldType.TEXT, "", false, 25));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"timeReconectDevices", "Tempo de aguardo para reconectar dispositivos (em segundos)", FieldType.TEXT, "5", true, 10));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"enableTCPServer", "Habilitar servidor TCP", FieldType.CHECKBOX, "false"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"tcpServerSocketPort", "Porta do servidor TCP", FieldType.TEXT, "2020", true, 10));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"enableBroadcastServer", "Habilitar servidor broadcast", FieldType.CHECKBOX, "false"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"broadcastServerSocketPort", "Porta do servidor de broadcast", FieldType.TEXT, "2019", true, 10));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"warningPaymentDueDate", "Dias para avisar sobre vencimento do pagamento", FieldType.TEXT, "0", true, 10));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"importExportDevices", "Importar/Exportar dispositivos do servidor", FieldType.CHECKBOX, "true"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"imageSizeRequestServer", "Tamanho das fotos recebidas do servidor (dimens„o em px)", FieldType.TEXT, "48", true, 10));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"registerAccessWithoutConnectedDevices", "Registrar acesso mesmo que n„o haja dispositivos conectados", FieldType.CHECKBOX, "false"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"pedestrianAlwaysOpen", "Cadastro de pedestre/visitante em sÈrie", FieldType.CHECKBOX, "false"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"hourAutomaticRoutines", "Hora para execuÁ„o das rotinas autom·ticas", FieldType.NUMERIC_LIST,  "00","0;1;23"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"enableCardAcessClear", "Habilita 'baixa' autom·tica de cartıes", FieldType.CHECKBOX, "false"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"enableDirectionClear", "Habilita reset de direÁıes registradas pelos pedestres", FieldType.CHECKBOX, "false"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"enableCardReset", "Habilita reset status de cart„o/comanda", FieldType.CHECKBOX, "false"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"enableOfflineCard", "Enviar cartıes para Catraca Offline", FieldType.CHECKBOX, "false"));
		
		
		// TODO NOVAS PREFERENCIAS SAO INSERIDAS AQUI
		
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, 
				"messageEnrollment", "Mensagem de cadastro de digital", FieldType.MESSAGE_LINES, "POSICIONE O DEDO;NO LEITOR"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, 
				"messageEnrollmentFinished", "Mensagem de cadastro de digital finalizado", FieldType.MESSAGE_LINES, "COLETA;FINALIZADA"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, 
				"messageEnrollmentCancelled", "Mensagem de cadastro de digital cancelado", FieldType.MESSAGE_LINES, "COLETA;CANCELADA"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, 
				"messageWait", "Mensagem de aguarde", FieldType.MESSAGE_LINES, "PROCURANDO...; "));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, 
				"messageAccessAuthorized", "Mensagem de acesso autorizado", FieldType.TEXT, "AUTORIZADO"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, 
				"messageAllowed", "Mensagem de acesso permitido", FieldType.TEXT, "GIRE A CATRACA"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES,
				"messageEntryAllowed", "Mensagem de entrada permitida", FieldType.TEXT, "ENTRE"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES,
				"messageExitAllowed", "Mensagem de saÌda permitida", FieldType.TEXT, "SAIA"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, 
				"messageNotAllowed", "Mensagem de acesso negado", FieldType.MESSAGE_LINES, "PEDESTRE;NAO PERMITIDO"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, 
				"messageNotFound", "Mensagem de pedestre n„o encontrado", FieldType.MESSAGE_LINES, "PEDESTRE;NAO ENCONTRADO"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, 
				"messageAllowedOnlyOnce", "Mensagem de pedestre que j· acessou no dia", FieldType.MESSAGE_LINES, "PEDESTRE JA;REGISTRADO HOJE"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, 
				"messageNotAllowedToday", "Mensagem de pedestre fora do dia permitido", FieldType.MESSAGE_LINES, "PEDESTRE NAO;PERMITIDO HOJE"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, 
				"messageNotAllowedNow", "Mensagem de pedestre fora do hor·rio permitido", FieldType.MESSAGE_LINES, "PEDESTRE NAO;PERMITIDO AGORA"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, 
				"messageError", "Mensagem de erro na verificaÁ„o", FieldType.MESSAGE_LINES, "ERRO NA;VERIFICACAO"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, 
				"messageTolerancePeriod", "Mensagem de pedestre no perÌodo de toler‚ncia", FieldType.MESSAGE_LINES, "BEM-VINDO;PLANO VENCIDO"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, 
				"messageAllowedAthleteScreen", "(Tela do pedestre) Mensagem de acesso permitido", FieldType.TEXT, "Liberado!"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, 
				"messageNotAllowedAthleteScreen", "(Tela do pedestre) Mensagem de acesso negado", 
				FieldType.TEXT, "N„o permitido. Procure a secretaria."));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES,
				"messageNotAllowedFaceRequired", "(Tela do pedestre) Mensagem de cadastro de face obrigatÛrio", 
				FieldType.TEXT, "ObrigatÛrio cadastro facial"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, 
				"messageAllowedOnlyOnceAthleteScreen", "(Tela do pedestre) Mensagem pedestre que j· acessou no dia", 
				FieldType.TEXT, "N„o permitido mais hoje."));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, 
				"messageNotAllowedTodayAthleteScreen", "(Tela do pedestre) Mensagem fora do dia permitido", 
				FieldType.TEXT, "Fora do dia permitido."));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, 
				"messageNotAllowedNowAthleteScreen", "(Tela do pedestre) Mensagem fora do hor·rio permitido", 
				FieldType.TEXT, "Fora do hor·rio permitido."));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, 
				"messageNotAllowedOrigem", "Mensagem para pedestre n„o permitido nesse equipamento", 
				FieldType.TEXT, "N„o permitido;no equipamento."));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES, 
				"messageNotAllowedSensor", "Mensagem para pedestre que n„o depositou cart„o na urna", 
				FieldType.TEXT, "Deposite;o cartao na urna."));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES,
				"messageSMSAfterPassInDevice", "Mensagem SMS apÛs passagem na catraca",
				FieldType.TEXT, "Acabou de passar na catraca"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.MESSAGES,
				"messageNotAllowedBox", "Mensagem de cart„o n„o permitido na urna", 
				FieldType.TEXT, "N„o permitido;na urna."));
		
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.ATHLETE_SCREEN, 
				"athleteScreenBackgroundImage", "Imagem de fundo da tela do pedestre", FieldType.IMAGE, ""));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.ATHLETE_SCREEN, 
				"athleteScreenFirstColor", "Cor prim·ria da tela do pedestre", FieldType.COLOR_CHOOSER, 
				(Main.firstColor.getRed() + ";" + Main.firstColor.getGreen() + ";" + Main.firstColor.getBlue())));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.ATHLETE_SCREEN, 
				"athleteScreenSecondColor", "Cor secund·ria da tela do pedestre", FieldType.COLOR_CHOOSER,
				(Main.secondColor.getRed() + ";" + Main.secondColor.getGreen() + ";" + Main.secondColor.getBlue())));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.ATHLETE_SCREEN, 
				"athleteScreenTimeout", "Tempo limite para apresentaÁ„o dos dados (segundos)", 
				FieldType.NUMERIC_LIST, "5", "5;5;60"));
		
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.FACE_RECOGNIZER, 
				"samplesNumberForTraining", "N˙mero de amostras para treinamento", FieldType.NUMERIC_LIST, "1", "1;1;10"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.FACE_RECOGNIZER,
				"intervalBetweenCapturesForTraining", "Intervalo entre capturas para treinamento (em ms)", 
				FieldType.NUMERIC_LIST, "250", "100;50;500"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.FACE_RECOGNIZER,
				"intervalBetweenCapturesForRecognition", "Intervalo entre capturas para reconhecimento (em ms)", 
				FieldType.NUMERIC_LIST, "50", "30;10;200"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.FACE_RECOGNIZER,
				"waitTimeAfterRecognizer", "Tempo de espera apÛs reconhecimento (em ms)", 
				FieldType.NUMERIC_LIST, "4000", "1000;500;10000"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.FACE_RECOGNIZER,
				"maxTimeForFaceCapturing", "Tempo m·ximo para captura de faces (em seg)", FieldType.NUMERIC_LIST, "20", "1;1;40"));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.FACE_RECOGNIZER,
				"serverRecognizerIP", "Ip do servidor de reconhecimento", FieldType.TEXT, "localhost:8080", false, 10));
		defaultPreferencesList.add(new PreferenceTO(PreferenceGroup.GENERAL, 
				"cardMaster", "Definir n˙mero do cart„o Master", FieldType.TEXT, "0", true, 10));
		
		
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
				deviceEntity.setLogin(deviceObj.get("login").isJsonNull() ? null : deviceObj.get("login").getAsString());
				deviceEntity.setPassword(deviceObj.get("password").isJsonNull() ? null : deviceObj.get("password").getAsString());
				deviceEntity.setLocation(deviceObj.get("location").isJsonNull() ? null : deviceObj.get("location").getAsString());
				deviceEntity.setDesiredStatus(DeviceStatus.valueFromImport(deviceObj.get("desiredStatus").getAsString()));
				deviceEntity.setDefaultDevice(deviceObj.get("defaultDevice").isJsonNull() ? false : deviceObj.get("defaultDevice").getAsBoolean());
				deviceEntity.setMirrorDevice(deviceObj.get("mirrorDevice").isJsonNull() ? false : deviceObj.get("mirrorDevice").getAsBoolean());
				deviceEntity.setAthleteScreenConfig(deviceObj.get("athleteScreenConfig").isJsonNull() ? null : deviceObj.get("athleteScreenConfig").getAsString());
				List<ConfigurationGroupEntity> configurationGroupList = new ArrayList<ConfigurationGroupEntity>();
				JsonArray configGroupArray = deviceObj.get("configurationGroups").getAsJsonArray();
				
				for (JsonElement elementConfigGroup : configGroupArray) {
					JsonObject configGroupObj = (JsonObject) elementConfigGroup;
					ConfigurationGroupEntity configGroupEntity = new ConfigurationGroupEntity();
					configGroupEntity.setDeviceEntity(deviceEntity);
					configGroupEntity.setName(configGroupObj.get("name").getAsString());
					List<ConfigurationEntity> configurationList = new ArrayList<ConfigurationEntity>();
					JsonArray configArray = configGroupObj.get("configurations") != null ? configGroupObj.get("configurations").getAsJsonArray() : new JsonArray();
				
					for (JsonElement elementConfig : configArray) {
						JsonObject configObj = (JsonObject) elementConfig;
						ConfigurationEntity configEntity = new ConfigurationEntity();
						configEntity.setGroup(configGroupEntity);
						configEntity.setName(configObj.get("name").getAsString());
						configEntity.setValue(configObj.get("value").isJsonNull() ? null : configObj.get("value").getAsString());
						configEntity.setType(FieldType.valueOf(configObj.get("type").getAsString()));
						configEntity.setComboboxValues(configObj.get("comboboxValues").isJsonNull() ? null : configObj.get("comboboxValues").getAsString());
						configEntity.setMaxCharacteres(configObj.get("maxCharacteres").isJsonNull() ? null : configObj.get("maxCharacteres").getAsInt());
						configEntity.setMinCharacteres(configObj.get("minCharacteres").isJsonNull() ? null : configObj.get("minCharacteres").getAsInt());
						configEntity.setNumeric(configObj.get("numeric").isJsonNull() ? null : configObj.get("numeric").getAsBoolean());
						configEntity.setRequired(configObj.get("required").isJsonNull() ? null : configObj.get("required").getAsBoolean());
						configurationList.add(configEntity);
					}
					configGroupEntity.setConfigurations(configurationList);
					configurationGroupList.add(configGroupEntity);
				}
			
				deviceEntity.setConfigurationGroups(configurationGroupList);
				JsonArray attachedDevicesArray = deviceObj.get("attachedDevices").getAsJsonArray();
				
				if(attachedDevicesArray != null && attachedDevicesArray.size() > 0) {
					deviceEntity.setAttachedDevices(attachedDevicesArray.toString());
				}
				
				HibernateUtil.save(DeviceEntity.class, deviceEntity);
			}
			
			List<DeviceEntity> lista = (List<DeviceEntity>) HibernateUtil.
					getResultList(DeviceEntity.class, "DeviceEntity.findAll");
			if (lista != null && !lista.isEmpty()) {
				for (DeviceEntity deviceEntity : lista)
					Main.devicesList.add(deviceEntity.recoverDevice());
			}
			boolean haveDefaultDevice = false;
			for (Device device : Main.devicesList){
				if (device.isDefaultDevice()){
					haveDefaultDevice = true;
					break;
				}
			}
			if (!haveDefaultDevice && !Main.devicesList.isEmpty())
				Main.devicesList.get(0).setDefaultDevice(true);
		}
	}
	
	@SuppressWarnings("unchecked")
	public static String exportDevices(){
		if (getPreferenceAsBoolean("importExportDevices")) {
			JsonArray deviceArray = new JsonArray();
			List<DeviceEntity> lista = (List<DeviceEntity>) HibernateUtil.
					getResultList(DeviceEntity.class, "DeviceEntity.findAll");
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
					if(deviceEntity.getAttachedDevices() != null && !deviceEntity.getAttachedDevices().isEmpty()) {
						String attachedDevices = deviceEntity.getAttachedDevices();
						Gson gson = new GsonBuilder().create();
						List<AttachedTO> list = gson.fromJson(attachedDevices, new TypeToken<List<AttachedTO>>() {}.getType());
						
						for(AttachedTO attachedTO : list) {
							JsonObject attachedDevicesObj = new JsonObject();
							attachedDevicesObj.addProperty("nomeDevice", attachedTO.getNomeDevice());
							attachedDevicesObj.addProperty("localDevice", attachedTO.getLocalDevice());
							attachedDevicesObj.addProperty("idDevice", attachedTO.getIdDevice());
							attachedDevicesArray.add(attachedDevicesObj);
						}
					}
					deviceObj.add("attachedDevices", attachedDevicesArray);
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
	 * Tenta enviar o backup para o servidor. Caso falhe, entao seta um flag indicando que o
	 * backup precisa ser enviado. Com isso, haveri· nova tentativa juntamente com o envio dos logs de acesso
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
					HttpConnection con = new HttpConnection(Main.urlApplication + "/restful-services/access/saveBackupByUser?idUser=" + Main.loggedUser.getId().toString());
					int responseCode = con.sendResponse(responseObj.toString()) ;
					if (responseCode == 200){ // OK
						System.out.println("BACKUP ENVIADO!");
					
					} else {
						System.out.println("FALHA AO ENVIAR BACKUP. Response code: " + responseCode);
						Main.loggedUser.setBackupChanged(true);
					}
				} catch (ConnectException ce) {
					System.out.println("FALHA AO ENVIAR BACKUP: ConnectException: " + ce.getMessage());
					Main.loggedUser.setBackupChanged(true);
				} catch (Exception e){
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
	
	public static void createNotification(String message, NotificationType type){
		createNotification(message, type, null, 5000);
	}
	
	public static void createNotification(String message, NotificationType type, byte[] photo){
		createNotification(message, type, photo, 5000);
	}
	
	public static void createNotification(String message, NotificationType type, byte[] photo, int duration){
		
		List<String> mensagens = new ArrayList<String>();
		String[] palavras = message.split(" ");
		String frase = new String();
		for (String palavra : palavras) {
			if ((frase.length() + palavra.length()) < 50){
				frase = frase + palavra + " ";
			}
			else {
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
		Font customFont = new Font(proTreinoLabel.getFont().getFontName(), Font.BOLD, proTreinoLabel.getFont().getSize()+1);
		proTreinoLabel.setFont(customFont);
		
		JPanel messageContainer= new JPanel();
		messageContainer.setLayout(new BoxLayout(messageContainer, BoxLayout.Y_AXIS));
		messageContainer.setPreferredSize(new Dimension(dialogWidth-65, dialogHeight-10));
		messageContainer.setOpaque(false);
		messageContainer.add(Box.createVerticalGlue());
		messageContainer.add(proTreinoLabel);
		messageContainer.add(Box.createRigidArea(new Dimension(0,3)));
		
		if (NotificationType.BIRTHDAY.equals(type)) {
			JLabel infoMessage = new JLabel("ANIVERSARIANTE!");
			infoMessage.setAlignmentX(Component.LEFT_ALIGNMENT);
			infoMessage.setForeground(Color.WHITE);
			messageContainer.add(infoMessage);
			messageContainer.add(Box.createRigidArea(new Dimension(0,1)));
		}
		
		for (String mensagem : mensagens) {
			JLabel infoMessage = new JLabel(mensagem);
			infoMessage.setAlignmentX(Component.LEFT_ALIGNMENT);
			infoMessage.setForeground(Color.WHITE);
			messageContainer.add(infoMessage);
			messageContainer.add(Box.createRigidArea(new Dimension(0,2)));
		}
		
		messageContainer.add(Box.createVerticalGlue());
		
		ImageIcon logoImageIcon = new ImageIcon(Toolkit.getDefaultToolkit().getImage(Main.class.
				getResource(Configurations.IMAGE_FOLDER + "comuns/" + type.getIconName())));
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
			public void mouseReleased(MouseEvent e) {}
			public void mousePressed(MouseEvent e) {}
			public void mouseExited(MouseEvent e) {}
			public void mouseEntered(MouseEvent e) {}
			public void mouseClicked(MouseEvent e) {
				if (dialog.isVisible()){
			        dialog.setVisible(false);
				    notifications.remove(dialog);
				    updateNotificationsPosition();
		    	}
			}
		});
	    
	    updateNotificationsPosition();
	    dialog.setLocation(screenSize.width - dialogWidth - 10, 
	    		screenSize.height - taskBarSize  - dialogHeight - 10 - notifications.size() * (dialogHeight + 10));
	    notifications.add(dialog);
	    dialog.setVisible(true);
	    
		if (!Boolean.parseBoolean(getPreference("blockSounds"))){
			new Utils.SoundPlayer().execute();
		}
		
		new java.util.Timer().schedule(new FaderOut(dialog), duration, 5);
	}
	
	private static void updateNotificationsPosition(){
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Rectangle winSize = GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();
	    int taskBarSize = screenSize.height - winSize.height;
		for (int i = 0; i < notifications.size(); i++) {
        	JDialog d = notifications.get(i);
        	d.setLocation((int) d.getLocation().getX(), (screenSize.height - taskBarSize  - dialogHeight - 10 - i * (dialogHeight + 10)));
        }
	}
	
	private static class FaderOut extends TimerTask {
	    private JDialog jDialog;
	    public FaderOut(JDialog jDialog) {
	        this.jDialog = jDialog;
	    }
	    @Override public void run() {
	    	if (jDialog.isVisible()){
		        while(jDialog.getOpacity() > 0.01f) {
		        	jDialog.setOpacity(jDialog.getOpacity() - 0.01f);
		        	try {
						Thread.sleep(3);
					} catch (InterruptedException e) {
						// ignore
					}
		        }
		        jDialog.setVisible(false);
		        notifications.remove(jDialog);
		        for (JDialog dialog : notifications){
		        	dialog.setLocation((int)dialog.getLocation().getX(), (int) dialog.getLocation().getY() + dialog.getHeight() + 10);
		        }
		        this.cancel();
	    	}
	    	else
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
	
	public static void openWebLink(String title, String link){
		try {
			URL url = new URL(link);
			Desktop desktop = Desktop.isDesktopSupported() ? Desktop.getDesktop() : null;
			if (desktop != null && desktop.isSupported(Desktop.Action.BROWSE)) 
		        desktop.browse(url.toURI());
			else
				JOptionPane.showConfirmDialog (null, 
						"N√£o foi poss√≠vel abrir a p√°gina de download.\nTente abri-la manualmente atrav√©s do link:\n" + link,
						title,JOptionPane.OK_CANCEL_OPTION);
	    } catch (Exception e) {
	    	e.printStackTrace();
        	JOptionPane.showConfirmDialog (null, 
					"N√£o foi poss√≠vel abrir a p√°gina de download.\nTente abri-la manualmente atrav√©s do link:\n" + link,
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
		}
		catch (IOException e) {
			e.printStackTrace();
		}
		return ip;
	}

	public static List<InetAddress> getAllLocalHostLANAddress() throws UnknownHostException {
		try {
			List<InetAddress> addressesList = new ArrayList<>();
			for (Enumeration<NetworkInterface> ifaces = NetworkInterface.getNetworkInterfaces(); ifaces.hasMoreElements();) {
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
			UnknownHostException unknownHostException = new UnknownHostException("Failed to determine LAN address: " + e);
			unknownHostException.initCause(e);
			throw unknownHostException;
		}
	}
	
	public static String[] getAllLocalIps() {
		List<String> local = new ArrayList<String>();
		try {
			List<InetAddress> addressesList = Utils.getAllLocalHostLANAddress();
			if (addressesList != null && !addressesList.isEmpty()) {
				for (InetAddress inetAddress : addressesList){
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
		}
		catch (Throwable e) {
			e.printStackTrace();
			return new String[0];
		}
	}
	
	public static String getInstallationPath(){
		String path = "";
		try {
			 path = Main.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath();
			 path = path.substring(1, path.lastIndexOf("/") + 1);
		} catch (URISyntaxException e) {
			e.printStackTrace();
		}
		return path;
	}
	
	public static String getJvmArchitecture(){
		return System.getProperty("sun.arch.data.model");
	}
	
	public static String getCLRVersion(){
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
	
	public static void addLibraryPath(String pathToAdd) throws Exception{
		final Field usrPathsField = ClassLoader.class.getDeclaredField("usr_paths");
	    usrPathsField.setAccessible(true);
	    final String[] paths = (String[])usrPathsField.get(null);
	    for(String path : paths) {
	        if(path.equals(pathToAdd)) {
	            return;
	        }
	    }
	    final String[] newPaths = Arrays.copyOf(paths, paths.length + 1);
	    newPaths[newPaths.length-1] = pathToAdd;
	    usrPathsField.set(null, newPaths);
	}
	
	public static OperationalSystem getOperationalSystem(){
		String os = System.getProperty("os.name", "generic").toLowerCase(Locale.ENGLISH);
		if ((os.indexOf("mac") >= 0) || (os.indexOf("darwin") >= 0)) 
			return OperationalSystem.MAC_OS;
		else if (os.indexOf("nux") >= 0) 
			return OperationalSystem.LINUX; 
		else
			return OperationalSystem.WINDOWS;
	}
	
	
	public static boolean isWindows64bits(){
		boolean is64bits = false;
		if (System.getProperty("os.name").contains("Windows")) {
		    is64bits = (System.getenv("ProgramFiles(x86)") != null);
		}
		else {
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
	
	public static String formatString16(String string){
    	String newString = "";
    	if (string.length() >= 16){
    		newString = string.substring(0, 16);
    	}
    	else{
    		int diferenca = 16 - string.length();
    		for (int i = 0; i < diferenca/2; i++)
    			newString = newString + " ";
    		newString = newString + string;
    		while (newString.length() < 16)
    			newString = newString + " ";
    	}
    	return newString;
    }
	
	public static String formatAcademyName(String name){
    	String newString = "";
    	String part1 = "";
    	String part2 = "";
    	if (name.length() > 16){
    		int cutIndex = 0;
    		int index = -1;
    		do {
    			index = name.indexOf(" ", index+1);
    			if (index < 16 && index != -1)
    				cutIndex = index;
    		}
    		while (index < 16 && index != -1);
    		if (cutIndex == 0){
    			part1 = name.substring(0, 15);
    			part2 = name.substring(16, name.length() > 32 ? 32 : name.length());
    			newString = part1 + formatString16(part2);
    		}
    		else{
    			part1 = name.substring(0, cutIndex);
    			part2 = name.substring(cutIndex+1, name.length() > cutIndex+16 ? cutIndex+16 : name.length());
    			newString = formatString16(part1) + formatString16(part2);
    		}
    		return newString;
    	}
    	return formatString16(name);
    }
	
	public static String formatMessage16(String message){
		String[] partes = message.split(";");
		String parte1 = partes.length > 0 ? partes[0] : "";
		String parte2 = partes.length > 1 ? partes[1] : "";
		return formatString16(parte1) + ";" + formatString16(parte2);
	}
	
	public static void correctIcons(){
		try {
			String[][] icons =
		    {
		    	{"OptionPane.informationIcon", "65587"},
		    	{"OptionPane.warningIcon", "65581"}, 
		        {"OptionPane.errorIcon", "65585"},
		        {"OptionPane.questionIcon", "65583"} 
		    };
		    Method getIconBits = Class.forName("sun.awt.shell.Win32ShellFolder2").getDeclaredMethod("getIconBits", new Class[]{long.class, int.class});
		    getIconBits.setAccessible(true);
		    double dpiScalingFactor = Toolkit.getDefaultToolkit().getScreenResolution() / 96.0;
		    int icon32Size = (dpiScalingFactor == 1) ? (32) 
		    		: ( (dpiScalingFactor == 1.25) ? (40) 
		    				: ((dpiScalingFactor == 1.5) ? (45) : ((int) (32 * dpiScalingFactor))));
		    Object[] arguments = {null, icon32Size};
		    for (String[] s : icons)
		    {
		        if (UIManager.get(s[0]) instanceof ImageIcon)
		        {
		            arguments[0] = Long.valueOf(s[1]);
		            int[] iconBits = (int[]) getIconBits.invoke(null, arguments);
		            if (iconBits != null)
		            {
		                BufferedImage img = new BufferedImage(icon32Size, icon32Size, BufferedImage.TYPE_INT_ARGB);
		                img.setRGB(0, 0, icon32Size, icon32Size, iconBits, 0, icon32Size);
		                ImageIcon newIcon = new ImageIcon(img);
		                UIManager.put(s[0], newIcon);
		            }
		        }
		    }
		    
		}
		catch (Exception e) {
		    e.printStackTrace();
		}
	}
	
	public static Date getZeroHora(Calendar calendar){
		calendar.set(Calendar.MILLISECOND, 0);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MINUTE, 0);
		calendar.set(Calendar.HOUR_OF_DAY, 0);
		return calendar.getTime();
	}
	
	public static Calendar getZeroHora(Date date){
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);
		calendar.set(Calendar.MILLISECOND, 0);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MINUTE, 0);
		calendar.set(Calendar.HOUR_OF_DAY, 0);
		return calendar;
	}
	
	public static Date zerarHora(Date date){
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
	 * @param acesso
	 * @return
	 */
	public static boolean isDiaLivre(String diasLivres) {
		if (diasLivres != null
				&& !diasLivres.isEmpty()) {
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
	 * @param acesso
	 * @return
	 */
	public static boolean isDiaPermitido(PedestrianAccessEntity acesso, Date data) {
		if (acesso.getHorariosPermitidos() == null
				|| acesso.getHorariosPermitidos().isEmpty())
			return true;
		Calendar cHoje = Calendar.getInstance();
		if(data != null)
			cHoje.setTime(data);
		Integer hoje = cHoje.get(Calendar.DAY_OF_WEEK);
		hoje--; // ajusta a numeracao dos dias para coincidir com a numeracao do pro-treino
		if (hoje == 0)
			hoje = 7;
		for (AllowedTimeEntity horario : acesso.getHorariosPermitidos()) {
			if (horario.getDiasPermitidos() != null 
					&& horario.getDiasPermitidos().contains(hoje.toString()))
				return true;
		}
		return false;
	}
	
	/**
	 * Identifica se a hora atual se encontra dentro dos limites de horario permitidos pelo acesso no dia de hoje
	 * @param acesso
	 * @return
	 */
	public static boolean isDentroDoHorario(PedestrianAccessEntity acesso, Date data) {
		if (acesso.getHorariosPermitidos() == null
				|| acesso.getHorariosPermitidos().isEmpty())
			return true;
		Calendar cHoje = Calendar.getInstance();
		if(data != null)
			cHoje.setTime(data);
		Integer hoje = cHoje.get(Calendar.DAY_OF_WEEK);
		hoje--; // ajusta a numeracao dos dias para coincidir com a numeracao do pro-treino
		if (hoje == 0)
			hoje = 7;
		for (AllowedTimeEntity horario : acesso.getHorariosPermitidos()) {
			if (horario.getDiasPermitidos() != null 
					&& horario.getDiasPermitidos().contains(hoje.toString())
					&& isDentroDoHorario(horario.getInicio(), horario.getFim(), data))
				return true;
		}
		return false;
	}
	
	/**
	 * Identifica se a hora atual se encontra dentro dos limites de horario fornecidos.
	 * Parametros nulos serao desconsiderados na comparacao
	 * @param inicio - String no formato HH:mm
	 * @param fim - String no formato HH:mm
	 * @return True se a hora atual esta dentro dos limites enviados
	 */
	public static boolean isDentroDoHorario(String inicio, String fim, Date data) {
		
		Calendar agora = Calendar.getInstance();
		if(data != null)
			agora.setTime(data);
		agora.set(Calendar.SECOND, 0);
		
		if (inicio != null) {
			String[] parts = inicio.split(":");
			Calendar horaInicio = Calendar.getInstance();
			horaInicio.set(Calendar.HOUR_OF_DAY, Integer.parseInt(parts[0]));
			horaInicio.set(Calendar.MINUTE, Integer.parseInt(parts[1]));
			horaInicio.set(Calendar.SECOND, 0);
			
			//verifica se esta dentro da tolerancia
			Long tolerancia = getPreferenceAsLong("toleranceAccess");
			if(tolerancia != null && tolerancia != 0)
				horaInicio.add(Calendar.MINUTE, tolerancia.intValue()*-1);
			
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
			if(tolerancia != null && tolerancia != 0)
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
		
		if(interval > 0 && now.isBefore(lastAccess.plusMinutes(interval))) {
			allowed = false;
		}
		
		return allowed;
	}
	
	/**
	 * CODIGO PARA LISTAR TODAS AS CONSTRAINTS DO BANCO DE DADOS
	 */
	@SuppressWarnings("unchecked")
	public static void listarConstraints(){
		Session session = HibernateUtil.getSessionFactory().getCurrentSession();
		session.beginTransaction();
		Query nativeQuery = session.createNativeQuery(
				  " SELECT "
				+ "  'ALTER TABLE '||S.SCHEMANAME||'.'||T.TABLENAME||' DROP CONSTRAINT '||C.CONSTRAINTNAME||';'"
				+ " FROM "
				+ "  SYS.SYSCONSTRAINTS C, "
				+ "  SYS.SYSSCHEMAS S,"
				+ "  SYS.SYSTABLES T"
				+ " WHERE"
				+ "  C.SCHEMAID = S.SCHEMAID"
				+ " AND"
				+ "  C.TABLEID = T.TABLEID"
				+ " AND"
				+ "  S.SCHEMANAME = 'SMARTACESSO'"
				+ " UNION"
				+ " SELECT 'DROP TABLE ' || schemaname ||'.' || tablename || ';'"
				+ " FROM SYS.SYSTABLES"
				+ " INNER JOIN SYS.SYSSCHEMAS ON SYS.SYSTABLES.SCHEMAID = SYS.SYSSCHEMAS.SCHEMAID"
				+ " where schemaname='SMARTACESSO'");
		List<String> queries = nativeQuery.getResultList();
		for (String query : queries)
			System.out.println(query);
		session.getTransaction().commit();
		session.close();
	}
	
	public static String getPublicKey(){
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
	
	public static boolean isNullOrEmpty(List<?> list){
		if (list == null || list.isEmpty())
			return true;
		return false;
	}
	
	public static boolean isNullOrEmpty(String string){
		if (string == null || string.trim().isEmpty())
			return true;
		return false;
	}
	
	
	public static boolean isNullOrZero(Number number){
		if (number == null || number.doubleValue() == 0d)
			return true;
		return false;
	}

	public static List<PreferenceTO> getDefaultPreferencesList() {
		return defaultPreferencesList;
	}
	
	public static boolean isBirthday(PedestrianAccessEntity athleteAccess, Date data){
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
			BufferedImage icon = ImageIO.read(Main.class.
					getResource(Configurations.IMAGE_FOLDER + "comuns/" + type.getSmallIconName()));
			int sizeIcon = icon.getWidth();
			Graphics2D g2 = clipedImage.createGraphics();
			g2.drawImage(clipedImage, 0, 0, sizeImage, sizeImage, null);
			if (NotificationType.BIRTHDAY.equals(type))
				g2.drawImage(icon, 0, 0, sizeIcon, sizeIcon, null);
			else
				g2.drawImage(icon, 0, (sizeImage-sizeIcon), sizeIcon, sizeIcon, null);
			g2.dispose();
			ByteArrayOutputStream bos = new ByteArrayOutputStream();
			ImageIO.write(clipedImage, "png", bos);
			return bos.toByteArray();
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		return original;
	}
	
	public static Image paintImage(BufferedImage buffMask, Color cor, int largura, int altura){
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
	        if(str.length() > lenght){
	        valor = str.substring(0,lenght);
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
	
	public static int getIntFromColor(Color cor){
	    int Red = (cor.getRed() << 16) & 0x00FF0000; //Shift red 16-bits and mask out other stuff
	    int Green = (cor.getGreen() << 8) & 0x0000FF00; //Shift Green 8-bits and mask out other stuff
	    int Blue = cor.getGreen() & 0x000000FF; //Mask out anything not blue.
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
		if(pedestreTemRegraDeAcessoPorPeriodoValido(pedestre)) {
			return;
		}
		
		if("VISITANTE".equals(pedestre.getTipo())) {
			if(pedestre.getQuantidadeCreditos() != null && pedestre.getQuantidadeCreditos() > 0) {
				pedestre.setQuantidadeCreditos(pedestre.getQuantidadeCreditos() - 1);
				if(pedestre.getQuantidadeCreditos() <= 0)
					pedestre.setCardNumber(null);
			} else {
				pedestre.setQuantidadeCreditos(null);
				pedestre.setCardNumber(null);
			}

		} else {
			if(pedestre.getQuantidadeCreditos() != null && pedestre.getQuantidadeCreditos() > 0)
				pedestre.setQuantidadeCreditos(pedestre.getQuantidadeCreditos() - 1);
		
		}
	}
	
	public static boolean pedestreTemRegraDeAcessoPorPeriodoValido(PedestrianAccessEntity pedestre) {
		if (pedestre.getPedestreRegra() == null || pedestre.getPedestreRegra().isEmpty()) {
			return false;
		}
		
		for(PedestreRegraEntity pedestreRegra : pedestre.getPedestreRegra()) {
			if(pedestreRegra.getRemovidoNoDesktop()) {
				continue;
			}
			
			if(isRegraDeAcessoValida(pedestreRegra.getDataInicioPeriodo(), pedestreRegra.getDataFimPeriodo(), pedestreRegra.getValidade())) {
				return true;
			}
		}
		
		return false;
	}
	
	private static boolean isRegraDeAcessoValida(Date dataInicioPeriodo, Date dataFimPeriodo, Date validade) {
		if(dataInicioPeriodo == null || dataFimPeriodo == null) {
			return false;
		}
		
		Date dataAtual = new Date();
		return dataInicioPeriodo.compareTo(dataAtual) >= 0 
				&& dataFimPeriodo.compareTo(dataAtual) <= 0 
				&& validade != null ? validade.compareTo(dataAtual) <= 0 : true;
	}
	
	public static void decrementaMensagens(List<PedestrianMessagesEntity> messages) {
		for(PedestrianMessagesEntity message : messages) {
			if(message.getQuantidade() > 0) {
				message.setQuantidade(message.getQuantidade() - 1);
			}
		}
	}
	
	public static void decrementaQRCodeUso(PedestrianAccessEntity pedestre) {
		
		if(pedestre.getQrCodeParaAcesso() != null 
				&& pedestre.getQrCodeParaAcesso().startsWith("U_")){
			String [] parts = pedestre.getQrCodeParaAcesso().split("_");
			Long usos = Long.valueOf(parts[parts.length-1]);
			usos++;
			
			pedestre.setQrCodeParaAcesso("U_"+EncryptionUtils.getRandomString(4)+"_"+usos);
			pedestre.setEditadoNoDesktop(true);
			pedestre.setDataAlteracao(new Date());
			
		}
		
	}
	
	public static void enviaSmsDeRegistro(PedestrianAccessEntity pedestre) {
		
		if(Main.loggedUser.getChaveIntegracaoComtele() == null
				|| Main.loggedUser.getChaveIntegracaoComtele().isEmpty())
			return;
		
		if(!pedestre.getEnviaSmsAoPassarNaCatraca()
				|| pedestre.getIdEmpresa() == null)
			return;
		
		EmpresaEntity empresa = buscaEmpresaDoPedestre(pedestre.getIdEmpresa());
		
		if(empresa == null
				|| empresa.getTelefone() == null
					|| empresa.getTelefone().isEmpty())
			return;
		
		try {
			SMSUtils sms = new SMSUtils(Main.loggedUser.getChaveIntegracaoComtele());
			String message = pedestre.getName() + " "
								+ getPreference("messageSMSAfterPassInDevice");
			
			String numeroTelefone = empresa.getCelular()
					.replace(" ", "")
					.replace("-", "")
					.replace("(", "")
					.replace(")", "");
			
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
	
	public static String verificaMensagemAvisoVencimento(PedestrianAccessEntity athleteAccess){
		try {
			
			//TODO : verifica mensagens para mostrar para o pedestre
			
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
		}
		catch (Exception e){}
		return null;
	}


	public static Date convertDataJson(JsonElement element) throws ParseException {
		
		try {
			return new Date(element.getAsLong());
		}catch (Exception e) {
			
			try {
				SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z['z']'");
				return sdf.parse(element.getAsString());
			}catch (Exception ex) {
				try {
					SimpleDateFormat sdf2 = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z['z']'");
					return sdf2.parse(element.getAsString());
				
				} catch(Exception exe) {
					SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'Z'");
					return sdf.parse(element.getAsString());
				}
			}
		}
	}
	
	public static PedestrianAccessEntity buscaPedestrePorIdOuIdTemp(Long idPedestre) {
		
		PedestrianAccessEntity pedestre = (PedestrianAccessEntity) HibernateUtil
							.getSingleResultById(PedestrianAccessEntity.class, idPedestre);
		
		if(pedestre != null && pedestre.getId() != null)
			return pedestre;
		
		pedestre = (PedestrianAccessEntity) HibernateUtil
				.getSingleResultByIdTemp(PedestrianAccessEntity.class, idPedestre);
		
		if(pedestre != null && pedestre.getId() != null)
			return pedestre;
		
		return null;
	}
	
	
	
	//colocar conversor de ABATRACK para WIGAN aqui como est·tico
	
	public static String toHEX (String Cartao) {
		
		try {
		
			long longAbatrack = Long.parseLong(Cartao);
			long fclong = Long.parseLong(Cartao);
			String hexAbatrack = Long.toHexString(longAbatrack);
			String hexWigan = hexAbatrack.substring(hexAbatrack.length()-4);
//			olhar a posiÁ„o pegar exatamente o mesmo if
			String fcWiegand = "";
			String temp = "";
			fcWiegand = hexAbatrack.substring(hexAbatrack.length()-6);
			temp += fcWiegand.charAt(0);
			temp += fcWiegand.charAt(1);
			longAbatrack  = new BigInteger(hexWigan, 16).longValue();
			fclong  = new BigInteger(temp, 16).longValue();
			String wiegand = String.valueOf(longAbatrack);
			if(wiegand.length() < 4) {
				wiegand = "00"+wiegand;
			}
			if(wiegand.length() < 5) {
				wiegand = "0"+wiegand;
			}
			
			String fcwiegand = String.valueOf(fclong);
			String fcwiegandtg = fcwiegand+wiegand;
	        return fcwiegandtg;
	       
		}catch (Exception e) {
			System.out.println("N„o foi possÌvel converter cart„o " + Cartao + "\n motivo " + e.getMessage());
			return Cartao;
		}
		
	}
	
	public static void main(String [] args) {
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'Z'");
		String aux = "1040991";
		String temp = "";
		for (int i = 0; i< aux.length(); i++) {
			if (!aux.substring(i, i + 1).equals("0")) {
				temp += aux.substring(i, aux.length());
				break;
			}
		}
		System.out.println(" o que È cart„o " + temp);
		
		String n1 = "4537";
		if(n1.matches("^(?=\\d{4}$)(?:(.)\\1*|0?1?2?3?4?5?6?7?8?9?|9?8?7?6?5?4?3?2?1?0?)$")) {
			System.out.println(true);
		}else {
			System.out.println(false);
		}
		
	}


	
	
	
	
}
