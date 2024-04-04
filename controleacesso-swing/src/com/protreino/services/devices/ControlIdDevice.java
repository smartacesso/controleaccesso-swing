package com.protreino.services.devices;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.HttpURLConnection;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.net.URL;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.imageio.ImageIO;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingWorker;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.entity.PedestrianMessagesEntity;
import com.protreino.services.constants.Tipo;
import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.enumeration.FieldType;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.enumeration.VerificationResult;
import com.protreino.services.main.Main;
import com.protreino.services.to.ConfigurationGroupTO;
import com.protreino.services.to.ConfigurationTO;
import com.protreino.services.usecase.EnviaSmsDeRegistroUseCase;
import com.protreino.services.utils.HibernateUtil;
import com.protreino.services.utils.HttpRequestParser;
import com.protreino.services.utils.Utils;

@SuppressWarnings("serial")
public class ControlIdDevice extends Device {
	
	private final EnviaSmsDeRegistroUseCase enviaSmsDeRegistroUseCase = new EnviaSmsDeRegistroUseCase();
	
	protected String serverIp;
	protected String serverPort;
	protected String session;
	protected String serverId;
	private Boolean digitalColetada;
	private Gson gson;
	protected Integer timeout = 5000;
	private HashSet<Long> usuariosComFoto = new HashSet<Long>();
	
	protected static final String CLOCKWISE = "clockwise";
	protected static final String ANTICLOCKWISE = "anticlockwise";
	
	protected String sentidoEntrada;
	
	protected String messagePersonalizedInDevice;
	
	
	protected Boolean habilitaBeep;
	
	public ControlIdDevice(DeviceEntity deviceEntity){
		this(deviceEntity.getIdentifier(), deviceEntity.getConfigurationGroupsTO());
		this.deviceEntity = deviceEntity;
		this.name = deviceEntity.getName();
		this.location = deviceEntity.getLocation();
		this.login = deviceEntity.getLogin();
		this.password = deviceEntity.getPassword();
		this.desiredStatus = deviceEntity.getDesiredStatus();
		this.defaultDevice = deviceEntity.getDefaultDevice();
		this.mirrorDevice = deviceEntity.getMirrorDevice();
		this.athleteScreenConfig = deviceEntity.getAthleteScreenConfig();
	}
	
	public ControlIdDevice(String identifier){
		this(identifier, null);
	}
	
	public ControlIdDevice(String identifier, List<ConfigurationGroupTO> configurationGroups){
		this.manufacturer = Manufacturer.CONTROL_ID;
		this.identifier = identifier;
		String[] partes = identifier.split(";");
		this.ip = partes[0];
		this.serverIp = partes[1];
		this.serverPort = partes[2];
		this.login = partes[3];
		this.password = partes[4];
		this.name = "Control Id " + ip;
		this.gson = new GsonBuilder().disableHtmlEscaping().create();
		if (configurationGroups != null)
			this.configurationGroups = configurationGroups;
		else 
			createDefaultConfiguration();
		createConfigurationMap();
	}
	
	/**
	 * Construtor usado para varredura de ip
	 * @param ip
	 * @param port
	 */
	public ControlIdDevice(Integer timeout, String identifier) {
		String[] partes = identifier.split(";");
		this.ip = partes[0];
		this.serverIp = partes[1];
		this.serverPort = partes[2];
		this.login = partes[3];
		this.password = partes[4];
		this.timeout = timeout;
		this.gson = new GsonBuilder().disableHtmlEscaping().create();
	}
	
	@Override
	public void connect(String... args) throws Exception {
		try {
			if (login == null || password == null)
				throw new Exception("Login e senha s√£o obrigat√≥rios.");
			
			doLogin();
			
			if (session == null)
				throw new Exception("N„o foi possÌvel iniciar uma sess√£o.");
			
			setStatus(DeviceStatus.CONNECTED);
			workerEnabled = true;
			watchDogEnabled = true;
			synchronizerEnabled = true;
			
			sentidoEntrada = getConfigurationValue("Sentido da entrada");
			habilitaBeep = getConfigurationValueAsBoolean("Habilita beep");
			
			worker = new SwingWorker<Void, Void>() {
				@Override
				protected Void doInBackground() throws Exception {
					ServerSocket server = new ServerSocket(Integer.valueOf(serverPort));
			        try {
			            while (workerEnabled) {
			                System.out.println("\n\r" + sdf.format(new Date()) + "  Server escutando...");
			            	Socket socket = server.accept();
			            	System.out.println("\n\r" + sdf.format(new Date()) + "  NOVO REQUEST---------------------------------------------");
			            	BufferedReader in = null;
			            	PrintWriter out = null;
			            	try {
			            		in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
			            		HttpRequestParser parser = new HttpRequestParser();
			            		parser.parseRequest(in);
			            		
			            		String requestLine = parser.getRequestLine();
			                    String body = parser.getMessageBody();
			                    String caminho = requestLine.split(" ")[1];
			            		
			                    out = new PrintWriter(socket.getOutputStream(), true);
			                    
			                    if (caminho.startsWith("/new_user_identified")
			                    		|| caminho.startsWith("/new_card")
			                    		|| caminho.startsWith("/new_user_id_and_password")) {
			                    	
			                    	// Ex: card_value=0&device_id=935449&duress=0&event=8&identifier_id=1651076864&portal_id=1&user_has_image=0&user_id=1657&user_name=
			                    	String[] partes = body.split("&");
			            			System.out.println(body);
			                    	Map<String, String> campos = new HashMap<String, String>();
			            			for (String parte : partes) {
			            				String[] subpartes = parte.split("=");
			            				String key = subpartes.length > 0 ? subpartes[0] : null;
			            				String value = subpartes.length > 1 ? subpartes[1] : null;
			            				if (key == null)
			            					continue;
			            				campos.put(key, value);
			            			}
			            			
			            			String idUsuario = campos.containsKey("user_id") ? campos.get("user_id") : "0";
			            			if ("0".equals(idUsuario)) {
			            				idUsuario = campos.containsKey("card_value") ? campos.get("card_value") : "0";
			            			}
			            			
			            			System.out.println("\n" + sdf.format(new Date()) + "  VALIDANDO CODIGO: " + idUsuario);
			            			
			            			processAccessRequest(idUsuario);
			            			
			            			System.out.println("\n" + sdf.format(new Date()) + "  RESULTADO DA VALIDACAO: " + verificationResult);
			                    	
			                    	if (athleteScreen != null)
			            				athleteScreen.requisicaoPorDigital(null, verificationResult, allowedUserName, matchedAthleteAccess);
			                    	
			                    	Integer event = null;
			                    	List<Action> actions = null;
			                    	String message = null;
			                    	
			                    	if (VerificationResult.ALLOWED.equals(verificationResult)) {
			                    		event = 7; // acesso autorizado
			                    		actions = allowAccessRequest();
			                    		message = messagePersonalizedInDevice;
			                    	
			                    	} else if (VerificationResult.TOLERANCE_PERIOD.equals(verificationResult)) {
			                    		event = 7; // acesso autorizado
			                    		actions = allowAccessRequest();
			                    		message = messagePersonalizedInDevice;
			                    	
			                    	} else if (VerificationResult.ERROR.equals(verificationResult)) {
			                    		event = 3; // par√¢metros de regra de identificaÁ„o inv√°lidos
			                    		message = verificationResult.getMessage().replace(";", " ");
			                    	
			                    	} else if (VerificationResult.NOT_ALLOWED.equals(verificationResult)
			                    			|| VerificationResult.NOT_ALLOWED_NOW.equals(verificationResult)
			                    			|| VerificationResult.NOT_ALLOWED_TODAY.equals(verificationResult)
			                    			|| VerificationResult.ALLOWED_ONLY_ONCE.equals(verificationResult)){
			                    		event = 6; // acesso negado
			                    		message = verificationResult.getMessage().replace(";", " ");
			                    	
			                    	} else if (VerificationResult.NOT_FOUND.equals(verificationResult)) {
			                    		event = 3; // N„o identificado
			                    		message = verificationResult.getMessage().replace(";", " ");
			                    	}
			                    	
			                    	System.out.println("\n" + sdf.format(new Date()) + "  MENSAGEM: " + message);
			                    	
			                    	Boolean userImage = getConfigurationValueAsBoolean("Enviar fotos") && matchedAthleteAccess.getFoto() != null;
			                    	System.out.println("\n" + sdf.format(new Date()) + "  Usuario com foto: " + userImage);
			                    	
			                    	Response response = new Response(new Result(event, Long.valueOf(idUsuario), allowedUserName, 
			                    			message, userImage, actions));
			                    	
			                    	allowedUserName = "";
			                    	String mensagemRetorno = gson.toJson(response);
			                    	
			                    	out.print("HTTP/1.1 200 OK" + "\r\n");
			                    	out.print("Content-Type: application/json; charset=utf-8" + "\r\n");
			                    	out.print("Content-Length: " + mensagemRetorno.length() + "\r\n");
			                    	out.print("Connection: close" + "\r\n");
									out.print("Server: SmartAcesso_Server" + "\r\n");
			                    	out.print("\r\n");
									out.print(mensagemRetorno + "\r\n");
									
			                    } else if (caminho.startsWith("/fingerprint_create")) {
			                    	// N„o faz nada
			                    	out.println("HTTP/1.1 200 OK" + "\r\n");
			                    
			                    } else if (caminho.startsWith("/template_create")) {
			                    	// caminho = /template_create.fcgi?card_id=&device_id=935449&finger_type=0&size0=2534&temp_num=1&user_id=0
			                    	out.println("HTTP/1.1 200 OK" + "\r\n");
			                    	template = body.getBytes();
			                    	digitalColetada = true;
			                    
			                    } else if(caminho.startsWith("/api/notification/catra_event")
			                    		|| caminho.startsWith("/api/notifications/door")) {
			                    	out.println("HTTP/1.1 200 OK\r\n");
			                    	Response notificacao = gson.fromJson(body, Response.class);
			                    	
			                    	registraGiro(notificacao);
			                    	
			                    }  else if (caminho.startsWith("/push")
			                    		|| caminho.startsWith("/master_password")
			                    		|| caminho.startsWith("/device_is_alive")
			                    		|| caminho.startsWith("/api/notifications")
			                    		|| caminho.startsWith("/api/notification")) {
			                    	// N„o faz nada
			                    	out.println("HTTP/1.1 200 OK\r\n");
			                    
			                    }else if (caminho.startsWith("/api/notifications/operation_mode")) {
			                    	out.println("HTTP/1.1 200 OK\r\n");
									Response notificacao = gson.fromJson(body, Response.class);
									if (Mode.CONTINGENCY.equals(notificacao.operation_mode.mode_name)) {
										setStatus(DeviceStatus.DISCONNECTED);
									
									} else if (Mode.DEFAULT.equals(notificacao.operation_mode.mode_name)) {
										setStatus(DeviceStatus.CONNECTED);
									
									} else {
										System.out.println("\n\n" + sdf.format(new Date()) + "  MODO DESCONHECIDO: " + notificacao.operation_mode + "\n");
									}
									
			                    } else if (caminho.startsWith("/user_get_image")) {
			                    	// caminho = /user_get_image.fcgi?session=&user_id=87848
			                    	
			                    	// busca a imagem do usuario e envia de volta
			                    	byte[] data = new byte[]{};
			                    	try {
				                    	String idUsuario = caminho.substring(caminho.indexOf("user_id=") + 8);
				                    	PedestrianAccessEntity acesso = (PedestrianAccessEntity) HibernateUtil
			            						.getSingleResultById(PedestrianAccessEntity.class, Long.valueOf(idUsuario));
				                    	if (acesso == null) {
				                    		// procura pelo cartao de acesso
				                    		acesso = (PedestrianAccessEntity) HibernateUtil
				            						.getSingleResultByCardNumber(PedestrianAccessEntity.class, Long.valueOf(idUsuario));
				                    	}
				                    	data = acesso.getFoto();
			                    	
			                    	} catch (Exception e) {
			                    		e.printStackTrace();
			                    	}
			                    	
			                    	BufferedOutputStream buffOut = new BufferedOutputStream(socket.getOutputStream());
			                    	buffOut.write("HTTP/1.1 200 OK\r\n".getBytes());
			                    	buffOut.write("Content-Type: application/octet-stream\r\n".getBytes());
			                    	buffOut.write("\r\n".getBytes());
			                    	buffOut.write(data);
			                    	buffOut.write("\r\n".getBytes());
			                    	buffOut.flush();
									
			                    } else if (caminho.startsWith("/user_set_image_list")) {
				                    
			                    } else {
			                    	System.out.println("\n\n" + sdf.format(new Date()) + "  CAMINHO: " + caminho);
			                    }
			                    out.flush();
			                
			            	} catch (Exception e){
			            		e.printStackTrace();
			            	} finally {
			                	if (in != null)
			                		in.close();
			                    if (socket != null)
			                    	socket.close();
			                    if (out != null)
			                    	out.close();
			                }
			            }
			        } finally {
			            server.close();
			        }
					return null;
				}
				
			};
			worker.execute();
			
			configure();
			
			Utils.sleep(500);
			
			watchDog = new SwingWorker<Void, Void>(){
				@Override
				protected Void doInBackground() throws Exception {
					while (watchDogEnabled) {
						try {
							if (isSessionValid(2000)) {
								setStatus(DeviceStatus.CONNECTED);
							} else {
								setStatus(DeviceStatus.DISCONNECTED);
								logout();
								doLogin();
								if (isSessionValid(2000)) {
									setStatus(DeviceStatus.CONNECTED);
									configure();
								}
							}
						} catch (Exception e) {
							setStatus(DeviceStatus.DISCONNECTED);
							if (!"connect timed out".equals(e.getMessage()))
								e.printStackTrace();
		                } finally {
							Utils.sleep(10000);
						}
					}
					return null;
				}
			};
			watchDog.execute();
			
			/*synchronizer = new SwingWorker<Void, Void>(){
				@Override
				protected Void doInBackground() throws Exception {
					while (synchronizerEnabled) {
						try {
							Utils.sleep(30000);
							sendPhotos();
						}
						catch (Exception e) {
							e.printStackTrace();
		                }
						finally {
							Utils.sleep(180000);
						}
					}
					return null;
				}
			};
			synchronizer.execute();*/
			
		} catch (SocketTimeoutException ste) {
			throw new SocketTimeoutException("N„o foi possÌvel conectar na catraca: timeout");
		} catch (Exception e){
			throw e;
		}
	}
	
	@Override
	public void disconnect(String... args) throws Exception {
		super.disconnect();
		logout();
		session = null;
		setStatus(DeviceStatus.DISCONNECTED);
	}
	
	public boolean quickConnect() throws Exception {
		doLogin(timeout);
		if (session != null) {
			logout();
			return true;
		}
		return false;
	}
	
	protected void registraGiro(Response notificacao) {
		if (GiroCatraca.EVENT_GIVE_UP.equals(notificacao.event.name))
			return;
		
		HashMap<String, Object> args = new HashMap<String, Object>();
		args.put("EQUIPAMENTO", "Control " + notificacao.device_id);
		LogPedestrianAccessEntity ultimo = (LogPedestrianAccessEntity) HibernateUtil
										.getUniqueResultWithParams(LogPedestrianAccessEntity.class,
										"LogPedestrianAccessEntity.findByEquipamentDesc", args);
		if(ultimo == null)
			return;
		
		String direction = "";

		String modoOperacao = getConfigurationValue("Modo de operaÁ„o");
		Boolean bloquearSaida = "blocked".equals(modoOperacao) ? true : false;
		
		if (GiroCatraca.EVENT_TURN_RIGHT.equals(notificacao.event.name))
			direction = ANTICLOCKWISE.equals(sentidoEntrada) ? Tipo.ENTRADA : Tipo.SAIDA;
		else if (GiroCatraca.EVENT_TURN_LEFT.equals(notificacao.event.name))
			direction = CLOCKWISE.equals(sentidoEntrada) ? Tipo.ENTRADA : Tipo.SAIDA;
		
		ultimo.setDirection(direction);
		ultimo.setStatus("ATIVO");
		ultimo.setBloquearSaida(bloquearSaida);
		
		HibernateUtil.save(LogPedestrianAccessEntity.class, ultimo);
		
		PedestrianAccessEntity pedestre = (PedestrianAccessEntity) HibernateUtil
							.getSingleResultById(PedestrianAccessEntity.class, ultimo.getIdPedestrian());
		
		if(pedestre == null)
			return;

		boolean ignoraRegras = getConfigurationValueAsBoolean("Ignorar regras de acesso");
		if(!ignoraRegras) {
			if(pedestre.getMensagens() != null && !pedestre.getMensagens().isEmpty()) {
				pedestre.decrementaMensagens();
			}
			
			if(Tipo.SAIDA.equals(direction) || !bloquearSaida) {
				pedestre.decrementaCreditos();
			}
			
			HibernateUtil.save(PedestrianAccessEntity.class, pedestre);
	
			if(Tipo.ENTRADA.equals(direction)) {
				enviaSmsDeRegistroUseCase.execute(pedestre);
			}
		}
	}
	
	@Override
	public void sendConfiguration() throws Exception {
		if (configurationGroups == null || configurationGroups.isEmpty())
			return;
		
		Configuration configuration = new Configuration(serverIp, serverPort, serverId, 
			getConfigurationValueAsBoolean("Habilita beep"), getConfigurationValue("Tempo de giro"), 
			getConfigurationValue("Tempo da requisiÁ„o"));
		Object[] retorno = send("http://" + ip + "/set_configuration.fcgi?session=" + session, configuration);
		String erro = (String) retorno[0];
		if (erro != null)
			throw new Exception(erro);
		String responseString = (String) retorno[1];
		if (responseString == null)
			throw new Exception("Resposta nula.");
		
		// Envia imagem da logo
		byte[] data = null;
		for (ConfigurationGroupTO group : configurationGroups){
			if ("Personaliza√ßao".equals(group.getName())) {
				for (ConfigurationTO config : group.getConfigurations()){
					if ("Logo".equals(config.getName())){
						if (Utils.isNullOrEmpty(config.getValue())){
							data = createImageLogoName();
						}
						else {
							data = org.apache.commons.codec.binary.Base64.decodeBase64(config.getValue());
						}
					}
					break;
				}
				break;
			}
		}
		sendLogo(data);
	}
	
	protected void procuraSeExisteMensagemParaPedestre() {
		if(matchedAthleteAccess != null && matchedAthleteAccess.getMensagens() != null
				&& !matchedAthleteAccess.getMensagens().isEmpty()) {
			for(PedestrianMessagesEntity m : matchedAthleteAccess.getMensagens()) {
				if(m.getQuantidade() <= 0)
					continue;

				messagePersonalizedInDevice = m.getMensagem();
				break;
			}
		}
	}
	
	private void definiMensagemExibidaNoDisplay() {
		procuraSeExisteMensagemParaPedestre();
		
		if (VerificationResult.ALLOWED.equals(verificationResult) 
						&& (messagePersonalizedInDevice == null 
									|| messagePersonalizedInDevice.isEmpty()))
			messagePersonalizedInDevice = verificationResult.getMessage().replace(";", " ") + " " + allowedUserName;
	}
	
	protected String decideLadoLiberarCatraca(String _sentidoEntrada) {
		String ladoLiberarCatraca = ""; // both, clockwise ou anticlockwise

		if(matchedAthleteAccess == null) {
			return "both";
		}
		
		String entrar = Utils.getPreference("messageEntryAllowed");
		String sair = Utils.getPreference("messageExitAllowed");
		
		long quantidadeAcessos = HibernateUtil.countAcessosPedestre(matchedAthleteAccess.getId());
		
		if(quantidadeAcessos == 0) {
			ladoLiberarCatraca = CLOCKWISE.equals(_sentidoEntrada) ? CLOCKWISE : ANTICLOCKWISE;
	
			if(messagePersonalizedInDevice == null || messagePersonalizedInDevice.isEmpty())
				messagePersonalizedInDevice = CLOCKWISE.equals(_sentidoEntrada) ? entrar + " ->" : "<- " + entrar;
			
		} else {
			//inverte passagem se possui catraca vinculada
			if(deviceEntity.getAttachedDevices() != null
					&& !"".equals(deviceEntity.getAttachedDevices())
					&& !"[]".equals(deviceEntity.getAttachedDevices())
					&& !"{}".equals(deviceEntity.getAttachedDevices()))
				quantidadeAcessos += 1;
			
			if(quantidadeAcessos % 2 == 0) {
				ladoLiberarCatraca = CLOCKWISE.equals(_sentidoEntrada) ? CLOCKWISE : ANTICLOCKWISE;
				
				if(messagePersonalizedInDevice == null || messagePersonalizedInDevice.isEmpty())
					messagePersonalizedInDevice = CLOCKWISE.equals(_sentidoEntrada) ? entrar + " ->" : "<- " + entrar;
					
			} else {
				ladoLiberarCatraca = CLOCKWISE.equals(_sentidoEntrada) ? ANTICLOCKWISE : CLOCKWISE;

				if(messagePersonalizedInDevice == null || messagePersonalizedInDevice.isEmpty())
					messagePersonalizedInDevice = CLOCKWISE.equals(_sentidoEntrada) ? "<- " + sair : sair + " ->";
			}
		}
		
		return ladoLiberarCatraca;
	}
	
	protected List<Action> allowAccessRequest() {
		try {
			if(habilitaBeep)
				beep();
		
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		definiMensagemExibidaNoDisplay();
		
		List<Action> actions = new ArrayList<Action>();

		String sentidoLiberacao = decideLadoLiberarCatraca(sentidoEntrada);
		
		actions.add(new Action("catra", "allow=" + sentidoLiberacao));
		
		return actions;
	}
	
	@Override
	public void allowAccess() {
		try {
			if(habilitaBeep)
				beep();
			
			String openGate = "both";
			
			if(Main.apertouF10)
				openGate = sentidoEntrada.equals(CLOCKWISE) ? ANTICLOCKWISE : CLOCKWISE;
			else if(Main.apertouF9)
				openGate = sentidoEntrada.equals(CLOCKWISE) ? CLOCKWISE : ANTICLOCKWISE;
			
			openGate(openGate);
		
		} catch (Exception e){
			e.printStackTrace();
		
		} finally{
			allowedUserName = "";
		}
	}

	
	@Override
	public void denyAccess() {
		try {
			if(habilitaBeep)
				beep();
		
		} catch (Exception e){
			e.printStackTrace();
		
		} finally{
			allowedUserName = "";
		}
	}

	
	@Override
	public void processSampleForEnrollment(Object obj) {
		// O processamento das digitais √© feito pela catraca
	}
	
	@Override
	public void processAccessRequest(Object obj) {
		try {
			Object[] retorno = HibernateUtil.processAccessRequest((String) obj, "Control " + serverId, 
								null, location, false, true, getConfigurationValueAsBoolean("Ignorar regras de acesso"));
			
			verificationResult = (VerificationResult) retorno[0];
			allowedUserName = (String) retorno[1];
			matchedAthleteAccess = (PedestrianAccessEntity) retorno[2];
		
		} catch (Exception e) {
			e.printStackTrace();
			verificationResult = VerificationResult.ERROR;
		}
	}
	
	private void doLogin(Integer... timeout) throws Exception {
		Object[] retorno = send("http://" + ip + "/login.fcgi", new Login(login, password), timeout);
		String erro = (String) retorno[0];
		String responseString = (String) retorno[1];
		if (erro != null) {
			if ("connect timed out".equals(erro))
				throw new SocketTimeoutException();
			if (erro.contains("Service Not Available"))
				throw new Exception("Catraca N„o responde. Verifique as conex√µes.");
			Response response = gson.fromJson(erro, Response.class);
			if (response.code != null && response.error != null) {
				if (response.code == 1)
					erro = "Login ou senha inv√°lidos";
				else
					erro = response.error;
			}
			else
				erro = "N„o foi possÌvel conectar";
			throw new Exception(erro);
		}
		if (responseString == null)
			throw new Exception("Sess√£o nula retornada.");
		Response response = gson.fromJson(responseString, Response.class);
		session = response.session;
		System.out.println(sdf.format(new Date()) + "  Sessao: " + response.session);
		
		//getInfo
	}
	
	private Boolean isSessionValid(int timeout) throws Exception {
		Object[] retorno = send("http://" + ip + "/session_is_valid.fcgi?session=" + session, null, timeout);
		String erro = (String) retorno[0];
		if (erro != null) {
			if (erro.contains("Service Not Available"))
				return false;
			throw new Exception(erro);
		}
		String responseString = (String) retorno[1];
		if (responseString == null)
			throw new Exception("Resposta nula.");
		Response response = gson.fromJson(responseString, Response.class);
		return response.session_is_valid;
	}
	
	
	private Response loadObjects(String object, WhereClause whereClause) throws Exception {
		Object[] retorno = send("http://" + ip + "/load_objects.fcgi?session=" + session, 
				new LoadObject(object, whereClause));
		String erro = (String) retorno[0];
		if (erro != null)
			throw new Exception(erro);
		String responseString = (String) retorno[1];
		if (responseString == null)
			throw new Exception("Resposta nula.");
		Response response = gson.fromJson(responseString, Response.class);
		return response;
	}
	
	
	private List<Integer> createObjects(String object, List<Object> values) throws Exception {
		Object[] retorno = send("http://" + ip + "/create_objects.fcgi?session=" + session, new CreateObject(object, values));
		String erro = (String) retorno[0];
		if (erro != null)
			throw new Exception(erro);
		String responseString = (String) retorno[1];
		if (responseString == null)
			throw new Exception("Resposta nula.");
		Response response = gson.fromJson(responseString, Response.class);
		return response.ids;
	}
	
	
	private Integer modifyObjects(String object, Object values, WhereClause whereClause) throws Exception {
		Object[] retorno = send("http://" + ip + "/modify_objects.fcgi?session=" + session, new ModifyObject(object, values, whereClause));
		String erro = (String) retorno[0];
		if (erro != null)
			throw new Exception(erro);
		String responseString = (String) retorno[1];
		if (responseString == null)
			throw new Exception("Resposta nula.");
		Response response = gson.fromJson(responseString, Response.class);
		return response.changes;
	}
	
	
	private Integer removeObjects(String object, WhereClause whereClause) throws Exception {
		Object[] retorno = send("http://" + ip + "/destroy_objects.fcgi?session=" + session, new RemoveObject(object, whereClause));
		String erro = (String) retorno[0];
		if (erro != null)
			throw new Exception(erro);
		String responseString = (String) retorno[1];
		if (responseString == null)
			throw new Exception("Resposta nula.");
		Response response = gson.fromJson(responseString, Response.class);
		return response.changes;
	}
	
	
	@SuppressWarnings("unchecked")
	public void sendPhotos() {
		System.out.println("\n" + sdf.format(new Date()) + "  ... Enviando fotos para a catraca...");
    	
		// procura na base local por usuarios com foto
		List<PedestrianAccessEntity> listaAcesso = (List<PedestrianAccessEntity>) HibernateUtil
				.getResultList(PedestrianAccessEntity.class, "PedestrianAccessEntity.findAllWithPhoto");
		if (listaAcesso != null && !listaAcesso.isEmpty()) {
			System.out.println("\n" + sdf.format(new Date()) + "  ... Total de fotos para enviar: " + listaAcesso.size());
			// percorre a lista de usuarios e cria pacotes de 50 fotos para enviar pra catraca por vez
			int contador = 0;
			List<UserImage> listaUsers = new ArrayList<UserImage>();
			List<Long> usuariosComFoto = new ArrayList<Long>();
			for (PedestrianAccessEntity acesso : listaAcesso) {
				String imageBase64 = Base64.getEncoder().encodeToString(acesso.getFoto());
				UserImage userImage = new UserImage(acesso.getId().intValue(), imageBase64);
				listaUsers.add(userImage);
				usuariosComFoto.add(acesso.getId());
				contador++;
				if (contador == 50) {
			    	// envia o pacote com 50 fotos pra catraca
					if (enviaListaFotos(listaUsers))
						this.usuariosComFoto.addAll(usuariosComFoto); // adiciona na lista os enviados com sucesso
					contador = 0;
					listaUsers = new ArrayList<UserImage>();
					usuariosComFoto = new ArrayList<Long>();
				}
			}
			// envia o restante das fotos
			if (!listaUsers.isEmpty()) {
		    	if (enviaListaFotos(listaUsers))
					this.usuariosComFoto.addAll(usuariosComFoto); // adiciona na lista os enviados com sucesso
			}
			System.out.println("\n" + sdf.format(new Date()) + "  ... Envio de fotos concluido!");
		}
	}
	
	private boolean enviaListaFotos(List<UserImage> listaUsers) {
		try {
			Object[] retorno = send("http://" + ip + "/user_set_image_list.fcgi?session=" + session, new SetImageList(listaUsers));
			String erro = (String) retorno[0];
			if (erro != null) {
				System.out.println(sdf.format(new Date()) + "  Erro ao enviar fotos: " + erro);
				return false;
			}
			return true;
		}
		catch (Exception e) {
			System.out.println(sdf.format(new Date()) + "  Erro ao enviar fotos: " + e.getMessage());
			e.printStackTrace();
		}
		return false;
	}
	
	public void resetToFactory() throws Exception {
		// limpa, conecta novamente e configura a catraca escrava
		Object[] retorno = send("http://" + ip + "/reset_to_factory_default.fcgi?session=" + session, new Request(true));
		String erro = (String) retorno[0];
		if (erro != null)
			throw new Exception(erro);
		String responseString = (String) retorno[1];
		if (responseString == null)
			throw new Exception("Falha ao resetar a catraca.");
		
		Utils.sleep(10000); // espera um tempo para o pingador enxergar a desconexao
		
		while (!isConnected()) { // espera o pingador fazer o servico de reconectar e configurar
			Utils.sleep(500);
		}
		
		Utils.sleep(8000); // espera a catraca terminar de iniciar
	}
	
	
	// Configura o servidor no equipamento, cria a lista de configuracoes, 
	// chama o metodo para enviar as configuracoes e o logo
	public void configure() throws Exception{
		verificaDeviceServidor();
		sendConfiguration();
	}
	
	
	private void verificaDeviceServidor() throws Exception{
		// Procura pelo servidor nos devices cadastrados na catraca. 
		// Procura primeiro pelo IP
		serverId = null;
		Device device = null;
		WhereClause whereClause = new WhereClause(new Device(null, serverIp + ":" + serverPort));
		Response response = loadObjects("devices", whereClause);
		List<Device> devicesCadastrados = response.devices;
		if (devicesCadastrados != null && !devicesCadastrados.isEmpty()) {
			device = devicesCadastrados.get(0);
			serverId = device.id.toString();
		}
		
		// Caso nao encontre pelo IP, caso tenha trocado de IP, ent√£o procura pelo nome ServidorSmartAcesso
		if (serverId == null) {
			whereClause = new WhereClause(new Device("ServidorSmartAcesso", null));
			response = loadObjects("devices", whereClause);
			devicesCadastrados = response.devices;
			if (devicesCadastrados != null && !devicesCadastrados.isEmpty()) {
				// Se encontrou, entao atualiza o IP do servidor
				device = devicesCadastrados.get(0);
				serverId = device.id.toString();
			}
		}
		
		if (serverId != null) {
			whereClause = new WhereClause(new Device(device.id));
			device.ip = serverIp + ":" + serverPort;
			device.name = "ServidorSmartAcesso";
			device.public_key = Utils.getPublicKey();
			Integer changes = modifyObjects("devices", device, whereClause);
			if (changes == null || changes < 1)
				throw new Exception("N„o foi possÌvel atualizar os dados do servidor.");
			return;
		}
		
		// Se nao encontrar nada, entao cadastra um novo device para o servidor
		List<Object> values = new ArrayList<Object>();
		values.add(new Device(-1, "ServidorSmartAcesso", serverIp + ":" + serverPort, Utils.getPublicKey()));
		List<Integer> idsCriados = createObjects("devices", values);
		if (idsCriados == null || idsCriados.isEmpty())
			throw new Exception("N„o foi possÌvel criar o servidor.");
		serverId = idsCriados.get(0).toString();
		
		System.out.println(sdf.format(new Date()) + "  Servidor criado: " + serverId);
	}
	
	
	@Override
	public void createDefaultConfiguration(){
		List<ConfigurationTO> geralConfigurations = new ArrayList<ConfigurationTO>();
		geralConfigurations.add(new ConfigurationTO("Habilita beep", "true", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO("Sentido da entrada", "Hor√°rio_clockwise", FieldType.COMBOBOX,
				"Hor√°rio_clockwise;Anti-hor√°rio_anticlockwise"));
		geralConfigurations.add(new ConfigurationTO("Modo de opera√ß√£o", "Ambos bloqueados_blocked", FieldType.COMBOBOX,
				"Entrada liberada_entrance_open;SaÌda liberada_exit_open;Ambos bloqueados_blocked;Ambos liberados_both_open"));
		geralConfigurations.add(new ConfigurationTO("Tempo de giro", "5000", FieldType.NUMERIC_LIST, "3000;1000;10000")); // inicio;passo;fim
		geralConfigurations.add(new ConfigurationTO("Tempo da requisi√ß√£o", "10000", FieldType.NUMERIC_LIST, "2000;1000;20000")); // inicio;passo;fim
		geralConfigurations.add(new ConfigurationTO("Enviar fotos", "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO("Ignorar regras de acesso", "false", FieldType.CHECKBOX));
		
		
		List<ConfigurationTO> customConfigurations = new ArrayList<ConfigurationTO>();
		customConfigurations.add(new ConfigurationTO("Logo", FieldType.IMAGE, false));
		
		configurationGroups = new ArrayList<ConfigurationGroupTO>();
		configurationGroups.add(new ConfigurationGroupTO("Geral", geralConfigurations));
		configurationGroups.add(new ConfigurationGroupTO("Personaliza√ß√£o", customConfigurations));
	}
	
	protected void logout() {
		try {
			Object[] retorno = send("http://" + ip + "/logout.fcgi?session=" + session, null);
			String erro = (String) retorno[0];
			if (erro != null)
				throw new Exception(erro);
			String responseString = (String) retorno[1];
			if (responseString == null)
				throw new Exception("Resposta nula.");
		}
		catch (Exception e) {
			System.out.println(sdf.format(new Date()) + "  FALHA DURANTE LOGOUT: " + e.getMessage());
			e.printStackTrace();
		}
	}
	
	
	protected void beep() throws Exception{
		Object[] retorno = send("http://" + ip + "/buzzer_buzz.fcgi?session=" + session, new Beep(50, 4000, 250));
		String erro = (String) retorno[0];
		if (erro != null)
			throw new Exception(erro);
	}
	
	
	protected void openGate(String side) throws Exception {
		// side pode ser "clockwise", "anticlockwise" ou "both"
		List<Action> actions = new ArrayList<Action>();
		actions.add(new Action("catra", "allow=" + side));
		Object[] retorno = send("http://" + ip + "/execute_actions.fcgi?session=" + session, new Request(actions));
		String erro = (String) retorno[0];
		if (erro != null)
			throw new Exception(erro);
	}
	
	protected Object[] send(String endereco, Object object, Integer... timeout){
		try {
			URL url = new URL(endereco);
			HttpURLConnection conn = (HttpURLConnection) url.openConnection();
			conn.setConnectTimeout(timeout != null && timeout.length > 0 ? timeout[0] : this.timeout);
			conn.setReadTimeout(timeout != null && timeout.length > 0 ? timeout[0] : this.timeout);
			conn.setRequestMethod("POST");
			conn.setRequestProperty("Content-type", "application/json");
			conn.setDoInput(true);
			conn.setDoOutput(true);
			
			byte[] data = null;
			if (object != null) {
				System.out.println(gson.toJson(object));
				data = (gson.toJson(object)).getBytes();
			}else 
				data = ("{}").getBytes();
			
			conn.setRequestProperty("Content-Length", String.valueOf(data.length));
			OutputStream os = conn.getOutputStream();
			os.write(data);
			
			if (conn.getResponseCode() != 200) {
			BufferedReader br = new BufferedReader(new InputStreamReader(conn.getErrorStream()));
				String output, result = "";
				while ((output = br.readLine()) != null) {
					result += output;
				}
				System.out.println(sdf.format(new Date()) + "  Result error: " + result);
				return new Object[] { result, null };
			}

			BufferedReader br = new BufferedReader(new InputStreamReader(conn.getInputStream()));
			String output, result = "";
			while ((output = br.readLine()) != null) {
				result += output;
			}
			conn.disconnect();
			return new Object[] { null, result };
		}
		catch (Exception e) {
			if (!(e instanceof SocketTimeoutException))
				e.printStackTrace();
			return new Object[] { e.getMessage(), null };
		}
	}

	
	private byte[] createImageLogoName() throws IOException{
		
		int width = 300;
		int height = 300;
		
		JFrame frame = new JFrame();
		
		JPanel pane = new JPanel();
		pane.setLayout(new BoxLayout(pane, BoxLayout.Y_AXIS));
		pane.setPreferredSize(new Dimension(width, height));
		pane.setSize(pane.getPreferredSize());
		pane.setOpaque(false);
		pane.add(Box.createGlue());
		
		String nomeAcademia = "ACESSO SMARTACESSO";
    	if (Main.loggedUser != null)
    		nomeAcademia = Utils.formatAcademyName(Main.loggedUser.getName());
    	
    	Font font = new JLabel().getFont();
		Font bigFont = new Font(font.getFontName(), Font.BOLD, 28);
		
		nomeAcademia = nomeAcademia.toUpperCase();
    	if (nomeAcademia.length() > 16) {
    		JLabel label = new JLabel(nomeAcademia.substring(0, 16));
    		label.setAlignmentX(Component.CENTER_ALIGNMENT);
    		label.setForeground(Color.white);
    		label.setFont(bigFont);
    		pane.add(label, BorderLayout.CENTER);
    		pane.add(Box.createVerticalStrut(20));
    		JLabel label2 = new JLabel(nomeAcademia.substring(16, 32));
    		label2.setAlignmentX(Component.CENTER_ALIGNMENT);
    		label2.setForeground(Color.white);
    		label2.setFont(bigFont);
    		pane.add(label2, BorderLayout.CENTER);
    	}
    	else {
    		JLabel label = new JLabel(nomeAcademia);
    		label.setAlignmentX(Component.CENTER_ALIGNMENT);
    		label.setForeground(Color.white);
    		label.setFont(bigFont);
    		pane.add(label, BorderLayout.CENTER);
    	}
    	pane.add(Box.createGlue());
    	
    	frame.setContentPane(pane);
        frame.pack();
    	
		BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
		Graphics g = image.createGraphics();
		pane.paint(g);
		
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		ImageIO.write( image, "png", baos );
		baos.flush();
		byte[] imageInByte = baos.toByteArray();
		baos.close();
		
		return imageInByte;
	}
	
	
	private Object[] sendLogo(byte[] data){
		try {
			URL url = new URL("http://" + ip + "/logo_change.fcgi?session=" + session);
			HttpURLConnection conn = (HttpURLConnection) url.openConnection();
			conn.setRequestMethod("POST");
			conn.setRequestProperty("Content-type", "application/octet-stream");
			conn.setRequestProperty("Content-Length", String.valueOf(data.length));
			conn.setDoInput(true);
			conn.setDoOutput(true);
			OutputStream os = conn.getOutputStream();
			os.write(data);
			
			if (conn.getResponseCode() != 200) {
			BufferedReader br = new BufferedReader(new InputStreamReader(conn.getErrorStream()));
				String output, result = "";
				while ((output = br.readLine()) != null) {
					result += output;
				}
				System.out.println(sdf.format(new Date()) + "  Result error: " + result);
				return new Object[] { result, null };
			}

			BufferedReader br = new BufferedReader(new InputStreamReader(conn.getInputStream()));
			String output, result = "";
			while ((output = br.readLine()) != null) {
				result += output;
			}
			conn.disconnect();
			return new Object[] { null, result };
		}
		catch (Exception e) {
			e.printStackTrace();
			return new Object[] { e.getMessage(), null };
		}
	}
	
	
	@Override
	public Set<Integer> getRegisteredUserList() throws Exception{
		/*Set<Integer> retorno = new HashSet<Integer>();
		Response response = loadObjects("users", null);
		List<User> listaCatraca = response.users;
		if (listaCatraca != null && !listaCatraca.isEmpty()) {
			for (User user : listaCatraca)
				retorno.add(user.id);
		}
		return retorno;*/
		return null;
	}
	
	
	@Override
	public String cadastrateUser(PedestrianAccessEntity athleteAccessEntity){
		digitalColetada = false;
		Boolean usuarioCriadoNestaCatraca = false; // usado para o caso de criar o usuario e nao conseguir criar o template. Nesse caso, apaga o usuario criado
		try {
			final Integer idUsuario = athleteAccessEntity.getId().intValue();
			System.out.println("\n\r" + sdf.format(new Date()) + "  ------ CADASTRO DE USUARIO - Criando usuario com id " + idUsuario + "...");
			
			// Dispara a solicitacao de coleta da digital
			Object[] retorno = send("http://" + ip + "/remote_enroll.fcgi?session=" + session, new RemoteEnroll());
			String erro = (String) retorno[0];
			System.out.println("\n\r" + sdf.format(new Date()) + "  ------ CADASTRO DE USUARIO - Erro ao solicitar digital: " + erro);
			if (erro != null) {
				throw new Exception(erro);
			}
			System.out.println("\n\r" + sdf.format(new Date()) + "  ------ CADASTRO DE USUARIO - Coleta de digital solicitada! Aguardando recebimento da digital...");
			
			// verifica se a digital foi coletada
			Long inicio = System.currentTimeMillis();
			while (!digitalColetada && (System.currentTimeMillis() - inicio < 60000)) {
				Utils.sleep(50);
			}
			
			if (!digitalColetada) {
				System.out.println("\n\r" + sdf.format(new Date()) + "  ------ CADASTRO DE USUARIO - Sistema nao recebeu a digital: " + erro);
				throw new Exception("Excedido tempo limite.");
			}
			System.out.println("\n\r" + sdf.format(new Date()) + "  ------ CADASTRO DE USUARIO - Digital coletada e recebida! Criando usuario...");
			
			// cria o hash a partir da senha
			String[] hash = getHash(idUsuario.toString());
			
			// cria o usuario
			List<Object> values = new ArrayList<Object>();
			values.add(new User(idUsuario, athleteAccessEntity.getName(), hash[0], hash[1]));
			List<Integer> idsCriados = createObjects("users", values);
			if (idsCriados == null || idsCriados.isEmpty()) {
				System.out.println("\n\r" + sdf.format(new Date()) + "  ------ CADASTRO DE USUARIO - N„o foi possÌvel criar o usuario: " + erro);
				throw new Exception("N„o foi possÌvel criar o usu·rio.");
			}
			usuarioCriadoNestaCatraca = true;
			System.out.println("\n\r" + sdf.format(new Date()) + "  ------ CADASTRO DE USUARIO - Usuario criado! Criando template...");
			
			// cria o template
			values = new ArrayList<Object>();
			values.add(new Template(idUsuario, Base64.getEncoder().encodeToString(template)));
			idsCriados = createObjects("templates", values);
			if (idsCriados == null || idsCriados.isEmpty()) {
				System.out.println("\n\r" + sdf.format(new Date()) + "  ------ CADASTRO DE USUARIO - N„o foi possÌvel criar o template: " + erro);
				excluirUsuario(new PedestrianAccessEntity(athleteAccessEntity.getId()), false);
				usuarioCriadoNestaCatraca = false;
				throw new Exception("N„o foi possÌvel criar o template.");
			}
			System.out.println("\n\r" + sdf.format(new Date()) + "  ------ CADASTRO DE USUARIO - Template criado! Replicando para outras catracas...");
			
			// Verifica se ser√£o necess√°rio criar tambem em outras catracas ControlId
			for (com.protreino.services.devices.Device device : Main.devicesList) {
				if (!device.isTheSame(this) 
						&& manufacturer.equals(device.getManufacturer())){
					
					System.out.println("\n\r" + sdf.format(new Date()) + "  ------ CADASTRO DE USUARIO - " + device.getName() 
						+ " - Status: " + device.isConnected() + " - Espelhar? " + device.isMirrorDevice());
					
					if (device.isConnected() 
							&& device.isMirrorDevice()) {
						ControlIdDevice otherDevice = (ControlIdDevice) device;
						
						Boolean usuarioEspelhadoNestaCatraca = false; // usado para o caso de criar o usuario e nao conseguir criar o template. Nesse caso, apaga o usuario criado
						try {
							// cria o usuario
							values = new ArrayList<Object>();
							values.add(new User(idUsuario, athleteAccessEntity.getName(), hash[0], hash[1]));
							idsCriados = otherDevice.createObjects("users", values);
							if (idsCriados == null || idsCriados.isEmpty()) {
								System.out.println("\n\r" + sdf.format(new Date()) + "  ------ CADASTRO DE USUARIO - N„o foi possÌvel espelhar o usuario em " + device.getName());
								throw new Exception("Erro ao espelhar usu·rio na catraca " + otherDevice.getName());
							}
							usuarioEspelhadoNestaCatraca = true;
							System.out.println("\n\r" + sdf.format(new Date()) + "  ------ CADASTRO DE USUARIO - Usuario replicada! Replicando template...");
							
							// cria o template
							values = new ArrayList<Object>();
							values.add(new Template(idUsuario, Base64.getEncoder().encodeToString(template)));
							idsCriados = otherDevice.createObjects("templates", values);
							if (idsCriados == null || idsCriados.isEmpty()) {
								// Nao foi possivel criar o template, entao apaga o usu·rio criado anteriormente
								System.out.println("\n\r" + sdf.format(new Date()) + "  ------ CADASTRO DE USUARIO - N„o foi possÌvel espelhar o template em " + device.getName());
								otherDevice.excluirUsuario(new PedestrianAccessEntity(athleteAccessEntity.getId()), false);
								usuarioEspelhadoNestaCatraca = false;
								throw new Exception("Erro ao espelhar template na catraca " + otherDevice.getName());
							}
							System.out.println("\n\r" + sdf.format(new Date()) + "  ------ CADASTRO DE USUARIO - Template replicado! Replicando para outras catracas...");
							
						}
						catch (Exception e2) {
							System.out.println("\n\r" + sdf.format(new Date()) + "  ------ CADASTRO DE USUARIO - Excecao durante o espelhamento em " 
									+ device.getName() + ": " + e2.getMessage());
							if (usuarioEspelhadoNestaCatraca)
								otherDevice.excluirUsuario(new PedestrianAccessEntity(athleteAccessEntity.getId()), false);
							e2.printStackTrace();
						}
					}
				}
			}
			
		}
		catch(Exception e) {
			if (usuarioCriadoNestaCatraca)
				excluirUsuario(new PedestrianAccessEntity(athleteAccessEntity.getId()), false);
			e.printStackTrace();
			return e.getMessage();
		}
		return "";
	}
	
	
	@Override
	public String removeUser(PedestrianAccessEntity athleteAccessEntity){
		return excluirUsuario(athleteAccessEntity, true);
	}
	
	
	private String[] getHash(String password) throws Exception{
		Object[] retorno = send("http://" + ip + "/user_hash_password.fcgi?session=" + session, new Request(password));
		String erro = (String) retorno[0];
		if (erro != null) {
			throw new Exception(erro);
		}
		String responseString = (String) retorno[1];
		if (responseString == null)
			throw new Exception("Hash nulo retornado.");
		Response response = gson.fromJson(responseString, Response.class);
		return new String[] { response.password, response.salt };
	}
	
	
	public int cadastrarUsuario(User user) throws Exception {
		List<Object> values = new ArrayList<Object>();
		values.add(user);
		List<Integer> idsCriados = createObjects("users", values);
		if (idsCriados == null || idsCriados.isEmpty())
			throw new Exception("N„o foi possÌvel criar o usu·rio.");
		return idsCriados.get(0);
	}
	
	
	public int cadastrarTemplate(Template template) throws Exception {
		List<Object> values = new ArrayList<Object>();
		values.add(template);
		List<Integer> idsCriados = createObjects("templates", values);
		if (idsCriados == null || idsCriados.isEmpty()) {
			throw new Exception("N„o foi possÌvel criar o template.");
		}
		return idsCriados.get(0);
	}
	
	
	public String excluirUsuario(PedestrianAccessEntity acesso, Boolean sincronizarExclusao){
		try {
			// apaga o usu·rio na catraca
			WhereClause whereClause = new WhereClause(new User(acesso.getId().intValue()));
			Integer retorno = removeObjects("users", whereClause);
			if (retorno == null)
				throw new Exception("N„o foi possÌvel remover o usu·rio.");
			
			// Verifica se ser√£o necess√°rio apagar tambem em outras catracas ControlId
			if (sincronizarExclusao) {
				for (com.protreino.services.devices.Device device : Main.devicesList) {
					if (!device.isTheSame(this) 
							&& manufacturer.equals(device.getManufacturer())
							&& device.isConnected()
							&& device.isMirrorDevice()) {
						ControlIdDevice otherDevice = (ControlIdDevice) device;
						
						// apaga o usu·rio na catraca
						retorno = otherDevice.removeObjects("users", whereClause);
						if (retorno == null)
							throw new Exception("Erro ao espelhar exclus√£o na catraca " + otherDevice.getName());
					}
				}
			}
			
		}
		catch(Exception e) {
			e.printStackTrace();
			return e.getMessage();
		}
		return "";
	}
	
	
	private String createSha1(byte[] bytes) {
		try {
			if (bytes != null) {
			    MessageDigest digest = MessageDigest.getInstance("SHA-1");
		        byte[] result = digest.digest(bytes);
		        StringBuffer sb = new StringBuffer();
		        for (int i = 0; i < result.length; i++) {
		            sb.append(Integer.toString((result[i] & 0xff) + 0x100, 16).substring(1));
		        }
		        return sb.toString();
			}
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}
	
	
	@SuppressWarnings("unused")
	protected class Login{
		private String login;
		private String password;
		
		public Login(String login, String password){
			this.login = login;
			this.password = password;
		}
	}
	
	
	protected class Response{
		public String session;
		public Boolean session_is_valid;
		public List<Device> devices;
		public List<Integer> ids;
		public List<User> users;
		public Integer changes;
		public Result result;
		public OperationMode operation_mode;
		public List<Template> templates;
		public String error;
		public Integer code;
		public Boolean success;
		public String password;
		public String salt;

		public Event event;
		public Long device_id;
		
		public Door door;
		
		public Response(Result result){
			this.result = result;
		}
		
	}
	
	protected class Door{
		public Integer id;
		public Boolean open; 
	}
	
	protected class Event {
		public Integer type;
		public GiroCatraca name;
		public Long time;
	}
	
	protected enum GiroCatraca {
		EVENT_TURN_LEFT,
		EVENT_TURN_RIGHT,
		EVENT_GIVE_UP;
	}
	
	@SuppressWarnings("unused")
	protected class LoadObject {
		private String object;
		private WhereClause where;
		
		public LoadObject(String object, WhereClause where){
			this.object = object;
			this.where = where;
		}
	}
	

	@SuppressWarnings("unused")
	protected class CreateObject {
		private String object;
		private List<Object> values;
		
		public CreateObject(String object, List<Object> values){
			this.object = object;
			this.values = values;
		}
	}
	

	@SuppressWarnings("unused")
	protected class ModifyObject {
		private String object;
		private Object values;
		private WhereClause where;
		
		public ModifyObject(String object, Object values, WhereClause where){
			this.object = object;
			this.values = values;
			this.where = where;
		}
	}
	

	@SuppressWarnings("unused")
	protected class RemoveObject {
		private String object;
		private WhereClause where;
		
		public RemoveObject(String object, WhereClause where){
			this.object = object;
			this.where = where;
		}
	}
	
	
	@SuppressWarnings("unused")
	protected class SetImageList {
		private List<UserImage> user_images;
		
		public SetImageList(List<UserImage> user_images){
			this.user_images = user_images;
		}
	}
	
	
	@SuppressWarnings("unused")
	protected class UserImage {
		private Integer user_id;
		private String image;
		
		public UserImage(Integer user_id, String image){
			this.user_id = user_id;
			this.image = image;
		}
	}
	

	@SuppressWarnings("unused")
	protected class WhereClause {
		private Device devices;
		private User users;
		private Template templates;
		
		public WhereClause(Device device){
			this.devices = device;
		}
		
		public WhereClause(User user){
			this.users = user;
		}
		
	}
	

	@SuppressWarnings("unused")
	protected class Device{
		private Integer id;
		private String name;
		private String ip;
		private String public_key;
		
		public Device(Integer id, String name, String ip, String public_key) {
			this.id = id;
			this.name = name;
			this.ip = ip;
			this.public_key = public_key;
		}
		
		public Device(Integer id) {
			this.id = id;
		}
		
		public Device(String name, String ip) {
			this.name = name;
			this.ip = ip;
		}
	}
	

	@SuppressWarnings("unused")
	protected class User{
		private Integer id;
		private String registration;
		private String name;
		private String password;
		private String salt;
		private Integer begin_time; // opcional, Inteiro representando a partir de que data e hora (unix timestamp) o usu·rio √© v√°lido.
		private Integer end_time; // opcional, Inteiro rperesentando at√© que data e hora (unix timestamp) o usu·rio √© v√°lido.
		
		public User(Integer id, String name, String password, String salt) {
			this.id = id;
			this.name = name;
			this.registration = id.toString();
			this.password = password;
			this.salt = salt;
		}
		
		public User(Integer id) {
			this.id = id;
		}
	}
	

	@SuppressWarnings("unused")
	protected class Configuration {
		private GeneralConfiguration general;
		private MonitorConfiguration monitor;
		private PushConfiguration push_server;
		private OnlineClient online_client;
		private UHFConfiguration uhf;
		
		protected Configuration(String hostname, String port, String serverId,
				Boolean beepEnabled, String tempoGiro, String requestTimeout) {
			this.general = new GeneralConfiguration(beepEnabled, tempoGiro);
			this.monitor = new MonitorConfiguration(hostname, port, requestTimeout);
			//this.push_server = new PushConfiguration("");
			this.online_client = new OnlineClient(serverId);
		}
		
		
		protected Configuration(String hostname, String port, String serverId,
				Boolean beepEnabled, String tempoGiro, String requestTimeout,
				Integer identification_bits, String reader_type, Integer read_interval,
				Integer read_interval_diff_tags, Integer transmit_power, String work_channel, String operation_mode,
				Integer trigger_timeout, Integer trig_idle) {
			this.general = new GeneralConfiguration(beepEnabled, tempoGiro);
			this.monitor = new MonitorConfiguration(hostname, port, requestTimeout);
			//this.push_server = new PushConfiguration("");
			this.online_client = new OnlineClient(serverId);
			this.uhf = new UHFConfiguration(identification_bits, reader_type, read_interval,
					read_interval_diff_tags, transmit_power, work_channel, 
					operation_mode, trigger_timeout, trig_idle);
		}
		
		
		protected class UHFConfiguration{
			private String identification_bits;
			private String reader_type;
			private String read_interval;
			private String read_interval_diff_tags;
			private String transmit_power;
			private String work_channel;
			private String operation_mode;
			private String trigger_timeout;
			private String trig_idle;
			
			public UHFConfiguration(Integer identification_bits, String reader_type, Integer read_interval,
					Integer read_interval_diff_tags, Integer transmit_power, String work_channel, String operation_mode,
					Integer trigger_timeout, Integer trig_idle) {
				super();
				this.identification_bits = identification_bits.toString();
				this.reader_type = reader_type;
				this.read_interval = read_interval.toString();
				this.read_interval_diff_tags = read_interval_diff_tags.toString();
				this.transmit_power = transmit_power.toString();
				this.work_channel = work_channel.toString();
				this.operation_mode = operation_mode.toString();
				this.trigger_timeout = trigger_timeout.toString();
				this.trig_idle = trig_idle.toString();
			}
			
		}

		
		protected class GeneralConfiguration {
			private String beep_enabled = "1";
			private String catra_timeout = "5000"; // milisegundos
			private String local_identification = "1";
			private String online = "1";
			
			public GeneralConfiguration(Boolean beepEnabled, String tempoGiro) {
				this.beep_enabled = beepEnabled ? "1" : "0";
				this.catra_timeout = tempoGiro;
			}
		}
		
		protected class MonitorConfiguration {
			private String request_timeout = "10000"; // milisegundos
			private String hostname;
			private String port;
			
			public MonitorConfiguration(String hostname, String port, String requestTimeout){
				this.request_timeout = requestTimeout;
				this.hostname = hostname;
				this.port = port;
			}
		}
		
		protected class PushConfiguration {
			private String push_request_timeout = "30"; // segundos ?
			private String push_request_period = "20"; // segundos
			private String push_remote_address;
			
			public PushConfiguration(String address){
				this.push_remote_address = address;
			}
		}
		
		protected class OnlineClient {
			private String server_id;
			private String extract_template = "1";
			
			public OnlineClient(String serverId){
				this.server_id = serverId;
			}
		}
		
	}


	@SuppressWarnings("unused")
	protected class Result {
		private Integer event;
		private Long user_id;
		private String user_name;
		private Integer portal_id = 1;
		private String message;
		private Boolean user_image;
		private String user_image_hash;
		private List<Action> actions;
		
		public Result(Integer event, Long user_id, String user_name, String message, 
				Boolean user_image, List<Action> actions){
			this.event = event;
			this.user_id = user_id;
			this.user_name = user_name;
			this.message = message;
			this.user_image = user_image;
			this.actions = actions;
		}
		
	}
	

	@SuppressWarnings("unused")
	protected class Request {
		private List<Action> actions;
		private Boolean keep_network_info;
		private String password;
		
		public Request(List<Action> actions){
			this.actions = actions;
		}
		
		public Request(Boolean keep_network_info){
			this.keep_network_info = keep_network_info;
		}
		
		public Request(String password){
			this.password = password;
		}
	}
	

	@SuppressWarnings("unused")
	protected class OperationMode{
		public Integer mode;
		public Mode mode_name;
		public String exception_mode;
		public Long time;
		public Long last_offline;
		public Integer device_id;
		
	}
	
	
	protected enum Mode {
		DEFAULT,
		CONTINGENCY;
	}
	
	@SuppressWarnings("unused")
	protected class Action {
		private String action;
		private String parameters;
		
		public Action(String action, String parameters) {
			this.action = action;
			this.parameters = parameters;
		}
	}
	

	@SuppressWarnings("unused")
	protected class Beep {
		private Integer duty_cycle;
		private Integer frequency;
		private Integer timeout;
		
		public Beep(Integer duty_cycle, Integer frequency, Integer timeout) {
			this.duty_cycle = duty_cycle;
			this.frequency = frequency;
			this.timeout = timeout;
		}
	}
	

	@SuppressWarnings("unused")
	protected class RemoteEnroll {
		public String type = "biometry";
		public Boolean save = false;
		public Integer user_id;
		public Integer panic_finger = 0;
		public String registration;
		public String msg = "Posicione o dedo no leitor";
		public Boolean sync = false;
	}
	

	protected class Template {
		public Long id;
		public String template;
		public Integer user_id;
		public Integer finger_type = 0; // 0 = dedo normal; 1 = dedo de panico
		
		public Template(Integer user_id, String template) {
			this.user_id = user_id;
			this.template = template;
		}
	}
	
	
}
