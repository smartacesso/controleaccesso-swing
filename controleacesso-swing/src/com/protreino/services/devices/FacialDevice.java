package com.protreino.services.devices;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import javax.swing.JOptionPane;
import javax.swing.SwingWorker;

import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.enumeration.DeviceMode;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.enumeration.FieldType;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.enumeration.NotificationType;
import com.protreino.services.enumeration.VerificationResult;
import com.protreino.services.main.Main;
import com.protreino.services.screens.FacialDialog;
import com.protreino.services.services.LuxandService;
import com.protreino.services.to.ConfigurationGroupTO;
import com.protreino.services.to.ConfigurationTO;
import com.protreino.services.to.DetectFaceResult;
import com.protreino.services.to.Face;
import com.protreino.services.to.FacePosition;
import com.protreino.services.utils.FSDK;
import com.protreino.services.utils.HibernateUtil;
import com.protreino.services.utils.Utils;

import Luxand.FSDK.FSDK_IMAGEMODE;
import Luxand.FSDK.HImage;
import Luxand.FSDKCam;
import Luxand.FSDKCam.FSDK_VideoFormatInfo;
import Luxand.FSDKCam.HCamera;

@SuppressWarnings("serial")
public class FacialDevice extends Device {
	
	public static final int ORIGEM_FACIAL = 888;
	private final int ICON_SIZE = 128; // tamanho em px da miniatura das fotos que aparece no card
	
	// Referencia sobre device path
	// https://www.silabs.com/community/interface/knowledge-base.entry.html/2013/11/21/windows_usb_devicep-aGxD
	
	private String devicePath; // identificador unico e completo do dispositivo no Windows. E uma longa string que e usada como chave pela SDK
	private String cameraShortId; // sequencia de digitos extraida do devicePath, com apenas 8 caracteres, e melhor para apresentar ao usuario
	private HCamera cameraHandle;
	private HImage imageHandle;
	private BufferedImage frameCapturado;
	private FSDK_VideoFormatInfo.ByValue[] videoFormatList;
	private boolean createNotification = true;
	
	private String tipoCamera;
	private String ipCamera;
	private String usuarioCamera;
	private String senhaCamera;
	
	private FacialDialog facialDialog;
	private int numeroDeImagensCapturadasParaTreinamento;
	private List<byte[]> imagesForEnrollment;
	private String nameToAssign;
	private int imageWidth;
	private int imageHeight;
	private long capturingTimeMarker;
	private boolean waitAfterRecognition = false;
	private int waitTimeAfterRecognizer;
	private int intervalBetweenCapturesForTraining;
	private int intervalBetweenCapturesForRecognition;
	private int samplesNumberForTraining;
	private long maxTimeForFaceCapturing;
	private int unsuccessfulTriesInSequence;
	private long timeMarkerRecognitionBlocked;
	private int countToVerifyOnServer;
	
	public FacialDevice(DeviceEntity deviceEntity) {
		this(deviceEntity.getIdentifier(), deviceEntity.getConfigurationGroupsTO());
		this.deviceEntity = deviceEntity;
		this.name = deviceEntity.getName();
		this.location = deviceEntity.getLocation();
		this.desiredStatus = deviceEntity.getDesiredStatus();
		this.defaultDevice = deviceEntity.getDefaultDevice();
		this.athleteScreenConfig = deviceEntity.getAthleteScreenConfig();
		
		if (deviceEntity.getConfigurationGroupsTO() != null) {
			List<ConfigurationTO> geral = deviceEntity.getConfigurationGroupsTO().get(0).getConfigurations();
			PRIMEIRO_FOR: for (ConfigurationTO to : geral) {
				if(to.getName().equals("Catraca vinculada")) {
					for(Device d : Main.devicesList) {
						if(d.getIdentifier().equals(to.getValue().replace("$", ";"))) {
							this.catracaVinculada = d;
							break PRIMEIRO_FOR;
						}
					}
				}
			}
		}
	}
	
	public FacialDevice(String identifier){
		this(identifier, null);
	}
	
	public FacialDevice(String identifier, List<ConfigurationGroupTO> configurationGroups){
		this.manufacturer = Manufacturer.FACIAL;
		String[] partesIdentifier = identifier.split(";");
		this.tipoCamera = partesIdentifier[0];
		
		if("USB".equals(tipoCamera)) {
			this.identifier = partesIdentifier[1]; // cameraName (cameraId)
			
		} else if("IP".equals(tipoCamera)) {
			this.ipCamera = partesIdentifier[1].trim();
			this.usuarioCamera = partesIdentifier[2].trim();
			this.senhaCamera = partesIdentifier[3].trim();
			this.identifier = "Camera IP: " + this.ipCamera;

		} else {
			this.identifier = partesIdentifier[0]; // cameraName (cameraId)
		}
		
		this.name = this.identifier;
		this.cameraShortId = this.identifier.substring(this.identifier.indexOf("(") + 1, this.identifier.length() - 1);
		
		if (configurationGroups != null)
			this.configurationGroups = configurationGroups;
		else
			createDefaultConfiguration();
		super.createConfigurationMap();
	}
	
	@Override
	public void createDefaultConfiguration() {
		List<ConfigurationTO> geralConfigurations = new ArrayList<ConfigurationTO>();
		geralConfigurations.add(new ConfigurationTO("Catraca vinculada", "Nenhuma_NULL", FieldType.COMBOBOX, 
								"Nenhuma_NULL;Todas_TODAS;COMM_COMM;USB_USB"));
		geralConfigurations.add(new ConfigurationTO("Exibe as imagens no card", "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO("Somente para cadastros", "false", FieldType.CHECKBOX));
		geralConfigurations.add(new ConfigurationTO("Ignorar regras de acesso", "false", FieldType.CHECKBOX));
		
		if("USB".equals(tipoCamera)) {
			try {
				String[] arrayResolucoes = getResolutions();
				String resolucaoPadrao = getLowerResolution(arrayResolucoes);
				String resolucoes = resolucaoPadrao + "_" + resolucaoPadrao;
				
				for (int i = 0; i < arrayResolucoes.length; i++) {
					String r = arrayResolucoes[i];
					
					if(r.equals(resolucaoPadrao))
						continue;
					
					resolucoes = resolucoes + ";" + r + "_" + r;
				}
				geralConfigurations.add(new ConfigurationTO("Resolução", resolucaoPadrao + "_" + resolucaoPadrao, FieldType.COMBOBOX, resolucoes));
			} catch (Exception e) {
				JOptionPane.showMessageDialog(null, e.getMessage(), "Erro na Luxand FaceSDK", JOptionPane.PLAIN_MESSAGE);
			}
		}
		
		configurationGroups = new ArrayList<ConfigurationGroupTO>();
		configurationGroups.add(new ConfigurationGroupTO("Geral", geralConfigurations));
	}
	
	private String getLowerResolution(String[] arrayResolucoes) {
		String lowerResolution = arrayResolucoes[0];
		
		for(int i = 1; i < arrayResolucoes.length; i++) {
			String auxResolution = arrayResolucoes[i];
			String [] splited = auxResolution.split("x"); 
			String [] splitedMenor = lowerResolution.split("x");
			
			if( Integer.valueOf(splited[0]) >= 640 
					&& Integer.valueOf(splited[2]) == 24 
					&& Integer.valueOf(splited[0]).compareTo(Integer.valueOf(splitedMenor[0])) <= 0) {
				lowerResolution = auxResolution;
			}
		}
		
		return lowerResolution;
	}

	@Override
	public void sendConfiguration() throws Exception {
		if (!getConfigurationValueAsBoolean("Exibe as imagens no card")) {
        	deviceCard.resetIcon();
        }
		
		catracaVinculada = null;
		String opcaoCatracaVinculada = getConfigurationValue("Catraca vinculada");
		if ("COMM".equals(opcaoCatracaVinculada))
			catracaVinculada = Manufacturer.COMM.getNewDevice("");
		else if ("USB".equals(opcaoCatracaVinculada)) {
			catracaVinculada = Manufacturer.USB.getNewDevice("");
			catracaVinculada.connect("");
		} 
		else if("TODAS".equalsIgnoreCase(opcaoCatracaVinculada)) {
			catracaVinculada = null;
		} 
		else if (!"NULL".equals(opcaoCatracaVinculada)) {
			String identifier = opcaoCatracaVinculada.replace("$", ";");
			for (Device device : Main.devicesList) {
				if (identifier.equals(device.getIdentifier())) {
					catracaVinculada = device;
					break;
				}
			}
		}
		
		String[] partes = athleteScreenConfig.split("%");
		boolean openAthleteScreenOnInit = Boolean.valueOf(partes[2]);
		boolean fullScreenAthleteScreen = Boolean.valueOf(partes[3]);
		boolean focusFieldAthleteScreen = Boolean.valueOf(partes[4]);
		setAthleteScreenConfig(getConfigurationValue("Catraca vinculada") + "%" + "null" + "%"
				+ openAthleteScreenOnInit + "%" + fullScreenAthleteScreen + "%" + focusFieldAthleteScreen + "%");
	}
	
	@Override
	public void connect(String... args) throws Exception {
		
		setMode(DeviceMode.VERIFICATION);
		workerEnabled = true;
		cameraHandle = new HCamera();
		
		waitTimeAfterRecognizer = Utils.getPreferenceAsInteger("waitTimeAfterRecognizer");
		intervalBetweenCapturesForTraining = Utils.getPreferenceAsInteger("intervalBetweenCapturesForTraining");
		intervalBetweenCapturesForRecognition = Utils.getPreferenceAsInteger("intervalBetweenCapturesForRecognition");
		samplesNumberForTraining = Utils.getPreferenceAsInteger("samplesNumberForTraining");
		maxTimeForFaceCapturing = Utils.getPreferenceAsLong("maxTimeForFaceCapturing");
		unsuccessfulTriesInSequence = 0;
		timeMarkerRecognitionBlocked = 0l;
		countToVerifyOnServer = 0;
		
		sendConfiguration();
		
		worker = new SwingWorker<Void, Void>() {
			@Override
			protected Void doInBackground() throws Exception {
				try {
					openVideoAndGrabFrame(true);
				}
				catch (Exception e) {
					JOptionPane.showMessageDialog(null, e.getMessage(), "Erro na Luxand FaceSDK", JOptionPane.PLAIN_MESSAGE);
					disconnect();
					return null;
				}
				
				setStatus(DeviceStatus.CONNECTED);
				
				while (workerEnabled) {
					
					try {
						
						// Para economizar recursos, apenas coleta frame e detecta faces caso seja necessario
						if (mode.equals(DeviceMode.VERIFICATION) 
								&& getConfigurationValueAsBoolean("Somente para cadastros")
								&& !getConfigurationValueAsBoolean("Exibe as imagens no card")) {
							continue;
						}
						
						while (System.currentTimeMillis() < timeMarkerRecognitionBlocked) {}
						
						try {
							imageHandle = LuxandService.getInstance().grabFrame(cameraHandle);
						}catch (Throwable e) {
							e.printStackTrace();
						}
						
						if (imageHandle != null) {
							
							Image awtImage[] = new Image[1];
		                    if (FSDK.SaveImageToAWTImage(imageHandle, awtImage, FSDK_IMAGEMODE.FSDK_IMAGE_COLOR_24BIT) == FSDK.FSDKE_OK) {
								
		                    	imageWidth = awtImage[0].getWidth(null);
		                    	imageHeight = awtImage[0].getHeight(null);
		                    	
		                    	frameCapturado = new BufferedImage(imageWidth, imageHeight, BufferedImage.TYPE_4BYTE_ABGR);
		                    	
		                    	Graphics2D gr = frameCapturado.createGraphics();
		                        gr.drawImage(awtImage[0], 0, 0, null);
		                        gr.setStroke(new BasicStroke(3));
		                        gr.setFont(new Font("Arial", Font.BOLD, 24));
		                    	
		                    	// Tenta detectar alguma face com o DetectFace antes de chamar o FeedFrame, por ser um metodo rapido.
		                
		                        DetectFaceResult resultadoDetectFace = LuxandService.getInstance().detectFace(imageHandle);
		                        if(resultadoDetectFace != null && resultadoDetectFace.getFace()!= null) {
		                        	 System.out.println("id do  luxand  Facial device  "+ resultadoDetectFace.getFace().getIdentifier());
				                       System.out.println("nome do luxand Facial device   "+ resultadoDetectFace.getFace().getName());
		                        }
		                      
		                    	
								if (resultadoDetectFace.getResultCode() == FSDK.FSDKE_OK) {
									
									drawFace(resultadoDetectFace.getFace(), gr);
									publish();
									
									countToVerifyOnServer++;
									
									if (countToVerifyOnServer == 5) {
										countToVerifyOnServer = 0;
									
										// Verifica se sera necessario tentar identificar alguem ou se apenas exibira a imagem no card
										if (mode.equals(DeviceMode.VERIFICATION) 
												&& !getConfigurationValueAsBoolean("Somente para cadastros")) {
											
											DetectFaceResult resultadoFeedFrame = LuxandService.getInstance().recognize(resultadoDetectFace.getFace().getBytes(), 
													imageWidth, imageHeight);
											
											
											System.out.println("id do usário no inicio da função  "+ resultadoDetectFace.getFace().getIdentifier());
											System.out.println("nome do usário no inicio da função  "+ resultadoDetectFace.getFace().getName());
											if (resultadoFeedFrame.getResultCode() == FSDK.FSDKE_OK) {
												
												drawFace(resultadoFeedFrame.getFace(), gr);
												
												if (athleteScreen != null && !athleteScreen.isTelaTravada()) { // Acesso feito pela tela do aluno
													createNotification = false;
													
													processAccessRequest(resultadoFeedFrame.getFace());
													
													boolean atualiza = false;
													if (VerificationResult.ALLOWED.equals(verificationResult)
															|| VerificationResult.TOLERANCE_PERIOD.equals(verificationResult)) {
														allowAccess();
														atualiza = true;
													} 
													else if (!VerificationResult.NOT_FOUND.equals(verificationResult)) {
														denyAccess();
														atualiza = true;
													}
													
													if (atualiza && athleteScreen != null) {
														athleteScreen.requisicaoPorFoto(verificationResult, allowedUserName, matchedAthleteAccess, 4000);
														allowedUserName = "";
														matchedAthleteAccess = null;
													}
												}
												else {
													createNotification = true;
													
													processAccessRequest(resultadoFeedFrame.getFace());
													
													if (VerificationResult.ALLOWED.equals(verificationResult)
															|| VerificationResult.TOLERANCE_PERIOD.equals(verificationResult)) {
														allowAccess();
													}
													else if (!VerificationResult.NOT_FOUND.equals(verificationResult)) {
														denyAccess();
													}
												}
												waitAfterRecognition = true;
												
											}
											else {
												
												if (resultadoFeedFrame.getResultCode() == FSDK.FSDKE_USER_NOT_FOUND) {
													unsuccessfulTriesInSequence++;
													
													if (unsuccessfulTriesInSequence == 3) {
														unsuccessfulTriesInSequence = 0;
														drawFace(resultadoDetectFace.getFace(), gr, "Usuário não cadastrado.");
														
														if (athleteScreen != null)
															athleteScreen.requisicaoPorFoto(VerificationResult.NOT_FOUND, "", null, 5000);
														
														timeMarkerRecognitionBlocked = System.currentTimeMillis() + 5000;
														
														System.out.println("Usuário não cadastrado. Aguardando 5 segundos.");
													}
												}
												else {
													drawFace(resultadoDetectFace.getFace(), gr);
													
													if (resultadoDetectFace.getResultCode() != FSDK.FSDKE_USER_NOT_LOGGED_IN) {
														// Erro inesperado no FeedFrame, imprime para ver
														System.out.println("Erro no FeedFrame: " + resultadoFeedFrame.getResultDescription());
														System.out.println("id do usário Else  "+ resultadoDetectFace.getFace().getIdentifier());
														System.out.println("nome do usário no Else  "+ resultadoDetectFace.getFace().getName());
													}
												}
												
											}
											
										}
										else if (mode.equals(DeviceMode.ENROLLMENT) && facialDialog != null) {
											
											drawFace(resultadoDetectFace.getFace(), gr);
											
											processSampleForEnrollment(resultadoDetectFace.getFace());
											
										}
										else {
											// Apenas exibe a imagem no card
											drawFace(resultadoDetectFace.getFace(), gr);
										}
										
									}
									
								}
								else if (resultadoDetectFace.getResultCode() != FSDK.FSDKE_FACE_NOT_FOUND) {
									// Erro inesperado no DetectFace, imprime para ver
									System.out.println("Erro no DetectFace: " + resultadoDetectFace.getResultDescription());
								}
		                    	 
								publish();
									
		                    }
		                    
		                    FSDK.FreeImage(imageHandle);
		                    
		                    if (waitAfterRecognition) {
		                    	Utils.sleep(waitTimeAfterRecognizer);
		                    	waitAfterRecognition = false;
		                    }
						}
					}
					catch (Exception e) {
	                    e.printStackTrace();
	                    disconnect();
	                    new Thread() {
	                    	public void run() {
	                    		try {
									Thread.sleep(1000);
									connect(); // Tenta conectar novamente automaticamente
								} catch (Exception e) {
									e.printStackTrace();
								}
	                    	};
	                    }.start();
	                    break;
	                
					} 
					finally {
						temMensagem = false;
						if (workerEnabled) {
							if (DeviceMode.ENROLLMENT.equals(mode))
								Utils.sleep(intervalBetweenCapturesForTraining);
							else
								Utils.sleep(intervalBetweenCapturesForRecognition);
						}
					}
				}
				
				disconnect();
				
				return null;
			}
			
			@Override
			protected void process(List<Void> chunks) {
				showImageOnCard();
			}
		};
		
		worker.execute();
	}
	
	private void openVideoAndGrabFrame(boolean isConnectMethod) throws Exception {
		
		if(this.ipCamera != null) {
			cameraHandle = new HCamera();

			int r = FSDKCam.OpenIPVideoCamera(FSDK.FSDK_VIDEOCOMPRESSIONTYPE.FSDK_MJPEG, ipCamera, usuarioCamera, 
									senhaCamera, 5, cameraHandle);
			
			System.out.println("resp: " + r);
			
			if (FSDK.FSDKE_OK !=  r) {
				System.out.println("Falha ao conectar.");
				throw new Exception("Não foi possível abrir a câmera " + name);
			}
			
		} else {
			
			devicePath = LuxandService.getInstance().getDevicePathFromDeviceId(cameraShortId);
			if (devicePath == null)
				throw new Exception("Não foi possível encontrar a câmera " + name);
			
			if (!workerEnabled && isConnectMethod)
				return;
			
			FSDK_VideoFormatInfo.ByValue format = getVideoFormatFromConfiguration();
			
			if (!workerEnabled && isConnectMethod)
				return;
			
			FSDKCam.InitializeCapturing();
			
			if (!workerEnabled && isConnectMethod)
				return;
			
			if (FSDKCam.SetVideoFormat(devicePath, format) != FSDK.FSDKE_OK)
				throw new Exception("Não foi possível definir o formato de vídeo da câmera " + name);
			
			if (!workerEnabled && isConnectMethod)
				return;
			
			if (FSDKCam.OpenVideoCamera(devicePath, cameraHandle) != FSDK.FSDKE_OK)
				throw new Exception("Não foi possível abrir a câmera " + name);
			
			if (!workerEnabled && isConnectMethod)
				return;
			
			imageHandle = LuxandService.getInstance().grabFrame(cameraHandle);
			if (imageHandle == null)
				throw new Exception("Não foi capturar imagem da câmera " + name);
		}
	}
	
	@Override
	public void disconnect(String... args) throws Exception {
		super.disconnect();
		
		if (cameraHandle != null) {
			FSDKCam.FinalizeCapturing();
			FSDKCam.CloseVideoCamera(cameraHandle);
			cameraHandle = null;
		}
		
//		FSDKCam.FinalizeCapturing();
				
		deviceCard.resetIcon();
		setStatus(DeviceStatus.DISCONNECTED);
		setMode(DeviceMode.NONE);
	}
	
	@Override
	public void processAccessRequest(Object obj) {
		try {
			Face face = (Face) obj;
			
			if (Boolean.TRUE.equals(Main.desenvolvimento))
				System.out.println("Predição: " + face.getName());
			
			if(face.getName() == null)
				return;
			
			String idUsuario = face.getName().substring(face.getName().indexOf("(") + 1, face.getName().length() - 1);
			
			String idEquipamento = null;
			if (catracaVinculada != null)
				idEquipamento = "Inner " + catracaVinculada.getIdentifier().split(";")[0];
			else {
				String opcaoCatracaVinculada = getConfigurationValue("Catraca vinculada");
				if ("TODAS".equalsIgnoreCase(opcaoCatracaVinculada)) {
					for (Device device : Main.devicesList) {
						if (device.isCatraca() && device.isConnected()) {
							if(device instanceof TopDataDevice)
								idEquipamento = "Inner " + device.getIdentifier().split(";")[0];
							else
								idEquipamento = device.getIdentifier().split(";")[0];
						}
					}
				}
			}
			
			Object[] retorno = null;
			if (idEquipamento != null) {
				retorno = HibernateUtil.processAccessRequest(idUsuario, idEquipamento, ORIGEM_FACIAL, location, false, createNotification, getConfigurationValueAsBoolean("Ignorar regras de acesso"));
			} else {
				retorno = HibernateUtil.processAccessRequest(idUsuario, location, createNotification, getConfigurationValueAsBoolean("Ignorar regras de acesso"));
			}
			
			if (retorno != null && retorno.length > 0) {
				this.verificationResult = (VerificationResult) retorno[0];
				this.allowedUserName = (String) retorno[1];
				this.matchedAthleteAccess = (PedestrianAccessEntity) retorno[2];
			
			} else {
				this.verificationResult = VerificationResult.NOT_FOUND;
				this.allowedUserName = "";
				this.matchedAthleteAccess = null;
			}
			
			System.out.println("allowedUserName: " + allowedUserName + " \tverificationResult: " + verificationResult);
		} 
		catch (Exception e) {
			e.printStackTrace();
			verificationResult = VerificationResult.ERROR;
			if (createNotification)
				Utils.createNotification("Erro na verificação", NotificationType.BAD);
		}
	}
	
	@Override
	public void allowAccess() {
		if (catracaVinculada != null) {
			if (catracaVinculada.isConnected()) {
				catracaVinculada.setVerificationResult(verificationResult);
				catracaVinculada.setAllowedUserName(allowedUserName);
				catracaVinculada.setMatchedFacialId(matchedAthleteAccess.getId());
				catracaVinculada.allowAccess(matchedAthleteAccess);
			}
			else {
				Utils.createNotification("Catraca " + catracaVinculada.getName() + " desconectada.", NotificationType.BAD);
			}
		}
		else {
			String opcaoCatracaVinculada = getConfigurationValue("Catraca vinculada");
			if("TODAS".equalsIgnoreCase(opcaoCatracaVinculada)) {
				for(Device catracaVinculada : Main.devicesList) {
					if (!(catracaVinculada instanceof FacialDevice) && catracaVinculada.isConnected()) {
						catracaVinculada.setVerificationResult(verificationResult);
						catracaVinculada.setAllowedUserName(allowedUserName);
						catracaVinculada.allowAccess(matchedAthleteAccess);
						break;
					}
				}
			}
		}
		allowedUserName = "";
	}
	
	@Override
	public void denyAccess() {
		// nao faz nada
		if(matchedAthleteAccess != null) {
			if (catracaVinculada != null) {
				if (catracaVinculada.isConnected()) {
					catracaVinculada.setVerificationResult(verificationResult);
					catracaVinculada.setAllowedUserName(allowedUserName);
					new Thread() {
						public void run() {
							catracaVinculada.denyAccess();
						};
					}.start();
				}
				else {
					Utils.createNotification("Catraca " + catracaVinculada.getName() + " desconectada.", NotificationType.BAD);
				}
			}
			else {
				String opcaoCatracaVinculada = getConfigurationValue("Catraca vinculada");
				if("TODAS".equalsIgnoreCase(opcaoCatracaVinculada)) {
					for(Device catracaVinculada : Main.devicesList) {
						if (catracaVinculada.isCatraca() && catracaVinculada.isConnected()) {
							catracaVinculada.setVerificationResult(verificationResult);
							catracaVinculada.setAllowedUserName(allowedUserName);
							new Thread() {
								public void run() {
									catracaVinculada.denyAccess();
								};
							}.start();
							break;
						}
					}
				}
			}
		}
	}
	
	@Override
	public void processSampleForEnrollment(Object obj) {
		
		// Recebe uma imagem e adiciona numa lista de imagens.
		// Quando tiver coletado imagens suficientes, entao enviar para o servidor para cadastro
		
		Face face = (Face) obj;
		
		imagesForEnrollment.add(face.getBytes());
		numeroDeImagensCapturadasParaTreinamento++;
		
		facialDialog.updateProgress();
		
		if ((System.currentTimeMillis() - capturingTimeMarker) > (maxTimeForFaceCapturing * 1000l)) {
			setMode(DeviceMode.NONE);
			facialDialog.cancelCollect("Tempo esgotado.");
			facialDialog = null;
		}
		
		if (numeroDeImagensCapturadasParaTreinamento == samplesNumberForTraining) {
			
			DetectFaceResult resultadoFeedFrame = LuxandService.getInstance().enroll(imagesForEnrollment, imageWidth, imageHeight, nameToAssign);
			
			if (resultadoFeedFrame.getResultCode() == FSDK.FSDKE_OK) {
				facialDialog.setLuxandIdentifier(resultadoFeedFrame.getFace().getIdentifier().toString());
				facialDialog.finishCollect(null);
			}
			else {
				
				if (resultadoFeedFrame.getResultCode() == FSDK.FSDKE_USER_ALREADY_REGISTERED) {
					facialDialog.setLuxandIdentifier(resultadoFeedFrame.getFace().getIdentifier().toString());
					facialDialog.finishCollect("Usuário já registrado.");
				}
				else if (resultadoFeedFrame.getResultCode() == FSDK.FSDKE_USER_REGISTERED_WITH_OTHER_NAME) {
					facialDialog.cancelCollect("Usuário registrado com o nome " + resultadoFeedFrame.getFace().getName());
				}
				else if (resultadoFeedFrame.getResultCode() == FSDK.FSDKE_RECOGNITION_FAILED_DURING_ENROLLMENT) {
					facialDialog.cancelCollect("Não foi possível distinguir a pessoa ou usuário cadastrado com outro nome.");
				}
				else {
					facialDialog.cancelCollect("Erro durante o cadastro: " + resultadoFeedFrame.getResultDescription());
				}
			}
			
			setMode(DeviceMode.NONE);
			facialDialog = null;
		}
		
	}
	
	@Override
	public List<ConfigurationGroupTO> getConfigurationGroups() {
		ConfigurationGroupTO geral = configurationGroups.get(0);
		ConfigurationTO configCatracaVinculada = geral.getConfigurations().get(0);
		String options = "Nenhuma_NULL;Todas_TODAS;COMM_COMM;USB_USB";
		for (Device device : Main.devicesList) { // adiciona as catracas na lista de opcoes, alem dos valores padroes
			if (device.isCatraca()) {
				options = options + ";" + device.getName() + "_" + device.getIdentifier().replace(";", "$");
			}
		}
		configCatracaVinculada.setComboboxValues(options);
		return configurationGroups;
	}
	
	@Override
	public Set<Integer> getRegisteredUserList() throws Exception {
		return null;
	}

	@Override
	public String cadastrateUser(PedestrianAccessEntity athleteAccessEntity) {
		return "";
	}

	@Override
	public String removeUser(PedestrianAccessEntity athleteAccessEntity) {
		if (athleteAccessEntity.getLuxandIdentifier() != null && !athleteAccessEntity.getLuxandIdentifier().isEmpty()) {
			Long luxandIdentifier = Long.valueOf(athleteAccessEntity.getLuxandIdentifier());
			LuxandService.getInstance().clearName(luxandIdentifier);
			athleteAccessEntity.setLuxandIdentifier(null);
		}
		return "";
	}
	
	public void prepareStartCapturing(FacialDialog facialDialog, String nameToAssign) {
		this.facialDialog = facialDialog;
		this.mode = DeviceMode.ENROLLMENT;
		this.numeroDeImagensCapturadasParaTreinamento = 0;
		this.imagesForEnrollment = new ArrayList<byte[]>();
		this.nameToAssign = nameToAssign;
		this.capturingTimeMarker = System.currentTimeMillis();
	}
	
	private String[] getResolutions() throws Exception {
		if (videoFormatList == null)
			getVideoFormatList();
		
		String[] resolucoes = new String[videoFormatList.length];
		for (int i = 0; i < videoFormatList.length; i++)
			resolucoes[i] = videoFormatList[i].Width + "x" + videoFormatList[i].Height + "x" + videoFormatList[i].BPP;
		
		return resolucoes;
	}
	
	private FSDK_VideoFormatInfo.ByValue getVideoFormatFromConfiguration() throws Exception {
		String resolucao = getConfigurationValue("Resolução");
		if (resolucao == null)
			throw new Exception("Não foi possível encontrar a configuração de resolução da câmera " + name);
			
		if (videoFormatList == null)
			getVideoFormatList();
				
		int width = new Integer(resolucao.split("x")[0]);
		int height = new Integer(resolucao.split("x")[1]);
		int bpp = new Integer(resolucao.split("x")[2]);
		
		for (int i = 0; i < videoFormatList.length; i++) {
			FSDK_VideoFormatInfo.ByValue format = videoFormatList[i];
			if (format.Width == width && format.Height == height && format.BPP == bpp) {
				return format;
			}
		}
		
		return null;
	}
	
	private void getVideoFormatList() throws Exception {
		if("IP".equals(tipoCamera))
			return;
		
		devicePath = LuxandService.getInstance().getDevicePathFromDeviceId(cameraShortId);
		if (devicePath == null)
			throw new Exception("Não foi possível encontrar a câmera " + name);
		
		videoFormatList = LuxandService.getInstance().getVideoFormatList(devicePath);
		
		if (videoFormatList.length == 0)
			throw new Exception("Não foi possível retornar formatos de video da câmera " + name);
	}
	
	private void drawFace(Face face, Graphics2D gr) {
		drawFace(face, gr, "");
	}
	
	private void drawFace(Face face, Graphics2D gr, String errorMessage) {
		FacePosition facePosition = face.getFacePosition();
		
		int left = facePosition.xc - (int)(facePosition.w * 0.6);
        int top = facePosition.yc - (int)(facePosition.w * 0.5);
        int w = (int)(facePosition.w * 1.2);
        
        if (errorMessage.isEmpty()) {
            if (mode.equals(DeviceMode.VERIFICATION) && face.getName() != null) {
        		gr.setColor(Color.green);
        		java.awt.geom.Rectangle2D textRect = gr.getFontMetrics().getStringBounds(face.getName(), gr);
                gr.drawString(face.getName(), (int)(facePosition.xc - textRect.getWidth()/2), (int)(top + w + textRect.getHeight()));
            }
            else if (mode.equals(DeviceMode.ENROLLMENT)) {
        		gr.setColor(Main.secondColor);
            }
            else {
            	gr.setColor(Color.gray);
            }
        }
        else {
        	gr.setColor(Color.red);
        	java.awt.geom.Rectangle2D textRect = gr.getFontMetrics().getStringBounds(errorMessage, gr);
            gr.drawString(errorMessage, (int)(facePosition.xc - textRect.getWidth()/2), (int)(top + w + textRect.getHeight()));
        }
        
        gr.drawRect(left, top, w, w);
	}
	
	private void showImageOnCard(){
		try {
			if (facialDialog != null) {
	        	if (facialDialog != null) // para o caso de fechar a janela enquanto convertia a imagem
	        		facialDialog.imageAcquired(frameCapturado);
			
			} 
			else {
				if (getConfigurationValueAsBoolean("Exibe as imagens no card"))
		            deviceCard.setIcon(resizeToIconSize(frameCapturado));
		        else
		        	deviceCard.resetIcon();
				
				if (athleteScreen != null)
					athleteScreen.imageAcquired(frameCapturado);
	        }
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public BufferedImage getSampleImage() {
		return frameCapturado;
	}
	
	private Image resizeToIconSize(BufferedImage srcImg) {
		double fatorDeReducao = srcImg.getWidth() / ICON_SIZE;
		int novaAltura = (int) (srcImg.getHeight() / fatorDeReducao);
		int y = (ICON_SIZE - novaAltura) / 2;
		
	    BufferedImage resizedImg = new BufferedImage(ICON_SIZE, ICON_SIZE, BufferedImage.TYPE_4BYTE_ABGR);
	    Graphics2D g2 = resizedImg.createGraphics();
	    g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
	    g2.drawImage(srcImg, 0, y, ICON_SIZE, novaAltura, null);
	    g2.dispose();
	    return resizedImg;
	}
	
	public int getImageWidth() {
		if (frameCapturado != null)
			return frameCapturado.getWidth();
		return 640;
	}
	
	public int getImageHeight() {
		if (frameCapturado != null)
			return frameCapturado.getHeight();
		return 480;
	}
	
	public void setFacialDialog(FacialDialog facialDialog) {
		this.facialDialog = facialDialog;
	}
	
	public int getMaxAmostras(){
		return samplesNumberForTraining;
	}

}