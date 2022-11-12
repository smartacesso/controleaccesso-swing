package com.protreino.services.devices;

import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.imageio.ImageIO;

import com.digitalpersona.onetouch.DPFPDataPurpose;
import com.digitalpersona.onetouch.DPFPFeatureSet;
import com.digitalpersona.onetouch.DPFPGlobal;
import com.digitalpersona.onetouch.DPFPSample;
import com.digitalpersona.onetouch.DPFPTemplate;
import com.digitalpersona.onetouch.capture.DPFPCapture;
import com.digitalpersona.onetouch.capture.DPFPCapturePriority;
import com.digitalpersona.onetouch.capture._impl.DPFPCaptureFactoryImpl;
import com.digitalpersona.onetouch.capture.event.DPFPDataEvent;
import com.digitalpersona.onetouch.capture.event.DPFPDataListener;
import com.digitalpersona.onetouch.capture.event.DPFPErrorEvent;
import com.digitalpersona.onetouch.capture.event.DPFPErrorListener;
import com.digitalpersona.onetouch.capture.event.DPFPImageQualityEvent;
import com.digitalpersona.onetouch.capture.event.DPFPImageQualityListener;
import com.digitalpersona.onetouch.capture.event.DPFPReaderStatusEvent;
import com.digitalpersona.onetouch.capture.event.DPFPReaderStatusListener;
import com.digitalpersona.onetouch.capture.event.DPFPSensorEvent;
import com.digitalpersona.onetouch.capture.event.DPFPSensorListener;
import com.digitalpersona.onetouch.processing.DPFPEnrollment;
import com.digitalpersona.onetouch.processing.DPFPImageQualityException;
import com.digitalpersona.onetouch.verification.DPFPVerification;
import com.digitalpersona.onetouch.verification.DPFPVerificationResult;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.entity.TemplateEntity;
import com.protreino.services.enumeration.BroadcastMessageType;
import com.protreino.services.enumeration.DeviceMode;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.enumeration.FieldType;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.enumeration.NotificationType;
import com.protreino.services.enumeration.VerificationResult;
import com.protreino.services.main.Main;
import com.protreino.services.to.BroadcastMessageTO;
import com.protreino.services.to.ConfigurationGroupTO;
import com.protreino.services.to.ConfigurationTO;
import com.protreino.services.utils.HibernateUtil;
import com.protreino.services.utils.Utils;

public class ComputerIdDevice extends Device {

	private DPFPCapture capturer;
	private DPFPEnrollment enroller;
	private String serialNumber;
	private Boolean createNotification;
	private Map<Long, List<DPFPTemplate>> templateDatabase;
	
	public ComputerIdDevice(DeviceEntity deviceEntity){
		this(deviceEntity.getIdentifier(), deviceEntity.getConfigurationGroupsTO());
		this.deviceEntity = deviceEntity;
		this.name = deviceEntity.getName();
		this.location = deviceEntity.getLocation();
		this.desiredStatus = deviceEntity.getDesiredStatus();
		this.defaultDevice = deviceEntity.getDefaultDevice();
		this.athleteScreenConfig = deviceEntity.getAthleteScreenConfig();
	}
	
	public ComputerIdDevice(String identifier){
		this(identifier, null);
	}
	
	public ComputerIdDevice(String identifier, List<ConfigurationGroupTO> configurationGroups){
		this.manufacturer = Manufacturer.COMPUTER_ID;
		String partes[] = identifier.split(";");
		this.identifier = identifier;
		this.serialNumber = partes[0];
		this.name = partes[1];
		if (configurationGroups != null)
			this.configurationGroups = configurationGroups;
		else
			createDefaultConfiguration();
		createConfigurationMap();
	}
	
	
	@Override
	public void connect(String... args) throws Exception {
		sendConfiguration();
		enroller = DPFPGlobal.getEnrollmentFactory().createEnrollment();
		capturer = new DPFPCaptureFactoryImpl().createCapture();
		capturer.setReaderSerialNumber(identifier);
		capturer.setPriority(DPFPCapturePriority.CAPTURE_PRIORITY_LOW);
		capturer.addSensorListener(new DPFPSensorListener() {
			public void imageAcquired(DPFPSensorEvent arg0) {}
			
			public void fingerTouched(DPFPSensorEvent arg0) {
				if (DeviceMode.ENROLLMENT.equals(mode) && biometricDialog != null)
					biometricDialog.fingerTouched();
			}
			
			public void fingerGone(DPFPSensorEvent arg0) {
				if (DeviceMode.ENROLLMENT.equals(mode) && biometricDialog != null)
					biometricDialog.fingerGone();
				else if (DeviceMode.VERIFICATION.equals(mode) && athleteScreen != null)
					athleteScreen.fingerGone();
			}
		});
		capturer.addDataListener(new DPFPDataListener() {
			public void dataAcquired(DPFPDataEvent arg0) {
				busy = true;
				if (arg0 != null && arg0.getSample() != null) {
					if (mode.equals(DeviceMode.ENROLLMENT) && biometricDialog != null) {
						processSampleForEnrollment(arg0.getSample());
					}
					else if (mode.equals(DeviceMode.VERIFICATION)) {
						if (athleteScreen != null) {
							// ACESSO FEITO PELA TELA DO ALUNO
							// caso tenha alguma informacao exibida na tela, aguarda a limpeza da tela antes de mostrar outra coisa
							if (athleteScreen.isTelaTravada())
								return;
							DPFPSample sample = arg0.getSample();
							athleteScreen.digitalObtida(sample);
							createNotification = false;
							processAccessRequest(sample);
							athleteScreen.requisicaoPorDigital(sample, verificationResult, allowedUserName, matchedAthleteAccess);
							allowedUserName = "";
							matchedAthleteAccess = null;
						}
						else {
							// ACESSO DIRETO PELO LEITOR
							DPFPSample sample = arg0.getSample();
							createNotification = true;
							processAccessRequest(sample);
							if (VerificationResult.ALLOWED.equals(verificationResult)
									|| VerificationResult.TOLERANCE_PERIOD.equals(verificationResult))
								allowAccess();
							else
								denyAccess();
						}
					}
				}
				busy = false;
			}
		});
		capturer.addReaderStatusListener(new DPFPReaderStatusListener() {
			public void readerDisconnected(DPFPReaderStatusEvent arg0) {
				setStatus(DeviceStatus.DISCONNECTED);
				setMessage("Leitor desconectado!", "erro");
				if (mode.equals(DeviceMode.ENROLLMENT) && biometricDialog != null)
					biometricDialog.cancelCollect();
			}
			
			public void readerConnected(DPFPReaderStatusEvent arg0) {
				setStatus(DeviceStatus.CONNECTED);
				if (DeviceMode.ENROLLMENT.equals(mode) && biometricDialog != null)
					biometricDialog.removeMessage();
			}
		});
		capturer.addErrorListener(new DPFPErrorListener() {
			public void exceptionCaught(DPFPErrorEvent arg0) {
				setMessage("Ocorreu um erro.", "erro");
			}
			public void errorOccured(DPFPErrorEvent arg0) {
				setMessage("Ocorreu um erro.", "erro");
			}
		});
		capturer.addImageQualityListener(new DPFPImageQualityListener() {
			public void onImageQuality(DPFPImageQualityEvent arg0) {
				setMessage("Qualidade ruim! Tente novamente.", "erro");
			}
		});
		capturer.startCapture();
		
		createTemplateDatabase();
		
		setStatus(DeviceStatus.CONNECTED);
		
		if (getAthleteScreenConfig() != null) {
			String[] partes = getAthleteScreenConfig().split("%");
			boolean openAthleteScreenOnInit = Boolean.valueOf(partes[2]);
			if (openAthleteScreenOnInit && isConnected())
				getDeviceCard().openAthleteScreen();
		}
	}

	@Override
	public void disconnect(String... args) throws Exception {
		if (capturer != null)
			capturer.stopCapture();
		capturer = null;
		if (enroller != null)
			enroller.clear();
		enroller = null;
		setStatus(DeviceStatus.DISCONNECTED);
	}
	
	@Override
	public void sendConfiguration() throws Exception {
		String opcaoCatracaVinculada = getConfigurationValue("Catraca vinculada");
		if ("COMM".equals(opcaoCatracaVinculada))
			catracaVinculada = Manufacturer.COMM.getNewDevice("");
		else if ("USB".equals(opcaoCatracaVinculada)) {
			catracaVinculada = Manufacturer.USB.getNewDevice("");
			catracaVinculada.connect("");
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
		else
			catracaVinculada = null;
		
		String[] partes = athleteScreenConfig.split("%");
		boolean openAthleteScreenOnInit = Boolean.valueOf(partes[2]);
		boolean fullScreenAthleteScreen = Boolean.valueOf(partes[3]);
		boolean focusFieldAthleteScreen = Boolean.valueOf(partes[4]);
		setAthleteScreenConfig(getConfigurationValue("Catraca vinculada") + "%" + "null" + "%"
				+ openAthleteScreenOnInit + "%" + fullScreenAthleteScreen + "%" + focusFieldAthleteScreen + "%");
	}
	
	@Override
	public void saveConfigurations(){
		try {
			if (!isConnected())
				sendConfiguration();
		}
		catch (Exception e) {
			e.printStackTrace();
			Main.mainScreen.addEvento("Erro ao salvar as configurações: " + e.getMessage());
		}
		
		super.saveConfigurations();
	}
	
	@Override
	public void allowAccess() {
		if (catracaVinculada != null) {
			if (catracaVinculada.isConnected()) {
				catracaVinculada.setVerificationResult(verificationResult);
				catracaVinculada.setAllowedUserName(allowedUserName);
				catracaVinculada.allowAccess();
			}
			else {
				Utils.createNotification("Catraca " + catracaVinculada.getName() + " desconectada.", NotificationType.BAD);
			}
		}
		allowedUserName = "";
	}
	
	@Override
	public String setMode(DeviceMode mode){
		this.mode = mode;
		if (DeviceMode.ENROLLMENT.equals(mode))
			enroller = DPFPGlobal.getEnrollmentFactory().createEnrollment();
		else
			enroller = null;
		return "";
	}

	@Override
	public void denyAccess() {
		// nao faz nada
	}

	@Override
	public void processSampleForEnrollment(Object obj) {
		DPFPSample sample = (DPFPSample) obj;
		try {
			DPFPFeatureSet features = DPFPGlobal.getFeatureExtractionFactory().createFeatureExtraction()
						.createFeatureSet(sample, DPFPDataPurpose.DATA_PURPOSE_ENROLLMENT);
			if (features != null)
				enroller.addFeatures(features);
		}
		catch (DPFPImageQualityException ex) {
			biometricDialog.setMessage("Qualidade ruim! Tente novamente.", "erro");
		}
		finally {
			this.sample = sample.serialize();
			switch(enroller.getTemplateStatus())
			{
				case TEMPLATE_STATUS_READY:
					biometricDialog.setImageSample(convertSampleToByteArray(sample));
					template = enroller.getTemplate().serialize();
					biometricDialog.finishCollect(template);
					break;
				case TEMPLATE_STATUS_FAILED:
					biometricDialog.setMessage("Ocorreu um erro no template durante a leitura.", "erro");
					break;
				case TEMPLATE_STATUS_INSUFFICIENT:
					Image sampleImage = DPFPGlobal.getSampleConversionFactory().createImage(sample).getScaledInstance(120, 150, Image.SCALE_SMOOTH);
					biometricDialog.updateSampleStatus(sampleImage, (manufacturer.getSamplesCount()-enroller.getFeaturesNeeded()));
					break;
				default:
					break;	
			}
		}
	}

	@Override
	public void processAccessRequest(Object obj) {
		
		// Primeiro é feito o processo de match para identificar o usuário.
		// Após a identificação é verificado se o acesso é permitido.
		
		Long idEncontrado = null;
		DPFPSample sample = (DPFPSample) obj;
		try {
			DPFPFeatureSet features = DPFPGlobal.getFeatureExtractionFactory().createFeatureExtraction()
					.createFeatureSet(sample, DPFPDataPurpose.DATA_PURPOSE_VERIFICATION);
			if (features == null) {
				this.verificationResult = VerificationResult.ERROR;
				if (createNotification)
					Utils.createNotification("Erro na verificação", NotificationType.BAD);
				return;
			}
			
			if (templateDatabase == null || templateDatabase.isEmpty()) {
				this.verificationResult = VerificationResult.NOT_FOUND;
				if (createNotification)
					Utils.createNotification("Sem digitais cadastradas.", NotificationType.BAD);
				return;
			}
			
			DPFPVerification verificator = DPFPGlobal.getVerificationFactory().createVerification();
			verificator.setFARRequested(DPFPVerification.MEDIUM_SECURITY_FAR);
			DPFPVerificationResult dpfpVerificationResult;
			
			Iterator<Entry<Long, List<DPFPTemplate>>> iterator = templateDatabase.entrySet().iterator();
		    WHILE: while (iterator.hasNext()) {
		        Map.Entry<Long, List<DPFPTemplate>> pair = iterator.next();
		        Long id = pair.getKey();
		        List<DPFPTemplate> templates = pair.getValue();
		        for (DPFPTemplate template : templates) {
		        	dpfpVerificationResult = verificator.verify(features, template);
		        	if (dpfpVerificationResult.isVerified()) {
						idEncontrado = id;
						break WHILE;
					}
		        }
		    }
			
			if (idEncontrado != null) {
				PedestrianAccessEntity pedestre = Utils.buscaPedestrePorIdOuIdTemp(idEncontrado);
				
				if(pedestre != null)
					idEncontrado = pedestre.getId();
				
				Object[] retorno = HibernateUtil.processAccessRequest(idEncontrado.toString(), location, createNotification, false);
				this.verificationResult = (VerificationResult) retorno[0];
				this.allowedUserName = (String) retorno[1];
				this.matchedAthleteAccess = (PedestrianAccessEntity) retorno[2];
			
			} else {
				verificationResult = VerificationResult.NOT_FOUND;
				if (createNotification)
					Utils.createNotification("Digital não encontrada.", NotificationType.BAD);
			}
			
		} catch (DPFPImageQualityException ex) {
			verificationResult = VerificationResult.ERROR;
			if (createNotification)
				Utils.createNotification("Qualidade ruim! Tente novamente.", NotificationType.BAD);
		
		} catch (Exception e) {
			e.printStackTrace();
			verificationResult = VerificationResult.ERROR;
			if (createNotification)
				Utils.createNotification("Erro na verificação", NotificationType.BAD);
		}
	}

	
	@Override
	public void createDefaultConfiguration() {
		List<ConfigurationTO> geralConfigurations = new ArrayList<ConfigurationTO>();
		geralConfigurations.add(new ConfigurationTO("Catraca vinculada", "Nenhuma_NULL", FieldType.COMBOBOX, "Nenhuma_NULL;COMM_COMM;USB_USB"));
		
		configurationGroups = new ArrayList<ConfigurationGroupTO>();
		configurationGroups.add(new ConfigurationGroupTO("Geral", geralConfigurations));
	}
	
	@Override
	public List<ConfigurationGroupTO> getConfigurationGroups() {
		ConfigurationGroupTO geral = configurationGroups.get(0);
		ConfigurationTO config = geral.getConfigurations().get(0);
		String options = "Nenhuma_NULL;COMM_COMM;USB_USB";
		for (Device device : Main.devicesList) {
			if (device.isCatraca()) {
				options = options + ";" + device.getName() + "_" + device.getIdentifier().replace(";", "$");
			}
		}
		config.setComboboxValues(options);
		return configurationGroups;
	}
	
	
	@Override
	public Set<Integer> getRegisteredUserList() throws Exception {
		return null;
	}

	
	@Override
	public String cadastrateUser(PedestrianAccessEntity athleteAccessEntity){
		return "";
	}
	
	
	@Override
	public String removeUser(PedestrianAccessEntity athleteAccessEntity) {
		HibernateUtil.removeTemplates(athleteAccessEntity.getId());
		HibernateUtil.removeTemplatesFromServer(athleteAccessEntity.getId());
		if (Main.broadcastServer != null)
			Main.broadcastServer.sendMessage(new BroadcastMessageTO(BroadcastMessageType.REMOVE_TEMPLATES, athleteAccessEntity.getId()));
		return "";
	}
	
	
	private void setMessage(String message, String tipo) {
		if (DeviceMode.ENROLLMENT.equals(mode) && biometricDialog != null) {
			biometricDialog.setMessage(message, tipo);
		}
		else if (DeviceMode.VERIFICATION.equals(mode) && athleteScreen != null) {
			athleteScreen.setErroDigital(message);
		}
	}
	
	private static byte[] convertSampleToByteArray(DPFPSample sample){
		Image sampleImage = DPFPGlobal.getSampleConversionFactory().createImage(sample).
				getScaledInstance(150, 200, Image.SCALE_SMOOTH);
		BufferedImage bufferedImage = new BufferedImage(
				sampleImage.getWidth(null), sampleImage.getHeight(null),
		        BufferedImage.OPAQUE);
		Graphics2D g = bufferedImage.createGraphics();
		g.drawImage(sampleImage, 0, 0, null);
		g.dispose();
		ByteArrayOutputStream buff = new ByteArrayOutputStream();
        try {
            ImageIO.write(bufferedImage, "JPG", buff);
        } catch (IOException e) {
        	e.printStackTrace();
        }
		return buff.toByteArray();
	}
	
	public String getSerialNumber() {
		return serialNumber.replace("{", "").replace("}", "");
	}

	public void setSerialNumber(String serialNumber) {
		this.serialNumber = serialNumber;
	}
	
	
	@SuppressWarnings("unchecked")
	public void createTemplateDatabase() {
		templateDatabase = new HashMap<Long, List<DPFPTemplate>>();
		List<TemplateEntity> templatesList = (List<TemplateEntity>) HibernateUtil.getResultList(TemplateEntity.class, "TemplateEntity.findAllNaoLocalComplete");
		if (templatesList != null && !templatesList.isEmpty()) {
			for (TemplateEntity templateEntity : templatesList) {
				addTemplateToTemplateDatabase(templateEntity);
			}
		}
	}
	
	public void addTemplateToTemplateDatabase(TemplateEntity templateEntity){
    	try {
    		if (templateDatabase == null)
    			return;
    		
    		DPFPTemplate template = DPFPGlobal.getTemplateFactory().createTemplate(templateEntity.getTemplate());
    		Long id = templateEntity.getPedestrianAccess().getId();
    		
    		List<DPFPTemplate> templates = templateDatabase.get(id);
        	if (templates == null)
        		templates = new ArrayList<DPFPTemplate>();
        	templates.add(template);
        	templateDatabase.put(id, templates);
		}
		catch (Exception e) {
			e.printStackTrace();
		}
    }
	
}
