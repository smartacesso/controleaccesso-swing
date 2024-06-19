package com.protreino.services.devices;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;

import javax.swing.JOptionPane;
import javax.swing.SwingWorker;

import org.apache.commons.codec.binary.Base64;

import com.protreino.services.client.SmartAcessoClient;
import com.protreino.services.constants.Configurations;
import com.protreino.services.entity.Aso15DefJNA;
import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.enumeration.BroadcastMessageType;
import com.protreino.services.enumeration.DeviceMode;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.enumeration.FieldType;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.enumeration.NotificationType;
import com.protreino.services.enumeration.VerificationResult;
import com.protreino.services.main.Main;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.to.BroadcastMessageTO;
import com.protreino.services.to.ConfigurationGroupTO;
import com.protreino.services.to.ConfigurationTO;
import com.protreino.services.usecase.ProcessAccessRequestUseCase;
import com.protreino.services.utils.Utils;
import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.ptr.ByteByReference;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.LongByReference;

@SuppressWarnings("serial")
public class LcDevice extends Device {
	
	private int amostrasColetadas = 0;
	
	private Memory memorySpaceStTemplates;
	private Memory memorySpaceStRegTem;
	private Aso15DefJNA interfaceJna;
	private static final int QUANTIDADE_TEMPLATES = 3;
	private Integer nivelConfianca;
	
	private Boolean createNotification;
	
	public LcDevice(DeviceEntity deviceEntity) {
		this(deviceEntity.getIdentifier(), deviceEntity.getConfigurationGroupsTO());
		this.deviceEntity = deviceEntity;
		this.name = deviceEntity.getName();
		this.location = deviceEntity.getLocation();
		this.desiredStatus = deviceEntity.getDesiredStatus();
		this.defaultDevice = deviceEntity.getDefaultDevice();
		this.athleteScreenConfig = deviceEntity.getAthleteScreenConfig();
	}
	
	public LcDevice(String identifier) {
		this(identifier, null);
	}

	public LcDevice(String identifier, List<ConfigurationGroupTO> configurationGroups) {
		this.manufacturer = Manufacturer.LC_DEVICE;
		String partes[] = identifier.split(";");
		this.identifier = identifier;
		this.name = "Leitor LC " + partes[0];

		if (configurationGroups != null)
			this.configurationGroups = configurationGroups;
		else
			createDefaultConfiguration();
		
		createConfigurationMap();
		
		if (Platform.isWindows()){
			if ("32".equals(Utils.getJvmArchitecture())){
				this.interfaceJna =  (Aso15DefJNA) Native.loadLibrary(Utils.getInstallationPath() 
						+ "lib/SPL_ASO15", Aso15DefJNA.class);
			
			} else {
				Utils.createNotification("Leitor LC requer JVM 32 bits", NotificationType.BAD);
				this.interfaceJna = null;
			}
		} else {
			Utils.createNotification("Leitor LC requer sistema Windows", NotificationType.BAD);
			this.interfaceJna = null;
		}
	}

	@Override
	public void connect(String... args) throws Exception {
		
		if(inicializarDispositivo()) {
			setStatus(DeviceStatus.CONNECTED);
			workerEnabled = true;
			setMode(DeviceMode.VERIFICATION);
			
			nivelConfianca = getConfigurationValueAsInteger("Nível de confiança");
			
			worker = new SwingWorker<Void, Void>(){
				@Override
				protected Void doInBackground() throws Exception {
					while (workerEnabled) {
						try {
							int resp = interfaceJna.SFEP_CaptureFingerImage(1);
							if (resp != Aso15DefJNA.RES_OK) {
								continue;
							}
							
							if (DeviceMode.ENROLLMENT.equals(mode)) {
	                			processSampleForEnrollment(null);
            				
							} else {
								if (athleteScreen != null) {
									if (!athleteScreen.isTelaTravada()) {
        								athleteScreen.digitalObtida(null);
        								createNotification = false;
	        							processAccessRequest(null);
	        							athleteScreen.requisicaoPorDigital(null, verificationResult, allowedUserName, matchedAthleteAccess);
	        							allowedUserName = "";
	        							matchedAthleteAccess = null;
        							}
								
								} else {
									processAccessRequest(null);
									createNotification = true;
									if (VerificationResult.ALLOWED.equals(verificationResult)
											|| VerificationResult.TOLERANCE_PERIOD.equals(verificationResult))
										allowAccess();
									else
										denyAccess();
								}
            				}
						} catch (Exception e) {
							e.printStackTrace();
						}
					}
					
					return null;
				}
			};
			worker.execute();
			
		} else {
			setStatus(DeviceStatus.DISCONNECTED);
		}
	}
	
	@Override
	public void createDefaultConfiguration() {
		List<ConfigurationTO> geralConfigurations = new ArrayList<ConfigurationTO>();
		geralConfigurations.add(new ConfigurationTO("Catraca vinculada", "Nenhuma_NULL", FieldType.COMBOBOX, "Nenhuma_NULL;COMM_COMM;USB_USB"));
		geralConfigurations.add(new ConfigurationTO("Nível de confiança", "1", FieldType.NUMERIC_LIST, "1;1;10"));
		
		configurationGroups = new ArrayList<ConfigurationGroupTO>();
		configurationGroups.add(new ConfigurationGroupTO("Geral", geralConfigurations));
	}

	@Override
	public void sendConfiguration() throws Exception {
		String opcaoCatracaVinculada = getConfigurationValue("Catraca vinculada");
		if ("COMM".equals(opcaoCatracaVinculada))
			catracaVinculada = Manufacturer.COMM.getNewDevice("");
		else if ("USB".equals(opcaoCatracaVinculada)) {
			catracaVinculada = Manufacturer.USB.getNewDevice("");
			catracaVinculada.connect("");
		
		} else if (!"NULL".equals(opcaoCatracaVinculada)) {
			String identifier = opcaoCatracaVinculada.replace("$", ";");
			for (Device device : Main.devicesList) {
				if (identifier.equals(device.getIdentifier())) {
					catracaVinculada = device;
					break;
				}
			}
		} else
			catracaVinculada = null;
		
		String[] partes = athleteScreenConfig.split("%");
		boolean openAthleteScreenOnInit = Boolean.valueOf(partes[2]);
		boolean fullScreenAthleteScreen = Boolean.valueOf(partes[3]);
		boolean focusFieldAthleteScreen = Boolean.valueOf(partes[4]);
		setAthleteScreenConfig(getConfigurationValue("Catraca vinculada") + "%" + "null" + "%"
				+ openAthleteScreenOnInit + "%" + fullScreenAthleteScreen + "%" + focusFieldAthleteScreen + "%");
	}

	@Override
	public void allowAccess() {
		if (catracaVinculada != null) {
			if (catracaVinculada.isConnected()) {
				catracaVinculada.setVerificationResult(verificationResult);
				catracaVinculada.setAllowedUserName(allowedUserName);
				catracaVinculada.allowAccess();
			
			} else {
				Utils.createNotification("Catraca " + catracaVinculada.getName() + " desconectada.", NotificationType.BAD);
			}
		}
		allowedUserName = "";
		Utils.sleep(2000);
	}

	@Override
	public void denyAccess() {
		Utils.sleep(1000);
	}
	
	@Override
	public void processAccessRequest(Object obj) {
		final ProcessAccessRequestUseCase processAccessRequestUseCase = new ProcessAccessRequestUseCase();
		
		Memory memorySpace = new Memory(Aso15DefJNA.SFEP_UFPDATA_SIZE);
		memorySpace.clear();
		int resp = interfaceJna.SFEP_CreateTemplate(memorySpace);
		if (resp != Aso15DefJNA.RES_OK) {
			return;
		}
		
		IntByReference idUser = new IntByReference();
		ByteByReference fingerNum = new ByteByReference();
		ByteByReference manager = new ByteByReference();
		
		resp = interfaceJna.SFEP_Identify(memorySpace, idUser, fingerNum, manager, (byte) nivelConfianca.byteValue());
		
		if(resp == Aso15DefJNA.RES_OK && idUser != null) {
			Integer id = idUser.getValue();
			PedestrianAccessEntity pedestre = (PedestrianAccessEntity) HibernateAccessDataFacade
							.getSingleResultById(PedestrianAccessEntity.class, Long.valueOf(id));
			
			if (pedestre != null) {
				Object[] retorno = processAccessRequestUseCase.processAccessRequest(pedestre.getId().toString(), location, 
						createNotification, getConfigurationValueAsBoolean("Ignorar regras de acesso"));
				
				this.verificationResult = (VerificationResult) retorno[0];
				this.allowedUserName = (String) retorno[1];
				this.matchedAthleteAccess = (PedestrianAccessEntity) retorno[2];
				
			} else {
				this.verificationResult = VerificationResult.NOT_FOUND;
				if (createNotification) {
					Utils.createNotification("Digital Não encontrada.", NotificationType.BAD);
				}
			}
		
		} else {
			this.verificationResult = VerificationResult.NOT_FOUND;
			if (createNotification) {
				Utils.createNotification("Digital Não encontrada.", NotificationType.BAD);
			}
		}
	}
	
	@Override
	public void processSampleForEnrollment(Object obj) {
		
		Memory memorySpace = new Memory(Aso15DefJNA.SFEP_UFPDATA_SIZE);
		memorySpace.clear();
		int resp = interfaceJna.SFEP_CreateTemplate(memorySpace);
		if (resp != Aso15DefJNA.RES_OK) {
			cancelaColeta("Ocorreu um erro no template durante a leitura.");
			return;
		}
		
		//adicionar na variavel da classe
		int i = amostrasColetadas * Aso15DefJNA.SFEP_UFPDATA_SIZE;
		for (byte b : memorySpace.getByteArray(0, Aso15DefJNA.SFEP_UFPDATA_SIZE)) {
			memorySpaceStTemplates.setByte(i, b);
			i++;
		}

		if(amostrasColetadas >= QUANTIDADE_TEMPLATES - 1) {
			resp = interfaceJna.SFEP_GetTemplateForRegister(memorySpaceStTemplates, memorySpaceStRegTem);
			if (resp != Aso15DefJNA.RES_OK) {
				cancelaColeta("Ocorreu um erro na geraÃ§Ã£o de template.");
				return;
			}
			
			//adiciona no banco de dados da DLL
			LongByReference idUser = new LongByReference(biometricDialog.acesso.getId().intValue());
//			String dedoString = (String) biometricDialog.fingerComboBox.getSelectedItem();
//			ByteByReference dedo = new ByteByReference((byte) (Finger.valueFromImport(dedoString).ordinal() + 1));
//			ByteByReference user = new ByteByReference((byte) 1);
//			
//			resp = interfaceJna.SFEP_Enroll(memorySpaceStRegTem, idUser, dedo, user);
//			if(resp != Aso15DefJNA.RES_OK) {
//				cancelaColeta("Erro ao registrar digital do pedestre.");
//				return;
//			}
			
			byte[] stRegTem = new byte[Aso15DefJNA.SFEP_UFPDATA_SIZE];
			stRegTem = memorySpaceStRegTem.getByteArray(0, amostrasColetadas * Aso15DefJNA.SFEP_UFPDATA_SIZE);
			biometricDialog.saveTemplates(stRegTem);
			biometricDialog.finishCollect();
			
			//verifica para inserir templates nas catracas TopData LC
			System.out.println(sdf.format(new Date()) + "   Adicionando template nas outras catracas...");
			for(Device d : Main.devicesList) {
				if(d != null 
						&& d instanceof TopDataDevice) {
					//verifica se pode adicionar
					TopDataDevice topData = (TopDataDevice)d;
					if(topData.modeloLC) {
						
						//adiciona bytes no template
						byte [] template1  = new byte[502];
						byte [] template2  = new byte[502];
						
						extracTopDataTemplate(stRegTem, template1, template2);
						
						String tStr = Base64.encodeBase64String(template2);
						//String tStr1 = Base64.encodeBase64String(template1);
						//verificar se segunda estÃ¡ vazia
						if(tStr.startsWith("AAAQAAAAAAAAAAA") ) {
							template2 = null;
						}
						
					
						
						topData.insereUserLC((long)idUser.getValue(), template1, template2);
						
					}
				}
			}
			
			Utils.sleep(1000);
			amostrasColetadas = 0;
			setMode(DeviceMode.VERIFICATION);
			return;
		
		} else {
			biometricDialog.updateSampleStatus(null, amostrasColetadas+1);
			Utils.sleep(1000);
			biometricDialog.fingerGone();
		}
		
		amostrasColetadas++;
	}

	public static void extracTopDataTemplate(byte[] stRegTem, byte[] template1, byte[] template2) {
		int bytes = 0;
		byte [] cabecalho = new byte[] {0, 0, 16, 0};
		//adiciona cabeÃ§alho
		for (byte b : cabecalho) {
			template1[bytes] = b;
			template2[bytes] = b;
			bytes++;
		}
		//adiciona template 1
		int lcBytes = 0;
		for (byte b : stRegTem) {
			if(lcBytes < 498) {
				//atÃ© aqui Ã© o template 1
				template1[bytes] = b; 
				bytes++;
			}else {
				if(lcBytes == 498)
					bytes = 4;
				template2[bytes] = b; 
				bytes++;
			}
			lcBytes++;
		}
	}
	
	private void cancelaColeta(String message) {
		Utils.sleep(1000);

		setMessage(message, "erro");

		Object[] options = {"OK"};
		JOptionPane.showOptionDialog(biometricDialog.instance, message, "Biometria cancelada",
				JOptionPane.PLAIN_MESSAGE, JOptionPane.PLAIN_MESSAGE, null, options, options[0]);

		amostrasColetadas = 0;
		setMode(DeviceMode.VERIFICATION);
		biometricDialog.dispose();
	}
	
	private boolean inicializarDispositivo() {
		
		try {
			informarCaminhoDatabaseDigitais();
			iniciarComunicacaoScanner();
			configurarBrilhoHamster();
			
			memorySpaceStTemplates = new Memory(QUANTIDADE_TEMPLATES * Aso15DefJNA.SFEP_UFPDATA_SIZE);
			memorySpaceStRegTem = new Memory(QUANTIDADE_TEMPLATES * Aso15DefJNA.SFEP_UFPDATA_SIZE);
			
			return true;

		} catch (Exception e) {
			System.out.println(e.getMessage());
			return false;
		}
	}
	
	private void finalizarComunicacaoScanner() {
		interfaceJna.SFEP_Uninitialize();
	}
	
	private void informarCaminhoDatabaseDigitais() throws Exception {
		Integer resposta = interfaceJna.SFEP_SetDatabasePath(Configurations.LC_DATABASE_PATH);
		
		if (resposta != Aso15DefJNA.RES_OK){
			throw new Exception(resposta.toString());
		}
	}
	
	private void iniciarComunicacaoScanner() throws Exception {
		Integer resposta = interfaceJna.SFEP_Initialize();
		
		if (resposta != Aso15DefJNA.RES_OK){
			throw new Exception(resposta.toString());
		}
	}
	
	private void configurarBrilhoHamster() throws Exception {
		Integer resposta = interfaceJna.SFEP_SetBrightness((byte) 130);
		
		if (resposta != Aso15DefJNA.RES_OK){
			throw new Exception(resposta.toString());
		}
	}
	
	private void setMessage(String message, String tipo) {
		if (DeviceMode.ENROLLMENT.equals(mode) && biometricDialog != null) {
			biometricDialog.setMessage(message, tipo);
		
		} else if (DeviceMode.VERIFICATION.equals(mode) && athleteScreen != null) {
			athleteScreen.setErroDigital(message);
		}
	}

	@Override
	public Set<Integer> getRegisteredUserList() throws Exception {
		return null;
	}

	@Override
	public String cadastrateUser(PedestrianAccessEntity athleteAccessEntity) {
		return null;
	}

	@Override
	public String removeUser(PedestrianAccessEntity athleteAccessEntity) {
		for(int i = 1; i <= 10; i++) {
			int resp = interfaceJna.SFEP_RemoveTemplate(athleteAccessEntity.getId().intValue(), (byte) i);
			System.out.println(resp);
		}
		
		//se for LC
		System.out.println(sdf.format(new Date()) + "   Remove template nas outras catracas...");
		for(Device d : Main.devicesList) {
			if(d != null && d instanceof TopDataDevice ) {
				TopDataDevice topData = (TopDataDevice)d;
				if(topData.modeloLC) 
					topData.removeUserLCDevice(athleteAccessEntity);
			}
		}
		
		HibernateAccessDataFacade.removeTemplates(athleteAccessEntity.getId());
		SmartAcessoClient.removeTemplatesFromServer(athleteAccessEntity.getId());
		if (Main.broadcastServer != null) {
			Main.broadcastServer.sendMessage(new BroadcastMessageTO(BroadcastMessageType.REMOVE_TEMPLATES, athleteAccessEntity.getId()));
		}

		return "";
	}
	
	@Override
	public void disconnect(String... args) throws Exception {
		if (worker != null)
			worker.cancel(true);
		
		workerEnabled = false;
		Utils.sleep(1500);
		
		finalizarComunicacaoScanner();
		
		setStatus(DeviceStatus.DISCONNECTED);
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
	public void saveConfigurations() {
		try {
			if (!isConnected())
				sendConfiguration();
		} catch (Throwable e) {
			e.printStackTrace();
			Main.mainScreen.addEvento("Erro ao salvar as configurações: " + e.getMessage());
		}
		
		super.saveConfigurations();
	}
	
	@Override
	public String setMode(DeviceMode mode) {
		this.mode = mode;
		if (DeviceMode.ENROLLMENT.equals(mode)){
			amostrasColetadas  = 0;
		}
		return "";
	}
}
