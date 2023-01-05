package com.protreino.services.devices;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;

import javax.swing.SwingWorker;

import org.apache.commons.codec.binary.Base64;
import com.nitgen.SDK.BSP.NBioBSPJNI;
import com.nitgen.SDK.BSP.NBioBSPJNI.FIR_HANDLE;
import com.nitgen.SDK.BSP.NBioBSPJNI.FIR_PURPOSE;
import com.nitgen.SDK.BSP.NBioBSPJNI.INPUT_FIR;
import com.nitgen.SDK.BSP.NBioBSPJNI.IndexSearch;
import com.nitgen.SDK.BSP.NBioBSPJNI.WINDOW_OPTION;
import com.nitgen.SDK.BSP.NBioBSPJNI.WINDOW_STYLE;
import com.nitgen.SDK.BSP.NBioBSPJNI.EXPORT_MINCONV_TYPE;
import com.nitgen.SDK.BSP.NBioBSPJNI.FIR;
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

@SuppressWarnings("serial")
public class NitgenDevice extends Device {
	
	private short nameId;
	private short instance;
	private NBioBSPJNI bsp;
	private IndexSearch indexSearchEngine;
	
	private int amostrasColetadas = 0;
	private FIR_HANDLE capturedFIRHandle;
	private INPUT_FIR storedInputFIR;
	
	private byte[] template;
	private Boolean createNotification;
	
	public NitgenDevice(DeviceEntity deviceEntity){
		this(deviceEntity.getIdentifier(), deviceEntity.getConfigurationGroupsTO());
		this.deviceEntity = deviceEntity;
		this.name = deviceEntity.getName();
		this.location = deviceEntity.getLocation();
		this.desiredStatus = deviceEntity.getDesiredStatus();
		this.defaultDevice = deviceEntity.getDefaultDevice();
		this.athleteScreenConfig = deviceEntity.getAthleteScreenConfig();
		
		if(deviceEntity.getConfigurationGroupsTO() != null) {
			List<ConfigurationTO> geral = deviceEntity.getConfigurationGroupsTO().get(0).getConfigurations();
			FORA: for (ConfigurationTO to : geral) {
				if(to.getName().equals("Catraca vinculada")) {
					for(Device d : Main.devicesList) {
						if(d.getIdentifier().equals(to.getValue().replace("$", ";"))) {
							this.catracaVinculada = d;
							//if(d.getDeviceCard() != null)
							//	d.getDeviceCard().setCatracaVinculada(true, this.getName());
							break FORA;
						}
					}
				}
			}
		}
	}
	
	public NitgenDevice(String identifier){
		this(identifier, null);
	}
	
	public NitgenDevice(String identifier, List<ConfigurationGroupTO> configurationGroups){
		this.manufacturer = Manufacturer.NITGEN;
		String partes[] = identifier.split(";");
		this.identifier = identifier;
		this.name = partes[0];
		this.nameId = Short.parseShort(partes[1]);
		this.instance = Short.parseShort(partes[2]);
		if (configurationGroups != null)
			this.configurationGroups = configurationGroups;
		else
			createDefaultConfiguration();
		createConfigurationMap();
	}

	private static boolean validando = false;
	
	@Override
	public void connect(String... args) throws Exception {
		bsp = new NBioBSPJNI();
		bsp.OpenDevice(nameId, instance);
		
		if (bsp.GetOpenedDeviceID() == nameId) {
			setStatus(DeviceStatus.CONNECTED);
			startIndexSearchEngine();
			workerEnabled = true;
			setMode(DeviceMode.VERIFICATION);
			
			worker = new SwingWorker<Void, Void>() {
				@Override
				protected Void doInBackground() throws Exception {
					
					WINDOW_OPTION winOption = bsp.new WINDOW_OPTION();
        			winOption.WindowStyle = WINDOW_STYLE.INVISIBLE;
        			
        			
					while (workerEnabled) {
						if(validando)
							continue;
						validando = true;
						try {
							
							capturedFIRHandle = bsp.new FIR_HANDLE();
							bsp.Capture(FIR_PURPOSE.IDENTIFY, capturedFIRHandle, 1000, null, winOption);
							if (!bsp.IsErrorOccured()) {
								if (DeviceMode.ENROLLMENT.equals(mode)) {
									
									processSampleForEnrollment(null);
								
								} else {
									if (athleteScreen != null) {
										// ACESSO FEITO PELA TELA DO ALUNO
										// caso tenha alguma informacao exibida na tela, aguarda a limpeza da tela antes de mostrar outra coisa
            							if (!athleteScreen.isTelaTravada()) {
            								athleteScreen.digitalObtida(null);
            								createNotification = false;
    	        							processAccessRequest(null);
    	        							athleteScreen.requisicaoPorDigital(null, verificationResult, allowedUserName, matchedAthleteAccess);
    	        							allowedUserName = "";
    	        							matchedAthleteAccess = null;
            							}
            						
									} else {
            							// ACESSO DIRETO PELO LEITOR
            							createNotification = true;
            							
            							processAccessRequest(null);
            							if (VerificationResult.ALLOWED.equals(verificationResult)
            									|| VerificationResult.TOLERANCE_PERIOD.equals(verificationResult))
            								allowAccess();
            							else
            								denyAccess();
            							
            						}
                				}
                			
							} else {
                				if (bsp.GetErrorCode() != 516)
                					System.out.println("Erro durante a coleta. Código: " + bsp.GetErrorCode());
                			}
						
						} catch (Throwable e) {
		                    e.printStackTrace();
		                }finally {
		                	validando = false;
		                }
					}
					return null;
				}
			};
			worker.execute();
			
			if (getAthleteScreenConfig() != null) {
				String[] partes = getAthleteScreenConfig().split("%");
				boolean openAthleteScreenOnInit = Boolean.valueOf(partes[2]);
				if (openAthleteScreenOnInit && isConnected())
					getDeviceCard().openAthleteScreen();
			}
		} else
			throw new Exception("Não foi possóvel conectar.");
	}

	@Override
	public void disconnect(String... args) throws Exception {
		if (worker != null)
			worker.cancel(true);
		workerEnabled = false;
		Utils.sleep(1500);
		
		if (bsp != null) {
            bsp.CloseDevice(nameId, instance);
            bsp.dispose();
            bsp = null;
        }
		
		if (indexSearchEngine != null)  {
			indexSearchEngine.dispose();
			indexSearchEngine = null;
        }
		setStatus(DeviceStatus.DISCONNECTED);
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
	public void saveConfigurations(){
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
	public String setMode(DeviceMode mode) {
		this.mode = mode;
		if (DeviceMode.ENROLLMENT.equals(mode)){
			amostrasColetadas = 0;
		}
		return "";
	}
	
	@Override
	public void denyAccess() {
		Utils.sleep(1000);
		// nao faz nada
	}

	@Override
	public void processSampleForEnrollment(Object obj) {
		try {
			// verifica a qualidade da imagem
			FIR fir = bsp.new FIR();
			bsp.GetFIRFromHandle(capturedFIRHandle, fir);
			
			if (fir.Header.Quality < 100) {
				setMessage("Qualidade ruim. Tente novamente.", "erro");
				return;
			}
			
			biometricDialog.fingerTouched();
			
			if (amostrasColetadas == 0) {
	            storedInputFIR = bsp.new INPUT_FIR();
	            storedInputFIR.SetFIRHandle(capturedFIRHandle);
        		amostrasColetadas++;
			
			} else {
				INPUT_FIR capturedInputFIR = bsp.new INPUT_FIR();
				capturedInputFIR.SetFIRHandle(capturedFIRHandle);
				NBioBSPJNI.FIR_HANDLE newFIRHandle = bsp.new FIR_HANDLE();
        		int retorno = bsp.CreateTemplate(capturedInputFIR, storedInputFIR, newFIRHandle, null);
        		if (retorno == 0) {
            		storedInputFIR.SetFIRHandle(newFIRHandle);
        			amostrasColetadas++;
        		} else {
					setMessage("Ocorreu um erro no template durante a leitura.", "erro");
					System.out.println("Erro durante o processamento da amostra: " + retorno);
				}
	    	}
			
	    	if (amostrasColetadas < manufacturer.getSamplesCount()) {
				biometricDialog.updateSampleStatus(null, amostrasColetadas);
				Utils.sleep(1000);
				biometricDialog.fingerGone();

	    	} else {
	    		NBioBSPJNI.Export export = bsp.new Export();
				NBioBSPJNI.Export.DATA exportData = export.new DATA();
				export.ExportFIR(storedInputFIR, exportData, EXPORT_MINCONV_TYPE.FIM01_HV);
				template = exportData.FingerData[0].Template[0].Data;
				biometricDialog.saveTemplates(template);
				addTemplateToIndexSearch(biometricDialog.getTemplateEntity());
				biometricDialog.finishCollect();

				Utils.sleep(1000);
			}
    	} catch (Throwable e){
    		e.printStackTrace();
    		setMessage("Ocorreu um erro no template durante a leitura.", "erro");
    		Utils.createNotification("Ocorreu um erro ao processar amostra: " + e.getMessage(), NotificationType.BAD);
    	}
	}

	@Override
	public void processAccessRequest(Object obj) {
		// Primeiro é feito o processo de match para identificar o usuário.
		// Após a identificação é verificado se o acesso é permitido.
		try {
			// digital coletada agora
			INPUT_FIR capturedInputFIR = bsp.new INPUT_FIR();
			capturedInputFIR.SetFIRHandle(capturedFIRHandle);
			
			Integer nivelSeguranca = getConfigurationValueAsInteger("Nível de segurança do reconhecimento");
			if (nivelSeguranca == 0) {
				nivelSeguranca = 6;
			}
			
			
			int nMaxSearchTime = 500;
			NBioBSPJNI.IndexSearch.FP_INFO fpInfo = indexSearchEngine.new FP_INFO();
			indexSearchEngine.Identify(capturedInputFIR, nivelSeguranca, fpInfo, nMaxSearchTime);
			
			if (bsp.IsErrorOccured()) {
				System.out.println(sdf.format(new Date()) + "  Erro no IndexSearch: " + bsp.GetErrorCode());
				if (bsp.GetErrorCode() == NBioBSPJNI.ERROR.NBioAPIERROR_INDEXSEARCH_IDENTIFY_FAIL)  {
					this.verificationResult = VerificationResult.NOT_FOUND;
					if (createNotification)
						Utils.createNotification("Digital não encontrada.", NotificationType.BAD);
				
				} else {
					verificationResult = VerificationResult.ERROR;
					if (createNotification)
						Utils.createNotification("Erro na verificação. Código: " + bsp.GetErrorCode(), NotificationType.BAD);
				}
				
			} else {
				Integer idTemplate = fpInfo.ID;
				TemplateEntity template = (TemplateEntity) HibernateUtil.getSingleResultById(TemplateEntity.class, idTemplate.longValue());
				
				if (template != null && template.getPedestrianAccess() != null) {
					Object[] retorno = HibernateUtil.processAccessRequest(template.getPedestrianAccess().getId().toString(), location, createNotification, false);
					this.verificationResult = (VerificationResult) retorno[0];
					this.allowedUserName = (String) retorno[1];
					this.matchedAthleteAccess = (PedestrianAccessEntity) retorno[2];
				}
				else {
					this.verificationResult = VerificationResult.NOT_FOUND;
					if (createNotification)
						Utils.createNotification("Digital não encontrada.", NotificationType.BAD);
				}
			}
			
		} catch (Throwable e) {
			e.printStackTrace();
			verificationResult = VerificationResult.ERROR;
			if (createNotification)
				Utils.createNotification("Erro na verificação", NotificationType.BAD);
		}
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
		
		//atualizar catracas envolvidas, quando necessário.
		for (Device d : Main.devicesList) {
			if(d instanceof TopDataDevice
					&& d.isConnected()) {
				TopDataDevice topData = (TopDataDevice) d;
				topData.removeDigitalInner(true, athleteAccessEntity);
			}
		}
		
		HibernateUtil.removeTemplates(athleteAccessEntity.getId());
		HibernateUtil.removeTemplatesFromServer(athleteAccessEntity.getId());
		if (Main.broadcastServer != null)
			Main.broadcastServer.sendMessage(new BroadcastMessageTO(BroadcastMessageType.REMOVE_TEMPLATES, athleteAccessEntity.getId()));
		return "";
	}
	
	private void setMessage(String message, String tipo) {
		if (DeviceMode.ENROLLMENT.equals(mode) && biometricDialog != null) {
			biometricDialog.setMessage(message, tipo);
		
		} else if (DeviceMode.VERIFICATION.equals(mode) && athleteScreen != null) {
			athleteScreen.setErroDigital(message);
		}
	}
	
	public String getNameIdAndInstance() {
		return Short.toString(nameId) + "-" + Short.toString(instance);
	}
	
	@SuppressWarnings("unchecked")
	private void startIndexSearchEngine(){
		
		if (DeviceMode.ENROLLMENT.equals(mode))
			return;
		
		if (validando)
			return;
		
		indexSearchEngine = bsp.new IndexSearch();
		indexSearchEngine.ClearDB();
		
        List<TemplateEntity> templatesList = (List<TemplateEntity>) HibernateUtil.getResultList(TemplateEntity.class, "TemplateEntity.findAllNaoRemovido");
		if (templatesList != null && !templatesList.isEmpty()) {
			int count = 0;
			System.out.println("tamanho do template list " + templatesList.size());
			for (TemplateEntity templateEntity : templatesList) {
				
				if (templateEntity.getTemplate().length != 404) { // Insere apenas templates Nitgen
					continue;
				}
				
				String template = Base64.encodeBase64String(templateEntity.getTemplate());
				if (!template.contains("AAAAAAAAAAAA")) {
					addTemplateToIndexSearch(templateEntity);
				
				} else {
					System.out.println("Tem AAAAA");
				}
			}
			System.out.println("Quantas biometrias foram inseridas " + count);
		}
	}
	
	private void addTemplateToIndexSearch(TemplateEntity templateEntity) {
		try {
			int exportType = NBioBSPJNI.EXPORT_MINCONV_TYPE.FIM01_HV;
			INPUT_FIR storedInputFIR = bsp.new INPUT_FIR();
			FIR_HANDLE storedFIRHandle = bsp.new FIR_HANDLE();
			NBioBSPJNI.Export export = bsp.new Export();
			
			if(templateEntity != null) {
				export.ImportFIR(templateEntity.getTemplate(), templateEntity.getTemplate().length, exportType,
						NBioBSPJNI.FIR_PURPOSE.IDENTIFY, storedFIRHandle);
				storedInputFIR.SetFIRHandle(storedFIRHandle);
				indexSearchEngine.AddFIR(storedInputFIR, templateEntity.getId().intValue(),
						indexSearchEngine.new SAMPLE_INFO());
			}
			
			if (bsp.IsErrorOccured()) {
				System.out.println("Erro ao adicionar template na IndexSearchEngine. Erro: " + bsp.GetErrorCode());
			}

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}
