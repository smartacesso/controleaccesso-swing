package com.protreino.services.main;

import com.google.gson.*;
import com.google.gson.reflect.TypeToken;
import com.protreino.services.constants.Configurations;
import com.protreino.services.constants.Origens;
import com.protreino.services.constants.Tipo;
import com.protreino.services.devices.*;
import com.protreino.services.entity.*; 
import com.protreino.services.enumeration.*;
import com.protreino.services.exceptions.ErrorOnSendLogsToWebException;
import com.protreino.services.exceptions.HikivisionIntegrationException;
import com.protreino.services.screens.SplashScreen;
import com.protreino.services.screens.*;
import com.protreino.services.services.LuxandService;
import com.protreino.services.to.BroadcastMessageTO;
import com.protreino.services.to.EmpresaTO;
import com.protreino.services.to.PedestrianAccessTO;
import com.protreino.services.to.RegraTO;
import com.protreino.services.to.hikivision.HikivisionDeviceTO;
import com.protreino.services.to.hikivision.HikivisionDeviceTO.MatchList;
import com.protreino.services.usecase.HikivisionUseCases;
import com.protreino.services.utils.*;
import it.sauronsoftware.junique.AlreadyLockedException;
import it.sauronsoftware.junique.JUnique;
import it.sauronsoftware.junique.MessageHandler;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.io.FileUtils;
import org.springframework.scheduling.annotation.Scheduled;
import org.jnativehook.GlobalScreen;
import org.jnativehook.NativeHookException;
import org.jnativehook.keyboard.NativeKeyEvent;
import org.jnativehook.keyboard.NativeKeyListener;

import javax.swing.Timer;
import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.*;
import java.lang.reflect.Type;
import java.net.ConnectException;
import java.net.SocketTimeoutException;
import java.net.UnknownHostException;
import java.nio.file.FileSystem;
import java.nio.file.*;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.List;
import java.util.*;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

public class Main {

    public static MainScreen mainScreen;
    private static SplashScreen splash;

    public static UserEntity loggedUser = null;
    public static UserEntity internoLoggedUser = null;

    public static ServerDevice servidor;

    public static Long lastSync = 0L;
    public static Long lastSyncLog = 0L;
    public static Long lastSyncHikivision = 0l;

    public static Long lastSyncGetUsers = 0L;
    public static Long lastSyncGetEmpresas = 0L;
    public static Long lastSyncGetRegras = 0L;
    public static Long lastSyncGetParametros = 0L;
    public static Long lastSyncGetPlanos = 0L;

    public static Long lastSyncUploadPhotos = 0L;

    public static boolean updatingAthleteAccessList = false;
    public static boolean updatingLogAccessList = false;
    public static boolean updatingHikivisionAccessList = false;
    public static boolean updatingUsersAccessList = false;
    public static boolean uploadingPhotosPedestres = false;

    public static boolean isCadastrandoBiometria = false;

    public static boolean apertouF9 = false;
    public static boolean apertouF10 = false;
    public static List<Device> devicesList;
    public static List<Device> dispositivosReconectando;
    public static Device selectedDevice;
    private static String motivoLiberacao;
    public static String idAlunoEspecifico;

    private static Properties properties;
    public static String urlApplication;
    public static String nomeAplicacao;
    public static String customImageFolder;
    public static Image favicon;
    public static Color firstColor;
    public static Color secondColor;

    public static Timer timerSyncUsersAccessList;
    public static Timer timerSyncAthleteAccessList;
    public static Timer timerSyncHikivision;
    public static Timer timerSyncLogAthleteAccess;
    public static Timer timerOnline;
    public static Timer timerHidePopupMenu;
    public static java.util.Timer timerTasksOfDay;
    public static TimerTask uTimerTask;

    private static Gson gson;
    private static SystemTray systemTray;
    private static TrayIcon trayIcon;
    private static Image trayIconImageLoading;
    private static Image trayIconImage;
    public static JMenuItem releaseTicketGateMenuItem;
    public static JMenuItem updateAccessListMenuItem;
    private static JMenuItem openScreenMenuItem;

    private static PrintStream outStream;
    private static FileOutputStream fileOutputStream;
    private static PrintStream originalOut;
    private static PrintStream originalErr;
    public static String logPath;
    public static ReleaseReasonDialog releaseReasonDialog;
    public static BroadcastServer broadcastServer;
    public static TcpServer tcpServer;
    public static HikivisionTcpServer hikivisionTcpServer;
    public static SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss:sss");
    public static SimpleDateFormat sdfWithoutTIme = new SimpleDateFormat("dd/MM/yyyy");
    public static boolean desenvolvimento;
    public static boolean possuiLeitorLcAdd;
    public static boolean validandoAcesso = false;

    public static final String CHAVE_DE_INTEGRACAO_COMTELE = "Chave de integraï¿½ï¿½o Comtele";

    public static void main(String[] args) {

        // Verifica JVM
//		String jvmArchitecture = Utils.getJvmArchitecture();
//		if ("64".equals(jvmArchitecture)) {
//			Object[] options = {"OK"};
//		    JOptionPane.showOptionDialog(null, "JVM de 64 bits detectada.  necessï¿½rio uma JVM de 32 bits.","JVM 32 bits necessï¿½ria",
//		                   JOptionPane.PLAIN_MESSAGE, JOptionPane.PLAIN_MESSAGE, null, options, options[0]);
//		    System.exit(0);
//		}

        configureWorkersLimits();

        // Carrega as propriedades do ambiente
        try {
            loadProperties();
            urlApplication = properties.getProperty("url");
            nomeAplicacao = properties.getProperty("name");
            customImageFolder = properties.getProperty("imageFolder");
            String firstColorCode = properties.getProperty("firstColor");
            String secondColorCode = properties.getProperty("secondColor");
            setColors(firstColorCode, secondColorCode);

        } catch (Exception e) {
            e.printStackTrace();
            urlApplication = Configurations.URL_APPLICATION;
            nomeAplicacao = "Pro-Treino";
            customImageFolder = "pro-treino/";
            firstColor = new Color(39, 57, 74);
            secondColor = new Color(70, 178, 202);
        }

        // Verifica se existe uma instancia rodando

        boolean alreadyRunning = false;
        try {
            JUnique.acquireLock(nomeAplicacao + "_Controle_Acesso_lockInstance", new MessageHandler() {
                public String handle(String message) {
                    if (mainScreen != null)
                        mainScreen.showScreen();
                    return null;
                }
            });
        } catch (AlreadyLockedException e) {
            JUnique.sendMessage(nomeAplicacao + "_Controle_Acesso_lockInstance", "Olï¿½!");
            alreadyRunning = true;
        }

        // Inicia a aplicacao se nao tiver outra rodando
        if (!alreadyRunning) {
            new Main().init(args);
        } else {
            System.exit(0);
        }
    }

    private static void configureWorkersLimits() {
        int corePoolSize = 280;
        int maximumPoolSize = 280;
        long keepAliveTime = 1L;
        TimeUnit unit = TimeUnit.HOURS;
        BlockingQueue<Runnable> workQueue = new ArrayBlockingQueue<>(maximumPoolSize);
        sun.awt.AppContext.getAppContext().put(SwingWorker.class,
                new ThreadPoolExecutor(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue));

        sun.awt.AppContext.getAppContext().put(Thread.class,
                new ThreadPoolExecutor(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue));
    }

    private static Serializable process(Serializable msg) {
        System.out.println("Nï¿½o iniciar nova instancia: " + msg);
        if (mainScreen != null) {
            System.out.println("Exibe tela para usuï¿½rio");
            mainScreen.showScreen();
        } else {
            System.out.println("Nï¿½o hï¿½ telas para exibir");
        }
        return null;
    }

    public static void verificaValidandoAcesso() {
        int count = 0;
        while (Main.validandoAcesso) {
            Utils.sleep(2000);
            count++;
            if (count >= 3) {
                Main.validandoAcesso = false;
                break;
            }
        }
    }

    public void init(String[] args) {
        splash = new SplashScreen();
        if (System.getProperty("os.name").toLowerCase().contains("linux")) {
            try {
                Thread.sleep(1000);
            } catch (InterruptedException e1) {
                e1.printStackTrace();
            }
        }
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                try {
                    recoverArgs(args);
                    if (!desenvolvimento)
                        configLogFile();

                    System.out.println(sdf.format(new Date()) + "  Iniciando " + Main.nomeAplicacao + " Desktop - v." + Configurations.VERSION);

                    //if(System.getProperty("os.name").toLowerCase().contains("linux"))
                    //    UIManager.setLookAndFeel(new SyntheticaPlainLookAndFeel());
                    //else
                    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
                    System.getProperties().setProperty("derby.system.home", Utils.getAppDataFolder());
                    HibernateUtil.getSessionFactory().getCurrentSession();
                    Utils.defineDefaultPreferences();
                    setSystemTrayIcon();
                    configureTimers();
                    registerNativeHook();
                    recoverLoggedUser();
                    if (Boolean.TRUE.equals(Utils.getPreferenceAsBoolean("enableBroadcastServer"))) {
                    	broadcastServer = new BroadcastServer();                    	
                    }
                    if (Boolean.TRUE.equals(Utils.getPreferenceAsBoolean("enableTCPServer"))) {
                    	tcpServer = new TcpServer();                    	
                    }
                    hikivisionTcpServer = new HikivisionTcpServer();
                    //initializeLuxandSDK();
                    mainScreen = new MainScreen();
                    splash.dispose();
                    decideSeMostraTelaPrincipal();

                } catch (Exception e) {
                    e.printStackTrace();
                    Object[] options = {"OK"};
                    JOptionPane.showOptionDialog(null, "Ocorreu uma falha ao iniciar o programa."
                                    + "\nVerifique o arquivo de log para mais detalhes ou entre em contato com nosso suporte.", "Falha ao iniciar",
                            JOptionPane.PLAIN_MESSAGE, JOptionPane.PLAIN_MESSAGE, null, options, options[0]);
                    System.exit(0);

                } finally {
                    if (splash != null && splash.isVisible())
                        splash.dispose();
                }
            }
        });
    }

    private void recoverArgs(String[] args) {
        desenvolvimento = args != null && args.length > 0 && "dev".equals(args[0]);

		/*if (desenvolvimento)
			urlApplication = "http://localhost:8080";*/
    }

    private void decideSeMostraTelaPrincipal() {
        if (loggedUser == null)
            mainScreen.showScreen();
        else { 
            boolean nenhumDispositivoConectado = true;
            for (Device device : Main.devicesList) {
                if (DeviceStatus.CONNECTED.equals(device.getDesiredStatus())) {
                    nenhumDispositivoConectado = false;
                    if (DeviceStatus.DISCONNECTED.equals(device.getStatus())) {
                        System.out.println(sdf.format(new Date()) + "  Setando timer para reconectar dispositivos...");
                        new java.util.Timer().schedule(
                                new java.util.TimerTask() {
                                    @Override
                                    public void run() {
                                        reconectarDispositivos();
                                    }
                                },
                                Integer.valueOf(Utils.getPreference("timeReconectDevices")) * 1000
                        );
                        break;
                    }
                }
            }
            if (nenhumDispositivoConectado)
                mainScreen.showScreen();
        }
    }

    private void reconectarDispositivos() {
        SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
            @Override
            public Void doInBackground() {
                try {
                    dispositivosReconectando = new ArrayList<Device>();
                    for (Device device : Main.devicesList) {

                        if (DeviceStatus.CONNECTED.equals(device.getDesiredStatus())) {
                            if (DeviceStatus.CONNECTED.equals(device.getStatus()))
                                device.getDeviceCard().setStatus(DeviceStatus.CONNECTED);
                            else {
                                System.out.println(sdf.format(new Date()) + "  Reconectando: " + device.getName());
                                device.getDeviceCard().setAutoConnect(true);
                                device.getDeviceCard().getConnectButton().doClick();
                                Thread.sleep(10000);
                                dispositivosReconectando.add(device);
                            }
                        }
                    }

                    // verifica se tem dispositivos reconectando
                    if (!dispositivosReconectando.isEmpty()) {
                        // seta um timer para verificar se conectou os dispositivos
                        new java.util.Timer().schedule(
                                new java.util.TimerTask() {
                                    @Override
                                    public void run() {
                                        boolean exibirMensagemErro = false;
                                        for (Device device : dispositivosReconectando) {
                                            if (DeviceStatus.DISCONNECTED.equals(device.getStatus())
                                                    && DeviceStatus.CONNECTED.equals(device.getDesiredStatus())) {

                                                //verifica se ï¿½ topdata e se esta sincronizando
//												if(device instanceof TopDataDevice) {
//													TopDataDevice
//												}

                                                device.setDesiredStatus(DeviceStatus.DISCONNECTED);
                                                exibirMensagemErro = true;
                                            }
                                        }
                                        if (exibirMensagemErro) {
                                            if (!mainScreen.isVisible())
                                                mainScreen.showScreen();
                                            mainScreen.refresh();
                                            String html = "<html><body width='%1s'>"
                                                    + "<p>Nï¿½o conseguimos reconectar a catraca e/ou leitor, verifique os itens abaixo:"
                                                    + "<br><br>"
                                                    + "- A catraca e/ou leitor estï¿½ ligado na tomada?"
                                                    + "<br>"
                                                    + "- Os cabos estï¿½o conectados de forma correta?"
                                                    + "<br><br>"
                                                    + "Se estiver tudo OK, clique em Conectar."
                                                    + "</p></html>";
                                            int width = 400;
                                            JOptionPane.showMessageDialog(null, String.format(html, width, width),
                                                    "Falha ao reconectar", JOptionPane.PLAIN_MESSAGE);
                                        }
                                    }
                                },
                                20000
                        );
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
                return null;
            }
        };
        worker.execute();
    }


    @SuppressWarnings("unchecked")
    private void recoverLoggedUser() {
        devicesList = new ArrayList<Device>();
        loggedUser = HibernateUtil.getLoggedUser("UserEntity.findAll");
        if (loggedUser != null) {
            lastSync = loggedUser.getLastSync() != null ? loggedUser.getLastSync().getTime() : 0L;
            lastSyncLog = loggedUser.getLastSyncLog() != null ? loggedUser.getLastSyncLog().getTime() : 0L;
            lastSyncHikivision = loggedUser.getLastSyncHikivision() != null
					? loggedUser.getLastSyncHikivision().getTime() : 0l;

            lastSyncGetUsers = loggedUser.getLastSyncUser() != null
                    ? loggedUser.getLastSyncUser().getTime() : 0L;
            lastSyncGetEmpresas = loggedUser.getLastSyncEmpresa() != null
                    ? loggedUser.getLastSyncEmpresa().getTime() : 0L;
            lastSyncGetRegras = loggedUser.getLastSyncRegra() != null
                    ? loggedUser.getLastSyncRegra().getTime() : 0L;
            lastSyncGetParametros = loggedUser.getLastSyncParametro() != null
                    ? loggedUser.getLastSyncParametro().getTime() : 0L;
            lastSyncGetPlanos = loggedUser.getLastSyncPlano() != null
                    ? loggedUser.getLastSyncPlano().getTime() : 0L;

            lastSyncUploadPhotos = loggedUser.getLastSyncUploadPhotos() != null
                    ? loggedUser.getLastSyncUploadPhotos().getTime() : 0L;

            releaseTicketGateMenuItem.setEnabled(true);
            updateAccessListMenuItem.setEnabled(true);

            inicializaTimers();
            
            
         verificaRemocaoDefacesHV();
          
		
            
            
             
    	
    		
       

            //tarefas diï¿½rias

            String hora = Utils.getPreference("hourAutomaticRoutines");
            hora = (hora == null || "".equals(hora) ? "0" : hora);

            Long periodExpirar = 24L * 3600L * 1000L;
            Calendar inicio = Calendar.getInstance();
            System.out.println(" hora para reset " + hora);


            inicio.set(Calendar.HOUR_OF_DAY, Integer.parseInt(hora));
            inicio.set(Calendar.MINUTE, 0);
            inicio.set(Calendar.SECOND, 0);
            if (new Date().getTime() > inicio.getTimeInMillis()) {
                //registra pra iniciar smanhï¿½
                inicio.add(Calendar.DATE, 1);
            }
            timerTasksOfDay.scheduleAtFixedRate(uTimerTask, inicio.getTime(),
                    periodExpirar.longValue());

            List<DeviceEntity> lista = (List<DeviceEntity>) HibernateUtil.
                    getResultList(DeviceEntity.class, "DeviceEntity.findAll");
            if (lista != null && !lista.isEmpty()) {
                for (DeviceEntity deviceEntity : lista)
                    devicesList.add(deviceEntity.recoverDevice());
            }
            boolean haveDefaultDevice = false;
            for (Device device : devicesList) {
                if (device.isDefaultDevice()) {
                    haveDefaultDevice = true;
                    break;
                }
            }

            if (!haveDefaultDevice && !devicesList.isEmpty())
                devicesList.get(0).setDefaultDevice(true);

            Main.servidor = verificaSePossuiServidorAdicionado();
            Main.possuiLeitorLcAdd = verificaSePossuiLeitorLcAdicionado();
        }
    }

    private void verificaRemocaoDefacesHV() {
    	  String  dataDeRemocao = Utils.getPreference("enableRemoveHVFacesForDate");
          try {
				Date date = sdfWithoutTIme.parse(dataDeRemocao);

				java.util.Timer timer = new java.util.Timer();
	    		
	    		timer.schedule(new TimerTask() {

	    			@Override
	    			public void run() {
	    		     	LocalDateTime localDate = LocalDateTime.now().minusMonths(6);
	    	    		Date date = Date.from(localDate.atZone(ZoneId.systemDefault()).toInstant());
	    	    		
	    	    	  HashMap<String, Object> args = new HashMap<>();
	    	        args.put("DATE_HIKIVISION", date); 

	    	        @SuppressWarnings("unchecked")
	    			final List<PedestrianAccessEntity> pedestres = (List<PedestrianAccessEntity>) HibernateUtil.getResultList
	    	                (PedestrianAccessEntity.class, "PedestrianAccessEntity.findAllWhitLastAccessHikivision");
	    	        if( pedestres != null && !pedestres.isEmpty()) {
	    	        	HikiVisionIntegrationService hikivision = HikiVisionIntegrationService.getInstace();
	    	        	HikivisionUseCases hiviVisionUseCase = new HikivisionUseCases(hikivision);
	    	            List<HikivisionDeviceTO.Device> devices  = hiviVisionUseCase.listarDispositivos();
	    	            
	    	            for(PedestrianAccessEntity pedestre : pedestres) {
	    	            	for(HikivisionDeviceTO.Device device : devices) {            	
	    	            		hiviVisionUseCase.apagarUsuario(pedestre, device.getDevIndex());
	    	        		}
	    	            }
	    	        }
	    				
	    			}
	    		//	trocar os parametros
	    		
	    		},date);
			} catch (Exception e) {
				System.out.println(e.getMessage());
		
				// TODO Auto-generated catch block
			
			}
		
	}

	private void inicializaTimers() {
        timerSyncUsersAccessList.start();
        timerSyncAthleteAccessList.start();
        timerSyncLogAthleteAccess.start();
    }

    /**
     * Libera o acesso na catraca padrao conectada.
     * Caso nao seja catraca ou nao esteja conectada,
     * entao exibe uma janela para selecionar uma catraca
     * ou escolhe a unica catraca conectada.
     */
    public static void releaseAccess() {
        SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
            @Override
            public Void doInBackground() {

                if (devicesList == null || devicesList.isEmpty()) {

                    if (Utils.getPreferenceAsBoolean("registerAccessWithoutConnectedDevices")
                            && idAlunoEspecifico != null) {
                        try {
                            Object[] resultado = HibernateUtil.processAccessRequest(idAlunoEspecifico, null,
                                    Origens.ORIGEM_LIBERADO_SISTEMA, null, false, true, false);

                            VerificationResult verificationResult = (VerificationResult) resultado[0];
                            PedestrianAccessEntity matchedPedestre = (PedestrianAccessEntity) resultado[2];

                            if (VerificationResult.ALLOWED.equals(verificationResult)) {
                                LogPedestrianAccessEntity logAccess = new LogPedestrianAccessEntity(Main.loggedUser.getId(),
                                        matchedPedestre.getId(), "SYSTEM", null, motivoLiberacao);
                                Utils.createNotification(
                                        "Usuï¿½rio " + matchedPedestre.getFirstName() + " liberado pelo sistema.",
                                        NotificationType.GOOD);
                                HibernateUtil.save(LogPedestrianAccessEntity.class, logAccess);
                                Thread.sleep(1000);

                                Utils.decrementaCreditos(matchedPedestre);
                                HibernateUtil.save(PedestrianAccessEntity.class, matchedPedestre);
                            }

                        } catch (Exception e) {
                            e.printStackTrace();

                        } finally {
                            idAlunoEspecifico = null;
                        }
                        return null;
                    }

                    Utils.createNotification("Sem dispositivos conectados.", NotificationType.BAD);
                    Utils.sleep(1000);
                    apertouF9 = false;
                    apertouF10 = false;

                    return null;
                }

                selectedDevice = null;
                List<Device> dispositivoConectados = new ArrayList<Device>();
                Device defaultDevice = null;
                for (Device device : devicesList) {
                    if (device.isConnected()) {
                        dispositivoConectados.add(device);
                        if (device.isDefaultDevice())
                            defaultDevice = device;
                    }
                }

                if (defaultDevice != null) {
                    selectedDevice = defaultDevice;

                } else if (dispositivoConectados.isEmpty()) {
                    Utils.createNotification("Sem catracas conectadas.", NotificationType.BAD);

                } else if (dispositivoConectados.size() == 1) {
                    selectedDevice = dispositivoConectados.get(0);

                } else {
                    ReleaseAccessDialog releaseAccessDialog = new ReleaseAccessDialog(mainScreen, dispositivoConectados);
                    releaseAccessDialog.setVisible(true);
                    if ("OK".equals(releaseAccessDialog.getOption()))
                        selectedDevice = releaseAccessDialog.getSelectedDevice();
                }

                if (selectedDevice != null) {

                    // F10 libera a saida da catraca Toletus, via leitor ComputerID
//					if (apertouF10
//							&& !Manufacturer.TOLETUS.equals(selectedDevice.getManufacturer())
//							&& !(Manufacturer.COMPUTER_ID.equals(selectedDevice.getManufacturer())
//									&& selectedDevice.getCatracaVinculada() != null
//									&& Manufacturer.TOLETUS.equals(selectedDevice.getCatracaVinculada().getManufacturer()))) {
//						// nao faz nada para outras catracas
//						Utils.sleep(1000);
//						apertouF9 = false;
//						apertouF10 = false;
//						return null;
//					}

                    Boolean exigeSenha = Utils.getPreferenceAsBoolean("releaseAccessRequiresPassword");
                    if (exigeSenha) {
                        AutenticationDialog autenticationDialog = new AutenticationDialog(null,
                                "Digite a senha do usuï¿½rio logado \npara liberar o acesso",
                                "Aguarde, verificando senha...");
                        Boolean retornoAuthentication = null;
                        try {
                            retornoAuthentication = autenticationDialog.authenticate();

                        } catch (Exception ex) {
                            ex.printStackTrace();
                            JOptionPane.showMessageDialog(null, "Ocorreu uma falha ao validar a senha.",
                                    "Erro na validaï¿½ï¿½o", JOptionPane.PLAIN_MESSAGE);
                            return null;
                        }

                        if (retornoAuthentication == null)
                            return null;
                        if (!retornoAuthentication) {
                            JOptionPane.showMessageDialog(null, "Nï¿½o foi possssivel validar a senha, ou senha invï¿½lida",
                                    "Erro na validaï¿½ï¿½o", JOptionPane.PLAIN_MESSAGE);
                            return null;
                        }
                    }

                    String motivos = Utils.getPreferenceWithNull("releaseAccessReason");
                    motivoLiberacao = null;

                    if (!Utils.isNullOrEmpty(motivos)) {
                        ReleaseReasonDialog releaseReasonDialog = new ReleaseReasonDialog(mainScreen, motivos);
                        Main.releaseReasonDialog = releaseReasonDialog;
                        releaseReasonDialog.setVisible(true);
                        Main.releaseReasonDialog = null;

                        if ("CANCEL".equals(releaseReasonDialog.getOption())) {
                            apertouF9 = false;
                            apertouF10 = false;
                            return null;
                        }

                        motivoLiberacao = releaseReasonDialog.getReason();

                        if (Utils.isNullOrEmpty(motivoLiberacao)) {
                            Utils.createNotification("ï¿½ necessï¿½rio informar um motivo.", NotificationType.BAD);
                            Utils.sleep(1000);
                            apertouF9 = false;
                            apertouF10 = false;
                            return null;
                        }
                    }

                    // TUDO PRONTO, PODE LIBERAR
                    JButton button = mainScreen.getLiberarAcessoButton();

                    Icon previousIcon = button.getIcon();

                    try {
                        releaseTicketGateMenuItem.setEnabled(false);
                        button.setEnabled(false);
                        button.setText("Acesso permitido!");
                        mainScreen.getLiberarAcessoMenuItem().setEnabled(false);

                        String direction = apertouF9 ? Tipo.ENTRADA : (apertouF10 ? Tipo.SAIDA : Tipo.ENTRADA);
                        String equipament = selectedDevice.getFullIdentifier();

                        if (idAlunoEspecifico != null) {
                            PedestrianAccessEntity athleteAccess = (PedestrianAccessEntity) HibernateUtil
                                    .getSingleResultById(PedestrianAccessEntity.class, Long.valueOf(idAlunoEspecifico));

                            LogPedestrianAccessEntity logAccess = new LogPedestrianAccessEntity(Main.loggedUser.getId(),
                                    athleteAccess.getId(), "SYSTEM", selectedDevice.getLocation(), motivoLiberacao,
                                    direction, equipament);

                            if (Manufacturer.SERVER.equals(selectedDevice.getManufacturer())) {
                                ((ServerDevice) selectedDevice).setLogAccess(logAccess);

                            } else {
                                Utils.createNotification(
                                        "Usuï¿½rio " + athleteAccess.getFirstName() + " liberado pelo sistema.",
                                        NotificationType.GOOD);
                                HibernateUtil.save(LogPedestrianAccessEntity.class, logAccess);
                                if (Main.broadcastServer != null)
                                    Main.broadcastServer.sendMessage(
                                            new BroadcastMessageTO(BroadcastMessageType.LOG_ACCESS, logAccess));
                            }

                        } else {
                            LogPedestrianAccessEntity logAccess = new LogPedestrianAccessEntity(loggedUser.getId(),
                                    null, "LIBERADO PELO SISTEMA", selectedDevice.getLocation(), motivoLiberacao,
                                    direction, equipament);

                            if (Manufacturer.SERVER.equals(selectedDevice.getManufacturer())) {
                                ((ServerDevice) selectedDevice).setLogAccess(logAccess);

                            } else {
                                Utils.createNotification("Acesso liberado pelo sistema.", NotificationType.GOOD);
                                HibernateUtil.save(LogPedestrianAccessEntity.class, logAccess);
                            }
                        }
                        selectedDevice.allowAccess();

                        Thread.sleep(3000);

                    } catch (Exception e) {
                        e.printStackTrace();

                    } finally {
                        releaseTicketGateMenuItem.setEnabled(true);
                        button.setEnabled(true);
                        button.setIcon(previousIcon);
                        button.setText("Liberar acesso (F9)/(F10)");
                        mainScreen.getLiberarAcessoMenuItem().setEnabled(true);
                        apertouF9 = false;
                        apertouF10 = false;
                        idAlunoEspecifico = null;
                    }
                    return null;

                } else {
                    Utils.sleep(1000);
                    apertouF9 = false;
                    apertouF10 = false;
                }

                return null;
            }
        };
        worker.execute();
    }

    public static void finalizarSessao() {
        if (devicesList != null && !devicesList.isEmpty()) {
            for (Device device : devicesList) {
                if (device.isConnected()) {
                    try {
                        device.disconnect();

                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }
        }
        loggedUser = null;
        internoLoggedUser = null;
        servidor = null;
        devicesList = new ArrayList<Device>();

        if (timerSyncAthleteAccessList.isRunning())
            timerSyncAthleteAccessList.stop();
        if (timerSyncLogAthleteAccess.isRunning())
            timerSyncLogAthleteAccess.stop();
        if (timerSyncUsersAccessList.isRunning())
            timerSyncUsersAccessList.stop();

        lastSyncGetUsers = 0L;
        lastSyncGetEmpresas = 0L;
        lastSyncGetRegras = 0L;
        lastSyncGetParametros = 0L;
        lastSyncGetPlanos = 0L;

        while (updatingAthleteAccessList) // aguarda atualizacao corrente perceber que foi desconectado e parar atualizacao
            Utils.sleep(50);

        if (!updatingLogAccessList) // tenta enviar log de acesso antes de apagar tudo
            syncLogAthleteAccess();
        while (updatingLogAccessList)
            Utils.sleep(50);

        while (updatingUsersAccessList)
            Utils.sleep(50);

        while (uploadingPhotosPedestres)
            Utils.sleep(50);

        Boolean sessaoLimpa = HibernateUtil.cleanUserSession();
        if (sessaoLimpa) {
            Utils.createNotification("Sessï¿½o de usuï¿½rio encerrada!", NotificationType.GOOD);
            releaseTicketGateMenuItem.setEnabled(false);
            updateAccessListMenuItem.setEnabled(false);

        } else
            Utils.createNotification("Ocorreu um erro ao finalizar a sessï¿½o.", NotificationType.BAD);

    }

    public static void exit(boolean exibirConfirmacao) {
        if (exibirConfirmacao) {
            int dialogResult = JOptionPane.showConfirmDialog(null, "As catracas serï¿½o desconectadas. Deseja realmente sair?", "Confirmaï¿½ï¿½o",
                    JOptionPane.YES_NO_OPTION, JOptionPane.PLAIN_MESSAGE);
            if (dialogResult != JOptionPane.YES_OPTION)
                return;
        }

        systemTray.remove(trayIcon);
        HibernateUtil.shutdown();
        finalizeDevices();
        if (!desenvolvimento)
            //closeLogFile();
            System.exit(0);
    }

    private void configureTimers() {
        try {
            gson = new GsonBuilder().registerTypeAdapter(Date.class, new JsonDeserializer<Date>() {
                public Date deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context) throws JsonParseException {
                    try {
                        return Utils.convertDataJson(json);
                    } catch (Exception e) {
                    }

                    return null;
                }
            }).create();
            timerSyncUsersAccessList = new Timer(Integer.valueOf(Utils.getPreference("timeUserAccessList")) * 60000, new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    syncUsersAccessList();
                }
            });
            timerSyncAthleteAccessList = new Timer(Integer.valueOf(Utils.getPreference("timeAccessList")) * 60000, new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    syncAthleteAccessList();
                }
            });
            timerSyncLogAthleteAccess = new Timer(Configurations.TIME_LOG_ATHLETE_ACCESS, new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    syncLogAthleteAccess();
                }
            });

            timerOnline = new Timer(10000, new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    verificaOnline();
                }
            });
         

            timerTasksOfDay = new java.util.Timer();
            uTimerTask = new TimerTask() {
                @Override
                public void run() {
                    tasksOfDay(true);
                }
            };

            timerSyncUsersAccessList.stop();
            timerSyncAthleteAccessList.stop();
            timerSyncLogAthleteAccess.stop();
            //inicia antes de todos
            if (!timerOnline.isRunning()) {
                verificaOnline();
                timerOnline.start();
            }
        } catch (Exception e) {
            e.printStackTrace();
            Utils.createNotification("Erro ao carregar os timers. Por favor, reinicie o aplicativo.", NotificationType.BAD);
        }
    }
   
 

    public static void verificaOnline() {
		new Thread() {
			public void run() {
				try {
					HttpConnection con = new HttpConnection(urlApplication + "/restful-services/login/action");
					int responseCode = con.getResponseCode();
					
					if(mainScreen != null) {
						mainScreen.setConnectionStatusLabel(responseCode == 200);
					}

				} catch (Exception e) {
					mainScreen.setConnectionStatusLabel(false);
				}

				try {
					boolean hikivisionServerIsConnected = false;
					if(Utils.isHikivisionConfigValid()) {
						HikiVisionIntegrationService hikiVisionIntegrationService = HikiVisionIntegrationService.getInstace();
						hikivisionServerIsConnected = hikiVisionIntegrationService.getSystemInformation();
					}

					if(mainScreen != null) {
						mainScreen.setHikivisionConnectionStatusLabel(hikivisionServerIsConnected);
					}

				} catch (Exception e) {
					mainScreen.setHikivisionConnectionStatusLabel(false);
				}
			};
		}.start();
	}


    public static void tasksOfDay(boolean timerCall) {

        if (timerCall) {
            limpaCartoesVisitantes();
        }
        
        

        limpaSentidoTodos();
        limpaStatusCartoes();
        limpaTelas();
        closeLogFile();
        configLogFile();
        //	enviarLogsComFalhaAoEnviar();
        
        //verificar task 
   
		java.util.Timer timer = new java.util.Timer();
		timer.scheduleAtFixedRate(new TimerTask() {

			@Override
			public void run() {
		     	LocalDateTime localDate = LocalDateTime.now().minusMonths(6);
	    		Date date = Date.from(localDate.atZone(ZoneId.systemDefault()).toInstant());
	    	  HashMap<String, Object> args = new HashMap<>();
	        args.put("DATE_HIKIVISION", date); 

	        @SuppressWarnings("unchecked")
			final List<PedestrianAccessEntity> pedestres = (List<PedestrianAccessEntity>) HibernateUtil
	                .getResultListWithParams(PedestrianAccessEntity.class, "PedestrianAccessEntity.findAllWhitLastAccessHikivision", args);
	        if( pedestres != null && !pedestres.isEmpty()) {
	        	HikiVisionIntegrationService hikivision = HikiVisionIntegrationService.getInstace();
	        	HikivisionUseCases hiviVisionUseCase = new HikivisionUseCases(hikivision);
	            List<HikivisionDeviceTO.Device> devices  = hiviVisionUseCase.listarDispositivos();
	            for( PedestrianAccessEntity pedestre : pedestres) {
	            	for(HikivisionDeviceTO.Device device : devices) {
	            		
	            		hiviVisionUseCase.apagarUsuario(pedestre, device.getDevIndex());
	        		}
	            }
	        }
	    	
				
			}
		
		}, 0,0);
		
	//	 180 * 86400000
	



    }
	/*

	private static void enviarLogsComFalhaAoEnviar() {
		try {
			Integer countLogs = HibernateUtil.getResultListCount(LogPedestrianAccessEntity.class,
					"LogPedestrianAccessEntity.findUnsubmittedLogsCount", null);
			System.out.println("Quantidade total de logs com falha ao enviar: " + countLogs);

			HibernateUtil.sendLogs(countLogs, "LogPedestrianAccessEntity.findUnsubmittedLogs", null, true);

		} catch (Exception e) {
			e.printStackTrace();
		}

	}
	*/

    private static void limpaTelas() {

        if (mainScreen != null) {
            //fecha tela
            mainScreen.setVisible(false);
            mainScreen.dispose();
        }
        mainScreen = null;

        Utils.sleep(1000);
        mainScreen = new MainScreen();

        //limpa lixos
        new Thread() {
            public void run() {
                Utils.sleep(1000);
                Runtime.getRuntime().gc();
            }

        }.start();

        System.out.println("Saiu limpa telas");
    }

    private static void limpaStatusCartoes() {

        if (!Utils.getPreferenceAsBoolean("enableCardReset"))
            return;

        HibernateUtil.resetStatusAllCards();

        System.out.println("saiu limpaStatusCartoes");
    }

    private static void limpaSentidoTodos() {

        //verifica se estï¿½ ativado
        if (Main.servidor != null)
            return;

        if (!Utils.getPreferenceAsBoolean("enableDirectionClear"))
            return;

        //adiciona data para que os calculos de quantidade
        //de giros sejam refeitos
        loggedUser.setDateNewAccess(new Date());
        HibernateUtil.save(UserEntity.class, loggedUser);

        //apaga tambï¿½m dados de giros anteriores nï¿½o registrados
        HibernateUtil.apagaDadosDeGiro(loggedUser.getDateNewAccess());

        System.out.println("Saiu limpaSentidoTodos");

    }

    private static void limpaCartoesVisitantes() {

        //verifica se estï¿½ ativado
        if (Main.servidor != null)
            return;

        if (!Utils.getPreferenceAsBoolean("enableCardAcessClear"))
            return;

        //pesquisa todos os pedestres que estï¿½o com cartï¿½o ativado
        //e que tenham um crï¿½dito
        HibernateUtil.apagaDadosCartao();
        HibernateUtil.apagaDadosDeUltimoSentido();
        HibernateUtil.apagaQuantidadeAcessosAsinc();
    }

    public static Device getDefaultDevice() {
        if (devicesList != null && !devicesList.isEmpty()) {
            for (Device device : devicesList) {
                if (device.isDefaultDevice())
                    return device;
            }
        }
        return null;
    }

    public static void syncUsersAccessList() {
        if (updatingUsersAccessList)
            return;

        if (Main.servidor != null) {
            System.out.println(sdf.format(new Date()) + " Sincronizaï¿½ï¿½o desabilitada: Mï¿½quina possui servidor");
            return;
        }
        updatingUsersAccessList = true;

        SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
            @Override
            public Void doInBackground() {
                try {
                    if (timerSyncUsersAccessList.isRunning())
                        timerSyncUsersAccessList.stop();

                    while (loggedUser == null) {
                        Thread.sleep(500);
                    }
                    while (isCadastrandoBiometria) {
                        Thread.sleep(2000);
                    }

                    timerSyncUsersAccessList
                            .setInitialDelay(Integer.valueOf(Utils.getPreference("timeUserAccessList")) * 60000);
                    trayIcon.setImage(trayIconImageLoading);

                    requestAllUsers();

                    requestAllEmpresas();

                    requestAllParametros();

                    requestAllRegras();

                    requestAllPlanos();

                } catch (ConnectException e) {
                    System.err.println("Nï¿½o foi possï¿½vel comunicar com o servidor.");

                } catch (Exception e) {
                    e.printStackTrace();
                    System.out.println(sdf.format(new Date()) + "  ERRO NA SINCRONIZACAO: Exception: " + e.getMessage());

                } finally {
                    if (loggedUser != null)
                        timerSyncUsersAccessList.start();
                    updatingUsersAccessList = false;
                    trayIcon.setImage(trayIconImage);
                    //if (mainScreen != null && mainScreen.isVisible()) {
                    //	mainScreen.refresh();
                    //}
                    if (mainScreen != null && mainScreen.isVisible()) {
                        mainScreen.getListaAcessoPanel().getSyncButton().setText("Atualizar lista com o servidor");
                        mainScreen.getListaAcessoPanel().getSyncButton().setEnabled(true);
                        mainScreen.getListaAcessoPanel().getSyncButton().revalidate();
                        mainScreen.getListaAcessoPanel().updateDateLastSync();
                    }
                }
                return null;
            }

            private void requestAllUsers() throws IOException {
                HttpConnection con = new HttpConnection(urlApplication + "/restful-services/access/requestAllUsers"
                        + "?client=" + loggedUser.getIdClient()
                        + "&lastsync=" + lastSyncGetUsers
                        + "&version=" + Configurations.VERSION);

                Integer responseCode = con.getResponseCode();

                if (responseCode == 404) {
                    System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: sem registros de usuarios para receber");
                    return;
                }

                if (responseCode != 200) {
                    System.out.println(sdf.format(new Date()) + "  ERRO NA SINCRONIZACAO: Error String: "
                            + con.getErrorString());
                    return;
                }

                BufferedReader bufferedReader = con.getResponseReader();
                Type type = new TypeToken<List<UserEntity>>() {
                }.getType();
                List<UserEntity> userAccessList = gson.fromJson(bufferedReader, type);

                if (userAccessList == null || userAccessList.isEmpty()) {
                    System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: sem registros de usuarios para receber");
                    return;
                }

                for (UserEntity user : userAccessList) {
                    if (loggedUser == null) // usuario deslogou durante a sincronizacao
                        break;

//					if (loggedUser.getId().equals(user.getId())) // retira usuï¿½rio logado
//						continue;

                    user.setIdClient(loggedUser.getIdClient());

                    UserEntity usuarioExistente = (UserEntity) HibernateUtil
                            .getSingleResultById(UserEntity.class, user.getId());

                    if (usuarioExistente != null) {
                        usuarioExistente.update(user);
                        if (loggedUser.getId().equals(user.getId())) {
                            loggedUser = (UserEntity) HibernateUtil.update(UserEntity.class, usuarioExistente)[0];

                        } else {
                            HibernateUtil.update(UserEntity.class, usuarioExistente);
                        }

                    } else {
                        HibernateUtil.save(UserEntity.class, user);
                    }
                }

                if (loggedUser != null) {
                    Utils.sleep(1000);
                    lastSyncGetUsers = Calendar.getInstance(new Locale("pt", "BR")).getTimeInMillis();
                    loggedUser.setLastSyncUser(new Date(lastSyncGetUsers));
                    loggedUser = (UserEntity) HibernateUtil.updateUser(UserEntity.class, loggedUser)[0];
                }
            }

            private void requestAllEmpresas() throws IOException {
                HttpConnection con = new HttpConnection(urlApplication + "/restful-services/access/requestAllEmpresas"
                        + "?client=" + loggedUser.getIdClient()
                        + "&lastsync=" + lastSyncGetEmpresas
                        + "&version=" + Configurations.VERSION);

                Integer responseCode = con.getResponseCode();

                if (responseCode == 404) {
                    System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: sem registros de empresas para receber");
                    return;
                }

                if (responseCode != 200) {
                    System.out.println(sdf.format(new Date())
                            + "  ERRO NA SINCRONIZACAO DE EMPRESAS: Error String: " + con.getErrorString());
                    return;
                }

                BufferedReader bufferedReader = con.getResponseReader();
                Type type = new TypeToken<List<EmpresaTO>>() {
                }.getType();
                List<EmpresaTO> empresasTOList = gson.fromJson(bufferedReader, type);

                if (empresasTOList == null || empresasTOList.isEmpty()) {
                    System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: sem registros de empresas para receber");
                    return;
                }

                for (EmpresaTO empresaTO : empresasTOList) {
                    if (loggedUser == null) // usuario deslogou durante a sincronizacao
                        break;

                    EmpresaEntity empresa = new EmpresaEntity(empresaTO);
                    empresa.setIdClient(loggedUser.getIdClient());

                    EmpresaEntity empresaExistente = (EmpresaEntity) HibernateUtil
                            .getSingleResultById(EmpresaEntity.class, empresa.getId());

                    if (empresaExistente != null) {
                        empresaExistente.update(empresa);
                        HibernateUtil.update(EmpresaEntity.class, empresaExistente);

                    } else {
                        HibernateUtil.save(EmpresaEntity.class, empresa);
                    }
                }

                if (loggedUser != null) {
                    Utils.sleep(1000);
                    lastSyncGetEmpresas = Calendar.getInstance(new Locale("pt", "BR")).getTimeInMillis();
                    loggedUser.setLastSyncEmpresa(new Date(lastSyncGetEmpresas));
                    loggedUser = (UserEntity) HibernateUtil.updateUser(UserEntity.class, loggedUser)[0];
                }
            }

            private void requestAllRegras() throws IOException {
                HttpConnection con = new HttpConnection(urlApplication + "/restful-services/access/requestAllRegras"
                        + "?client=" + loggedUser.getIdClient()
                        + "&lastsync=" + lastSyncGetRegras);

                Integer responseCode = con.getResponseCode();

                if (responseCode == 404) {
                    System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: sem registros de regras para receber");
                    return;
                }

                if (responseCode != 200) {
                    System.out.println(sdf.format(new Date())
                            + "  ERRO NA SINCRONIZACAO DE REGRAS: Error String: " + con.getErrorString());
                    return;
                }

                BufferedReader bufferedReader = con.getResponseReader();
                Type type = new TypeToken<List<RegraTO>>() {
                }.getType();
                List<RegraTO> regrasTOList = gson.fromJson(bufferedReader, type);

                if (regrasTOList == null || regrasTOList.isEmpty()) {
                    System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: sem registros de regras para receber");
                    return;
                }

                for (RegraTO regraTO : regrasTOList) {
                    if (loggedUser == null) // usuario deslogou durante a sincronizacao
                        break;

                    RegraEntity regra = new RegraEntity(regraTO);
                    regra.setIdClient(loggedUser.getIdClient());
                    EmpresaEntity empresa = (EmpresaEntity) HibernateUtil.getSingleResultById(EmpresaEntity.class, regraTO.getIdEmpresa());
                    regra.setEmpresa(empresa);

                    RegraEntity regraExistente = (RegraEntity) HibernateUtil
                            .getSingleResultById(RegraEntity.class, regra.getId());

                    if (regraExistente != null) {
                        regraExistente.update(regra);
                        HibernateUtil.update(RegraEntity.class, regraExistente);
                    } else {
                        HibernateUtil.save(RegraEntity.class, regra);
                    }
                }

                if (loggedUser != null) {
                    Utils.sleep(1000);
                    lastSyncGetRegras = Calendar.getInstance(new Locale("pt", "BR")).getTimeInMillis();
                    loggedUser.setLastSyncRegra(new Date(lastSyncGetRegras));
                    loggedUser = (UserEntity) HibernateUtil.updateUser(UserEntity.class, loggedUser)[0];
                }
            }

            private void requestAllParametros() throws IOException {
                HttpConnection con = new HttpConnection(urlApplication + "/restful-services/access/requestAllParametros"
                        + "?client=" + loggedUser.getIdClient()
                        + "&lastsync=" + lastSyncGetParametros);

                Integer responseCode = con.getResponseCode();

                if (responseCode == 404) {
                    System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: sem registros de parametros para receber");
                    return;
                }

                if (responseCode != 200) {
                    System.out.println(sdf.format(new Date())
                            + "  ERRO NA SINCRONIZACAO DE PARAMETROS: Error String: " + con.getErrorString());
                    return;
                }

                BufferedReader bufferedReader = con.getResponseReader();
                Type type = new TypeToken<List<ParametroEntity>>() {
                }.getType();
                List<ParametroEntity> parametros = gson.fromJson(bufferedReader, type);

                if (parametros == null || parametros.isEmpty()) {
                    System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: sem registros de parametros para receber");
                    return;
                }

                parametros.forEach(parametro -> {
                    if (CHAVE_DE_INTEGRACAO_COMTELE.equals(parametro.getNome())) {
                        loggedUser.setChaveIntegracaoComtele(parametro.getValor());
                    }

                    parametro.setIdClient(loggedUser.getIdClient());

                    ParametroEntity parametroExistente = (ParametroEntity) HibernateUtil
                            .getSingleResultById(ParametroEntity.class, parametro.getId());

                    if (parametroExistente != null) {
                        parametroExistente.update(parametro);
                        HibernateUtil.update(ParametroEntity.class, parametroExistente);

                    } else {
                        HibernateUtil.save(ParametroEntity.class, parametro);
                    }

                });

                if (loggedUser != null) {
                    Utils.sleep(1000);
                    lastSyncGetParametros = Calendar.getInstance(new Locale("pt", "BR")).getTimeInMillis();
                    loggedUser.setLastSyncParametro(new Date(lastSyncGetParametros));
                    loggedUser = (UserEntity) HibernateUtil.updateUser(UserEntity.class, loggedUser)[0];
                }
            }

            private void requestAllPlanos() throws IOException {
                HttpConnection con = new HttpConnection(urlApplication + "/restful-services/access/requestAllPlanos"
                        + "?client=" + loggedUser.getIdClient()
                        + "&lastsync=" + lastSyncGetPlanos);

                Integer responseCode = con.getResponseCode();

                if (responseCode == 404) {
                    System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: sem registros de planos para receber");
                    return;
                }

                if (responseCode != 200) {
                    System.out.println(sdf.format(new Date())
                            + "  ERRO NA SINCRONIZACAO DE PLANOS: Error String: " + con.getErrorString());
                    return;
                }

                BufferedReader bufferedReader = con.getResponseReader();
                Type type = new TypeToken<List<PlanoEntity>>() {
                }.getType();
                List<PlanoEntity> planos = gson.fromJson(bufferedReader, type);

                if (planos == null || planos.isEmpty()) {
                    System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: sem registros de planos para receber");
                    return;
                }

                for (PlanoEntity plano : planos) {
                    if (loggedUser == null) // usuario deslogou durante a sincronizacao
                        break;

                    plano.setIdClient(loggedUser.getIdClient());

                    PlanoEntity planoExistente = (PlanoEntity) HibernateUtil
                            .getSingleResultById(PlanoEntity.class, plano.getId());

                    if (planoExistente != null) {
                        planoExistente.update(plano);
                        HibernateUtil.update(PlanoEntity.class, planoExistente);

                    } else {
                        HibernateUtil.save(PlanoEntity.class, plano);
                    }
                }

                if (loggedUser != null) {
                    Utils.sleep(1000);
                    lastSyncGetPlanos = Calendar.getInstance(new Locale("pt", "BR")).getTimeInMillis();
                    loggedUser.setLastSyncPlano(new Date(lastSyncGetPlanos));
                    loggedUser = (UserEntity) HibernateUtil.updateUser(UserEntity.class, loggedUser)[0];
                }
            }
        };
        worker.execute();
    }

    public static void syncHikivisionAccessList() {
        if (updatingHikivisionAccessList) {
            return;
        }

        if (Main.servidor != null) {
            System.out.println(sdf.format(new Date()) + " SincronizaÃ§Ã£o Hikivision desabilitada: MÃ¡quina possui servidor");
            return;
        }

        updatingHikivisionAccessList = true;

        SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
            @Override
            public Void doInBackground() {
                try {
                    executeHikivisionAccessListSync();

                    if (loggedUser != null) {
                        lastSyncHikivision = Calendar.getInstance(new Locale("pt", "BR")).getTimeInMillis();
                        loggedUser.setLastSyncHikivision(new Date(lastSyncHikivision));
                        Main.loggedUser = (UserEntity) HibernateUtil.updateUser(UserEntity.class, loggedUser)[0];
                    }

                    System.out.println(sdf.format(new Date()) + "  Servidor Hikivision sincronizado com sucesso!");

                } catch (Exception e) {
                    e.printStackTrace();
                    System.out.println(sdf.format(new Date()) + "  ERRO NA SINCRONIZACAO Hikivision: Exception: " + e.getMessage());

                } finally {
                    if (loggedUser != null) {
                        timerSyncHikivision.start();
                    }

                    updatingHikivisionAccessList = false;
                }

                return null;
            }

            private void executeHikivisionAccessListSync() {
                if (!Utils.isHikivisionConfigValid()) {
                    return;
                }

                HikivisionUseCases hikivisionUseCases = new HikivisionUseCases(HikiVisionIntegrationService.getInstace());

                if (!hikivisionUseCases.getSystemInformation()) {
                    System.out.println(sdf.format(new Date()) + "  Sincronização interrompida - Servidor offline");
                    return;
                }

                List<HikivisionDeviceTO.Device> devices = hikivisionUseCases.listarDispositivos();

                if (Objects.isNull(devices)) {
                    System.out.println(sdf.format(new Date()) + "  Sincronização interrompida - Sem dispositivos disponiveis");
                    return;
                }

                HashMap<String, Object> args = new HashMap<>();
                args.put("LAST_SYNC_HIKIVISION", new Date(lastSyncHikivision));

                @SuppressWarnings("unchecked")
				final List<PedestrianAccessEntity> pedestres = (List<PedestrianAccessEntity>) HibernateUtil
                        .getResultListWithDynamicParams(PedestrianAccessEntity.class, "PedestrianAccessEntity.findAllWithPhotoByLastSync", args);

                for (PedestrianAccessEntity pedestre : pedestres) {
                    for (HikivisionDeviceTO.Device device : devices) {
                    	try {
                    		hikivisionUseCases.syncronizaUsuario(device.getDevIndex(), pedestre);
                    	} catch (HikivisionIntegrationException e) {
							System.out.println(e.getMessage());
						}
                    }
                    
                }

            }
        };
        worker.execute();
    }

    public static void syncAthleteAccessList() {
        if (updatingAthleteAccessList) {
            return;
        }

        if (Main.servidor != null) {
            System.out.println(sdf.format(new Date()) + " Sincronização desabilitada: Máquina possui servidor");
            System.out.println(sdf.format(new Date()) + " Sincronizaï¿½ï¿½o desabilitada: Mï¿½quina possui servidor");
            return;
        }

        updatingAthleteAccessList = true;
        SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {

            @Override
            public Void doInBackground() {
                try {
                    while (isCadastrandoBiometria) {
                        Thread.sleep(2000);
                    }
                    while (updatingUsersAccessList) {
                        Thread.sleep(500);
                    }

                    while (loggedUser == null) {
                        Thread.sleep(500);
                    }

                    if (mainScreen != null && mainScreen.isVisible()) {
                        mainScreen.getListaAcessoPanel().getSyncButton().setText("Atualizando lista com o servidor...");
                        mainScreen.getListaAcessoPanel().getSyncButton().setEnabled(false);
                        mainScreen.getListaAcessoPanel().getSyncButton().revalidate();
                    }

                    if (timerSyncAthleteAccessList.isRunning()) {
                        timerSyncAthleteAccessList.stop();
                    }

                    timerSyncAthleteAccessList.setInitialDelay(Integer.valueOf(Utils.getPreference("timeAccessList")) * 60000);
                    trayIcon.setImage(trayIconImageLoading);

                    //Sincroniza os logs de acesso antes de sincronizar os pedestres
                    syncLogAthleteAccess();

                    while (updatingLogAccessList) {
                        Thread.sleep(500);
                    }

                    HttpConnection con = new HttpConnection(urlApplication + "/restful-services/login/action");
                    int responseCode = con.getResponseCode();

                    if (responseCode != 200) {
                        return null;
                    }

                    Long backUpLastSync = lastSync != null ? lastSync.longValue() : 0L;

                    List<PedestrianAccessEntity> visitantesLocais = enviaPedestresCadastradosOuEditadosDesktop();

                    enviaBiometriasColetadasLocalmente();
                    recebePedestresEBiometriasDaWeb();
                    buscaFotosDosPedestres(backUpLastSync);

                    if (visitantesLocais != null && !visitantesLocais.isEmpty()) {
                        apagaDadosNovos(visitantesLocais);
                    }

                    atualizaListadeAcessoCatracaOffline();

                } catch (UnknownHostException | SocketTimeoutException | ConnectException ce) {
                    System.out.println(ce.getMessage());
                    ce.printStackTrace();
                } catch (Exception e) {
                    e.printStackTrace();
                    System.out.println(sdf.format(new Date()) + "  ERRO NA SINCRONIZACAO: Exception: " + e.getMessage());

                } finally {
                    if (loggedUser != null) {
                        timerSyncAthleteAccessList.start();
                    }

                    updatingAthleteAccessList = false;
                    trayIcon.setImage(trayIconImage);
                    //if (mainScreen != null && mainScreen.isVisible())
                    //	mainScreen.refresh();

                    if (mainScreen != null && mainScreen.isVisible()) {
                        mainScreen.getListaAcessoPanel().getSyncButton().setText("Atualizar lista com o servidor");
                        mainScreen.getListaAcessoPanel().getSyncButton().setEnabled(true);
                        mainScreen.getListaAcessoPanel().getSyncButton().revalidate();
                        mainScreen.getListaAcessoPanel().updateDateLastSync();
                    }
                }

                //limpa lixos
                new Thread() {
                    public void run() {
                        Runtime.getRuntime().gc();
                    }

                }.start();

                return null;
            }

            private void atualizaListadeAcessoCatracaOffline() throws Exception {
                // TODO Auto-generated method stub
                if (devicesList == null || devicesList.isEmpty()) {
                    return;
                }
                for (Device device : devicesList) {
                    if (device instanceof TopDataDevice) {
                        TopDataDevice topDataDevice = (TopDataDevice) device;
                        if (!topDataDevice.isConnected()) {
                            topDataDevice.enviaCartaoCatracaOffline();
                        }
                    }
                }
            }

            @SuppressWarnings("unchecked")
            private List<PedestrianAccessEntity> enviaPedestresCadastradosOuEditadosDesktop() throws IOException {
                Main.verificaValidandoAcesso();

                List<PedestrianAccessEntity> visitantesLocais = (List<PedestrianAccessEntity>) HibernateUtil
                        .getResultListLimited(PedestrianAccessEntity.class, "PedestrianAccessEntity.findAllCadastradosOuEditadosDesktop", 100L);

                if (visitantesLocais == null || visitantesLocais.isEmpty()) {
                    System.out.println(sdf.format(new Date()) + "  PEDESTRES/VISITANTES LOCAIS: sem registros para enviar");
                    return null;
                }

                System.out.println("Iniciando sincronismo de pedestres para web");

                JsonArray responseArray = new JsonArray();
                for (PedestrianAccessEntity visitante : visitantesLocais) {
                    // se foi criado, envia os dados para o servidor
                    // porque estï¿½ sem o ID
                    if (Boolean.TRUE.equals(visitante.getCadastradoNoDesktop())) {
                        visitante.setListaAcessosTransient(buscaAcessosVisitante(visitante.getId()));

                        if (visitante.getListaAcessosTransient() != null
                                && !visitante.getListaAcessosTransient().isEmpty()) {
                            int countAtivos = 0;
                            for (LogPedestrianAccessEntity l : visitante.getListaAcessosTransient()) {
                                if (!"Regras ignoradas".equals(l.getReason())
                                        && l.getStatus() != null
                                        && "ATIVO".equalsIgnoreCase(l.getStatus())) {
                                    countAtivos++;
                                }
                            }
                            if (countAtivos > 0) {
                                visitante.setQtdAcessoAntesSinc(countAtivos);
                            }
                        }

                        visitante.setListaBiometriasTransient(buscaBiometriasVisitante(visitante.getId()));
                    }

                    JsonObject responseObj = getNewVisitanteResponseObj(visitante);
                    responseArray.add(responseObj);
                }

                System.out.println("Enviando request com visitantes: " + responseArray.size());

                HttpConnection con = new HttpConnection(urlApplication + "/restful-services/access/uploadVisitantes");
                int responseCode = con.sendResponse(responseArray.toString());

                if (responseCode != 200) {
                    System.out.println(sdf.format(new Date())
                            + "  ERRO AO ENVIAR VISITANTES LOCAIS: Response code: " + responseCode);
                    System.out.println(sdf.format(new Date())
                            + "  ERRO AO ENVIAR VISITANTES LOCAIS: Error String: " + con.getErrorString());
                    return null;
                }

                atualizaDadosAlterados(visitantesLocais);

                return visitantesLocais;
            }

            private void atualizaDadosAlterados(List<PedestrianAccessEntity> visitantesLocais) {
                if (visitantesLocais == null || visitantesLocais.isEmpty()) {
                    return;
                }

                //antes de alterar dados, verifica se existe validaï¿½ï¿½o de acesso em andamento
                Main.verificaValidandoAcesso();

                for (PedestrianAccessEntity visitante : visitantesLocais) {
                    if (!Boolean.TRUE.equals(visitante.getCadastradoNoDesktop())) {

                        if (visitante.getMensagens() != null && !visitante.getMensagens().isEmpty()) {
                            for (PedestrianMessagesEntity m : visitante.getMensagens()) {
                                HibernateUtil.remove(m);
                            }
                        }

                        if (visitante.getDocumentos() != null && !visitante.getDocumentos().isEmpty()) {
                            for (DocumentoEntity d : visitante.getDocumentos()) {
                                HibernateUtil.remove(d);
                            }
                        }

                        if (visitante.getPedestreRegra() != null && !visitante.getPedestreRegra().isEmpty()) {
                            for (PedestreRegraEntity pr : visitante.getPedestreRegra()) {
                                HibernateUtil.remove(pr);
                            }
                        }

                        if (visitante.getEquipamentos() != null && !visitante.getEquipamentos().isEmpty()) {
                            for (PedestrianEquipamentEntity pe : visitante.getEquipamentos()) {
                                HibernateUtil.remove(pe);
                            }
                        }

                        visitante.setDocumentos(new ArrayList<>());
                        visitante.setPedestreRegra(new ArrayList<>());
                        visitante.setEquipamentos(new ArrayList<>());
                        visitante.setMensagens(new ArrayList<>());

                        visitante.setEditadoNoDesktop(false);
                        HibernateUtil.update(PedestrianAccessEntity.class, visitante);
                    }
                }
            }

            private void apagaDadosNovos(List<PedestrianAccessEntity> visitantesLocais) {
                //antes de alterar dados, verifica se existe validaï¿½ï¿½o de acesso em andamento
                Main.verificaValidandoAcesso();

                for (PedestrianAccessEntity visitante : visitantesLocais) {
                    if (Boolean.TRUE.equals(visitante.getCadastradoNoDesktop())) {

                        if (visitante.getMensagens() != null && !visitante.getMensagens().isEmpty())
                            for (PedestrianMessagesEntity m : visitante.getMensagens())
                                HibernateUtil.remove(m);

                        if (visitante.getDocumentos() != null && !visitante.getDocumentos().isEmpty())
                            for (DocumentoEntity d : visitante.getDocumentos())
                                HibernateUtil.remove(d);

                        if (visitante.getPedestreRegra() != null && !visitante.getPedestreRegra().isEmpty())
                            for (PedestreRegraEntity pr : visitante.getPedestreRegra())
                                HibernateUtil.remove(pr);

                        if (visitante.getEquipamentos() != null && !visitante.getEquipamentos().isEmpty())
                            for (PedestrianEquipamentEntity pe : visitante.getEquipamentos())
                                HibernateUtil.remove(pe);

                        if (visitante.getListaAcessosTransient() != null
                                && !visitante.getListaAcessosTransient().isEmpty())
                            for (LogPedestrianAccessEntity acesso : visitante.getListaAcessosTransient())
                                HibernateUtil.remove(acesso);

                        List<LogPedestrianAccessEntity> novosLogs = buscaAcessosVisitante(visitante.getId());

                        if (novosLogs != null && !novosLogs.isEmpty()) {
                            PedestrianAccessEntity novoPedestre = buscaPedestrePorIdTemp(visitante.getId());
                            System.out.println("id do pedestre antigo" + visitante.getId());
                            for (LogPedestrianAccessEntity log : novosLogs) {
                                log.setIdPedestrian(novoPedestre.getId());
                                log.setPedestre(novoPedestre);

                                HibernateUtil.save(LogPedestrianAccessEntity.class, log);
                            }
                            System.out.println("id do pedestre novo" + novoPedestre.getId());
                            visitante.setVersao(visitante.getVersao() + 1);
                        }

                        if (visitante.getListaBiometriasTransient() != null
                                && !visitante.getListaBiometriasTransient().isEmpty()) {
                            for (BiometricEntity biometria : visitante.getListaBiometriasTransient()) {
                                biometria = (BiometricEntity) HibernateUtil
                                        .getSingleResultById(BiometricEntity.class, biometria.getId());

                                if (biometria != null) {
                                    try {
                                        HibernateUtil.remove(biometria);
                                    } catch (Exception e) {
                                        System.out.println("Sem digital pra remover");
                                    }
                                }
                            }
                        }

                        try {
                            HibernateUtil.remove(visitante);

                        } catch (Exception e) {
                            //visitante serï¿½o excluï¿½do na prï¿½xima sincronizaï¿½ï¿½o
                            //marca para nï¿½o aparecer nas listagens

                            visitante = (PedestrianAccessEntity) HibernateUtil
                                    .getSingleResultById(PedestrianAccessEntity.class, visitante.getId());
                            visitante.setInvisivel(true);

                            HibernateUtil.update(PedestrianAccessEntity.class, visitante);
                        }
                    }
                }
            }

            private void recebePedestresEBiometriasDaWeb() throws IOException {
                HttpConnection con = new HttpConnection(urlApplication + "/restful-services/access/request"
                        + "?client=" + loggedUser.getIdClient()
                        + "&lastsync=" + lastSync
                        + "&version=" + Configurations.VERSION);
                Integer responseCode = con.getResponseCode();

                if (responseCode != 200 && responseCode != 404) {
                    System.out.println(sdf.format(new Date()) + "  ERRO NA SINCRONIZACAO: Error String: " + con.getErrorString());
                    return;
                }

                if (responseCode == 404) {
                    System.out.println(sdf.format(new Date()) + "  SEM REGISTROS PARA RECEBER: Error String: " + con.getErrorString());
                    return;
                }

                BufferedReader bufferedReader = con.getResponseReader();
                Type type = new TypeToken<List<PedestrianAccessTO>>() {
                }.getType();
                List<PedestrianAccessTO> athleteAccessTOList = gson.fromJson(bufferedReader, type);

                if (athleteAccessTOList == null || athleteAccessTOList.isEmpty()) {
                    System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: sem registros para receber");
                    return;
                }

                if ("true".equals(Utils.getPreference("printLog"))) {
                	System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: Response string: " + gson.toJson(athleteAccessTOList, type));                	
                }

                boolean atualizaDigitais = false;
                for (PedestrianAccessTO athleteAccessTO : athleteAccessTOList) {
                    if (loggedUser == null) { // usuario deslogou durante a sincronizacao
                    	break;
                    }

                    // TODO : criar novo mï¿½todo para pegar pedestre removido ou nï¿½o
                    //        isso pode resolver vï¿½rios bugs
                    // TODO : verificar onde o luxand ID e removido para nao fazer mais. Pode ser
                    //		  aqui ou ne

                    PedestrianAccessEntity existentAthleteAccess = (PedestrianAccessEntity) HibernateUtil.
                            getAllPedestresById(athleteAccessTO.getId());

                    if (existentAthleteAccess != null) {

                        // verifica se houve alteracao nos campos principais e se recebeu templates
                        if (existentAthleteAccess.toString().equals(athleteAccessTO.toString())) {
                            continue;
                        }

                        // Procedimento de atualizacao de usuarios nas catracas RWTech
                        if (Boolean.TRUE.equals(existentAthleteAccess.getCadastradoNaCatracaRWTech())
                                && !Boolean.TRUE.equals(existentAthleteAccess.getDesatualizadoNaCatracaRWTech())) {
                            existentAthleteAccess.setDesatualizadoNaCatracaRWTech(true);
                        }

                        //verifica se usuï¿½rio foi apagado e se tem facial para apagar tambï¿½m no servidor facial
                        String idFacial = null;
                        if ((Boolean.TRUE.equals(athleteAccessTO.getRemovido())
                                || !"ATIVO".equals(athleteAccessTO.getStatus()))
                                && existentAthleteAccess.getLuxandIdentifier() != null) {
                            idFacial = existentAthleteAccess.getLuxandIdentifier();
                        }

                        //apaga dados do facial
//						if(idFacial != null && !"".equals(idFacial) && LuxandService.getInstance() != null) {
//							System.out.println("Estou deletando automaticamente, face: " + idFacial);
//							LuxandService.getInstance().clearName(Long.valueOf(idFacial));
//						}
                        
                        final String oldStatus = existentAthleteAccess.getStatus();

                        existentAthleteAccess.update(athleteAccessTO);
                        HibernateUtil.update(PedestrianAccessEntity.class, existentAthleteAccess);

                        if((existentAthleteAccess.getRemovido() || !Objects.equals(oldStatus, existentAthleteAccess.getStatus())) 
                        		&& Objects.nonNull(athleteAccessTO.getDataCadastroFotoNaHikivision()) 
                        		&& Utils.isHikivisionConfigValid() ) {
                        	
                        	final HikivisionUseCases hikivisionUseCases = new HikivisionUseCases(HikiVisionIntegrationService.getInstace());
                        	hikivisionUseCases.syncronizarUsuarioAllDevices(existentAthleteAccess);
                        }

                        if (!atualizaDigitais && Boolean.TRUE.equals(existentAthleteAccess.getNovasDigitais())) {
                            atualizaDigitais = true;
                        }
                        

                    } else {
                        PedestrianAccessEntity newAthleteAccess = new PedestrianAccessEntity(athleteAccessTO);
                        if (!atualizaDigitais && Boolean.TRUE.equals(newAthleteAccess.getNovasDigitais())) {
                            atualizaDigitais = true;
                        }
                        HibernateUtil.save(PedestrianAccessEntity.class, newAthleteAccess);
                    }
                }

                if (loggedUser == null) {
                    return;
                }

                while (isCadastrandoBiometria) {
                    try {
                        Thread.sleep(2000);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }

                if (atualizaDigitais) {
                    for (Device device : devicesList) {
                        try {
                            //reinicializa digitais na catraca topdata
                            if (device instanceof TopDataDevice && device.isConnected()) {
                                System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: atualizando templates " + device.getName());
                                TopDataDevice topData = (TopDataDevice) device;
                                if (topData.modeloLC)
                                    topData.verificaCadastroNoInner(true, false, lastSync != null ? new Date(lastSync) : null);
                                else if (topData.getIndexSearchEngine() != null)
                                    topData.restartIndexSearchEngine();
                                else
                                    topData.atualizaDigitaisLFD(true, false, lastSync != null ? new Date(lastSync) : null);
                            }
                        } catch (Exception e) {
                            //nï¿½o deixa o erro para o processo
                            e.printStackTrace();
                        }
                    }
                } else {
                    System.out.println(sdf.format(new Date()) + "SINCRONIZACAO: Sem alteraï¿½ï¿½es de templates para catracas");
                }

                Utils.sleep(1000);
                lastSync = Calendar.getInstance(new Locale("pt", "BR")).getTimeInMillis();
                loggedUser.setLastSync(new Date(lastSync));
                loggedUser = (UserEntity) HibernateUtil.updateUser(UserEntity.class, loggedUser)[0];

                //dispara para servidores
                if (Main.broadcastServer != null) {
                    Main.broadcastServer.sendMessage(new BroadcastMessageTO(BroadcastMessageType.REFRESH_TEMPLATES));
                }
            }

			@SuppressWarnings("unchecked")
            private void enviaBiometriasColetadasLocalmente() throws IOException {
                // Enviando as biometrias coletadas localmente
                List<BiometricEntity> biometriasLocais = (List<BiometricEntity>) HibernateUtil.getResultList(BiometricEntity.class, "BiometricEntity.findAll");

                if (biometriasLocais == null || biometriasLocais.isEmpty()) {
                    System.out.println(sdf.format(new Date()) + "  BIOMETRIAS LOCAIS: sem registros para enviar");
                    return;
                }

                System.out.println("\r\n" + sdf.format(new Date()) + "  BIOMETRIAS LOCAIS: " + biometriasLocais.size() + " registros para enviar");
                JsonArray responseArray = new JsonArray();

                for (BiometricEntity biometria : biometriasLocais) {
                    JsonObject responseObj = new JsonObject();
                    responseObj.addProperty("idUser", biometria.getUser());
                    responseObj.addProperty("finger", biometria.getFinger().toString());
                    responseObj.addProperty("template", Base64.encodeBase64String(biometria.getTemplate()));
                    responseObj.addProperty("sample", biometria.getSample() != null ? Base64.encodeBase64String(biometria.getSample()) : null);
                    responseArray.add(responseObj);
                }

                HttpConnection con = new HttpConnection(urlApplication + "/restful-services/access/saveBiometry");

                int responseCode = con.sendResponse(responseArray.toString());

                if (responseCode == 200) { // OK
                    System.out.println(sdf.format(new Date()) + "  BIOMETRIAS LOCAIS: dados enviados!");
                    // Biometrias enviadas com sucesso, podem ser apagadas localmente porque serao recebidas novamente atraves do AthleteAccess
                    for (BiometricEntity biometria : biometriasLocais) {
                        HibernateUtil.remove(biometria);
                        Utils.sleep(10);
                    }

                } else {
                    System.out.println(sdf.format(new Date()) + "  ERRO AO ENVIAR BIOMETRIAS LOCAIS: Response code: " + responseCode);
                    System.out.println(sdf.format(new Date()) + "  ERRO AO ENVIAR BIOMETRIAS LOCAIS: Error String: " + con.getErrorString());
                }
            }

            private void buscaFotosDosPedestres(Long backUpLastSync) throws IOException {
                HttpConnection con = new HttpConnection(urlApplication + "/restful-services/photo/query?client="
                        + loggedUser.getIdClient() + "&lastsync=" + backUpLastSync);

                int responseCode = con.getResponseCode();

                if (responseCode != 200) {
                    return;
                }

                String resposta = con.getResponseReader().readLine();
                if (Utils.isNullOrEmpty(resposta)) {
                    return;
                }

                String[] ids = resposta.split(";");
                System.out.println(sdf.format(new Date()) + "  BUSCANDO FOTOS: " + ids.length);
                int cont = 0;
                Integer imageSize = Utils.getPreferenceAsInteger("imageSizeRequestServer");
                while (cont < ids.length) {

                    // monta uma string com no maximo 50 ids
                    StringBuilder stringBuilder = new StringBuilder();
                    int i = 0;
                    while (i < 50 && cont < ids.length) {
                        stringBuilder.append(ids[cont] + ";");
                        i++;
                        cont++;
                    }

                    // busca o pacote de fotos
                    con = new HttpConnection(urlApplication + "/restful-services/photo/request?ids="
                            + stringBuilder + "&type=PEDESTRES&resize=true&imageSize="
                            + imageSize);
                    responseCode = con.getResponseCode();
                    if (responseCode == 200) {
                        BufferedReader bufferedReader = con.getResponseReader();
                        Type type = new TypeToken<List<PedestrianAccessTO>>() {
                        }.getType();

                        List<PedestrianAccessTO> athleteAccessTOList = gson.fromJson(bufferedReader, type);
                        if (athleteAccessTOList != null && !athleteAccessTOList.isEmpty()) {
                            for (PedestrianAccessTO athleteAccessTO : athleteAccessTOList) {
                                if (loggedUser == null) {
                                    break;
                                }

                                PedestrianAccessEntity existentAthleteAccess = (PedestrianAccessEntity) HibernateUtil
                                        .getSingleResultById(PedestrianAccessEntity.class, athleteAccessTO.getId());
                                if (existentAthleteAccess != null) {
                                    existentAthleteAccess.setFoto(Base64.decodeBase64(athleteAccessTO.getFotoBase64()));
                                    HibernateUtil.update(PedestrianAccessEntity.class, existentAthleteAccess);
                                }
                            }
                        }
                    }
                }
            }
        };
        worker.execute();
    }

    public static void syncLogAthleteAccess() {
        if (updatingLogAccessList) {
            return;
        }

        if (Main.servidor != null) {
            return;
        }

        while (isCadastrandoBiometria) {
            try {
                Thread.sleep(2000);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        updatingLogAccessList = true;
        SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {

            @Override
            public Void doInBackground() {
                try {
                    if (mainScreen != null && mainScreen.isVisible()) {
                        mainScreen.getHistoricoAcessoPanel().getSyncButton().setText("Enviando lista para o servidor...");
                        mainScreen.getHistoricoAcessoPanel().getSyncButton().setEnabled(false);
                        mainScreen.getHistoricoAcessoPanel().getSyncButton().revalidate();
                    }

                    if (timerSyncLogAthleteAccess.isRunning()) {
                        timerSyncLogAthleteAccess.stop();
                    }

                    trayIcon.setImage(trayIconImageLoading);

                    enviaBackupAndPreferences();

                    enviaLogsDeAcesso();

                } catch (Exception e) {
                    System.out.println(sdf.format(new Date()) + "  ERRO AO ENVIAR LOG DE ACESSO: Exception: " + e.getMessage());

                } finally {
                    if (loggedUser != null)
                        timerSyncLogAthleteAccess.start();
                    updatingLogAccessList = false;
                    trayIcon.setImage(trayIconImage);

                    if (mainScreen != null && mainScreen.isVisible()) {
                        mainScreen.getHistoricoAcessoPanel().getSyncButton().setText("Enviar lista para o servidor");
                        mainScreen.getHistoricoAcessoPanel().getSyncButton().setEnabled(true);
                        mainScreen.getHistoricoAcessoPanel().getSyncButton().revalidate();
                        mainScreen.getHistoricoAcessoPanel().updateDateLastSync();
                    }
                }
                return null;
            }

            private void enviaBackupAndPreferences() {
                if (loggedUser == null || !loggedUser.getBackupChanged()) {
                    System.out.println(sdf.format(new Date()) + "  BACKUP: sem dados para enviar");
                    return;
                }

                System.out.println(sdf.format(new Date()) + "  BACKUP: enviando dados");
                JsonObject responseObj = new JsonObject();
                responseObj.addProperty("backupPreferences", loggedUser.getBackupPreferences());
                responseObj.addProperty("backupDevices", loggedUser.getBackupDevices());
                HttpConnection con;
                try {
                    con = new HttpConnection(urlApplication + "/restful-services/access/saveBackupByUser?idUser="
                            + loggedUser.getId().toString());
                    int responseCode = con.sendResponse(responseObj.toString());
                    if (responseCode != 200) {
                        System.out.println(sdf.format(new Date()) + "  ERRO AO ENVIAR BACKUP. Response code: " + responseCode);
                        System.out.println(sdf.format(new Date()) + "  ERRO AO ENVIAR BACKUP: Error String: " + con.getErrorString());
                        return;
                    }

                    System.out.println(sdf.format(new Date()) + "  BACKUP: dados enviados!");
                    Main.loggedUser.setBackupChanged(false);
                    Main.loggedUser = (UserEntity) HibernateUtil.saveUser(UserEntity.class, Main.loggedUser)[0];

                } catch (ConnectException e) {
                    System.err.println("Nï¿½o foi possï¿½vel comunicar com o servidor.");

                } catch (Exception e) {
                    e.printStackTrace();
                }
            }


            private void enviaLogsDeAcesso() throws Exception {
                final Date newLastSyncLog = new Date();
                final int pageSize = 100;
                final int qtdeLogsOnline = buscaQuantidadeDeLogsDeAcesso(lastSyncLog, newLastSyncLog, "findByAccessDateCount");
                final int qtdeLogsOffline = buscaQuantidadeDeLogsDeAcesso(lastSyncLog, newLastSyncLog, "findByCreateDateCount");
                int offsetLogsOnline = 0;
                int offsetLogsOffline = 0;

                while ((offsetLogsOnline < qtdeLogsOnline) || (offsetLogsOffline < qtdeLogsOffline)) {
                    List<LogPedestrianAccessEntity> logsOnline = null;
                    List<LogPedestrianAccessEntity> logsOffline = null;

                    if (offsetLogsOnline < qtdeLogsOnline) {
                        logsOnline = buscaLogsAcesso(offsetLogsOnline, pageSize, lastSyncLog, newLastSyncLog, "findByAccessDate");
                        offsetLogsOnline += pageSize;
                    }

                    if (offsetLogsOffline < qtdeLogsOffline) {
                        logsOffline = buscaLogsAcesso(offsetLogsOffline, pageSize, lastSyncLog, newLastSyncLog, "findByCreateDate");
                        offsetLogsOffline += pageSize;
                    }

                    if (logsOffline != null) {
                        if (logsOnline == null) {
                            logsOnline = new ArrayList<LogPedestrianAccessEntity>();
                        }
                        logsOnline.addAll(logsOffline);
                    }
                    if (logsOnline == null || logsOnline.isEmpty()) {
                        System.out.println("Sem logs para enviar");
                        break;
                    }
                    enviaLogsParaWeb(logsOnline);

                }
                if (loggedUser != null) {
                    lastSyncLog = newLastSyncLog.getTime();
                    loggedUser.setLastSyncLog(new Date(lastSyncLog));
                    Main.loggedUser = (UserEntity) HibernateUtil.updateUser(UserEntity.class, loggedUser)[0];
                }
                System.out.println(sdf.format(new Date()) + "  LOG DE ACESSO: dados enviados!");

            }
        };
        worker.execute();

    }

    private static void enviaLogsParaWeb(List<LogPedestrianAccessEntity> logsOnline) throws IOException {
        JsonArray responseArray = new JsonArray();
        for (LogPedestrianAccessEntity log : logsOnline) {
            JsonObject responseObj = new JsonObject();
            responseObj.addProperty("idLoggedUser", log.getIdLoggedUser().toString());
            responseObj.addProperty("idPedestrian", log.getIdPedestrian() == null
                    ? "" : log.getIdPedestrian().toString());
            responseObj.addProperty("accessDate", String.valueOf(log.getAccessDate().getTime()));
            responseObj.addProperty("status", log.getStatus());
            responseObj.addProperty("location", log.getLocation());
            responseObj.addProperty("reason", log.getReason());
            responseObj.addProperty("direction", log.getDirection() == null ? "ENTRADA" : log.getDirection());
            responseObj.addProperty("equipament", log.getEquipament() == null ? "--" : log.getEquipament());
            responseObj.addProperty("bloquearSaida", log.getBloquearSaida() != null ? log.getBloquearSaida() : false);
            responseObj.addProperty("cartaoAcessoRecebido", log.getCartaoAcessoRecebido() != null
                    ? log.getCartaoAcessoRecebido() : "");
            responseArray.add(responseObj);
        }

        HttpConnection con = new HttpConnection(urlApplication + "/restful-services/access/registerlog");
        int responseCode = con.sendResponse(responseArray.toString());
        if (responseCode != 200) {
            System.out.println(sdf.format(new Date()) + "  ERRO AO ENVIAR LOG DE ACESSO: Response Code: " + responseCode
                    + "  Error String: " + con.getErrorString());
            throw new ErrorOnSendLogsToWebException(" ERRO AO ENVIAR LOG DE ACESSO: Response Code: " + responseCode);
        }

    }

    private static int buscaQuantidadeDeLogsDeAcesso(Long lastSyncLog, Date newLastSyncLog, String namedQuery) {
        HashMap<String, Object> args = new HashMap<String, Object>();
        args.put("LAST_SYNC", new Date(lastSyncLog));
        args.put("NEW_LAST_SYNC", newLastSyncLog);

        return HibernateUtil.
                getResultListWithParamsCount(LogPedestrianAccessEntity.class, "LogPedestrianAccessEntity." + namedQuery, args);
    }

    private static List<LogPedestrianAccessEntity> buscaLogsAcesso(int offset, int pageSize, Long lastSyncLog, Date newLastSyncLog, String namedQuery) {
        HashMap<String, Object> args = new HashMap<String, Object>();
        args.put("LAST_SYNC", new Date(lastSyncLog));
        args.put("NEW_LAST_SYNC", newLastSyncLog);

        return (List<LogPedestrianAccessEntity>) HibernateUtil.
                buscaLogsDeAcessoPaginados("LogPedestrianAccessEntity." + namedQuery, args, offset, pageSize);
    }

    @SuppressWarnings("unchecked")
    public static void dateSync(String inicio, String fim) throws ParseException {
        SimpleDateFormat sdf2 = new SimpleDateFormat("dd/MM/yyyy HH:mm");

        HashMap<String, Object> args = new HashMap<>();
        args.put("CURRENT_DATE_INICIO", sdf2.parse(inicio));
        args.put("CURRENT_DATE_FIM", sdf2.parse(fim));

        List<LogPedestrianAccessEntity> listaAcessoNaoEnvidados = (List<LogPedestrianAccessEntity>) HibernateUtil.
                getResultListWithParams(LogPedestrianAccessEntity.class, "LogPedestrianAccessEntity.findByCurrentDate", args);

        JsonArray responseArray = new JsonArray();
        System.out.println(sdf.format(new Date()) + "  LOG DE ACESSO: " + listaAcessoNaoEnvidados.size() + " registros sincronizados manualmente para enviar");
        for (LogPedestrianAccessEntity log : listaAcessoNaoEnvidados) {
//			System.out.println("log de acesso " + log);
            JsonObject responseObj = new JsonObject();
            responseObj.addProperty("idLoggedUser", log.getIdLoggedUser().toString());
            responseObj.addProperty("idPedestrian", log.getIdPedestrian() == null
                    ? "" : log.getIdPedestrian().toString());
            responseObj.addProperty("accessDate", String.valueOf(log.getAccessDate().getTime()));
            responseObj.addProperty("status", log.getStatus());
            responseObj.addProperty("location", log.getLocation());
            responseObj.addProperty("reason", log.getReason());
            responseObj.addProperty("direction", log.getDirection() == null ? Tipo.ENTRADA : log.getDirection());
            responseObj.addProperty("equipament", log.getEquipament() == null ? "--" : log.getEquipament());
            responseObj.addProperty("bloquearSaida", log.getBloquearSaida() != null ? log.getBloquearSaida() : false);
            responseObj.addProperty("cartaoAcessoRecebido", log.getCartaoAcessoRecebido() != null
                    ? log.getCartaoAcessoRecebido() : "");
            responseArray.add(responseObj);
        }

        HttpConnection con;
        try {
            con = new HttpConnection(urlApplication + "/restful-services/access/registerlog");
            int responseCode = con.sendResponse(responseArray.toString());
            if (responseCode != 200) {
                System.out.println(sdf.format(new Date()) + "  ERRO AO ENVIAR LOG DE ACESSO: Response Code: " + responseCode
                        + "  Error String: " + con.getErrorString());
                return;
            }
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        System.out.println(sdf.format(new Date()) + "  Dados offline armazenados");
    }

    private void setSystemTrayIcon() {

        if (!SystemTray.isSupported()) {
            Object[] options = {"OK"};
            JOptionPane.showOptionDialog(null, "Nï¿½o ï¿½ possï¿½vel adicionar ï¿½cones na bandeja do sistema.", "Bandeja do sistema nï¿½o suportada.",
                    JOptionPane.PLAIN_MESSAGE, JOptionPane.PLAIN_MESSAGE, null, options, options[0]);
            HibernateUtil.shutdown();
            System.exit(0);
        }


        loadImages();
        trayIcon = new TrayIcon(trayIconImage, nomeAplicacao + " Controle de acesso");
        openScreenMenuItem = new JMenuItem("Abrir");
        updateAccessListMenuItem = new JMenuItem("Atualizar lista de acesso");
        releaseTicketGateMenuItem = new JMenuItem("Liberar acesso na catraca");
        JMenuItem exitMenuItem = new JMenuItem("Sair");
        releaseTicketGateMenuItem.setEnabled(false);
        updateAccessListMenuItem.setEnabled(false);
        JPopupMenu jPopup = new JPopupMenu();
        jPopup.add(openScreenMenuItem);
        jPopup.add(updateAccessListMenuItem);
        jPopup.add(releaseTicketGateMenuItem);
        jPopup.addSeparator();
        jPopup.add(exitMenuItem);

        timerHidePopupMenu = new javax.swing.Timer(1000, new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if (jPopup.isVisible() && !Utils.isMouseWithinComponent(jPopup))
                    jPopup.setVisible(false);
                timerHidePopupMenu.stop();
            }
        });

        jPopup.addMouseListener(new MouseAdapter() {
            public void mouseEntered(MouseEvent e) {
                timerHidePopupMenu.stop();
            }

            public void mouseExited(MouseEvent e) {
                timerHidePopupMenu.start();
            }
        });

        trayIcon.addMouseListener(new MouseAdapter() {
            public void mouseReleased(MouseEvent e) {
                if (e.getClickCount() == 2) {
                    sync();
                } else if (e.getButton() == 3) {
                    jPopup.setLocation(e.getX(), e.getY());
                    jPopup.setInvoker(jPopup);
                    jPopup.setVisible(true);
                }
            }
        });

        try {
            systemTray.add(trayIcon);
        } catch (AWTException e) {
            Object[] options = {"OK"};
            JOptionPane.showOptionDialog(mainScreen, "Nï¿½o foi possï¿½vel adicionar ï¿½cones na bandeja do sistema.", "Bandeja do sistema nï¿½o suportada.",
                    JOptionPane.PLAIN_MESSAGE, JOptionPane.PLAIN_MESSAGE, null, options, options[0]);
            HibernateUtil.shutdown();
            System.exit(0);
        }

        updateAccessListMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                sync();
            }
        });

        releaseTicketGateMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                releaseAccess();
            }
        });

        openScreenMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                mainScreen.showScreen();
            }
        });

        exitMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                exit(true);
            }
        });
    }

    public static void sync() {
        try {
            mainScreen.setCursor(new Cursor(Cursor.WAIT_CURSOR));
            if (!Main.updatingAthleteAccessList) {
                Main.syncAthleteAccessList();
                while (Main.updatingAthleteAccessList)
                    Thread.sleep(100);
                mainScreen.getListaAcessoPanel().cleanFilter();
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            mainScreen.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        }
    }

    private void registerNativeHook() {
        try {
            //desabilita os logs
            Logger logger = Logger.getLogger(GlobalScreen.class.getPackage().getName());
            logger.setLevel(Level.OFF);
            logger.setUseParentHandlers(false);

            GlobalScreen.registerNativeHook();
            GlobalScreen.addNativeKeyListener(new GlobalKeyListener());
        } catch (NativeHookException ex) {
            System.err.println("ERRO AO REGISTRAR A CAPTURA DE TECLAS DE ATALHO");
            System.err.println(ex.getMessage());
        }
    }

    private void loadImages() {
        try {
            systemTray = SystemTray.getSystemTray();
            Toolkit toolkit = Toolkit.getDefaultToolkit();
            trayIconImage = toolkit.getImage(Main.class.getResource(Configurations.IMAGE_FOLDER + customImageFolder + "system_tray_image.png"));
            trayIconImageLoading = toolkit.getImage(Main.class.getResource(Configurations.IMAGE_FOLDER + customImageFolder + "system_tray_image_loading.png"));
            trayIconImage = trayIconImage.getScaledInstance((int) systemTray.getTrayIconSize().getWidth(), -1, Image.SCALE_SMOOTH);
            trayIconImageLoading = trayIconImageLoading.getScaledInstance((int) systemTray.getTrayIconSize().getWidth(),
                    -1, Image.SCALE_SMOOTH);
            favicon = toolkit.getImage(Main.class.getResource(Configurations.IMAGE_FOLDER + customImageFolder + "favicon.png"));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public class GlobalKeyListener implements NativeKeyListener {
        public void nativeKeyPressed(NativeKeyEvent e) {
        }

        public void nativeKeyReleased(NativeKeyEvent e) {

            if (e.getKeyCode() == NativeKeyEvent.VC_F8) {
                if (Main.internoLoggedUser != null) {
                    mainScreen.abreCadastroVisitante(null);
                }
                return;
            }

            if (e.getKeyCode() == NativeKeyEvent.VC_F7) {
                if (Main.internoLoggedUser != null) {
                    mainScreen.abreCadastroPedestre(null);
                }
                return;
            }


            if (e.getKeyCode() == NativeKeyEvent.VC_F9) {
                if (!apertouF9) {
                    apertouF9 = true;
                    apertouF10 = false;
                    releaseAccess();
                }
            }

            if (e.getKeyCode() == NativeKeyEvent.VC_F10) {
                if (!apertouF10) {
                    apertouF10 = true;
                    apertouF9 = false;
                    releaseAccess();
                }
            }

            if (releaseReasonDialog != null
                    && e.getKeyCode() >= 2
                    && e.getKeyCode() <= 10) {
                releaseReasonDialog.pressionarTeclaAtalho(e.getKeyCode() - 1);
            }

            if (EscolherSentidoLiberarAcessoDialog.escolherSentidoDialog != null) {
                if (e.getKeyCode() == NativeKeyEvent.VC_1 || e.getKeyCode() == NativeKeyEvent.VC_KP_1) {
                    JPanel panel = (JPanel) EscolherSentidoLiberarAcessoDialog.escolherSentidoDialog.getContentPane().getComponent(0);
                    JButton botao = (JButton) panel.getComponent(0);
                    botao.doClick();
                    EscolherSentidoLiberarAcessoDialog.escolherSentidoDialog = null;
                } else if (e.getKeyCode() == NativeKeyEvent.VC_2 || e.getKeyCode() == NativeKeyEvent.VC_KP_2) {
                    JPanel panel = (JPanel) EscolherSentidoLiberarAcessoDialog.escolherSentidoDialog.getContentPane().getComponent(0);
                    JButton botao = (JButton) panel.getComponent(2);
                    botao.doClick();
                    EscolherSentidoLiberarAcessoDialog.escolherSentidoDialog = null;
                }
            }
        }

        public void nativeKeyTyped(NativeKeyEvent e) {
        }
    }

    private static void loadProperties() throws IOException {
        properties = new Properties();
        InputStream inputStream = Main.class.getResourceAsStream("/ambiente.properties");
        if (inputStream != null) {
            properties.load(inputStream);
            inputStream.close();
        } else {
            properties = null;
            System.err.println("NAO FOI POSSIVEL CARREGAR O AMBIENTE.PROPERTIES");
        }
    }

    private static void setColors(String firstColorCode, String secondColorCode) {
        if (firstColorCode != null && !firstColorCode.isEmpty()) {
            String[] partes = firstColorCode.split(",");
            firstColor = new Color(Integer.valueOf(partes[0]), Integer.valueOf(partes[1]), Integer.valueOf(partes[2]));
        } else
            firstColor = new Color(39, 57, 74); // DARK BLUE
        if (secondColorCode != null && !secondColorCode.isEmpty()) {
            String[] partes = secondColorCode.split(",");
            secondColor = new Color(Integer.valueOf(partes[0]), Integer.valueOf(partes[1]), Integer.valueOf(partes[2]));
        } else
            secondColor = new Color(70, 178, 202); // LIGHT BLUE
    }

    private static void configLogFile() {
        try {
            System.out.println(sdf.format(new Date()) + "  Enviando as mensagens do console para um arquivo.");

            // salva o printStrem do console
            originalOut = System.out;
            originalErr = System.err;

            // cria o arquivo
            logPath = Utils.getAppDataFolder() + "/Logs_controle_de_acesso/";
            new File(logPath).mkdirs();
            SimpleDateFormat df = new SimpleDateFormat("yyyy_MM_dd__HH_mm_ss");
            String fileName = logPath + "Log_Controle_de_acesso_"
                    + df.format(Calendar.getInstance(new Locale("pt", "BR")).getTime()) + ".txt";
            File file = new File(fileName);
            fileOutputStream = new FileOutputStream(file);

            //outStream = new MyPrintStream(fileOutputStream, originalOut);
            outStream = new PrintStream(fileOutputStream);
            System.out.println(sdf.format(new Date()) + "  Arquivo de destino: " + fileName);

            System.setOut(outStream);
            System.setErr(outStream);
        } catch (Exception e) {
            e.printStackTrace();
        }
        System.out.println("saiu configLogFile");
    }

    private static void closeLogFile() {
        if (fileOutputStream != null) {
            try {
                fileOutputStream.close();
            } catch (Exception e) {
            }
        }
        if (outStream != null) {
            try {
                outStream.close();
            } catch (Exception e) {
            }
        }
        System.setOut(originalOut);
        System.setErr(originalErr);
        System.out.println(sdf.format(new Date()) + "  Voltando as mensagens para o console.");
        System.out.println("Saiu closeLogFile");
    }

    private static JsonObject getNewVisitanteResponseObj(PedestrianAccessEntity visitante) {
        JsonObject responseObj = new JsonObject();

        if (visitante.getEditadoNoDesktop() && !visitante.getCadastradoNoDesktop()) {
            responseObj.addProperty("id", visitante.getId().toString());
        } else {
            responseObj.addProperty("id", "");
        }

        responseObj.addProperty("idTemp", visitante.getIdTemp() != null ? visitante.getIdTemp().toString() : "");
        responseObj.addProperty("idCliente", loggedUser.getIdClient());
        responseObj.addProperty("idUsuario", visitante.getIdUsuario() != null ? visitante.getIdUsuario().toString() : "");

        //Dados basicos
        responseObj.addProperty("nome", visitante.getName() != null ? visitante.getName() : "");
        try {
            responseObj.addProperty("dataNascimento", sdf.format(visitante.getDataNascimento()));
        } catch (Exception e) {
            responseObj.addProperty("dataNascimento", "");
        }
        responseObj.addProperty("email", visitante.getEmail() != null ? visitante.getEmail() : "");
        responseObj.addProperty("cpf", visitante.getCpf() != null ? visitante.getCpf() : "");
        responseObj.addProperty("genero", visitante.getGenero() != null ? visitante.getGenero() : "");
        responseObj.addProperty("rg", visitante.getRg() != null ? visitante.getRg() : "");
        responseObj.addProperty("telefone", visitante.getTelefone() != null ? visitante.getTelefone() : "");
        responseObj.addProperty("celular", visitante.getCelular() != null ? visitante.getCelular() : "");
        responseObj.addProperty("responsavel", visitante.getResponsavel() != null ? visitante.getResponsavel() : "");
        responseObj.addProperty("observacoes", visitante.getObservacoes() != null ? visitante.getObservacoes() : "");

        //Dados empresa
        responseObj.addProperty("idEmpresa", visitante.getIdEmpresa() != null ? visitante.getIdEmpresa().toString() : "");
        responseObj.addProperty("idDepartamento", visitante.getIdDepartamento() != null ? visitante.getIdDepartamento().toString() : "");
        responseObj.addProperty("idCentroCusto", visitante.getIdCentroCusto() != null ? visitante.getIdCentroCusto().toString() : "");
        responseObj.addProperty("idCargo", visitante.getIdCargo() != null ? visitante.getIdCargo().toString() : "");

        //Dados aba lateral
        responseObj.addProperty("foto", visitante.getFoto() != null ? Base64.encodeBase64String(visitante.getFoto()) : "");
        responseObj.addProperty("tipo", visitante.getTipo() != null ? visitante.getTipo() : "");
        responseObj.addProperty("status", visitante.getStatus() != null ? visitante.getStatus() : "INATIVO");
        responseObj.addProperty("matricula", visitante.getMatricula() != null ? visitante.getMatricula() : "");
        responseObj.addProperty("numeroCartao", visitante.getCardNumber() != null ? visitante.getCardNumber() : "");
        responseObj.addProperty("sempreLiberado", visitante.getSempreLiberado() != null ? visitante.getSempreLiberado().toString() : "false");
        responseObj.addProperty("habilitarTeclado", visitante.getHabilitarTeclado() != null
                ? visitante.getHabilitarTeclado().toString() : "false");
        responseObj.addProperty("enviaSmsAoPassarNaCatraca", visitante.getEnviaSmsAoPassarNaCatraca() != null
                ? visitante.getEnviaSmsAoPassarNaCatraca().toString() : "false");

        //Dados endereco
        responseObj.addProperty("cep", visitante.getCep() != null ? visitante.getCep() : "");
        responseObj.addProperty("logradouro", visitante.getLogradouro() != null ? visitante.getLogradouro() : "");
        responseObj.addProperty("numero", visitante.getNumero() != null ? visitante.getNumero() : "");
        responseObj.addProperty("complemento", visitante.getComplemento() != null ? visitante.getComplemento() : "");
        responseObj.addProperty("bairro", visitante.getBairro() != null ? visitante.getBairro() : "");
        responseObj.addProperty("cidade", visitante.getCidade() != null ? visitante.getCidade() : "");
        responseObj.addProperty("estado", visitante.getEstado() != null ? visitante.getEstado() : "");
        responseObj.addProperty("qtdeCreditos", visitante.getQuantidadeCreditos() != null ?
                visitante.getQuantidadeCreditos().toString() : "");

        responseObj.addProperty("luxandIdentifier", visitante.getLuxandIdentifier() != null ?
                visitante.getLuxandIdentifier() : "");
        responseObj.addProperty("idRegra", visitante.getIdRegra() != null ? visitante.getIdRegra().toString() : "");

        responseObj.addProperty("login", visitante.getLogin() != null ? visitante.getLogin() : "");
        responseObj.addProperty("senha", visitante.getSenha() != null ? visitante.getSenha() : "");
        responseObj.addProperty("tipoAcesso", visitante.getTipoAcesso() != null ? visitante.getTipoAcesso() : "");
        responseObj.addProperty("tipoQRCode", visitante.getTipoQRCode() != null ? visitante.getTipoQRCode() : "");
        responseObj.addProperty("qrCodeParaAcesso", visitante.getQrCodeParaAcesso() != null ? visitante.getQrCodeParaAcesso() : "");
        responseObj.addProperty("qtdeAcessosAntesSinc", visitante.getQtdAcessoAntesSinc() != null
                ? visitante.getQtdAcessoAntesSinc().toString() : "");

        adicionaListaDeRegras(responseObj, visitante.getPedestreRegra());

        adicionaListaDeDocumentos(responseObj, visitante.getDocumentos());

        adicionaListaDeEquipamentos(responseObj, visitante.getEquipamentos());

        adicionaListaDeMensagens(responseObj, visitante.getMensagens());

        adicionaListaDeAcessosTransiente(responseObj, visitante.getListaAcessosTransient());

        adicionaListaDeBiometriasTransiente(responseObj, visitante.getListaBiometriasTransient());

        return responseObj;
    }

    private static void adicionaListaDeRegras(JsonObject responseObj, List<PedestreRegraEntity> pedestresRegras) {
        if (pedestresRegras == null || pedestresRegras.isEmpty()) {
            responseObj.add("pedestresRegras", new JsonArray());
            return;
        }

        JsonArray pedestresRegrasArray = new JsonArray();

        for (PedestreRegraEntity pedestreRegra : pedestresRegras) {
            if (!pedestreRegra.getCadastradoNoDesktop()
                    && !pedestreRegra.getRemovidoNoDesktop())
                continue;

            JsonObject pedestreRegraObj = new JsonObject();
            pedestreRegraObj.addProperty("idRegraPR", pedestreRegra.getRegra() != null ? pedestreRegra.getRegra().getId().toString() : "0");
            try {
                pedestreRegraObj.addProperty("validadeRegraPR", sdf.format(pedestreRegra.getValidade()));

            } catch (Exception e) {
                pedestreRegraObj.addProperty("validadeRegraPR", "");
            }
            pedestreRegraObj.addProperty("qtdeDeCreditosPR", pedestreRegra.getQtdeDeCreditos() != null
                    ? pedestreRegra.getQtdeDeCreditos().toString() : "");
            pedestreRegraObj.addProperty("qtdeTotalDeCreditosPR", pedestreRegra.getQtdeTotalDeCreditos() != null
                    ? pedestreRegra.getQtdeTotalDeCreditos().toString() : "");
            pedestreRegraObj.addProperty("diasValidadeCreditoPR", pedestreRegra.getDiasValidadeCredito() != null
                    ? pedestreRegra.getDiasValidadeCredito().toString() : "");
            try {
                pedestreRegraObj.addProperty("dataInicioPeriodoPR", sdf.format(pedestreRegra.getDataInicioPeriodo()));
            } catch (Exception e) {
                pedestreRegraObj.addProperty("dataInicioPeriodoPR", "");
            }

            try {
                pedestreRegraObj.addProperty("dataFimPeriodo", sdf.format(pedestreRegra.getDataFimPeriodo()));

            } catch (Exception e) {
                pedestreRegraObj.addProperty("dataFimPeriodo", "");
            }
            pedestreRegraObj.addProperty("removido", pedestreRegra.getRemovidoNoDesktop().toString());
            pedestreRegraObj.addProperty("idPedestreRegra", pedestreRegra.getId().toString());

            pedestresRegrasArray.add(pedestreRegraObj);
        }

        responseObj.add("pedestresRegras", pedestresRegrasArray);
    }

    private static void adicionaListaDeDocumentos(JsonObject responseObj, List<DocumentoEntity> documentos) {
        if (documentos == null || documentos.isEmpty()) {
            responseObj.add("documentos", new JsonArray());
            return;
        }

        JsonArray documentosArray = new JsonArray();

        for (DocumentoEntity documento : documentos) {
            if (!documento.getCadastradoNoDesktop()
                    && !documento.getRemovidoNoDesktop())
                continue;

            JsonObject documentoObj = new JsonObject();
            documentoObj.addProperty("nomeDoc", documento.getNome() != null ? documento.getNome() : "");
            documentoObj.addProperty("arquivoDoc", documento.getArquivo() != null ? Base64.encodeBase64String(documento.getArquivo()) : "");
            try {
                documentoObj.addProperty("validadeDoc", sdf.format(documento.getValidade()));

            } catch (Exception e) {
                documentoObj.addProperty("validadeDoc", "");
            }
            documentoObj.addProperty("removido", documento.getRemovidoNoDesktop().toString());
            documentoObj.addProperty("idDocumento", documento.getId().toString());

            documentosArray.add(documentoObj);
        }

        responseObj.add("documentos", documentosArray);
    }

    private static void adicionaListaDeEquipamentos(JsonObject responseObj, List<PedestrianEquipamentEntity> equipamentos) {
        if (equipamentos == null || equipamentos.isEmpty()) {
            responseObj.add("equipamentos", new JsonArray());
            return;
        }

        JsonArray equipamentosArray = new JsonArray();

        for (PedestrianEquipamentEntity equipamento : equipamentos) {
            if (!equipamento.getCadastradoNoDesktop()
                    && !equipamento.getRemovidoNoDesktop())
                continue;

            JsonObject equipamentoObj = new JsonObject();
            equipamentoObj.addProperty("idEquipamento", equipamento.getIdEquipamento() != null ? equipamento.getIdEquipamento() : "");
            try {
                equipamentoObj.addProperty("validadeEquipamento", sdf.format(equipamento.getValidadeEquipamento()));

            } catch (Exception e) {
                equipamentoObj.addProperty("validadeEquipamento", "");
            }
            equipamentoObj.addProperty("nomeEquipamento", equipamento.getNomeEquipamento() != null ? equipamento.getNomeEquipamento() : "");
            equipamentoObj.addProperty("removido", equipamento.getRemovidoNoDesktop().toString());
            equipamentoObj.addProperty("id", equipamento.getId().toString());

            equipamentosArray.add(equipamentoObj);
        }

        responseObj.add("equipamentos", equipamentosArray);
    }

    private static void adicionaListaDeMensagens(JsonObject responseObj, List<PedestrianMessagesEntity> mensagens) {
        if (mensagens == null || mensagens.isEmpty()) {
            responseObj.add("mensagens", new JsonArray());
            return;
        }

        JsonArray mensagensArray = new JsonArray();

        for (PedestrianMessagesEntity mensagem : mensagens) {
            if (!mensagem.getCadastradoNoDesktop()
                    && !mensagem.getRemovidoNoDesktop())
                continue;

            JsonObject mensagemObj = new JsonObject();
            mensagemObj.addProperty("nomeMsg", mensagem.getNome() != null ? mensagem.getNome() : "");
            mensagemObj.addProperty("statusMsg", mensagem.getStatus() != null ? mensagem.getStatus().toString() : "");
            mensagemObj.addProperty("mensagemMsg", mensagem.getMensagem() != null ? mensagem.getMensagem() : "");
            mensagemObj.addProperty("quantidadeMsg", mensagem.getQuantidade() != null ? mensagem.getQuantidade().toString() : "0");
            try {
                mensagemObj.addProperty("validadeMsg", sdf.format(mensagem.getValidade()));
            } catch (Exception e) {
                mensagemObj.addProperty("validadeMsg", "");
            }
            mensagemObj.addProperty("removido", mensagem.getRemovidoNoDesktop().toString());
            mensagemObj.addProperty("idMensagem", mensagem.getId().toString());

            mensagensArray.add(mensagemObj);
        }

        responseObj.add("mensagens", mensagensArray);
    }

    private static void adicionaListaDeBiometriasTransiente(JsonObject responseObj, List<BiometricEntity> biometrias) {
        if (biometrias == null || biometrias.isEmpty()) {
            responseObj.add("biometrias", new JsonArray());
            return;
        }

        JsonArray biometriasArray = new JsonArray();

        for (BiometricEntity biometria : biometrias) {
            JsonObject biometriaObj = new JsonObject();

            biometriaObj.addProperty("idBiometria", biometria.getId() != null ? biometria.getId().toString() : "");
            biometriaObj.addProperty("idUserBiometria", biometria.getUser() != null ? biometria.getUser().toString() : "");
            biometriaObj.addProperty("userNameBiometria", biometria.getUserName() != null ? biometria.getUserName() : "");
            biometriaObj.addProperty("finger", biometria.getFinger() != null ? biometria.getFinger().toString() : "");
            biometriaObj.addProperty("template", biometria.getTemplate() != null
                    ? Base64.encodeBase64String(biometria.getTemplate()) : "");
            biometriaObj.addProperty("sample", biometria.getSample() != null ? Base64.encodeBase64String(biometria.getSample()) : null);

            biometriasArray.add(biometriaObj);
        }
        responseObj.add("biometrias", biometriasArray);
    }

    private static void adicionaListaDeAcessosTransiente(JsonObject responseObj, List<LogPedestrianAccessEntity> acessos) {
        if (acessos == null || acessos.isEmpty()) {
            responseObj.add("acessos", new JsonArray());
            return;
        }

        JsonArray acessosArray = new JsonArray();

        for (LogPedestrianAccessEntity acesso : acessos) {
            JsonObject acessoObj = new JsonObject();

            acessoObj.addProperty("idAcesso", acesso.getId() != null ? acesso.getId().toString() : "");
            try {
                acessoObj.addProperty("dataAcesso", sdf.format(acesso.getAccessDate()));
            } catch (Exception e) {
                acessoObj.addProperty("dataAcesso", "");
            }
            acessoObj.addProperty("statusAcesso", acesso.getStatus() != null ? acesso.getStatus() : "");
            acessoObj.addProperty("localizacao", acesso.getLocation() != null ? acesso.getLocation() : "");
            acessoObj.addProperty("razao", acesso.getReason() != null ? acesso.getReason() : "");
            acessoObj.addProperty("direcao", acesso.getDirection() != null ? acesso.getDirection() : "");
            acessoObj.addProperty("equipamento", acesso.getEquipament() != null ? acesso.getEquipament() : "");

            acessosArray.add(acessoObj);
        }
        responseObj.add("acessos", acessosArray);
    }

    @SuppressWarnings("unchecked")
    private static List<LogPedestrianAccessEntity> buscaAcessosVisitante(Long idVisitante) {
        HashMap<String, Object> args = new HashMap<String, Object>();
        args.put("ID_PEDESTRE", idVisitante);

        List<LogPedestrianAccessEntity> acessosVisitantesLocais = (List<LogPedestrianAccessEntity>)
                HibernateUtil.getResultListWithParams(LogPedestrianAccessEntity.class,
                        "LogPedestrianAccessEntity.findAllByPedestre", args);

        if (acessosVisitantesLocais != null
                && acessosVisitantesLocais.size() == 1
                && acessosVisitantesLocais.get(0) != null
                && "INDEFINIDO".equals(acessosVisitantesLocais.get(0).getStatus())) {
            //Aguarda um pouco e pesquisa mais, pode estar passando
            System.out.println("Visitante com log de acesso Indefinido. Aguarda para pesquisar mais dados.");
            Utils.sleep(2000);
            acessosVisitantesLocais = (List<LogPedestrianAccessEntity>)
                    HibernateUtil.getResultListWithParams(LogPedestrianAccessEntity.class,
                            "LogPedestrianAccessEntity.findAllByPedestre", args);
        }

        return acessosVisitantesLocais;
    }

    @SuppressWarnings("unchecked")
    private static PedestrianAccessEntity buscaPedestrePorIdTemp(Long idPedestreTemp) {
        HashMap<String, Object> args = new HashMap<String, Object>();
        args.put("ID_TEMP", idPedestreTemp);

        List<PedestrianAccessEntity> pedestres = (List<PedestrianAccessEntity>)
                HibernateUtil.getResultListWithParams(PedestrianAccessEntity.class,
                        "PedestrianAccessEntity.findByIdTemp", args);

        if (pedestres != null && !pedestres.isEmpty())
            return pedestres.get(0);

        return null;
    }

    @SuppressWarnings("unchecked")
    private static List<BiometricEntity> buscaBiometriasVisitante(Long idVisitante) {
        HashMap<String, Object> args = new HashMap<String, Object>();
        args.put("ID_USER", idVisitante);

        List<BiometricEntity> biometriasVisitantesLocais = (List<BiometricEntity>)
                HibernateUtil.getResultListWithParams(BiometricEntity.class,
                        "BiometricEntity.findByIdUser", args);

        return biometriasVisitantesLocais;
    }

    public static void apagarArquivo(String path) {
        File arquivo = new File(path);
        arquivo.delete();
    }

    private static void apagarPasta(String path) {
        try {
            FileUtils.deleteDirectory(new File(path));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static void zip(String sourceDirPath, String zipFilePath) throws IOException {
        Path p = Files.createFile(Paths.get(zipFilePath));

        try (ZipOutputStream zs = new ZipOutputStream(Files.newOutputStream(p))) {
            Path pp = Paths.get(sourceDirPath);
            Files.walk(pp)
                    .filter(path -> !Files.isDirectory(path))
                    .forEach(path -> {
                        ZipEntry zipEntry = new ZipEntry(pp.relativize(path).toString());
                        try {
                            zs.putNextEntry(zipEntry);
                            Files.copy(path, zs);
                            zs.closeEntry();
                        } catch (IOException e) {
                            System.err.println(e);
                        }
                    });
        }
    }

    public static void unzip(String sourceFile, String uncompressedDirectory) {
        try (ZipFile file = new ZipFile(sourceFile)) {

            FileSystem fileSystem = FileSystems.getDefault();
            Enumeration<? extends ZipEntry> entries = file.entries();

            try {
                Files.createDirectory(fileSystem.getPath(uncompressedDirectory));
            } catch (FileAlreadyExistsException e) {
                apagarPasta(uncompressedDirectory);
                Files.createDirectory(fileSystem.getPath(uncompressedDirectory));
            }

            while (entries.hasMoreElements()) {
                ZipEntry entry = entries.nextElement();
                InputStream is = file.getInputStream(entry);
                BufferedInputStream bis = new BufferedInputStream(is);
                String uncompressedFileName = uncompressedDirectory + entry.getName();
                Path uncompressedFilePath = fileSystem.getPath(uncompressedFileName);
                Files.createFile(uncompressedFilePath);
                FileOutputStream fileOutput = new FileOutputStream(uncompressedFileName);
                while (bis.available() > 0) {
                    fileOutput.write(bis.read());
                }
                fileOutput.close();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    protected static void gravaFotosNoDisco(Long idPedestre, JsonObject responseObj) {
        byte[] photos = null;
        if (!responseObj.get("newPhotos").isJsonNull())
            photos = Base64.decodeBase64(responseObj.get("newPhotos").getAsString());

        if (photos != null) {
            String saveZipPath = Utils.getAppDataFolder() + "/reconhecimento_facial/fotos/" + idPedestre + ".zip";
            String folderPath = Utils.getAppDataFolder() + "/reconhecimento_facial/fotos/" + idPedestre + "/";

            String path = Utils.getAppDataFolder() + "/reconhecimento_facial/fotos/";
            File diretorioFotos = new File(path);
            if (!diretorioFotos.exists())
                diretorioFotos.mkdirs();

            try (FileOutputStream fos = new FileOutputStream(saveZipPath)) {
                fos.write(photos);
            } catch (FileNotFoundException e) {
                e.printStackTrace();
            } catch (IOException e) {
                e.printStackTrace();
            }

            unzip(saveZipPath, folderPath);

            apagarArquivo(saveZipPath);
        }
    }

    @SuppressWarnings("unchecked")
    protected static List<PedestrianAccessEntity> buscaTodosOsPedestresAtivos() {
        List<PedestrianAccessEntity> pedestres = null;

        pedestres = (List<PedestrianAccessEntity>) HibernateUtil.getResultList(PedestrianAccessEntity.class,
                "PedestrianAccessEntity.findAllActivesOnlyIdAndLastPhotosTaken");

        return pedestres;
    }

    public static boolean verificaSePossuiCamerasAdicionadas() {
        boolean possuiCamera = false;

        if (Main.devicesList != null && !Main.devicesList.isEmpty()) {
            for (Device device : Main.devicesList) {
                if (device instanceof FacialDevice) {
                    possuiCamera = true;
                    break;
                }
            }
        }
        return possuiCamera;
    }

    protected static FacialDevice buscaDeviceFacial() {
        FacialDevice device = null;
        for (Device d : Main.devicesList) {
            if (d instanceof FacialDevice) {
                device = (FacialDevice) d;
                break;
            }
        }
        return device;
    }

    private boolean verificaSePossuiLeitorLcAdicionado() {
        for (Device device : devicesList) {
            if (device instanceof LcDevice) {
                return true;
            }
        }
        return false;
    }

    private ServerDevice verificaSePossuiServidorAdicionado() {
        for (Device device : devicesList) {
            if (device instanceof ServerDevice) {
                return (ServerDevice) device;
            }
        }
        return null;
    }

    private void initializeLuxandSDK() {
        if (Configurations.LUXAND_KEY != null && !Configurations.LUXAND_KEY.isEmpty()) {
            LuxandService.getInstance().initializeSDK(Configurations.LUXAND_KEY);
        }
    }

    private static void finalizeDevices() {
        if (Main.devicesList != null && !Main.devicesList.isEmpty()) {
            for (Device device : Main.devicesList) {
                if (device instanceof FacialDevice) {
                    FacialDevice facialDevice = (FacialDevice) device;
                    if (facialDevice.isConnected())
                        try {
                            facialDevice.disconnect("");
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                }
                if (device instanceof LcDevice) {
                    LcDevice lc = (LcDevice) device;
                    if (lc.isConnected()) {
                        try {
                            lc.disconnect("");
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    }
                }
            }
        }

        if (LuxandService.getInstance().serviceInitialized)
            LuxandService.getInstance().FinalizeSDK();
    }

}
