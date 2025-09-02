package com.protreino.services.main;

import java.awt.AWTException; 
import java.awt.Color; 
import java.awt.Cursor;
import java.awt.Image;
import java.awt.SystemTray;
import java.awt.Toolkit;
import java.awt.TrayIcon;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.net.ConnectException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.TimerTask;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

import javax.swing.JButton;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;
import javax.swing.Timer;
import javax.swing.UIManager;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.io.FileUtils;
import org.java_websocket.client.WebSocketClient;
import org.jnativehook.GlobalScreen;
import org.jnativehook.NativeHookException;
import org.jnativehook.keyboard.NativeKeyEvent;
import org.jnativehook.keyboard.NativeKeyListener;

import com.protreino.services.exceptions.HikivisionIntegrationException; // Sincroniza o sistema com o facial da Hikivision
import com.protreino.services.entity.HikivisionIntegrationErrorEntity; //Hikivision
import com.protreino.services.entity.LocalEntity;
import com.protreino.services.enumeration.HikivisionAction; //Hikivision
import com.protreino.services.repository.HikivisionIntegrationErrorRepository; //Hikivision
import com.protreino.services.repository.LocalRepository;
import com.protreino.services.to.hikivision.HikivisionDeviceSimplificadoTO;
import com.protreino.services.to.hikivision.HikivisionDeviceTO; //Hikivision
import com.protreino.services.usecase.HikivisionUseCases; //Hikivision
import com.protreino.services.utils.HikivisionTcpServer; //Hikivision
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.protreino.services.client.SmartAcessoClient;
import com.protreino.services.constants.Configurations;
import com.protreino.services.constants.Tipo;
import com.protreino.services.devices.AlmitecDevice;
import com.protreino.services.devices.Device;
import com.protreino.services.devices.FacialDevice;
import com.protreino.services.devices.LcDevice;
import com.protreino.services.devices.ServerDevice;
import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.entity.EmpresaEntity;
import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.entity.ParametroEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.entity.PlanoEntity;
import com.protreino.services.entity.RegraEntity;
import com.protreino.services.entity.UserEntity;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.enumeration.NotificationType;
import com.protreino.services.enumeration.TcpMessageType;
import com.protreino.services.exceptions.ErrorOnSendLogsToWebException;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.repository.HibernateLocalAccessData;
import com.protreino.services.repository.LogPedestrianAccessRepository;
import com.protreino.services.repository.PedestrianAccessRepository;
import com.protreino.services.repository.RegraRepository;
import com.protreino.services.screens.EscolherSentidoLiberarAcessoDialog;
import com.protreino.services.screens.MainScreen;
import com.protreino.services.screens.ReleaseReasonDialog;
import com.protreino.services.screens.SplashScreen;
import com.protreino.services.services.LuxandService;
import com.protreino.services.services.WebSocketCadastroClientService;
import com.protreino.services.services.WebSocketLiberacaoClientService;
import com.protreino.services.to.EmpresaTO;
import com.protreino.services.to.LocalTo;
import com.protreino.services.to.RegraTO;
import com.protreino.services.to.TcpMessageTO;
import com.protreino.services.usecase.ReleaseAccessUseCase;
import com.protreino.services.usecase.SincronismoHorariosHikivision;
import com.protreino.services.usecase.SyncPedestrianAccessListUseCase;
import com.protreino.services.usecase.SyncTemplatesInTopDataDevices;
import com.protreino.services.utils.BroadcastServer;
import com.protreino.services.utils.FacialTopDataIntegrationService;
import com.protreino.services.utils.HttpConnection;
import com.protreino.services.utils.TcpServer;
import com.protreino.services.utils.Utils;
import it.sauronsoftware.junique.AlreadyLockedException;
import it.sauronsoftware.junique.JUnique;
import it.sauronsoftware.junique.MessageHandler;

public class Main {
	
	private static final String MAQUINA_TEM_SERVER_MESSAGE = "Sincronizacao desabilitada: Maquina possui servidor";

    public static MainScreen mainScreen;
    private static SplashScreen splash;

    public static UserEntity loggedUser = null;
    public static UserEntity internoLoggedUser = null;

    private static ServerDevice servidor;

    private static Long lastSyncLog = 0L;
    private static Long lastSyncHikivision = 0l;

    private static Long lastSyncGetUsers = 0L;
    private static Long lastSyncGetLocais = 0L;
    private static Long lastSyncGetEmpresas = 0L;
    private static Long lastSyncGetRegras = 0L;
    private static Long lastSyncGetParametros = 0L;
    private static Long lastSyncGetPlanos = 0L;

    private static boolean updatingLogAccessList = false;
    private static boolean updatingHikivisionAccessList = false;
    private static boolean updatingUsersAccessList = false;
    private static boolean uploadingPhotosPedestres = false;

    private static boolean isCadastrandoBiometria = false;

    public static List<Device> devicesList;
    public static List<Device> dispositivosReconectando;

    private static Properties properties;
    public static String urlApplication;
    public static String nomeAplicacao;
    public static String customImageFolder;
    public static Image favicon;
    public static Color firstColor;
    public static Color secondColor;

    public static Timer timerSyncUsersAccessList;
    public static Timer timerSyncAthleteAccessList;
    public static Timer timerFingerProcessingHikivision;
    public static Timer timerSyncHikivision;
    public static Timer timerSyncLogAthleteAccess;
    public static Timer timerOnline;
    public static Timer timerHidePopupMenu;
    public static java.util.Timer timerTasksOfDay;
    public static TimerTask uTimerTask;

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
    public static AlmitecDevice almTCP;
    public static HikivisionTcpServer hikivisionTcpServer;
    public static FacialTopDataIntegrationService facialTopDataIntegrationService;
    public static WebSocketCadastroClientService webSocketClientCadastroService;
    public static WebSocketLiberacaoClientService webSocketClientLiberacaoService;
    public static SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss:sss");
    public static SimpleDateFormat sdfWithoutTIme = new SimpleDateFormat("dd/MM/yyyy");
    public static boolean desenvolvimento;
    public static boolean possuiLeitorLcAdd;
    public static boolean validandoAcesso = false;
    public static final String CHAVE_DE_INTEGRACAO_COMTELE = "Chave de integracao Comtele";
    
    private static final LogPedestrianAccessRepository logPedestrianAccessRepository = new LogPedestrianAccessRepository();
    private static final SmartAcessoClient smartAcessoClient = new SmartAcessoClient();
    private static final SyncPedestrianAccessListUseCase syncPedestrianAccessListUseCase = new SyncPedestrianAccessListUseCase();
    private static final ReleaseAccessUseCase releaseAccessUseCase = new ReleaseAccessUseCase();
    private static final SyncTemplatesInTopDataDevices syncTemplatesInTopDataDevices = new SyncTemplatesInTopDataDevices();
    private static final LocalRepository localRepository = new LocalRepository();
    private HikivisionUseCases hikivisionUseCases;

    public static void main(String[] args) {

        // Verifica JVM
//		String jvmArchitecture = Utils.getJvmArchitecture();
//		if ("64".equals(jvmArchitecture)) {
//			Object[] options = {"OK"};
//		    JOptionPane.showOptionDialog(null, "JVM de 64 bits detectada.  necessario uma JVM de 32 bits.","JVM 32 bits necessoria",
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
                    if (mainScreen != null) {
                    	mainScreen.showScreen();
                    }

                    return null;
                }
            });
        } catch (AlreadyLockedException e) {
            JUnique.sendMessage(nomeAplicacao + "_Controle_Acesso_lockInstance", "Ola!");
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
                    HibernateLocalAccessData.getSessionFactory().getCurrentSession();
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
                    
                    if(Utils.webSocketClienteHikivisionHabilitado() && Objects.isNull(Main.servidor)) {
                    	hikivisionUseCases = new HikivisionUseCases();
                    	
                        webSocketClientCadastroService = new WebSocketCadastroClientService(Main.urlApplication, Main.loggedUser.getIdClient());
                        webSocketClientCadastroService.conectar();
                        
                        webSocketClientLiberacaoService = new WebSocketLiberacaoClientService(Main.urlApplication, Main.loggedUser.getIdClient());
                        webSocketClientLiberacaoService.conectar();
                        
                        List<HikivisionDeviceTO.Device> devices = hikivisionUseCases.listarDispositivos();
                        List<HikivisionDeviceSimplificadoTO> devicesSimplificados = devices.stream().map(device -> new HikivisionDeviceSimplificadoTO(device.getDevName(),device.getDevIndex()))
                        .collect(Collectors.toList());
                       
                        webSocketClientLiberacaoService.enviarEquipamentos(devicesSimplificados);
                    }
                    
                    
                    if (Utils.isTopDataFacialEnable()) {
                    	if(!Main.temServidor()) {
                        	String ipServidor = Utils.getPreference("TopdataServerRecognizerURL");
                        	Integer porta =  Integer.valueOf(Utils.getPreference("topDataSocketPort"));
                        	
                        	facialTopDataIntegrationService = new FacialTopDataIntegrationService(ipServidor, porta);
                        	facialTopDataIntegrationService.conectarPorta();
                    	}
                    }
                    
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
    }

    private void decideSeMostraTelaPrincipal() {
        if (loggedUser == null) {
        	mainScreen.showScreen();
        	return;
        }
        
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
        
        if (nenhumDispositivoConectado) {
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

                                                //verifica se e topdata e se esta sincronizando
//												if(device instanceof TopDataDevice) {
//													TopDataDevice
//												}

                                                device.setDesiredStatus(DeviceStatus.DISCONNECTED);
                                                exibirMensagemErro = true;
                                            }
                                        }
                                        if (exibirMensagemErro) {
                                            if (!mainScreen.isVisible()) {
                                            	mainScreen.showScreen();
                                            }

                                            mainScreen.refresh();
                                            String html = "<html><body width='%1s'>"
                                                    + "<p>Nao conseguimos reconectar a catraca e/ou leitor, verifique os itens abaixo:"
                                                    + "<br><br>"
                                                    + "- A catraca e/ou leitor está ligado na tomada?"
                                                    + "<br>"
                                                    + "- Os cabos estáo conectados de forma correta?"
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
        loggedUser = HibernateAccessDataFacade.getLoggedUser("UserEntity.findAll");
        
        if (Objects.isNull(loggedUser)) {
        	return;
        }

    	SyncPedestrianAccessListUseCase.setLastSync(loggedUser.getLastSync() != null ? loggedUser.getLastSync().getTime() : 0L);;

    	lastSyncLog = loggedUser.getLastSyncLog() != null ? loggedUser.getLastSyncLog().getTime() : 0L;
        lastSyncHikivision = loggedUser.getLastSyncHikivision() != null
				? loggedUser.getLastSyncHikivision().getTime() : 0l;

        lastSyncGetUsers = loggedUser.getLastSyncUser() != null
                ? loggedUser.getLastSyncUser().getTime() : 0L;
        lastSyncGetLocais = loggedUser.getLastSyncGetLocais() != null
        		? loggedUser.getLastSyncGetLocais().getTime() : 0L;
        lastSyncGetEmpresas = loggedUser.getLastSyncEmpresa() != null
                ? loggedUser.getLastSyncEmpresa().getTime() : 0L;
        lastSyncGetRegras = loggedUser.getLastSyncRegra() != null
                ? loggedUser.getLastSyncRegra().getTime() : 0L;
        lastSyncGetParametros = loggedUser.getLastSyncParametro() != null
                ? loggedUser.getLastSyncParametro().getTime() : 0L;
        lastSyncGetPlanos = loggedUser.getLastSyncPlano() != null
                ? loggedUser.getLastSyncPlano().getTime() : 0L;

        releaseTicketGateMenuItem.setEnabled(true);
        updateAccessListMenuItem.setEnabled(true);

        inicializaTimers();
        
       // verificaRemocaoDefacesHV();
      
        String hora = Utils.getPreference("hourAutomaticRoutines");
        hora = (hora == null || "".equals(hora) ? "0" : hora);

        Long periodExpirar = 24L * 3600L * 1000L;
        Calendar inicio = Calendar.getInstance();
        System.out.println(sdf.format(new Date()) + "  ... Hora para reset definido para " + hora + "Hr");

        inicio.set(Calendar.HOUR_OF_DAY, Integer.parseInt(hora));
        inicio.set(Calendar.MINUTE, 0);
        inicio.set(Calendar.SECOND, 0);
        if (new Date().getTime() > inicio.getTimeInMillis()) {
            inicio.add(Calendar.DATE, 1);
        }

        timerTasksOfDay.scheduleAtFixedRate(uTimerTask, inicio.getTime(), periodExpirar.longValue());

        List<DeviceEntity> lista = (List<DeviceEntity>) HibernateAccessDataFacade.
                getResultList(DeviceEntity.class, "DeviceEntity.findAll");
        if (lista != null && !lista.isEmpty()) {
            for (DeviceEntity deviceEntity : lista) {
            	devicesList.add(deviceEntity.recoverDevice());
            }
        }
        
        boolean haveDefaultDevice = false;
        
        for (Device device : devicesList) {
            if (device.isDefaultDevice()) {
                haveDefaultDevice = true;
                break;
            }
        }

        if (!haveDefaultDevice && !devicesList.isEmpty()) {
        	devicesList.get(0).setDefaultDevice(true);
        }

        Main.servidor = verificaSePossuiServidorAdicionado();
        Main.possuiLeitorLcAdd = verificaSePossuiLeitorLcAdicionado();
    }

	private void inicializaTimers() {
        timerSyncUsersAccessList.start();
        timerSyncAthleteAccessList.start();
        timerSyncLogAthleteAccess.start();
        timerSyncHikivision.start();
        if(!Objects.isNull(timerFingerProcessingHikivision))
        timerFingerProcessingHikivision.start();
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

        if (timerSyncAthleteAccessList.isRunning()) {
        	timerSyncAthleteAccessList.stop();
        }
        if (timerSyncLogAthleteAccess.isRunning()) {
        	timerSyncLogAthleteAccess.stop();
        }
        if (timerSyncUsersAccessList.isRunning()) {
        	timerSyncUsersAccessList.stop();
        }
        if(timerSyncHikivision.isRunning()) {
        	timerSyncHikivision.stop();
        }
        if(timerFingerProcessingHikivision.isRunning()) {
        	timerFingerProcessingHikivision.stop();
        }

        lastSyncGetUsers = 0L;
        lastSyncGetEmpresas = 0L;
        lastSyncGetRegras = 0L;
        lastSyncGetLocais = 0L;
        lastSyncGetParametros = 0L;
        lastSyncGetPlanos = 0L;

        while (SyncPedestrianAccessListUseCase.getUpdatingPedestrianAccessList()) { 
        	// aguarda atualizacao corrente perceber que foi desconectado e parar atualizacao
            Utils.sleep(50);
        }

        if (!getUpdatingLogAccessList()) { // tenta enviar log de acesso antes de apagar tudo
            syncLogAthleteAccess();
        }
        
        while (getUpdatingLogAccessList()) {
        	Utils.sleep(50);
        }

        while (getUpdatingUsersAccessList()) {
        	Utils.sleep(50);
        }

        while (getUploadingPhotosPedestres()) {
        	Utils.sleep(50);
        }

        Boolean sessaoLimpa = HibernateLocalAccessData.cleanUserSession();
        if (sessaoLimpa) {
            Utils.createNotification("Sessoo de usuario encerrada!", NotificationType.GOOD);
            releaseTicketGateMenuItem.setEnabled(false);
            updateAccessListMenuItem.setEnabled(false);

        } else
            Utils.createNotification("Ocorreu um erro ao finalizar a sessoo.", NotificationType.BAD);

    }

    public static void exit(boolean exibirConfirmacao) {
        if (exibirConfirmacao) {
            int dialogResult = JOptionPane.showConfirmDialog(null, "As catracas serao  desconectadas. Deseja realmente sair?", "Confirmacao",
                    JOptionPane.YES_NO_OPTION, JOptionPane.PLAIN_MESSAGE);
            if (dialogResult != JOptionPane.YES_OPTION)
                return;
        }

        systemTray.remove(trayIcon);
        HibernateLocalAccessData.shutdown();
        finalizeDevices();
        if (!desenvolvimento) {
        	//closeLogFile();
        	System.exit(0);
        }
    }

    private void configureTimers() {
        try {
            timerSyncUsersAccessList = new Timer(Integer.valueOf(Utils.getPreference("timeUserAccessList")) * 60000, new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    syncUsersAccessList();
                }
            });
            timerSyncAthleteAccessList = new Timer(Integer.valueOf(Utils.getPreference("timeAccessList")) * 60000, new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                	syncPedestrianAccessListUseCase.syncPedestrianAccessList();
                }
            });
            timerSyncLogAthleteAccess = new Timer(Configurations.TIME_LOG_ATHLETE_ACCESS, new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    syncLogAthleteAccess();
                }
            });
            
            final ActionListener reproccessHikivisionErrorsListner = new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    syncHikivisionAccessList();
                }
            };
            final Integer reproccessHikivisionErrorsMinutes = Integer.valueOf(Utils.getPreference("reproccessHikivisionErrors"));
            timerSyncHikivision = new Timer(reproccessHikivisionErrorsMinutes * 60000, !reproccessHikivisionErrorsMinutes.equals(0) 
            		? reproccessHikivisionErrorsListner : null);
            
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
            
            if(Utils.getPreferenceAsBoolean("hikiVisionFingerRegistration")) {
            	timerFingerProcessingHikivision = new Timer(Integer.valueOf(Utils.getPreference("hikivisionTimeProcessing"))* 60000, new ActionListener() {
                     public void actionPerformed(ActionEvent e) {
                    	hikivisionUseCases = new HikivisionUseCases();
                    	hikivisionUseCases.processarBiometriasComErros();
                     }
                 });
            	
            }
            
            timerSyncUsersAccessList.stop();
            if(!Objects.isNull(timerFingerProcessingHikivision)) {
            	 timerFingerProcessingHikivision.stop();
            }
           
            timerSyncAthleteAccessList.stop();
            timerSyncLogAthleteAccess.stop();
            timerSyncHikivision.stop();
            
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
						HikivisionUseCases hikivisionUseCases = new HikivisionUseCases();
						hikivisionServerIsConnected = hikivisionUseCases.getSystemInformation();
					}

					if(mainScreen != null && Utils.isHikivisionConfigValid()) {
						mainScreen.setHikivisionConnectionStatusLabel(hikivisionServerIsConnected);
					}

				} catch (Exception e) {
					if(Utils.isHikivisionConfigValid()) {
						mainScreen.setHikivisionConnectionStatusLabel(false);
					}
				}
			};
		}.start();
	}


    public static void tasksOfDay(boolean timerCall) {
        if (timerCall) {
            limpaCartoesVisitantes();
            buscaVisitantesFotoAlterada();
        }
        
        syncTemplatesInTopDataDevices.execute();
        
        limpaSentidoTodos();
        limpaStatusCartoes();
        limpaTelas();
        closeLogFile();
        configLogFile();
    }
	
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

        HibernateAccessDataFacade.resetStatusAllCards();

        System.out.println("saiu limpaStatusCartoes");
    }

    private static void limpaSentidoTodos() {
        if (Main.temServidor()) {
        	return;
        }

        if (!Utils.getPreferenceAsBoolean("enableDirectionClear")) {
        	return;
        }

        //adiciona data para que os calculos de quantidade
        //de giros sejam refeitos
        loggedUser.setDateNewAccess(new Date());
        HibernateAccessDataFacade.save(UserEntity.class, loggedUser);

        //apaga tambem dados de giros anteriores nao registrados
        HibernateAccessDataFacade.apagaDadosDeGiro(loggedUser.getDateNewAccess());

        System.out.println("Saiu limpaSentidoTodos");

    }
    
    @SuppressWarnings("unchecked")
	private static void buscaVisitantesFotoAlterada() {
    	System.out.println("Buscando visitantes do dia com foto");
    	HashMap<String, Object> args = new HashMap<String, Object>();
    	
    	final int resultListCount = HibernateAccessDataFacade
    			.getResultListCount(PedestrianAccessEntity.class, "PedestrianAccessEntity.countAllVisitantesWhithPassagemHikivision");
    	System.out.println("Quantidade de visitantes hoje: " + resultListCount);
    	
    	if(resultListCount == 0) {
    		return;
    	}
    	
    	int pageSize = 500;
    	int offset = 0;
    	
    	HikivisionUseCases hikivisionUseCases = new HikivisionUseCases();
    	
    	do {
    		List<PedestrianAccessEntity> visitantes = (List<PedestrianAccessEntity>) HibernateAccessDataFacade
    				.getResultListWithParams(PedestrianAccessEntity.class, 
    						"PedestrianAccessEntity.findAllVisitantesWhithWhithPassagemHikivision", args, offset, pageSize);
    		
    		visitantes.forEach(visitante -> {
    			hikivisionUseCases.removerUsuarioFromDevices(visitante);
    			visitante.setFotoEnviada(null);
    			HibernateLocalAccessData.update(PedestrianAccessEntity.class, visitante);
    		});
    		
    		offset = offset + pageSize;
    		System.out.println("quantidade : " + offset);
    	} while(offset < resultListCount);
    }

    private static void limpaCartoesVisitantes() {
        if (Main.temServidor()) {
        	return;
        }

        if (!Utils.getPreferenceAsBoolean("enableCardAcessClear")) {
        	return;
        }

        //pesquisa todos os pedestres que estáo com cartao ativado
        //e que tenham um credito
        HibernateAccessDataFacade.apagaDadosCartao();
        HibernateAccessDataFacade.apagaDadosDeUltimoSentido();
        HibernateAccessDataFacade.apagaQuantidadeAcessosAsinc();
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
        if (getUpdatingUsersAccessList()) {
        	return;
        }

        if (Objects.nonNull(Main.servidor)) {
            System.out.println(sdf.format(new Date()) + " " + MAQUINA_TEM_SERVER_MESSAGE);
            return;
        }
        
        if(Objects.nonNull(webSocketClientCadastroService)) {
        	webSocketClientCadastroService.ping();
        }
        
		if (Objects.nonNull(webSocketClientLiberacaoService)) {
			HikivisionUseCases hikivisionUseCases = new HikivisionUseCases();
			
			webSocketClientLiberacaoService.ping();
			
			List<HikivisionDeviceTO.Device> devices = hikivisionUseCases.listarDispositivos();
			List<HikivisionDeviceSimplificadoTO> devicesSimplificados = devices.stream()
					.map(device -> new HikivisionDeviceSimplificadoTO(device.getDevName(), device.getDevIndex()))
					.collect(Collectors.toList());

			webSocketClientLiberacaoService.enviarEquipamentos(devicesSimplificados);
		}
        
        setUpdatingUsersAccessList(true);

        SwingWorker<Void, Void> worker = getSyncUsersAccessListWorker();
        worker.execute();
    }
    
    public static synchronized void setUpdatingUsersAccessList(final boolean status) {
    	updatingUsersAccessList = status;
    }
    
    public static synchronized boolean getUpdatingUsersAccessList() {
    	return updatingUsersAccessList;
    }
    
    public static synchronized void setUpdatingLogAccessList(final boolean status) {
    	updatingLogAccessList = status;
    }
    
    public static synchronized boolean getUpdatingLogAccessList() {
    	return updatingLogAccessList;
    }
    
    public static synchronized void setUpdatingHikivisionAccessList(final boolean status) {
    	updatingHikivisionAccessList = status;
    }
    
    public static synchronized boolean getUpdatingHikivisionAccessList() {
    	return updatingHikivisionAccessList;
    }
    
    public static synchronized void setUploadingPhotosPedestres(final boolean status) {
    	uploadingPhotosPedestres = status;
    }
    
    public static synchronized boolean getUploadingPhotosPedestres() {
    	return uploadingPhotosPedestres;
    }
    
    private static SwingWorker<Void, Void> getSyncUsersAccessListWorker() {
    	return new SwingWorker<Void, Void>() {
            @Override
            public Void doInBackground() {
                try {
                    if (timerSyncUsersAccessList.isRunning()) {
                    	timerSyncUsersAccessList.stop();
                    }

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
                    
                    uploadRegrasComPlano();

                    requestAllRegras();

                    requestAllPlanos();
                    
                    requestAllLocais();

                } catch (ConnectException e) {
                    System.err.println("Nao foi possivel comunicar com o servidor.");

                } catch (Exception e) {
                    e.printStackTrace();
                    System.out.println(sdf.format(new Date()) + "  ERRO NA SINCRONIZACAO: Exception: " + e.getMessage());

                } finally {
                    if (loggedUser != null) {
                    	timerSyncUsersAccessList.start();
                    }
                    setUpdatingUsersAccessList(false);
                    trayIcon.setImage(trayIconImage);

                    if (mainScreen != null && mainScreen.isVisible()) {
                        mainScreen.getListaAcessoPanel().getSyncButton().setText("Atualizar lista com o servidor");
                        mainScreen.getListaAcessoPanel().getSyncButton().setEnabled(true);
                        mainScreen.getListaAcessoPanel().getSyncButton().revalidate();
                        mainScreen.getListaAcessoPanel().updateDateLastSync();
                    }
                }
                return null;
            }

            private void requestAllLocais() {
                List<LocalTo> locaisTo = smartAcessoClient.requestAllLocais(lastSyncGetLocais);

                if (Objects.isNull(locaisTo) || locaisTo.isEmpty()) {
                    System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: sem registros de locais para receber");
                    return;
                }

                for (LocalTo localTo : locaisTo) {
                    if (Objects.isNull(loggedUser)) {
                        break;
                    }
                    
                    Optional<LocalEntity> localExistente = localRepository.getLocalByName(localTo.getNome());
                    
                    if(localExistente.isPresent()) {
                    	localExistente.get().update(localTo);
                    	HibernateAccessDataFacade.update(LocalEntity.class, localExistente.get());
                    } else {
                    	LocalEntity local = localTo.toLocalEntity();
                    	HibernateAccessDataFacade.save(LocalEntity.class, local);
                    }
                    
                }

                if (loggedUser != null) {
                    Utils.sleep(1000);
                    lastSyncGetLocais = Calendar.getInstance(new Locale("pt", "BR")).getTimeInMillis();
                    loggedUser.setLastSyncGetLocais(new Date(lastSyncGetLocais));
                    loggedUser = (UserEntity) HibernateAccessDataFacade.updateUser(UserEntity.class, loggedUser)[0];
                }
            }

			private void requestAllUsers() throws IOException {
                List<UserEntity> userAccessList = smartAcessoClient.requestAllUsers(lastSyncGetUsers);

                if (Objects.isNull(userAccessList) || userAccessList.isEmpty()) {
                    System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: sem registros de usuarios para receber");
                    return;
                }

                for (UserEntity user : userAccessList) {
                    if (Objects.isNull(loggedUser)) {
                        break;
                    }

                    user.setIdClient(loggedUser.getIdClient());

                    UserEntity usuarioExistente = (UserEntity) HibernateAccessDataFacade
                            .getSingleResultById(UserEntity.class, user.getId());

                    if (usuarioExistente != null) {
                        usuarioExistente.update(user);
                        if (loggedUser.getId().equals(user.getId())) {
                            loggedUser = (UserEntity) HibernateAccessDataFacade.update(UserEntity.class, usuarioExistente)[0];

                        } else {
                        	HibernateAccessDataFacade.update(UserEntity.class, usuarioExistente);
                        }

                    } else {
                    	HibernateAccessDataFacade.save(UserEntity.class, user);
                    }
                }

                if (loggedUser != null) {
                    Utils.sleep(1000);
                    lastSyncGetUsers = Calendar.getInstance(new Locale("pt", "BR")).getTimeInMillis();
                    loggedUser.setLastSyncUser(new Date(lastSyncGetUsers));
                    loggedUser = (UserEntity) HibernateAccessDataFacade.updateUser(UserEntity.class, loggedUser)[0];
                }
            }

            private void requestAllEmpresas() throws IOException {
                List<EmpresaTO> empresasTOList = smartAcessoClient.requestAllEmpresas(lastSyncGetEmpresas);

                if (Objects.isNull(empresasTOList) || empresasTOList.isEmpty()) {
                    System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: sem registros de empresas para receber");
                    return;
                }

                for (EmpresaTO empresaTO : empresasTOList) {
                    if (Objects.isNull(loggedUser)) {
                        break;
                    }

                    EmpresaEntity empresa = new EmpresaEntity(empresaTO);
                    empresa.setIdClient(loggedUser.getIdClient());

                    EmpresaEntity empresaExistente = (EmpresaEntity) HibernateAccessDataFacade
                            .getSingleResultById(EmpresaEntity.class, empresa.getId());

                    if (empresaExistente != null) {
                        empresaExistente.update(empresa);
                        HibernateAccessDataFacade.update(EmpresaEntity.class, empresaExistente);

                    } else {
                    	HibernateAccessDataFacade.save(EmpresaEntity.class, empresa);
                    }
                }

                if (loggedUser != null) {
                    Utils.sleep(1000);
                    lastSyncGetEmpresas = Calendar.getInstance(new Locale("pt", "BR")).getTimeInMillis();
                    loggedUser.setLastSyncEmpresa(new Date(lastSyncGetEmpresas));
                    loggedUser = (UserEntity) HibernateAccessDataFacade.updateUser(UserEntity.class, loggedUser)[0];
                }
            }

            private void requestAllRegras() throws IOException {
                List<RegraTO> regrasTOList = smartAcessoClient.requestAllRegras(lastSyncGetRegras);

                if (regrasTOList == null || regrasTOList.isEmpty()) {
                    System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: sem registros de regras para receber");
                    return;
                }

                for (RegraTO regraTO : regrasTOList) {
                    if (loggedUser == null) {
                    	break;
                    }

                    RegraEntity regra = new RegraEntity(regraTO);
                    regra.setIdClient(loggedUser.getIdClient());
                    EmpresaEntity empresa = (EmpresaEntity) HibernateAccessDataFacade.getSingleResultById(EmpresaEntity.class, regraTO.getIdEmpresa());
                    regra.setEmpresa(empresa);

                    RegraEntity regraExistente = (RegraEntity) HibernateAccessDataFacade
                            .getSingleResultById(RegraEntity.class, regra.getId());

                    if (regraExistente != null) {
                        regraExistente.update(regra);
                        HibernateAccessDataFacade.update(RegraEntity.class, regraExistente);
                        
                    } else {
                    	HibernateAccessDataFacade.save(RegraEntity.class, regra);
                    }
                }
                
                SincronismoHorariosHikivision sincronismoHorariosHikivision = new SincronismoHorariosHikivision();
                sincronismoHorariosHikivision.execute();

                if (loggedUser != null) {
                    Utils.sleep(1000);
                    lastSyncGetRegras = Calendar.getInstance(new Locale("pt", "BR")).getTimeInMillis();
                    loggedUser.setLastSyncRegra(new Date(lastSyncGetRegras));
                    loggedUser = (UserEntity) HibernateAccessDataFacade.updateUser(UserEntity.class, loggedUser)[0];
                }
            }
            
            private void uploadRegrasComPlano() {
            	final RegraRepository regraRepository = new RegraRepository();
            	List<RegraEntity> regras = regraRepository.buscaRegrasComPlanoETemplate();
            	
            	if(Objects.isNull(regras) || regras.isEmpty()) {
            		System.out.println("Sem regras com plano ou template para enviar");
            		return;
            	}
            	
            	final JsonArray regrasJsonArray = new JsonArray();

            	for(RegraEntity regra : regras) {
            		final JsonObject regraJsonObject = new JsonObject();
            		regraJsonObject.addProperty("id", regra.getId());
            		regraJsonObject.addProperty("idPlano", regra.getIdPlano());
            		regraJsonObject.addProperty("idTemplate", regra.getIdTemplate());
            		
            		regrasJsonArray.add(regraJsonObject);
            	}

            	smartAcessoClient.uploadRegras(regrasJsonArray);
            }

            private void requestAllParametros() throws IOException {
                List<ParametroEntity> parametros = smartAcessoClient.requestAllParametros(lastSyncGetParametros);

                if (parametros == null || parametros.isEmpty()) {
                    System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: sem registros de parametros para receber");
                    return;
                }

                parametros.forEach(parametro -> {
                    if (CHAVE_DE_INTEGRACAO_COMTELE.equals(parametro.getNome())) {
                        loggedUser.setChaveIntegracaoComtele(parametro.getValor());
                    }

                    parametro.setIdClient(loggedUser.getIdClient());

                    ParametroEntity parametroExistente = (ParametroEntity) HibernateAccessDataFacade
                            .getSingleResultById(ParametroEntity.class, parametro.getId());

                    if (parametroExistente != null) {
                        parametroExistente.update(parametro);
                        HibernateAccessDataFacade.update(ParametroEntity.class, parametroExistente);

                    } else {
                    	HibernateAccessDataFacade.save(ParametroEntity.class, parametro);
                    }

                });

                if (loggedUser != null) {
                    Utils.sleep(1000);
                    lastSyncGetParametros = Calendar.getInstance(new Locale("pt", "BR")).getTimeInMillis();
                    loggedUser.setLastSyncParametro(new Date(lastSyncGetParametros));
                    loggedUser = (UserEntity) HibernateAccessDataFacade.updateUser(UserEntity.class, loggedUser)[0];
                }
            }

            private void requestAllPlanos() throws IOException {
                List<PlanoEntity> planos = smartAcessoClient.requestAllPlanos(lastSyncGetPlanos);

                if (planos == null || planos.isEmpty()) {
                    System.out.println(sdf.format(new Date()) + "  SINCRONIZACAO: sem registros de planos para receber");
                    return;
                }

                for (PlanoEntity plano : planos) {
                    if (loggedUser == null) {
                        break;
                    }

                    plano.setIdClient(loggedUser.getIdClient());

                    PlanoEntity planoExistente = (PlanoEntity) HibernateAccessDataFacade
                            .getSingleResultById(PlanoEntity.class, plano.getId());

                    if (planoExistente != null) {
                        planoExistente.update(plano);
                        HibernateAccessDataFacade.update(PlanoEntity.class, planoExistente);

                    } else {
                    	HibernateAccessDataFacade.save(PlanoEntity.class, plano);
                    }
                }

                if (loggedUser != null) {
                    Utils.sleep(1000);
                    lastSyncGetPlanos = Calendar.getInstance(new Locale("pt", "BR")).getTimeInMillis();
                    loggedUser.setLastSyncPlano(new Date(lastSyncGetPlanos));
                    loggedUser = (UserEntity) HibernateAccessDataFacade.updateUser(UserEntity.class, loggedUser)[0];
                }
            }
        };
    }

    public static void syncHikivisionAccessList() {
        if (getUpdatingHikivisionAccessList()) {
            return;
        }

        if (Main.servidor != null) {
            System.out.println(sdf.format(new Date()) + " Sincronizacao Hikivision desabilitada: Maquina possui servidor");
            return;
        }
        
        setUpdatingHikivisionAccessList(true);

        SwingWorker<Void, Void> worker = getSyncHikivisionAccessListWorker();
        worker.execute();
    }
    
    private static SwingWorker<Void, Void> getSyncHikivisionAccessListWorker() {
    	final HikivisionIntegrationErrorRepository hikivisionIntegrationErrorRepository = new HikivisionIntegrationErrorRepository();
        final HikivisionUseCases hikivisionUseCases = new HikivisionUseCases();
        final PedestrianAccessRepository pedestrianAccessRepository = new PedestrianAccessRepository();
        
    	return new SwingWorker<Void, Void>() {
            @Override
            public Void doInBackground() {
                try {
                	System.out.println(sdf.format(new Date()) + " Iniciando reprocessamento de erros na integracao com a Hikivision");
                	executeSyncFromHikivisionIntegrationError();
                    //executeHikivisionAccessListSync();

                    if (loggedUser != null) {
                        lastSyncHikivision = Calendar.getInstance(new Locale("pt", "BR")).getTimeInMillis();
                        loggedUser.setLastSyncHikivision(new Date(lastSyncHikivision));
                        Main.loggedUser = (UserEntity) HibernateAccessDataFacade.updateUser(UserEntity.class, loggedUser)[0];
                    }

                    System.out.println(sdf.format(new Date()) + "  Servidor Hikivision sincronizado com sucesso!");

                } catch (Exception e) {
                    e.printStackTrace();
                    System.out.println(sdf.format(new Date()) + "  ERRO NA SINCRONIZACAO Hikivision: Exception: " + e.getMessage());

                } finally {
                    if (loggedUser != null) {
                        timerSyncHikivision.start();
                    }

                    setUpdatingHikivisionAccessList(false);
                }

                return null;
            }
            
            private void executeSyncFromHikivisionIntegrationError() {
            	final long maxRetries = 20; //TODO Mudar para configuracoes
        		final List<HikivisionIntegrationErrorEntity> errors = hikivisionIntegrationErrorRepository.findFirts(500);
        		
        		if(Objects.isNull(errors) || errors.isEmpty()) {
        			return;
        		}
        		
        		for(HikivisionIntegrationErrorEntity integrationError : errors) {
        			final Optional<PedestrianAccessEntity> pedestreOpt = pedestrianAccessRepository.findByCardNumber(integrationError.getCardNumber());
        			if(!pedestreOpt.isPresent()) {
        				continue;
        			}
        			
        			final PedestrianAccessEntity pedestre = pedestreOpt.get();
        			boolean executadoComSucesso = true;

        			if(HikivisionAction.CREATE == integrationError.getHikivisionAction()) {
        				executadoComSucesso = hikivisionUseCases.reprocessarCadastroInDevice(pedestre, integrationError.getDeviceId());

        			} else if(HikivisionAction.REMOVE == integrationError.getHikivisionAction()) {
        				executadoComSucesso = hikivisionUseCases.reprocessarRemocaoInDevice(pedestre, integrationError.getDeviceId());
        			}
        			
        			if(executadoComSucesso) {
        				hikivisionIntegrationErrorRepository.remove(integrationError);
        				pedestrianAccessRepository.save(pedestre);

        			} else if(integrationError.getRetries() > maxRetries) {
        				hikivisionIntegrationErrorRepository.remove(integrationError);

        			} else {
        				integrationError.incrementRetry();
        				hikivisionIntegrationErrorRepository.update(integrationError);
        			}
        		}
            }

            private void executeHikivisionAccessListSync() {
                if (!Utils.isHikivisionConfigValid()) {
                    return;
                }

                HikivisionUseCases hikivisionUseCases = new HikivisionUseCases();
                LocalRepository localRepository = new LocalRepository();

                if (!hikivisionUseCases.getSystemInformation()) {
                    System.out.println(sdf.format(new Date()) + "  Sincronizacao interrompida - Servidor offline");
                    return;
                }

                List<HikivisionDeviceTO.Device> devices = hikivisionUseCases.listarDispositivos();

                if (Objects.isNull(devices)) {
                    System.out.println(sdf.format(new Date()) + "  Sincronizacao interrompida - Sem dispositivos disponiveis");
                    return;
                }

                HashMap<String, Object> args = new HashMap<>();
                args.put("LAST_SYNC_HIKIVISION", new Date(lastSyncHikivision));

                @SuppressWarnings("unchecked")
				final List<PedestrianAccessEntity> pedestres = (List<PedestrianAccessEntity>) HibernateAccessDataFacade
                        .getResultListWithDynamicParams(PedestrianAccessEntity.class, "PedestrianAccessEntity.findAllWithPhotoByLastSync", args);

                //Sincroniza os pedestre com o sistema, então se esse codigo for comentado ele não manda para o facial da hikivision
                
                for (PedestrianAccessEntity pedestre : pedestres) {
                	try {
                		List<String> devicesName = localRepository.getDevicesNameByPedestreLocal(pedestre);
                		hikivisionUseCases.syncronizarUsuarioInDevices(pedestre, null, devicesName);
                		HibernateAccessDataFacade.save(PedestrianAccessEntity.class, pedestre);
                	
                	} catch (HikivisionIntegrationException e) {
						System.out.println(e.getMessage());
					}
                }

            }
        };
    }


    public static void syncLogAthleteAccess() {
        if (getUpdatingLogAccessList()) {
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

        setUpdatingLogAccessList(true);
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
                    if (loggedUser != null) {
                    	timerSyncLogAthleteAccess.start();
                    }
                    setUpdatingLogAccessList(false);
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
                    Main.loggedUser = (UserEntity) HibernateAccessDataFacade.saveUser(UserEntity.class, Main.loggedUser)[0];

                } catch (ConnectException e) {
                    System.err.println("Nao foi possivel comunicar com o servidor.");

                } catch (Exception e) {
                    e.printStackTrace();
                }
            }


            private void enviaLogsDeAcesso() throws Exception {
                final Date newLastSyncLog = new Date();
                final int pageSize = Utils.getPreferenceAsInteger("syncLogPageSize");
                final int qtdeLogsOnline = logPedestrianAccessRepository.buscaQuantidadeDeLogsDeAcesso(lastSyncLog, newLastSyncLog, "findByAccessDateCount");
                final int qtdeLogsOffline = logPedestrianAccessRepository.buscaQuantidadeDeLogsDeAcesso(lastSyncLog, newLastSyncLog, "findByCreateDateCount");
                int offsetLogsOnline = 0;
                int offsetLogsOffline = 0;

                while ((offsetLogsOnline < qtdeLogsOnline) || (offsetLogsOffline < qtdeLogsOffline)) {
                	System.out.println("Enviando logs de acesso | offsetLogsOnline: " + offsetLogsOnline + " | offsetLogsOffline: " + offsetLogsOffline);
                    List<LogPedestrianAccessEntity> logsOnline = null;
                    List<LogPedestrianAccessEntity> logsOffline = null;

                    if (offsetLogsOnline < qtdeLogsOnline) {
                        logsOnline = logPedestrianAccessRepository.buscaLogsAcesso(offsetLogsOnline, pageSize, lastSyncLog, newLastSyncLog, "findByAccessDate");
                        offsetLogsOnline += pageSize;
                    }

                    if (offsetLogsOffline < qtdeLogsOffline) {
                        logsOffline = logPedestrianAccessRepository.buscaLogsAcesso(offsetLogsOffline, pageSize, lastSyncLog, newLastSyncLog, "findByCreateDate");
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
                    Main.loggedUser = (UserEntity) HibernateAccessDataFacade.updateUser(UserEntity.class, loggedUser)[0];
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

    @SuppressWarnings("unchecked")
    public static void dateSync(String inicio, String fim) throws ParseException {
        SimpleDateFormat sdf2 = new SimpleDateFormat("dd/MM/yyyy HH:mm");

        HashMap<String, Object> args = new HashMap<>();
        args.put("CURRENT_DATE_INICIO", sdf2.parse(inicio));
        args.put("CURRENT_DATE_FIM", sdf2.parse(fim));

        List<LogPedestrianAccessEntity> listaAcessoNaoEnvidados = (List<LogPedestrianAccessEntity>) HibernateAccessDataFacade.
                getResultListWithParams(LogPedestrianAccessEntity.class, "LogPedestrianAccessEntity.findByCurrentDate", args);
        if(Objects.isNull(listaAcessoNaoEnvidados) || listaAcessoNaoEnvidados.isEmpty()) {
        	System.out.println(sdf.format(new Date()) + "  LOG DE ACESSO: " + (listaAcessoNaoEnvidados != null ? listaAcessoNaoEnvidados.size() : 0)  + " registros sincronizados manualmente para enviar");
        	return;
        }
        
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
            JOptionPane.showOptionDialog(null, "Nao e possivel adicionar icones na bandeja do sistema.", "Bandeja do sistema nao suportada.",
                    JOptionPane.PLAIN_MESSAGE, JOptionPane.PLAIN_MESSAGE, null, options, options[0]);
            HibernateAccessDataFacade.shutdown();
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
            JOptionPane.showOptionDialog(mainScreen, "Nao foi possivel adicionar icones na bandeja do sistema.", "Bandeja do sistema nao suportada.",
                    JOptionPane.PLAIN_MESSAGE, JOptionPane.PLAIN_MESSAGE, null, options, options[0]);
            HibernateAccessDataFacade.shutdown();
            System.exit(0);
        }

        updateAccessListMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                sync();
            }
        });

        releaseTicketGateMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
            	releaseAccessUseCase.execute(null, null);
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
            if (!SyncPedestrianAccessListUseCase.getUpdatingPedestrianAccessList()) {
            	syncPedestrianAccessListUseCase.syncPedestrianAccessList();
                while (SyncPedestrianAccessListUseCase.getUpdatingPedestrianAccessList()) {
                	Thread.sleep(100);
                }
                
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
                if (!ReleaseAccessUseCase.getApertouF9()) {
                	ReleaseAccessUseCase.setApertouF9(true);
                	ReleaseAccessUseCase.setApertouF10(false);
                	releaseAccessUseCase.execute(null, null);
                }
            }

            if (e.getKeyCode() == NativeKeyEvent.VC_F10) {
                if (!ReleaseAccessUseCase.getApertouF10()) {
                	ReleaseAccessUseCase.setApertouF10(true);
                	ReleaseAccessUseCase.setApertouF9(false);
                	releaseAccessUseCase.execute(null, null);
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
        } else {
        	firstColor = new Color(39, 57, 74); // DARK BLUE
        }
        	
        if (secondColorCode != null && !secondColorCode.isEmpty()) {
            String[] partes = secondColorCode.split(",");
            secondColor = new Color(Integer.valueOf(partes[0]), Integer.valueOf(partes[1]), Integer.valueOf(partes[2]));
        } else {
        	secondColor = new Color(70, 178, 202); // LIGHT BLUE
        }
    }

    private static void configLogFile() {
        try {
            originalOut = System.out;
            originalErr = System.err;

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
    	if (LuxandService.getInstance().serviceInitialized) {
        	LuxandService.getInstance().FinalizeSDK();
        }
    	
        if (Objects.isNull(devicesList) || devicesList.isEmpty()) {
            return;
        }
        
        for (Device device : Main.devicesList) {
            if (device instanceof FacialDevice) {
                FacialDevice facialDevice = (FacialDevice) device;
                if (facialDevice.isConnected()) {
                	try {
                		facialDevice.disconnect("");
                	} catch (Exception e) {
                		e.printStackTrace();
                	}
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
    
    public static synchronized boolean temServidor() {
    	return Objects.nonNull(servidor);
    }
    
    public static synchronized ServerDevice getServidor() {
    	return servidor;
    }
    
    public static synchronized void removeServidor() {
    	servidor = null;
    }
    
    public static synchronized void addServidor(final ServerDevice serverDevice) {
    	servidor = serverDevice;
    }
    
    public static Long getLastSyncLog() {
    	return lastSyncLog;
    }
    
    public static void setLastSyncLog(final Long value) {
    	lastSyncLog = value;
    }
    
    public static synchronized TrayIcon getTrayIcon() {
    	return trayIcon;
    }
    
    public static synchronized Image getTrayIconImageLoading() {
    	return trayIconImageLoading;
    }
    
    public static synchronized Image getTrayIconImage() {
    	return trayIconImage;
    }
    
    public static synchronized boolean isCadastrandoBiometria() {
    	return isCadastrandoBiometria;
    }

    public static synchronized void setIsCadastrandoBiometria(final boolean value) {
    	isCadastrandoBiometria = value;
    }
}
