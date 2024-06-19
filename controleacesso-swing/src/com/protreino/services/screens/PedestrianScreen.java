package com.protreino.services.screens;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.geom.RoundRectangle2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;

import javax.imageio.ImageIO;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.SwingWorker;
import javax.swing.Timer;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;

import org.apache.commons.codec.binary.Base64;

import com.digitalpersona.onetouch.DPFPGlobal;
import com.digitalpersona.onetouch.DPFPSample;
import com.protreino.services.constants.Configurations;
import com.protreino.services.devices.Device;
import com.protreino.services.devices.FacialDevice;
import com.protreino.services.devices.UsbDevice;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.entity.PedestrianMessagesEntity;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.enumeration.VerificationResult;
import com.protreino.services.main.Main;
import com.protreino.services.usecase.ProcessAccessRequestUseCase;
import com.protreino.services.utils.SelectItem;
import com.protreino.services.utils.Utils;

import static com.protreino.services.constants.TopDataDeviceConstants.*;

@SuppressWarnings("serial")
public class PedestrianScreen extends JFrame {
	
	private PedestrianScreen instance;
	
	private Device readerDevice;
	private Device ticketGateDevice;
	
	private Manufacturer ticketGateDeviceManufacturer;
	private String ticketGateDeviceIdentifier;
	private boolean openAthleteScreenOnInit = false;
	private boolean fullScreenAthleteScreen = false;
	private boolean focusFieldAthleteScreen = true;
	
	private Font font;
	private ImageIcon logoImageIcon;
	private ImageIcon semDigitalImageIcon;
	private ImageIcon digitalGenericaImageIcon;
	private ImageIcon digitalGenericaPermitidaImageIcon;
	private ImageIcon digitalGenericaNegadaImageIcon;
	private Image digitalVerificadaBordaImage;
	private Image digitalNaoVerificadaBordaImage;
	private Image digitalRecebidaBordaImage;
	private ImageIcon configImageIcon;
	private ImageIcon permitidoImageIcon;
	private ImageIcon naoPermitidoImageIcon;
	private ImageIcon vazioImageIcon;
	
	private JPanel leftContainer;
	private JPanel rightContainer;
	
	private JLabel clockLabel;
	private JLabel welcomeLabel;
	private JLabel sampleLabel;
	private JLabel nomeLabel;
	private JLabel mensagemLabel;
	private JLabel statusLabel;
	private JLabel resultadoLabel;
	private JLabel messageErroCodigoLabel;
	private JLabel messageErroDigitalLabel;
	private JLabel leitorLabel;
	private JComboBox<SelectItem> catracasCombobox;
	
	private JTextField codigoTextField;
	
	private Timer timerHidePopupMenu;
	private SwingWorker<Void, Void> clock;
	private SwingWorker<Void, Void> limpador;
	private Boolean paraRelogio = false;
	private Boolean cancelarLimpador = true;
	private Boolean telaTravada = false;
	private DateFormat clockFormat = new SimpleDateFormat("HH:mm:ss");
	private VerificationResult resultadoVerificacao;
	private String allowedUserName;
	private String mensagemPedestre;
	private PedestrianAccessEntity matchedAthleteAccess;
	private Boolean digitalCapturada = false;
	private Integer larguraRestaurada;
	private Integer alturaRestaurada;
	private Integer athleteScreenTimeoutMilis;
	
	private Color firstColor;
	private Color secondColor;
	
	private int imageWidth = 480;
	private int imageHeight; // sera calculada de acordo com o tamanho da imagem da camera para manter a proporcao
	private double ratio;
	private ProcessAccessRequestUseCase processAccessRequestUseCase = new ProcessAccessRequestUseCase();
	
	public PedestrianScreen(Device readerDevice, Device ticketGateDevice_, String athleteScreenConfig){
		
		this.instance = this;
		this.readerDevice = readerDevice;
		this.ticketGateDevice = ticketGateDevice_;
		
		if (athleteScreenConfig != null) {
			String[] partes = athleteScreenConfig.split("%");
			ticketGateDeviceManufacturer = Manufacturer.valueFromImport(partes[0]);
			ticketGateDeviceIdentifier = partes[1];
			openAthleteScreenOnInit = Boolean.valueOf(partes[2]);
			fullScreenAthleteScreen = Boolean.valueOf(partes[3]);
			focusFieldAthleteScreen = Boolean.valueOf(partes[4]);
		}
		
		String firstColorPref = Utils.getPreference("athleteScreenFirstColor");
		String secondColorPref = Utils.getPreference("athleteScreenSecondColor");
		firstColor = Main.firstColor;
		secondColor = Main.secondColor;
		if (!Utils.isNullOrEmpty(firstColorPref)) {
			String partes[] = firstColorPref.split(";");
			firstColor = new Color(Integer.valueOf(partes[0]), Integer.valueOf(partes[1]), Integer.valueOf(partes[2]));
		}
		if (!Utils.isNullOrEmpty(secondColorPref)) {
			String partes[] = secondColorPref.split(";");
			secondColor = new Color(Integer.valueOf(partes[0]), Integer.valueOf(partes[1]), Integer.valueOf(partes[2]));
		}
		
		loadImages();
		
		font = new JLabel().getFont();
		Font normalFont = new Font(font.getFontName(), Font.BOLD, font.getSize() + 5);
		Font bigFont = new Font(font.getFontName(), Font.BOLD, font.getSize() + 10);
		Font bigFont2 = new Font(font.getFontName(), Font.BOLD, font.getSize() + 30);
		
		larguraRestaurada = 1100;
		alturaRestaurada = 700;
		
		setPreferredSize(new Dimension(larguraRestaurada, alturaRestaurada));
		setMinimumSize(new Dimension(larguraRestaurada, alturaRestaurada));
		setIconImage(Main.favicon);
		setTitle(Main.nomeAplicacao + " Controle de acesso " + (this.readerDevice.getName() != null 
								? " - " + this.readerDevice.getName() : ""));
		
		String athleteScreenTimeout = Utils.getPreference("athleteScreenTimeout");
		if(athleteScreenTimeout != null && !"".equals(athleteScreenTimeout))
			athleteScreenTimeoutMilis = Integer.valueOf(athleteScreenTimeout) * 1000;
		
		String backgroundImageBase64 = Utils.getPreference("athleteScreenBackgroundImage");
		Boolean possuiBackgroundImage = backgroundImageBase64 != null && !backgroundImageBase64.isEmpty();
		if (possuiBackgroundImage) {
			try {
				byte[] bytes = Base64.decodeBase64(backgroundImageBase64);
				ByteArrayInputStream bis = new ByteArrayInputStream(bytes);
				BufferedImage bImage = ImageIO.read(bis);
				setContentPane(new BackgroundContentPane(bImage));
				getContentPane().setLayout(new BorderLayout());
			
			} catch (Exception e){
				e.printStackTrace();
			}
		}
		
		//
		// TOP CONTAINER
		//
		JPanel topContainer = new JPanel();
		topContainer.setBorder(new EmptyBorder(5, 10, 5, 20));
		topContainer.setBackground(Main.firstColor);
		topContainer.setLayout(new BoxLayout(topContainer, BoxLayout.X_AXIS));
		
		getContentPane().add(topContainer, BorderLayout.PAGE_START);
		
		JLabel logoLabel = new JLabel(logoImageIcon);
		
		clockLabel = new JLabel("00:00:00");
		clockLabel.setFont(new Font(font.getFontName(), Font.BOLD, font.getSize() + 20));
		clockLabel.setForeground(secondColor);
		
		topContainer.add(logoLabel);
		topContainer.add(Box.createHorizontalGlue());
		topContainer.add(clockLabel);
		
		//
		// MAIN CONTAINER
		//
		JPanel mainContainer = new JPanel();
		mainContainer.setLayout(new BoxLayout(mainContainer, BoxLayout.Y_AXIS));
		mainContainer.setOpaque(possuiBackgroundImage ? false : true);
		
		getContentPane().add(mainContainer,BorderLayout.CENTER);
		
		JPanel horizontalContainer = new JPanel();
		horizontalContainer.setLayout(new BoxLayout(horizontalContainer, BoxLayout.X_AXIS));
		horizontalContainer.setOpaque(possuiBackgroundImage ? false : true);
		horizontalContainer.setPreferredSize(new Dimension(0, 550));
		
		mainContainer.add(Box.createVerticalGlue());
		mainContainer.add(horizontalContainer);
		mainContainer.add(Box.createVerticalGlue());
		
		// LEFT CONTAINER
		leftContainer = new JPanel();
		leftContainer.setBorder(new CompoundBorder(new MatteBorder(0, 0, 0, 1, firstColor), 
				new EmptyBorder(10, 20, 10, 20)));
		leftContainer.setLayout(new BoxLayout(leftContainer, BoxLayout.Y_AXIS));
		leftContainer.setOpaque(possuiBackgroundImage ? false : true);
		Dimension dimension = new Dimension((larguraRestaurada/2), 0);
		leftContainer.setPreferredSize(dimension);
		
		horizontalContainer.add(leftContainer);
		
		welcomeLabel = new JLabel("Seja bem-vindo!");
		welcomeLabel.setFont(new Font(font.getFontName(), Font.BOLD, 40));
		welcomeLabel.setForeground(firstColor);
		welcomeLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		
		JLabel insiraCodigoLabel = new JLabel("Digite seu CÛdigo");
		insiraCodigoLabel.setFont(bigFont);
		insiraCodigoLabel.setForeground(firstColor);
		insiraCodigoLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		
		codigoTextField = new JTextField(20);
		codigoTextField.setMaximumSize(codigoTextField.getPreferredSize());
		codigoTextField.setHorizontalAlignment(SwingConstants.CENTER);
		codigoTextField.setFont(bigFont2);
		codigoTextField.setForeground(Main.firstColor);
		codigoTextField.setAlignmentX(Component.CENTER_ALIGNMENT);
		
		messageErroCodigoLabel = new JLabel("CÛdigo n„o encontrado");
		messageErroCodigoLabel.setFont(normalFont);
		messageErroCodigoLabel.setForeground(Color.RED);
		messageErroCodigoLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		
		JLabel insiraDigitalLabel = new JLabel("Ou insira sua digital");
		insiraDigitalLabel.setFont(bigFont);
		insiraDigitalLabel.setForeground(firstColor);
		insiraDigitalLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		
		sampleLabel = new JLabel(semDigitalImageIcon);
		sampleLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		
		messageErroDigitalLabel = new JLabel("Digital n„o encontrada");
		messageErroDigitalLabel.setFont(normalFont);
		messageErroDigitalLabel.setForeground(Color.RED);
		messageErroDigitalLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		
		if (readerDevice.isCamera()) {
			// calcula altura da imagem para mostrar no dialog
			FacialDevice facialDevice = (FacialDevice) this.readerDevice;
			this.ratio =  ((double) facialDevice.getImageWidth()) / facialDevice.getImageHeight();
			this.imageHeight = (int) (imageWidth / ratio);
			
			BufferedImage sampleImage = ((FacialDevice) readerDevice).getSampleImage();
			if (sampleImage != null)
				imageAcquired(sampleImage);
			else {
				sampleLabel.setIcon(getEmptySample());
			}
			messageErroDigitalLabel.setText(" ");
		}
		
		leftContainer.add(welcomeLabel);
		leftContainer.add(Box.createVerticalStrut(50));
		if (readerDevice.isLeitorBiometrico()
				|| readerDevice.isCatraca()) {
			leftContainer.add(insiraCodigoLabel);
			leftContainer.add(Box.createVerticalStrut(10));
			leftContainer.add(codigoTextField);
			leftContainer.add(Box.createVerticalStrut(5));
			leftContainer.add(messageErroCodigoLabel);
			leftContainer.add(Box.createVerticalStrut(20));
			leftContainer.add(insiraDigitalLabel);
			leftContainer.add(Box.createVerticalStrut(10));
			leftContainer.add(sampleLabel);
			leftContainer.add(Box.createVerticalStrut(5));
			leftContainer.add(messageErroDigitalLabel);
			leftContainer.add(Box.createVerticalGlue());
		
		} else if (readerDevice.isCamera()) {
			leftContainer.add(sampleLabel);
			leftContainer.add(Box.createVerticalStrut(5));
			leftContainer.add(messageErroDigitalLabel);
			leftContainer.add(Box.createVerticalGlue());
		}
		
		// RIGHT CONTAINER
		rightContainer = new JPanel();
		rightContainer.setBorder(new EmptyBorder(20, 30, 20, 20));
		rightContainer.setLayout(new BoxLayout(rightContainer, BoxLayout.Y_AXIS));
		rightContainer.setOpaque(possuiBackgroundImage ? false : true);
		dimension = new Dimension((larguraRestaurada/2), 0);
		rightContainer.setPreferredSize(dimension);
		rightContainer.setMinimumSize(dimension);
		
		horizontalContainer.add(rightContainer);
		
		nomeLabel = new JLabel(" ");
		nomeLabel.setFont(bigFont2);
		nomeLabel.setForeground(secondColor);
		nomeLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		
		mensagemLabel = new JLabel("");
		mensagemLabel.setFont(normalFont);
		mensagemLabel.setForeground(secondColor);
		mensagemLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		
		statusLabel = new JLabel(" ");
		statusLabel.setFont(bigFont);
		statusLabel.setForeground(secondColor);
		statusLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		
		resultadoLabel = new JLabel(vazioImageIcon);
		resultadoLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		
		rightContainer.add(nomeLabel);
		rightContainer.add(mensagemLabel);
		rightContainer.add(Box.createVerticalStrut(20));
		rightContainer.add(statusLabel);
		rightContainer.add(Box.createVerticalStrut(60));
		rightContainer.add(resultadoLabel);
		rightContainer.add(Box.createVerticalGlue());
		
		//
		// BOTTOM CONTAINER
		//
		JPanel bottomContainer = new JPanel();
		bottomContainer.setBorder(new EmptyBorder(5, 20, 5, 20));
		bottomContainer.setLayout(new BoxLayout(bottomContainer, BoxLayout.X_AXIS));
		bottomContainer.setOpaque(possuiBackgroundImage ? false : true);
		
		getContentPane().add(bottomContainer,BorderLayout.PAGE_END);
		
		JButton configButton = new JButton(configImageIcon);
		configButton.setBorderPainted(false);
		configButton.setBorder(null);
		configButton.setMargin(new Insets(0, 0, 0, 0));
		configButton.setContentAreaFilled(false);
		configButton.setIcon(configImageIcon);
		configButton.setToolTipText("Op√ß√µes");
		
		leitorLabel = new JLabel(readerDevice.getName() + " - " 
				+ (DeviceStatus.CONNECTED.equals(readerDevice.getStatus()) ? "conectado" : "desconectado"));
		leitorLabel.setFont(new Font(font.getFontName(), Font.BOLD, font.getSize()));
		
		if (readerDevice.isLeitorBiometrico()) {
		
			catracasCombobox = new JComboBox<>();
			catracasCombobox.setPreferredSize(new Dimension(200, 20));
			catracasCombobox.setMaximumSize(catracasCombobox.getPreferredSize());
			for (Device device : Main.devicesList) {
				if (device.isCatraca()){
					catracasCombobox.addItem(new SelectItem(device.getName(), device));
				}
			}
//			CommDevice commDevice = (CommDevice) Manufacturer.COMM.getNewDevice("");
//			if (commDevice.exist())
//				catracasCombobox.addItem(new SelectItem(commDevice.getName(), commDevice));
			
			UsbDevice usbDevice = (UsbDevice) Manufacturer.USB.getNewDevice("");
			if (usbDevice.exist())
				catracasCombobox.addItem(new SelectItem(usbDevice.getName(), usbDevice));
			
			catracasCombobox.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					SelectItem selectedItem = (SelectItem) catracasCombobox.getSelectedItem();
					ticketGateDevice = (Device) selectedItem.getValue();
					ticketGateDeviceManufacturer = ticketGateDevice.getManufacturer();
					ticketGateDeviceIdentifier = ticketGateDevice.getIdentifier();
					saveAthleteScreenConfig();
				}
			});
			
			if (ticketGateDeviceManufacturer != null) {
				Device savedDevice = ticketGateDeviceManufacturer.getNewDevice(ticketGateDeviceIdentifier);
				for (int i = 0; i < catracasCombobox.getItemCount(); i++){
					SelectItem item = catracasCombobox.getItemAt(i);
					if (savedDevice.isTheSame(item.getValue())){
						catracasCombobox.setSelectedItem(item);
						ticketGateDevice = (Device) item.getValue();
						break;
					}
				}
			} else {
				if (catracasCombobox.getItemCount() > 0) {
					catracasCombobox.setSelectedItem(0);
					SelectItem selectedItem = (SelectItem) catracasCombobox.getSelectedItem();
					ticketGateDevice = (Device) selectedItem.getValue();
					saveAthleteScreenConfig();
				}
			}
			
			bottomContainer.add(Box.createHorizontalGlue());
			JLabel lbl1 = new JLabel("Catraca: ");
			lbl1.setForeground(firstColor);
			bottomContainer.add(lbl1);
			bottomContainer.add(catracasCombobox);
			bottomContainer.add(Box.createHorizontalStrut(30));
			JLabel lbl2 = new JLabel("Leitor biomÈtrico: ");
			lbl2.setForeground(firstColor);
			bottomContainer.add(lbl2);
			bottomContainer.add(leitorLabel);
			bottomContainer.add(Box.createHorizontalStrut(10));
			bottomContainer.add(configButton);
			
		} else {
			bottomContainer.add(Box.createHorizontalGlue());
			JLabel lbl1 = new JLabel("Catraca: ");
			lbl1.setForeground(firstColor);
			bottomContainer.add(lbl1);
			bottomContainer.add(leitorLabel);
			bottomContainer.add(Box.createHorizontalStrut(10));
			bottomContainer.add(configButton);
		}
		
		JMenuItem closeMenuItem = new JMenuItem("Fechar");
		JCheckBoxMenuItem focusFieldMenuItem = new JCheckBoxMenuItem("Focar campo de texto");
		focusFieldMenuItem.setSelected(focusFieldAthleteScreen);
		JCheckBoxMenuItem openOnInitMenuItem = new JCheckBoxMenuItem("Abrir ao iniciar");
		openOnInitMenuItem.setSelected(openAthleteScreenOnInit);
		JCheckBoxMenuItem fullScreenMenuItem = new JCheckBoxMenuItem("Tela cheia");
		fullScreenMenuItem.setSelected(fullScreenAthleteScreen);
		
		JPopupMenu jPopup = new JPopupMenu();
        jPopup.add(closeMenuItem);
        jPopup.addSeparator();
        jPopup.add(focusFieldMenuItem);
        jPopup.add(openOnInitMenuItem);
        jPopup.add(fullScreenMenuItem);
        
        timerHidePopupMenu = new javax.swing.Timer(2000, new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (jPopup.isVisible() && !Utils.isMouseWithinComponent(jPopup))
					jPopup.setVisible(false);
				timerHidePopupMenu.stop();
			}
		});
        
        configButton.addMouseListener(new MouseAdapter() {
        	public void mouseEntered(MouseEvent evt) {
		    	setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
		    }
		    public void mouseExited(MouseEvent evt) {
		    	setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		    }
            public void mousePressed(MouseEvent e) {
            	focusFieldMenuItem.setSelected(focusFieldAthleteScreen);
        		openOnInitMenuItem.setSelected(openAthleteScreenOnInit);
        		fullScreenMenuItem.setSelected(fullScreenAthleteScreen);
        		Point p = e.getLocationOnScreen();
        		jPopup.setLocation(p.x-165, p.y-104);
            	jPopup.setInvoker(instance);
            	jPopup.setVisible(true);
            }
        });
        
        jPopup.addMouseListener(new MouseAdapter() {
        	public void mouseEntered( MouseEvent e ) {timerHidePopupMenu.stop(); }        	
        	public void mouseExited( MouseEvent e ) { timerHidePopupMenu.start(); }
		});
        
        closeMenuItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setVisible(false);
			}
		});
        
        focusFieldMenuItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				focusFieldAthleteScreen = focusFieldMenuItem.isSelected();
				saveAthleteScreenConfig();
			}
		});
        
        openOnInitMenuItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				openAthleteScreenOnInit = openOnInitMenuItem.isSelected();
				saveAthleteScreenConfig();
			}
		});
        
        fullScreenMenuItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				fullScreenAthleteScreen = fullScreenMenuItem.isSelected();
				saveAthleteScreenConfig();
				if (fullScreenMenuItem.isSelected())
					maximizarJanela();
				else
					restaurarJanela();
			}
		});
		
        codigoTextField.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				processarRequisicaoPorCodigo();
			}
		});
        
		clock = new SwingWorker<Void, Void>(){
			@Override
			protected Void doInBackground() throws Exception {
				while (!paraRelogio) {
					try {
						// atualiza o relogio
						clockLabel.setText(clockFormat.format(Calendar.getInstance().getTime()));
						
						// verifica status do leitor
						if (readerDevice != null) {
							leitorLabel.setText(readerDevice.getName() + " - " 
									+ (DeviceStatus.CONNECTED.equals(readerDevice.getStatus()) ? "conectado" : "desconectado"));
							leitorLabel.setForeground(DeviceStatus.CONNECTED.equals(readerDevice.getStatus()) 
									? secondColor : Color.RED);
						} else {
							leitorLabel.setText("Sem leitor");
							leitorLabel.setForeground(Color.RED);
						}
						
						// joga o foco para o campo de texto
						if (focusFieldAthleteScreen
								&& !jPopup.isVisible() 
								&& !codigoTextField.isFocusOwner() 
								&& catracasCombobox != null
								&& !catracasCombobox.isPopupVisible())
							codigoTextField.requestFocus();
						
						Utils.sleep(999);
					
					} catch (Exception e) {
	                    e.printStackTrace();
	                }
				}
				return null;
			}
		};
		clock.execute();
		
		addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
            	readerDevice.setAthleteScreen(null);
            	paraRelogio = true;
            	clock.cancel(true);
				dispose();
            }
        });
		
		addComponentListener(new ComponentAdapter() {
			public void componentResized(ComponentEvent evt) {
				if (readerDevice.isCamera()) {
					imageWidth = (int) (getWidth() * 0.43);
					imageHeight = (int) (imageWidth / ratio);
				}
			}
		});
		
		readerDevice.setAthleteScreen(this);
		
		pack();
		showScreen();
	}
	
	public void showScreen() {
		cleanScreen(true, true);
		setLocationRelativeTo(null);
		setVisible(true);
		toFront();
		if (fullScreenAthleteScreen)
			maximizarJanela();
		else
			restaurarJanela();
	}
	
	
	// METODOS DE INTERFACE COM LEITORES E CATRACA
	
	/**
	 * Exibe mensagem de erro quando houver tentativa de acesso por codigo
	 * @param message
	 */
	public void setErroCodigo(String message){
		agendarLimpeza(athleteScreenTimeoutMilis);
		messageErroCodigoLabel.setText(message);
	}
	
	/**
	 * Exibe mensagem de erro quando houver tentativa de acesso pela digital
	 * @param message
	 */
	public void setErroDigital(String message){
		agendarLimpeza(athleteScreenTimeoutMilis);
		messageErroDigitalLabel.setText(message);
	}
	
	/**
	 * Informa se a tela esta travada, impossibilitando assim novos acessos que a tela seja limpa
	 * @param message
	 */
	public boolean isTelaTravada(){
		return telaTravada;
	}
	
	/**
	 * Reseta o icone para o padrao caso a digital nao tenha sido capturada
	 * @param message
	 */
	public void fingerGone(){
		if (!digitalCapturada)
			sampleLabel.setIcon(semDigitalImageIcon);
	}
	
	/**
	 * Troca o icone da digital, colocando a imagem capturada pelo leitor
	 * @param message
	 */
	public void digitalObtida(DPFPSample sample) {
		if (sample != null) {
			digitalCapturada = true;
			Image sampleImage = DPFPGlobal.getSampleConversionFactory()
					.createImage(sample).getScaledInstance(160, 201, Image.SCALE_SMOOTH);
			if (sampleImage != null) {
				BufferedImage sampleCombined = new BufferedImage(160, 201, BufferedImage.TYPE_INT_ARGB);
				Graphics g = sampleCombined.getGraphics();
				g.drawImage(digitalRecebidaBordaImage, 0, 0, null);
				g.drawImage(sampleImage, 0, 0, null);
				g.drawImage(digitalRecebidaBordaImage, 0, 0, null);
				sampleLabel.setIcon(new ImageIcon(sampleCombined));
			}
		}
		else {
			sampleLabel.setIcon(digitalGenericaImageIcon);
		}
	}
	
	/**
	 * Metodo chamado apos o leitor/catraca processar uma requisicao de acesso por digital.
	 * O metodo extrai a imagem da amostra caso haja, e exibe as mensagens na tela 
	 * de acordo com o resultado da verificacao recebido
	 * @param sample
	 * @param verificationResult
	 * @param allowedUserName
	 * @param matchedAthleteAccess
	 */
	public void requisicaoPorDigital(DPFPSample sample, VerificationResult verificationResult, 
							String allowedUserName, PedestrianAccessEntity matchedAthleteAccess) {
		try {
			setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
			
			this.resultadoVerificacao = verificationResult;
			this.allowedUserName = allowedUserName;
			this.matchedAthleteAccess = matchedAthleteAccess;
			
			if (sample != null) {
				Image sampleImage = DPFPGlobal.getSampleConversionFactory()
						.createImage(sample).getScaledInstance(160, 201, Image.SCALE_SMOOTH);
				if (sampleImage != null){
					BufferedImage sampleCombined = new BufferedImage(160, 201, BufferedImage.TYPE_INT_ARGB);
					Graphics g = sampleCombined.getGraphics();
					g.drawImage(VerificationResult.ALLOWED.equals(resultadoVerificacao) 
							|| VerificationResult.TOLERANCE_PERIOD.equals(resultadoVerificacao) ? digitalVerificadaBordaImage : 
								digitalNaoVerificadaBordaImage, 0, 0, null);
					g.drawImage(sampleImage, 0, 0, null);
					g.drawImage(VerificationResult.ALLOWED.equals(resultadoVerificacao) 
							|| VerificationResult.TOLERANCE_PERIOD.equals(resultadoVerificacao) ? digitalVerificadaBordaImage : 
								digitalNaoVerificadaBordaImage, 0, 0, null);
					sampleLabel.setIcon(new ImageIcon(sampleCombined));
				}
			}
			
			if(matchedAthleteAccess != null) {
				verificaSePossuiMensagem(matchedAthleteAccess);
			} else {
				this.mensagemPedestre = "";
			}
			
			processarResultadoVerificacao("DIGITAL");
		} catch (Exception e) {
			e.printStackTrace();
			setErroDigital("Erro ao processar digital.");
		} finally {
			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
	}
	
	
	/**
	 * Metodo chamado apos processar uma requisicao de acesso por reconhecimento facial
	 * O metodo exibe as mensagens na tela de acordo com o resultado da verificacao recebido
	 * @param sample
	 * @param verificationResult
	 * @param allowedUserName
	 * @param matchedAthleteAccess
	 */
	public void requisicaoPorFoto(VerificationResult verificationResult, String allowedUserName, 
									PedestrianAccessEntity matchedAthleteAccess, int tempoDeEspera) {
		try {
			setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
			
			this.resultadoVerificacao = verificationResult;
			this.allowedUserName = allowedUserName;
			this.matchedAthleteAccess = matchedAthleteAccess;
			
			if(matchedAthleteAccess != null) {
				if("".equals(this.allowedUserName) && 
						matchedAthleteAccess.getName() != null) {
					this.allowedUserName = matchedAthleteAccess.getName();
				}
				
				verificaSePossuiMensagem(matchedAthleteAccess);
			} else {
				this.mensagemPedestre = "";
			}
			
			processarResultadoVerificacao("FACIAL", tempoDeEspera);
		
		} catch (Exception e) {
			e.printStackTrace();
			setErroDigital("Erro ao processar a face.");
		
		} finally {
			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
	}
	
	public void requisicaoPorFotoNaoEncontrada() {
		this.resultadoVerificacao = VerificationResult.NOT_FOUND;
		this.allowedUserName = "";
		processarResultadoVerificacao("FACIAL");
	}
	
	/**
	 * Metodo chamado apos o leitor/catraca processar uma requisicao de acesso por CÛdigo.
	 * O metodo exibe as mensagens na tela de acordo com o resultado da verificacao recebido
	 * @param sample
	 * @param verificationResult
	 * @param allowedUserName
	 * @param matchedAthleteAccess
	 */
	public void requisicaoPorCodigo(VerificationResult verificationResult, 
						String allowedUserName, PedestrianAccessEntity matchedAthleteAccess) {
		try {
			setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
			
			this.resultadoVerificacao = verificationResult;
			this.allowedUserName = allowedUserName;
			this.matchedAthleteAccess = matchedAthleteAccess;
			
			processarResultadoVerificacao("CODIGO");
		
		} catch (Exception e) {
			e.printStackTrace();
			setErroDigital("Erro ao processar digital.");
		
		} finally {
			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
	}
	
	// Faz a verificacao de acordo com o codigo digitado
	private void processarRequisicaoPorCodigo(){
		try {
			setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
//			Object[] retorno = HibernateUtil.processAccessRequest(codigoTextField.getText(), readerDevice.getLocation(), false);
			Object[] retorno = processAccessRequestUseCase.processAccessRequest(codigoTextField.getText(), readerDevice.getName(), 
																	1, readerDevice.getName(), false, true, 
																	readerDevice.getConfigurationValueAsBoolean(IGNORAR_REGRAS_DE_ACESSO));
			
			resultadoVerificacao = (VerificationResult) retorno[0];
			allowedUserName = (String) retorno[1];
			matchedAthleteAccess = (PedestrianAccessEntity) retorno[2];
			
			if(matchedAthleteAccess != null) {
				verificaSePossuiMensagem(matchedAthleteAccess);
			
			} else {
				this.mensagemPedestre = "";
			}
			processarResultadoVerificacao("CODIGO");
		
		} catch (Exception e) {
			e.printStackTrace();
			setErroCodigo("Erro ao procurar CÛdigo.");
		
		} finally {
			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
	}
	
	private void verificaSePossuiMensagem(PedestrianAccessEntity matchedAthleteAccess) {
		if (matchedAthleteAccess.getMensagens() != null
				&& !matchedAthleteAccess.getMensagens().isEmpty()) {
			
			this.mensagemPedestre = "- ";
			for(PedestrianMessagesEntity p : matchedAthleteAccess.getMensagens()) {
				if(p.getQuantidade() > 0) {
					this.mensagemPedestre += p.getMensagem() + " - ";
					p.setQuantidade(p.getQuantidade() - 1);
				}
			}
		} else {
			this.mensagemPedestre = "";
		}
	}
	
	private void agendarLimpeza(Integer tempo){
		limpador = new SwingWorker<Void, Void>(){
			@Override
			protected Void doInBackground() throws Exception {
				int contador = (int) (tempo * 0.6);
				for (int i = 0; i < contador; i ++) {
					// aguarda o tempo necessario, mas sempre verificando se precisa ser cancelado ou se pode continuar
					if (cancelarLimpador)
						break;
					Utils.sleep(1);
				}
				cleanScreen(true, false);
				return null;
			}
		};
		cancelarLimpador = false;
		limpador.execute();
	}
	
	private void cleanScreen(Boolean limparImagemDigital, Boolean limparTextField){
		messageErroCodigoLabel.setText(" ");
		messageErroDigitalLabel.setText(" ");
		nomeLabel.setText(" ");
		statusLabel.setText(" ");
		mensagemLabel.setText(" ");
		resultadoLabel.setIcon(vazioImageIcon);
		if (limparTextField)
			codigoTextField.setText("");
		if (limparImagemDigital && !readerDevice.isCamera()) {
			sampleLabel.setIcon(semDigitalImageIcon);
			if (digitalCapturada)
				digitalCapturada = false;
		}
		telaTravada = false;
	}
	
	private void maximizarJanela(){
		setExtendedState(JFrame.MAXIMIZED_BOTH);
		welcomeLabel.setFont(new Font(font.getFontName(), Font.BOLD, 50));
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Integer largura = Double.valueOf(screenSize.getWidth()).intValue();
		Dimension dimension = new Dimension((largura/2), 0);
		leftContainer.setPreferredSize(dimension);
		dimension = new Dimension((largura/2), 0);
		rightContainer.setPreferredSize(dimension);
		rightContainer.setMinimumSize(dimension);
		if (focusFieldAthleteScreen)
			codigoTextField.requestFocus();
	}
	
	private void restaurarJanela(){
		setPreferredSize(new Dimension(larguraRestaurada, alturaRestaurada));
		setExtendedState(JFrame.NORMAL);
		welcomeLabel.setFont(new Font(font.getFontName(), Font.BOLD, 40));
		Dimension dimension = new Dimension((larguraRestaurada/2), 0);
		leftContainer.setPreferredSize(dimension);
		dimension = new Dimension((larguraRestaurada/2), 0);
		rightContainer.setPreferredSize(dimension);
		rightContainer.setMinimumSize(dimension);
		if (focusFieldAthleteScreen)
			codigoTextField.requestFocus();
	}
	
	private void processarResultadoVerificacao(String tipoAcesso){
		processarResultadoVerificacao(tipoAcesso, 4000);
	}
	
	private void processarResultadoVerificacao(String tipoAcesso, int tempoDeEspera){
		try {
			cancelarLimpador = true;
			Utils.sleep(3);
			cleanScreen("DIGITAL".equals(tipoAcesso) ? false : true, true);
			
			nomeLabel.setText(allowedUserName.isEmpty() ? " " : allowedUserName);
			mensagemLabel.setText(mensagemPedestre);
    		
    		if (VerificationResult.ERROR.equals(resultadoVerificacao)
    				|| VerificationResult.NOT_FOUND.equals(resultadoVerificacao))
    			telaTravada = false;
    		else
    			telaTravada = true;
    		
    		agendarLimpeza(athleteScreenTimeoutMilis);
    		
    		switch (resultadoVerificacao) {
	    		case ERROR:
	    			if ("CODIGO".equals(tipoAcesso))
	    				messageErroCodigoLabel.setText("Erro ao procurar CÛdigo.");
	    			else if ("DIGITAL".equals(tipoAcesso)) {
	    				messageErroDigitalLabel.setText("Erro ao procurar digital.");
	    				sampleLabel.setIcon(digitalGenericaNegadaImageIcon);
	    			}
	    			else if ("FACIAL".equals(tipoAcesso))
	    				messageErroDigitalLabel.setText("Erro ao procurar face.");
	    			break;
	    		case NOT_FOUND:
	    			if (!"FACIAL".equals(tipoAcesso)) {
		    			if ("CODIGO".equals(tipoAcesso))
		    				messageErroCodigoLabel.setText("CÛdigo n„o encontrado.");
		    			else {
		    				messageErroDigitalLabel.setText("Digital n„o encontrada.");
		    				sampleLabel.setIcon(digitalGenericaNegadaImageIcon);
		    			}
	    			} else if("FACIAL".equals(tipoAcesso)) {
	    				messageErroDigitalLabel.setText("Face n„o reconhecida ou mal posicionada.");
	    			}
	    			break;
	    		case NOT_ALLOWED:
	    			statusLabel.setText(Utils.getPreference("messageNotAllowedAthleteScreen"));
	    			resultadoLabel.setIcon(naoPermitidoImageIcon);
	    			if ("DIGITAL".equals(tipoAcesso))
	    				sampleLabel.setIcon(digitalGenericaNegadaImageIcon);
	    			break;
	    		case NOT_ALLOWED_FACE_REQUIRED:
	    			statusLabel.setText(Utils.getPreference("messageNotAllowedFaceRequired"));
	    			resultadoLabel.setIcon(naoPermitidoImageIcon);
	    			if ("DIGITAL".equals(tipoAcesso))
	    				sampleLabel.setIcon(digitalGenericaNegadaImageIcon);
	    			break;
	    		case NOT_ALLOWED_TODAY:
	    			statusLabel.setText(Utils.getPreference("messageNotAllowedTodayAthleteScreen"));
	    			resultadoLabel.setIcon(naoPermitidoImageIcon);
	    			if ("DIGITAL".equals(tipoAcesso))
	    				sampleLabel.setIcon(digitalGenericaNegadaImageIcon);
	    			break;
	    		case ALLOWED_ONLY_ONCE:
	    			statusLabel.setText(Utils.getPreference("messageAllowedOnlyOnceAthleteScreen"));
	    			resultadoLabel.setIcon(naoPermitidoImageIcon);
	    			if ("DIGITAL".equals(tipoAcesso))
	    				sampleLabel.setIcon(digitalGenericaNegadaImageIcon);
	    			break;
	    		case NOT_ALLOWED_NOW:
	    			statusLabel.setText(Utils.getPreference("messageNotAllowedNowAthleteScreen"));
	    			resultadoLabel.setIcon(naoPermitidoImageIcon);
	    			if ("DIGITAL".equals(tipoAcesso))
	    				sampleLabel.setIcon(digitalGenericaNegadaImageIcon);
	    			break;
	    		case ALLOWED:
	    			statusLabel.setText(Utils.getPreference("messageAllowedAthleteScreen"));
	    			
	    			//TODO : verificar mensagens personalizadas
//	    			if (matchedAthleteAccess.getDataPermitido() != null) {
//	    				Long dataPermitido = matchedAthleteAccess.getDataPermitido().getTime();
//						Long diasRestantes = TimeUnit.DAYS.convert(dataPermitido-agora, TimeUnit.MILLISECONDS);
//						if (diasRestantes > 5) {
//							if (HibernateUtil.isAniversariante())
//								statusLabel.setText("Parab√©ns!");
//							else
//								statusLabel.setText(Utils.getPreference("messageAllowedAthleteScreen"));
//						}
//						else
//							statusLabel.setText("Sua mensalidade vence em " + diasRestantes + " dias");
//	    			}
//	    			else {
//	    				statusLabel.setText(Utils.getPreference("messageAllowedAthleteScreen"));
//	    			}
	    			
	    			if(matchedAthleteAccess != null && matchedAthleteAccess.getFoto() != null) {
	    				resultadoLabel.setIcon(new ImageIcon(createMiniImage(matchedAthleteAccess.getFoto())));
	    				
	    			} else {
		    			resultadoLabel.setIcon(permitidoImageIcon);
	    			}
	    			
	    			if ("DIGITAL".equals(tipoAcesso))
	    				sampleLabel.setIcon(digitalGenericaPermitidaImageIcon);
	    			if (ticketGateDevice != null) {
	    				if (readerDevice.isLeitorBiometrico() 
	    						|| readerDevice.isCamera()
	    						|| "CODIGO".equals(tipoAcesso)) {
	    					ticketGateDevice.setVerificationResult(resultadoVerificacao);
		    				ticketGateDevice.setAllowedUserName(allowedUserName);
		    				ticketGateDevice.setMatchedAthleteAccess(matchedAthleteAccess);
		    				
		    				Thread thread = new Thread(new Runnable() {
								@Override
								public void run() {
									ticketGateDevice.allowAccess();
								}
							});
			    			thread.setDaemon(true);
			    			thread.start();
	    				}
	    			}
	    			break;
	    		case TOLERANCE_PERIOD:
	    			//TODO : verificar mensagens personalizadas
//	    			Calendar calendar = Calendar.getInstance();
//	    			calendar.setTime(matchedAthleteAccess.getDataPermitido());
//	    			calendar.add(Calendar.DAY_OF_YEAR, matchedAthleteAccess.getTolerance());
//	    			Long dataTolerancia = calendar.getTime().getTime();
//	    			Long diasRestantes = TimeUnit.DAYS.convert(dataTolerancia - Calendar.getInstance().getTimeInMillis(), TimeUnit.MILLISECONDS);
//					statusLabel.setText("Sua mensalidade est√° vencida. Voc√™ ainda tem " + diasRestantes + " para o pagamento.");
	    			resultadoLabel.setIcon(permitidoImageIcon);
	    			if ("DIGITAL".equals(tipoAcesso))
	    				sampleLabel.setIcon(digitalGenericaPermitidaImageIcon);
	    			if (ticketGateDevice != null) {
	    				if (readerDevice.isLeitorBiometrico() 
	    						|| readerDevice.isCamera()
	    						|| "CODIGO".equals(tipoAcesso)) {
	    					ticketGateDevice.setVerificationResult(resultadoVerificacao);
		    				ticketGateDevice.setAllowedUserName(allowedUserName);
		    				ticketGateDevice.setMatchedAthleteAccess(matchedAthleteAccess);
		    				
		    				Thread thread = new Thread(new Runnable() {
								@Override
								public void run() {
									ticketGateDevice.allowAccess();
								}
							});
			    			thread.setDaemon(true);
			    			thread.start();
	    				}
	    			}
	    			break;
	    		default:
	    			break;
    		}
		} catch (Exception e) {
			e.printStackTrace();
			if ("CODIGO".equals(tipoAcesso))
				messageErroCodigoLabel.setText("Erro ao procurar CÛdigo. " + e.getMessage());
			else if ("DIGITAL".equals(tipoAcesso))
				messageErroDigitalLabel.setText("Erro ao procurar digital. " + e.getMessage());
			else if ("FACIAL".equals(tipoAcesso))
				messageErroDigitalLabel.setText("Erro ao procurar face. " + e.getMessage());
		}
	}
	
	private byte[] createMiniImage(byte[] original) {
		try {
			BufferedImage originalImage = ImageIO.read(new ByteArrayInputStream(original));
			int sizeImage = 384; // em px
			BufferedImage clipedImage = new BufferedImage(sizeImage, sizeImage, BufferedImage.TYPE_INT_ARGB);
			Graphics2D g1 = clipedImage.createGraphics();
			g1.setClip(new RoundRectangle2D.Double(0, 0, sizeImage, sizeImage, 5, 5));
			g1.drawImage(originalImage, 0, 0, sizeImage, sizeImage, null);
			g1.dispose();
			
			ByteArrayOutputStream bos = new ByteArrayOutputStream();
			ImageIO.write(clipedImage, "png", bos);
			return bos.toByteArray();
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		return original;
	}
	
	private void saveAthleteScreenConfig(){
		readerDevice.setAthleteScreenConfig(ticketGateDeviceManufacturer + "%" + ticketGateDeviceIdentifier + "%"
				+ openAthleteScreenOnInit + "%" + fullScreenAthleteScreen + "%" + focusFieldAthleteScreen + "%");
	}
	
	public void imageAcquired(BufferedImage bufferedImage){
		sampleLabel.setIcon(new ImageIcon(resizeToIconSize(bufferedImage)));
	}
	
	private ImageIcon getEmptySample() {
		BufferedImage emptyImage = new BufferedImage(imageWidth, imageHeight, BufferedImage.TYPE_4BYTE_ABGR);
		Graphics2D g2 = emptyImage.createGraphics();
		g2.setColor(Color.BLACK);
		g2.fillRect(0, 0, imageWidth, imageHeight);
	    g2.dispose();
	    return new ImageIcon(emptyImage);
	}

	private Image resizeToIconSize(BufferedImage srcImg) {
		if (srcImg.getWidth() == imageWidth && srcImg.getHeight() == imageHeight)
			return srcImg;
		
		// cria uma nova imagem redimensionada a partir da original
	    BufferedImage resizedImg = new BufferedImage(imageWidth, imageHeight, BufferedImage.TYPE_4BYTE_ABGR);
	    Graphics2D g2 = resizedImg.createGraphics();
	    g2.setColor(Color.BLACK);
		g2.fillRect(0, 0, imageWidth, imageHeight);
	    g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
	    g2.drawImage(srcImg, 0, 0, imageWidth, imageHeight, null);
	    g2.dispose();
	    return resizedImg;
	}
	
	private void loadImages() {
		try {
			Toolkit toolkit = Toolkit.getDefaultToolkit();
			logoImageIcon = new ImageIcon(toolkit.getImage(Main.class.
					getResource(Configurations.IMAGE_FOLDER + Main.customImageFolder + "logo_grd001.png")));
			configImageIcon = new ImageIcon(toolkit.getImage(Main.class.
					getResource(Configurations.IMAGE_FOLDER + Main.customImageFolder + "gear.png")));
			naoPermitidoImageIcon = new ImageIcon(toolkit.getImage(Main.class.
					getResource(Configurations.IMAGE_FOLDER + "comuns/nao_permitido.png")));
			vazioImageIcon = new ImageIcon(toolkit.getImage(Main.class.
					getResource(Configurations.IMAGE_FOLDER + "comuns/vazio.png")));
			digitalNaoVerificadaBordaImage = toolkit.getImage(Main.class.
					getResource(Configurations.IMAGE_FOLDER + "comuns/digital_nao_verificada_borda.png"));
			digitalGenericaNegadaImageIcon = new ImageIcon(toolkit.getImage(Main.class.
					getResource(Configurations.IMAGE_FOLDER + "comuns/digital_generica_negada.png")));
			
			Image semDigitalImage = toolkit.getImage(Main.class.
					getResource(Configurations.IMAGE_FOLDER + "comuns/sem_digital.png"));
			semDigitalImageIcon = new ImageIcon(semDigitalImage);
			
			BufferedImage mascara = ImageIO.read(Main.class.
					getResource(Configurations.IMAGE_FOLDER + "comuns/permitido_mask.png"));
			permitidoImageIcon = new ImageIcon(Utils.paintImage(mascara, secondColor, 256, 256));
			
			mascara = ImageIO.read(Main.class.
					getResource(Configurations.IMAGE_FOLDER + "comuns/digital_generica_mask.png"));
			digitalGenericaPermitidaImageIcon = new ImageIcon(Utils.paintImage(mascara, secondColor, 160, 201));
			
			mascara = ImageIO.read(Main.class.
					getResource(Configurations.IMAGE_FOLDER + "comuns/digital_borda_mask.png"));
			digitalRecebidaBordaImage = Utils.paintImage(mascara, secondColor, 160, 201);
			
			BufferedImage sampleCombined = new BufferedImage(160, 201, BufferedImage.TYPE_INT_ARGB);
			Graphics g = sampleCombined.getGraphics();
			g.drawImage(semDigitalImage, 0, 0, null);
			g.drawImage(digitalRecebidaBordaImage, 0, 0, null);
			digitalGenericaImageIcon = new ImageIcon(sampleCombined);
			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	private class BackgroundContentPane extends JPanel {
		
		private Image bImage;
		
		public BackgroundContentPane(Image bImage){
			this.bImage = bImage;
		}
		
		protected void paintComponent(final Graphics g) {
			super.paintComponent(g);
			g.drawImage(getScaledImage(bImage, getWidth(), getHeight(), "jpg"), 0, 0, this);
		}
		
		private Image getScaledImage(Image srcImg, int w, int h, String extension){
		    BufferedImage resizedImg = new BufferedImage(w, h, "png".equals(extension) ? BufferedImage.TYPE_INT_ARGB : BufferedImage.OPAQUE);
		    Graphics2D g2 = resizedImg.createGraphics();
		    g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
		    g2.drawImage(srcImg, 0, 0, w, h, null);
		    g2.dispose();
		    return resizedImg;
		}
	}
}
