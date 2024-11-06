package com.protreino.services.screens;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.LayoutManager;
import java.awt.RenderingHints;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.WindowConstants;

import com.protreino.services.constants.Configurations;
import com.protreino.services.devices.Device;
import com.protreino.services.entity.HikivisionFingerEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.entity.TemplateEntity;
import com.protreino.services.enumeration.DeviceMode;
import com.protreino.services.enumeration.Finger;
import com.protreino.services.main.Main;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.repository.HikivisionFingerRepository;
import com.protreino.services.to.hikivision.CaptureFingerPrintTO.CaptureFingerPrint;
import com.protreino.services.to.hikivision.HikivisionDeviceTO;
import com.protreino.services.usecase.HikivisionUseCases;
import com.protreino.services.utils.Utils;

public class ColetarBiometriaHikvisionDialog extends JDialog {
	public ColetarBiometriaHikvisionDialog instance;
	
	private final 	HikivisionFingerRepository hikivisionFingerRepository = new HikivisionFingerRepository();
	
	private JLabel userLabel;
	public JComboBox<String> fingerComboBox;
	public JComboBox<String> deviceComboBox;
	private JButton startReadingButton;
	public JLabel sampleLabel;
	private JPanel samplesLabelContainer;
	private JLabel messageLabel;
	private JButton cancelButton;
	
	private JPanel  deviceContainer;
	private Image sampleOverlay;
	private ImageIcon sampleNoneImageIcon;
	private ImageIcon sampleCollectedImageIcon;
	private ImageIcon fingerTouchedImageIcon;
	public PedestrianAccessEntity visitante;
	private TemplateEntity templateEntity;
	private HikivisionUseCases hikivisionUseCases = new HikivisionUseCases();
	private Device selectedDevice;
	private int sampleCount;
	private List<RoundedPanel> samplesCountList;
	
	public ColetarBiometriaHikvisionDialog(PedestrianAccessEntity visitante){
		super(new Frame(), "Cadastro de digital Hikivision");
		this.visitante = visitante;
		this.instance = this;
		
		loadImages();
		
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
		setPreferredSize(new Dimension(630,500));
		setIconImage(Main.favicon);
	    setModal(true);
        setResizable(false);
        setLayout(new BorderLayout());
		
		
		Font font = new JLabel().getFont();
		Font boldFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
		
		JLabel label1 = new JLabel("Usuario: ");
		userLabel = new JLabel(visitante.getName());
		userLabel.setFont(boldFont);
		userLabel.setForeground(Main.firstColor);
		JPanel userContainer = new JPanel(new FlowLayout(FlowLayout.LEFT, 2, 5));
		userContainer.add(label1);
		userContainer.add(userLabel);
		
		JLabel label2 = new JLabel("Dedo selecionado: ");
		fingerComboBox = new JComboBox<String>();
		fingerComboBox.setPreferredSize(new Dimension(150, 30));
		for (Finger finger : Finger.values())
			fingerComboBox.addItem(finger.toString());
		JPanel  fingerContainer = new JPanel(new FlowLayout(FlowLayout.LEFT, 2, 5));
		fingerContainer.add(label2);
		fingerContainer.add(fingerComboBox);	
		
		
		HikivisionDeviceTO.Device device = new HikivisionDeviceTO.Device();
		List<HikivisionDeviceTO.Device> devices = hikivisionUseCases.listarDispositivos();
		if(!devices.isEmpty()) {
			listDevices(devices);	
		}
		
		startReadingButton = new JButton("Iniciar coleta");
		startReadingButton.setPreferredSize(new Dimension(160, 40));
		JPanel startReadingButtonContainer = new JPanel(new FlowLayout(FlowLayout.CENTER, 2, 5));
		startReadingButtonContainer.add(startReadingButton);
		
		sampleLabel = new JLabel(sampleNoneImageIcon);
		sampleLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		
		JPanel sampleCountPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
		samplesCountList = new ArrayList<RoundedPanel>();
		for (int i = 1; i <= sampleCount; i++) {
			RoundedPanel panel = new RoundedPanel(new FlowLayout(FlowLayout.CENTER), 20);
			panel.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY, 5, true), 
					BorderFactory.createEmptyBorder(0, 10, 0, 10)));
			JLabel label = new JLabel(String.valueOf(i));
			label.setFont(new Font(label.getFont().getName(), label.getFont().getStyle(), 80));
			label.setForeground(Color.LIGHT_GRAY);
			panel.add(label);
			sampleCountPanel.add(panel);
			sampleCountPanel.add(Box.createRigidArea(new Dimension(10,0)));
			samplesCountList.add(panel);
		}
		
		samplesLabelContainer = new JPanel(new FlowLayout(FlowLayout.CENTER));
		samplesLabelContainer.add(sampleLabel);
		samplesLabelContainer.add(Box.createRigidArea(new Dimension(10,0)));
		samplesLabelContainer.add(sampleCountPanel);
		
		messageLabel = new JLabel(" ");
		messageLabel.setFont(boldFont);
		JPanel messageLabelContainer = new JPanel(new FlowLayout(FlowLayout.CENTER));
		messageLabelContainer.add(messageLabel);
		
		JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
		mainPanel.setBorder(BorderFactory.createEmptyBorder(10,10,10,10));
		
		mainPanel.add(userContainer);
		mainPanel.add(fingerContainer);
		mainPanel.add(deviceContainer);
		mainPanel.add(startReadingButtonContainer);
		mainPanel.add(Box.createVerticalStrut(5));
		mainPanel.add(samplesLabelContainer);
		mainPanel.add(messageLabelContainer);
		mainPanel.add(Box.createVerticalGlue());
		
		cancelButton = new JButton("Cancelar");
		cancelButton.setPreferredSize(new Dimension(100, 40));
		JPanel buttonPane = new JPanel(new FlowLayout(FlowLayout.CENTER, 3, 10));
		buttonPane.add(cancelButton);
		
		Container contentPane = getContentPane();
		contentPane.add(mainPanel, BorderLayout.CENTER);
		contentPane.add(buttonPane, BorderLayout.PAGE_END);
		
		addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) { 
			    cancelButton.doClick();
			}
		});
		Finger finger = Finger.valueFromImport(fingerComboBox.getSelectedItem().toString());
		
		
		startReadingButton.addActionListener(e -> {
			startReading(finger, String.valueOf(deviceComboBox.getSelectedItem().toString()), visitante);
		});
		
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				dispose();
			}
		});
		
		
		addComponentListener(new ComponentListener() {
			@Override
			public void componentShown(ComponentEvent e) {
				toFront();
			}
			@Override
			public void componentResized(ComponentEvent e) {}
			@Override
			public void componentMoved(ComponentEvent e) {}
			@Override
			public void componentHidden(ComponentEvent e) {}
		});
		
		addWindowListener(new WindowAdapter() {
		    @Override
		    public void windowClosed(WindowEvent e) {
		    	Main.setIsCadastrandoBiometria(false);
		    	if (selectedDevice != null) {
		    		selectedDevice.setBiometricDialog(null);
		    		selectedDevice.setMode(DeviceMode.VERIFICATION);
		    	}
		    }
		});
		
		pack();
		
		setLocationRelativeTo(null);
		setVisible(true);
	}
	
	
	private void loadImages() {
		try {
			Toolkit toolkit = Toolkit.getDefaultToolkit();
			Image sampleNoneImage = toolkit.getImage(Main.class.
					getResource(Configurations.IMAGE_FOLDER + "comuns/sample_none.png"));
			sampleNoneImageIcon = new ImageIcon(sampleNoneImage);
			
			BufferedImage mascara = ImageIO.read(Main.class.
					getResource(Configurations.IMAGE_FOLDER + "comuns/sample_collected_mask.png"));
			sampleCollectedImageIcon = new ImageIcon(Utils.paintImage(mascara, Main.secondColor, 120, 150));
			
			mascara = ImageIO.read(Main.class.
					getResource(Configurations.IMAGE_FOLDER + "comuns/sample_overlay_mask.png"));
			sampleOverlay = Utils.paintImage(mascara, Main.secondColor, 120, 150);
			
			BufferedImage sampleCombined = new BufferedImage(120, 150, BufferedImage.TYPE_INT_ARGB);
			Graphics g = sampleCombined.getGraphics();
			g.drawImage(sampleNoneImage, 0, 0, null);
			g.drawImage(sampleOverlay, 0, 0, null);
			fingerTouchedImageIcon = new ImageIcon(sampleCombined);
			
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	private void listDevices(List<HikivisionDeviceTO.Device> devices) {
		JLabel label2 = new JLabel("Selecione o device: ");
		deviceComboBox = new JComboBox<String>();
		deviceComboBox.setPreferredSize(new Dimension(150, 30));
		for (HikivisionDeviceTO.Device device : devices) {
			deviceComboBox.addItem(device.getDevIndex());			
		}
		deviceContainer = new JPanel(new FlowLayout(FlowLayout.LEFT, 2, 5));
		deviceContainer.add(label2);
		deviceContainer.add(deviceComboBox);
		
	}
	
	private void startReading(final Finger finger, final String device, PedestrianAccessEntity visitante) {
		Optional<CaptureFingerPrint> digitalCadastrada = hikivisionUseCases.coletarBiometriabiometria(device, finger);
	
		//TODO Exibir dialogo progressbar
		
		if (!digitalCadastrada.isPresent()) {
			System.out.println(String.format("Nao foi possivel cadastrar a digital cadastrada %d", finger.ordinal()));
			//TODO Exibir dialogo
			return;
		}
		
		Long cardNumber = Long.valueOf(visitante.getCardNumber());
		Long idUser = visitante.getId();
		HikivisionFingerEntity hikivisionSaved = hikivisionFingerDataSaved(finger, digitalCadastrada.get().fingerData, cardNumber, idUser);
		
		HibernateAccessDataFacade.save(HikivisionFingerEntity.class, hikivisionSaved);

		hikivisionUseCases.adicionarDigitalNoDevice(finger, Long.valueOf(visitante.getCardNumber()),
				digitalCadastrada.get().fingerData, hikivisionSaved);
	}
	
	private HikivisionFingerEntity hikivisionFingerDataSaved(final Finger finger, final String fingerData, final Long cardNumber, final Long idUser) {
		HikivisionFingerEntity hikivisionSaved = null;
		
		hikivisionFingerRepository.findByFingerNoAndIdUser(finger, idUser);
		 
		 if(Objects.isNull(hikivisionSaved)) {
			 hikivisionSaved = new HikivisionFingerEntity(finger, fingerData, cardNumber);
		 }
		
		return hikivisionSaved;
	}
	
	public void cancelCollect(){
		setMessage("Coleta de biometria cancelada!", "erro");
		Object[] options = {"OK"};
		JOptionPane.showOptionDialog(instance, "Biometria cancelada.","Biometria cancelada",
				JOptionPane.PLAIN_MESSAGE, JOptionPane.PLAIN_MESSAGE, null, options, options[0]);
		dispose();
	}
	
	private void setMessage(final String message, String type){
		messageLabel.setText(message);
		if (type.equals("erro"))
			messageLabel.setForeground(Color.RED);
		else
			messageLabel.setForeground(Main.firstColor);
	}
	
	class RoundedPanel extends JPanel {
        private Color backgroundColor;
        private int cornerRadius = 15;

        public RoundedPanel(LayoutManager layout, int radius) {
            super(layout);
            cornerRadius = radius;
        }

        public RoundedPanel(LayoutManager layout, int radius, Color bgColor) {
            super(layout);
            cornerRadius = radius;
            backgroundColor = bgColor;
        }

        public RoundedPanel(int radius) {
            super();
            cornerRadius = radius;
        }

        public RoundedPanel(int radius, Color bgColor) {
            super();
            cornerRadius = radius;
            backgroundColor = bgColor;
        }

        @Override
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);
            Dimension arcs = new Dimension(cornerRadius, cornerRadius);
            int width = getWidth();
            int height = getHeight();
            Graphics2D graphics = (Graphics2D) g;
            graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            //Draws the rounded panel with borders.
            if (backgroundColor != null) {
                graphics.setColor(backgroundColor);
            } else {
                graphics.setColor(getBackground());
            }
            graphics.fillRoundRect(0, 0, width-1, height-1, arcs.width, arcs.height); //paint background
            graphics.setColor(getForeground());
            graphics.drawRoundRect(0, 0, width-1, height-1, arcs.width, arcs.height); //paint border
        }

		public Color getBackgroundColor() {
			return backgroundColor;
		}

		public void setBackgroundColor(Color backgroundColor) {
			this.backgroundColor = backgroundColor;
		}

		public int getCornerRadius() {
			return cornerRadius;
		}

		public void setCornerRadius(int cornerRadius) {
			this.cornerRadius = cornerRadius;
		}
        
    }

}
