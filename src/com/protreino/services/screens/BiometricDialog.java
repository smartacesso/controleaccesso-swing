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
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

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

import com.protreino.services.devices.ComputerIdDevice;
import com.protreino.services.devices.Device;
import com.protreino.services.devices.TopDataDevice;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.entity.BiometricEntity;
import com.protreino.services.entity.TemplateEntity;
import com.protreino.services.enumeration.BroadcastMessageType;
import com.protreino.services.enumeration.DeviceMode;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.enumeration.Finger;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.main.Main;
import com.protreino.services.to.BroadcastMessageTO;
import com.protreino.services.utils.Constants;
import com.protreino.services.utils.HibernateUtil;
import com.protreino.services.utils.Utils;

@SuppressWarnings("serial")
public class BiometricDialog extends JDialog{
	
	public BiometricDialog instance;
	
	private JLabel userLabel;
	public JComboBox<String> fingerComboBox;
	private JLabel deviceLabel;
	private JButton startReadingButton;
	public JLabel sampleLabel;
	private JPanel samplesLabelContainer;
	private JLabel messageLabel;
	private JButton cancelButton;
	
	private Image sampleOverlay;
	private ImageIcon sampleNoneImageIcon;
	private ImageIcon sampleCollectedImageIcon;
	private ImageIcon fingerTouchedImageIcon;
	private byte[] imageSample;
	private BiometricEntity biometry;
	public PedestrianAccessEntity acesso;
	private TemplateEntity templateEntity;
	private Device selectedDevice;
	private int sampleCount;
	private List<RoundedPanel> samplesCountList;
	private SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss:sss");
	
	public BiometricDialog(Frame owner, Device device, PedestrianAccessEntity acesso){
		super(owner, "Coleta de biometria", true);
		this.selectedDevice = device;
		this.acesso = acesso;
		this.instance = this;
		this.sampleCount = device.getManufacturer().getSamplesCount();
		
		loadImages();
		
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
		setPreferredSize(new Dimension(630,500));
		setResizable(false);
		setIconImage(Main.favicon);
		
		Font font = new JLabel().getFont();
		Font boldFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
		
		JLabel label1 = new JLabel("Usuário: ");
		userLabel = new JLabel(acesso.getName());
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
		
		JLabel label3 = new JLabel("Leitor biométrico:");
		JPanel devicesContainer= new JPanel(new FlowLayout(FlowLayout.LEFT, 2, 5));
		devicesContainer.add(label3);
		deviceLabel = new JLabel(device.getName());
		deviceLabel.setFont(boldFont);
		deviceLabel.setForeground(Main.firstColor);
		devicesContainer.add(deviceLabel);
		
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
		mainPanel.add(devicesContainer);
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
		
		startReadingButton.addActionListener( (e) -> startReading() );
		
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
		    	if (selectedDevice != null) {
		    		selectedDevice.setBiometricDialog(null);
		    		selectedDevice.setMode(DeviceMode.VERIFICATION);
		    	}
		    }
		});
		
		pack();
	}
	
	public void showScreen() {
		setLocationRelativeTo(null);
		setVisible(true);
	}
	
	public void startReading(){
		try {
			if (selectedDevice == null) {
				setMessage("Selecione um leitor!", "");
				return;
			}
			for (Device device : Main.devicesList){
				if (selectedDevice.isTheSame(device)) {
					selectedDevice = device;
					break;
				}
			}
			selectedDevice.setBiometricDialog(this);
			selectedDevice.setMode(DeviceMode.ENROLLMENT);
			sampleLabel.setIcon(sampleNoneImageIcon);
			fingerComboBox.setEnabled(false);
			startReadingButton.setEnabled(false);
			removeMessage();
			if (!selectedDevice.isConnected())
				selectedDevice.connect();
		
		} catch (Exception e1){
			setMessage("Ocorreu um erro ao iniciar a leitura.", "erro");
			e1.printStackTrace();
		}
	}
	
	public void cancelCollect(){
		setMessage("Coleta de biometria cancelada!", "erro");
		Object[] options = {"OK"};
		JOptionPane.showOptionDialog(instance, "Coleta de biometria cancelada.","Biometria cancelada",
				JOptionPane.PLAIN_MESSAGE, JOptionPane.PLAIN_MESSAGE, null, options, options[0]);
		dispose();
	}
	
	public void finishCollect(byte[] template) {
		try {
			sampleLabel.setIcon(sampleCollectedImageIcon);
			updateSamplesCount(sampleCount);
			selectedDevice.setMode(DeviceMode.VERIFICATION);
			
			//antes de salvar, verifica se o pedestre está correto
			PedestrianAccessEntity pedestre = (PedestrianAccessEntity) HibernateUtil.getSingleResultByIdTemp(PedestrianAccessEntity.class, acesso.getId());
			if(pedestre != null)
				acesso = pedestre;
			
			// Cria e salva uma BiometricEntity para ser enviada para o servidor
			biometry = new BiometricEntity();
			biometry.setUser(acesso.getId());
			biometry.setUserName(acesso.getName());
			biometry.setFinger(Finger.valueFromImport((String) fingerComboBox.getSelectedItem()));
			biometry.setTemplate(template);
			biometry.setSample(imageSample);
			HibernateUtil.save(BiometricEntity.class, biometry);
			
			// salva o template
			templateEntity = new TemplateEntity();
			templateEntity.setPedestrianAccess(acesso);
			templateEntity.setTemplate(template);
			templateEntity.setLocal(true);
			templateEntity.setManufacturer(selectedDevice.getManufacturer());
			templateEntity = (TemplateEntity) HibernateUtil.save(TemplateEntity.class, templateEntity)[0];
			
			// adiciona ao templateDatabase (banco de templates na memoria)
			if (Manufacturer.COMPUTER_ID.equals(selectedDevice.getManufacturer())) {
				for (Device device : Main.devicesList) {
					if (Manufacturer.COMPUTER_ID.equals(device.getManufacturer())
							&& DeviceStatus.CONNECTED.equals(device.getStatus())) {
						System.out.println(sdf.format(new Date()) + "   Adicionando template ao templateDatabase...");
						ComputerIdDevice computerIdDevice = (ComputerIdDevice) device;
						computerIdDevice.addTemplateToTemplateDatabase(templateEntity);
					}
				}
			}
			
			if (Manufacturer.NITGEN.equals(selectedDevice.getManufacturer())) {
				//adicionar também nas outras catracas conectadas
				for(Device device : Main.devicesList) {
					if(device != null && device instanceof TopDataDevice) {
						//verifica se pode adicionar
						TopDataDevice topDataDevice = (TopDataDevice) device;
						if(topDataDevice.getIndexSearchEngine() != null) {
							//para digitais na servidor
							System.out.println(sdf.format(new Date()) + 
								"   Enviando template para indexSearch do Inner "+topDataDevice.getInnerNumber()+"...");
							topDataDevice.addTemplateToIndexSearch(templateEntity);
							Thread.sleep(1000);
						}else if(device.isConnected()){
							//para digitais na catraca e que tenham espelhamento
							topDataDevice.insereDigitalInner(true, acesso);
						}
					}
				}
			}
			
			// envia o template via broadcast
			TemplateEntity temp = new TemplateEntity(templateEntity);
			if (Main.broadcastServer != null)
				Main.broadcastServer.sendMessage(new BroadcastMessageTO(BroadcastMessageType.NEW_TEMPLATE, temp));
			
			JOptionPane.showMessageDialog(this, "Biometria cadastrada com sucesso!", "Coleta concluída", JOptionPane.PLAIN_MESSAGE);
			dispose();
		
		} catch (Exception e) {
			setMessage("Ocorreu um erro ao finalizar a coleta.", "erro");
			e.printStackTrace();
		}
	}
	
	public void fingerTouched(){
		sampleLabel.setIcon(fingerTouchedImageIcon);
		removeMessage();
	}
	
	
	public void fingerGone(){
		sampleLabel.setIcon(sampleNoneImageIcon);
	}
	
	
	public void updateSampleStatus(Image sampleImage, int samplesCollected) {
		if (sampleImage != null){
			BufferedImage sampleCombined = new BufferedImage(120, 150, BufferedImage.TYPE_INT_ARGB);
			Graphics g = sampleCombined.getGraphics();
			g.drawImage(sampleOverlay, 0, 0, null);
			g.drawImage(sampleImage, 0, 0, null);
			g.drawImage(sampleOverlay, 0, 0, null);
			sampleLabel.setIcon(new ImageIcon(sampleCombined));
		}
		else {
			sampleLabel.setIcon(sampleCollectedImageIcon);
		}
		updateSamplesCount(samplesCollected);
		try {
			Thread.sleep(600);
		} catch (InterruptedException e1) {
			// ignore
		}
	}
	
	private void updateSamplesCount(int samplesCollected){
		RoundedPanel panel = samplesCountList.get(samplesCollected-1);
		panel.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(Main.secondColor, 5, true), 
				BorderFactory.createEmptyBorder(0, 10, 0, 10)));
		panel.setBackgroundColor(Main.secondColor);
		JLabel label = (JLabel) panel.getComponent(0);
		label.setForeground(Main.firstColor);
	}
	
	
	private void loadImages() {
		try {
			Toolkit toolkit = Toolkit.getDefaultToolkit();
			Image sampleNoneImage = toolkit.getImage(Main.class.
					getResource(Constants.IMAGE_FOLDER + "comuns/sample_none.png"));
			sampleNoneImageIcon = new ImageIcon(sampleNoneImage);
			
			BufferedImage mascara = ImageIO.read(Main.class.
					getResource(Constants.IMAGE_FOLDER + "comuns/sample_collected_mask.png"));
			sampleCollectedImageIcon = new ImageIcon(Utils.paintImage(mascara, Main.secondColor, 120, 150));
			
			mascara = ImageIO.read(Main.class.
					getResource(Constants.IMAGE_FOLDER + "comuns/sample_overlay_mask.png"));
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
	
	
	public void setMessage(String message, String type){
		messageLabel.setText(message);
		if (type.equals("erro"))
			messageLabel.setForeground(Color.RED);
		else
			messageLabel.setForeground(Main.firstColor);
	}
	
	
	public void removeMessage(){
		messageLabel.setText(" ");
	}
	
	public JLabel getSampleLabel() {
		return sampleLabel;
	}
	
	public void setImageSample(byte[] imageSample) {
		this.imageSample = imageSample;
	}

	public TemplateEntity getTemplateEntity() {
		return templateEntity;
	}

	public void setTemplateEntity(TemplateEntity templateEntity) {
		this.templateEntity = templateEntity;
	}


	class RoundedPanel extends JPanel
    {
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
