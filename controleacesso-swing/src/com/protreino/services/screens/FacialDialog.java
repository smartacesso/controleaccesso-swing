package com.protreino.services.screens;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.BufferedImage;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.WindowConstants;

import com.protreino.services.devices.Device;
import com.protreino.services.devices.FacialDevice;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.enumeration.DeviceMode;
import com.protreino.services.main.Main;
import com.protreino.services.utils.HibernateUtil;

@SuppressWarnings("serial")
public class FacialDialog extends JDialog {
	
	private FacialDialog instance;
	
	private JLabel userLabel;
	private JLabel deviceLabel;
	private JButton startCapturingButton;
	private JProgressBar progressBar;
	public JLabel sampleLabel;
	private JPanel samplesLabelContainer;
	private JLabel messageLabel;
	private JButton cancelButton;
	private PedestrianAccessEntity acesso;
	private FacialDevice device;
	private int maxAmostras;
	
	private int imageWidth = 480;
	private int imageHeight; // sera calculada de acordo com o tamanho da imagem da camera para manter a proporcao
	
	public FacialDialog(Frame owner, Device device, PedestrianAccessEntity acesso){
		super(owner, "Captura facial", true);
		this.device = (FacialDevice) device;
		this.acesso = acesso;
		this.instance = this;
		this.maxAmostras = this.device.getMaxAmostras();
		
		// calcula altura da imagem para mostrar no dialog
		double fatorDeReducao = (double) ((FacialDevice) this.device).getImageWidth() / imageWidth;
		this.imageHeight = (int) (((FacialDevice) this.device).getImageHeight() / fatorDeReducao);
		
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
		setPreferredSize(new Dimension(700,630));
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
		
		JLabel label3 = new JLabel("Câmera selecionada:");
		JPanel devicesContainer= new JPanel(new FlowLayout(FlowLayout.LEFT, 2, 5));
		devicesContainer.add(label3);
		deviceLabel = new JLabel(device.getName());
		deviceLabel.setFont(boldFont);
		deviceLabel.setForeground(Main.firstColor);
		devicesContainer.add(deviceLabel);
		
		startCapturingButton = new JButton("Iniciar captura");
		startCapturingButton.setPreferredSize(new Dimension(160, 40));
		progressBar = new JProgressBar(0, maxAmostras);
		progressBar.setPreferredSize(new Dimension(300, 40));
		progressBar.setVisible(false);
		JPanel startCapturingButtonContainer = new JPanel(new FlowLayout(FlowLayout.CENTER, 2, 5));
		startCapturingButtonContainer.add(startCapturingButton);
		startCapturingButtonContainer.add(progressBar);
		
		sampleLabel = new JLabel(getEmptySample());
		sampleLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		samplesLabelContainer = new JPanel(new FlowLayout(FlowLayout.CENTER));
		samplesLabelContainer.add(sampleLabel);
		
		messageLabel = new JLabel(" ");
		messageLabel.setFont(boldFont);
		JPanel messageLabelContainer = new JPanel(new FlowLayout(FlowLayout.CENTER));
		messageLabelContainer.add(messageLabel);
		
		JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
		mainPanel.setBorder(BorderFactory.createEmptyBorder(10,10,10,10));
		
		mainPanel.add(userContainer);
		mainPanel.add(devicesContainer);
		mainPanel.add(startCapturingButtonContainer);
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
		
		startCapturingButton.addActionListener( (e) -> startCapturing() );
		
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				cancelCollect("");
			}
		});
		
		addComponentListener(new ComponentListener() {
			@Override
			public void componentShown(ComponentEvent e) {
				toFront();
				for (Device d : Main.devicesList) {
					if(d instanceof FacialDevice)
						d.setMode(DeviceMode.NONE);
				}
				// nao faz nada enquanto a janela estiver aberta sem capturar, e so retorna ao normal quando fechar
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
		    public void windowClosing(WindowEvent e) {
		    	if (device != null)
		    		((FacialDevice) device).setFacialDialog(null);
		    }
		});
		
		pack();
	}
	
	public void showScreen() {
		this.device.setFacialDialog(this);
		setLocationRelativeTo(null);
		setVisible(true);
	}
	
	public void startCapturing(){
		try {
			String tagName = acesso.getName() + " (" + acesso.getId().toString() + ")";
			device.prepareStartCapturing(this, tagName);
			startCapturingButton.setVisible(false);
			progressBar.setVisible(true);
			removeMessage();
			if (!device.isConnected())
				device.connect();
		} 
		catch (Exception e1){
			setMessage("Ocorreu um erro ao iniciar a coleta.", "erro");
			e1.printStackTrace();
		}
	}
	
	public void cancelCollect(String mensagem){
		device.setFacialDialog(null);
		device.removeUser(acesso);
		setMessage("Coleta facial cancelada! " + mensagem, "erro");
		Object[] options = {"OK"};
	    JOptionPane.showOptionDialog(instance, "Coleta facial cancelada. " + mensagem, "Coleta cancelada",
	                   JOptionPane.PLAIN_MESSAGE, JOptionPane.PLAIN_MESSAGE, null, options, options[0]);
	    HibernateUtil.apagarPastaDeFotos(acesso.getId());
	    dispose();
	}
	
	public void setLuxandIdentifier(String identifier) {
		acesso.setLuxandIdentifier(identifier);
	}
	
	public void finishCollect(String mensagem) {
		try {
			HibernateUtil.save(PedestrianAccessEntity.class, acesso);
			
			JOptionPane.showMessageDialog(this, mensagem != null ? mensagem : "Coleta facial realizada com sucesso!", "Coleta concluída", JOptionPane.PLAIN_MESSAGE);
			dispose();
		
		} catch (Exception e) {
			setMessage("Ocorreu um erro ao finalizar a coleta.", "erro");
			e.printStackTrace();
		}
	}
	
	public void imageAcquired(BufferedImage bufferedImage){
		sampleLabel.setIcon(new ImageIcon(resizeToIconSize(bufferedImage)));
		removeMessage();
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
	
	public void updateProgress(){
		int newValue = progressBar.getValue() + 1;
		progressBar.setValue(newValue);
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
	
}
