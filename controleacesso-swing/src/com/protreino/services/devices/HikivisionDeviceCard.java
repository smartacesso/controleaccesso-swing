package com.protreino.services.devices;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Toolkit;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;

import com.protreino.services.constants.Configurations;
import com.protreino.services.enumeration.MessageType;
import com.protreino.services.main.Main;
import com.protreino.services.to.hikivision.HikivisionDeviceTO;

@SuppressWarnings("serial")
public class HikivisionDeviceCard extends JPanel {

	private JPanel namePanel;
	private JLabel nameLabel;
	private JLabel icon;
	private JLabel mensagemLabel;
	private ImageIcon catracaIcon;
	private ImageIcon connectedIcon;
	private ImageIcon disconnectedIcon;
	private JLabel statusIcon;
	private JLabel linkIconLabel;
	private JLabel link;
	
	public HikivisionDeviceCard(HikivisionDeviceTO.Device device) {
		loadImages();
		Font font = new JLabel().getFont();
		Font boldFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
		
		setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
		setBorder(BorderFactory.createLineBorder(Main.firstColor, 1, true));
		
		namePanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 5));
		namePanel.setBackground(Main.firstColor);
		nameLabel = new JLabel(device.getDevName());
		nameLabel.setFont(boldFont);
		nameLabel.setForeground(Main.secondColor);
		namePanel.add(nameLabel);
		add(namePanel);
		
		JPanel iconsPanel = new JPanel();
		iconsPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 5, 5));
		icon = new JLabel(catracaIcon);
		icon.setAlignmentY(Component.BOTTOM_ALIGNMENT);
		iconsPanel.add(icon);
		
		statusIcon = new JLabel(disconnectedIcon);
		statusIcon.setAlignmentY(Component.BOTTOM_ALIGNMENT);
		iconsPanel.add(statusIcon);
		add(iconsPanel);
		
		JPanel linkPanel = new JPanel();
		linkPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 0, 0));
		linkPanel.setPreferredSize(new Dimension(200, 16));
		linkPanel.setMinimumSize(linkPanel.getPreferredSize());
		linkIconLabel = new JLabel(" ");
		linkIconLabel.setAlignmentY(Component.BOTTOM_ALIGNMENT);
		linkPanel.add(linkIconLabel);
		link = new JLabel(" ");
		link.setAlignmentY(Component.BOTTOM_ALIGNMENT);
		linkPanel.add(link);
		add(linkPanel);
		add(Box.createVerticalStrut(2));
		
		JPanel mensagemPanel = new JPanel(); //new FlowLayout(FlowLayout.LEFT));
		mensagemPanel.setLayout(new BoxLayout(mensagemPanel, BoxLayout.X_AXIS));
		mensagemPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		mensagemPanel.setBackground(Main.firstColor);
		mensagemLabel = new JLabel(" ");
		mensagemLabel.setFont(boldFont);
		mensagemLabel.setForeground(Main.secondColor);
		mensagemPanel.add(mensagemLabel);
		mensagemPanel.add(Box.createHorizontalGlue());
		add(mensagemPanel);
	}
	
	public void setMensagem(String novaMensagem, MessageType tipoMensagem){
		if (novaMensagem == null || "".equals(novaMensagem)) {
			novaMensagem = " ";
		}
		mensagemLabel.setText(novaMensagem);
		if (MessageType.ERROR.equals(tipoMensagem)) {
			mensagemLabel.setForeground(Color.PINK);
		} else {
			mensagemLabel.setForeground(Main.secondColor);
		}
	}
	
	public void setStatus(final String status){
		if ("online".equalsIgnoreCase(status)){
			statusIcon.setIcon(connectedIcon);
			setMensagem("Conectado", MessageType.NORMAL);
		} else {
			statusIcon.setIcon(disconnectedIcon);
			setMensagem("Offline", MessageType.ERROR);
		}
		revalidate();
	}
	
	private void loadImages(){
		Toolkit toolkit = Toolkit.getDefaultToolkit();
		catracaIcon = new ImageIcon(toolkit.getImage(Main.class.getResource(Configurations.IMAGE_FOLDER + "thumbnails/facial.png")));
		connectedIcon = new ImageIcon(toolkit.getImage(Main.class.getResource(Configurations.IMAGE_FOLDER + "comuns/ok.png")));
		disconnectedIcon = new ImageIcon(toolkit.getImage(Main.class.getResource(Configurations.IMAGE_FOLDER + "comuns/erro.png")));
	}

}
