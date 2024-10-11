package com.protreino.services.screens;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Toolkit;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JWindow;

import com.protreino.services.constants.Configurations;
import com.protreino.services.main.Main;

@SuppressWarnings("serial")
public class SplashScreen extends JWindow {
    
    private ImageIcon logoImageIcon;
    
    public SplashScreen() {
        JPanel content = new JPanel();
        content.setLayout(new BoxLayout(content, BoxLayout.Y_AXIS));
        Color cinza = new Color(245, 245, 245,  255);
        content.setBackground(cinza);
        
        loadImages();
        
        int width = 400;
        int height = 200;
        Dimension screen = Toolkit.getDefaultToolkit().getScreenSize();
        int x = (screen.width-width)/2;
        int y = (screen.height-height)/2;
        setBounds(x,y,width,height);
        
        JLabel icon = new JLabel(logoImageIcon);
        icon.setAlignmentX(Component.CENTER_ALIGNMENT);
        JLabel message = new JLabel("Carregando o " + Main.nomeAplicacao + " Controle de Acesso...", JLabel.CENTER);
        message.setFont(new Font("Sans-Serif", Font.BOLD, 13));
        message.setForeground(Main.firstColor);
        message.setAlignmentX(Component.CENTER_ALIGNMENT);
        JLabel versao = new JLabel("Versao " + Configurations.VERSION, JLabel.CENTER);
        versao.setFont(new Font("Sans-Serif", Font.BOLD, 11));
        versao.setForeground(Main.firstColor);
        versao.setAlignmentX(Component.CENTER_ALIGNMENT);
        content.add(Box.createVerticalGlue());
        content.add(icon);
        content.add(Box.createVerticalStrut(20));
        content.add(message);
        content.add(versao);
        content.add(Box.createVerticalGlue());
        content.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(Main.firstColor), BorderFactory.createEmptyBorder(20, 10, 20, 10)));        
        
        setContentPane(content);
        setVisible(true);
        toFront();
    }
    
    private void loadImages() {
		try {
			Toolkit toolkit = Toolkit.getDefaultToolkit();
			logoImageIcon = new ImageIcon(toolkit.getImage(Main.class.
					getResource(Configurations.IMAGE_FOLDER + Main.customImageFolder + "logo_grd003.png")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}

}
