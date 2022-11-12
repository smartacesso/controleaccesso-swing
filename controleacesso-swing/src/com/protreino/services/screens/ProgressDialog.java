package com.protreino.services.screens;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;

import com.protreino.services.constants.Configurations;
import com.protreino.services.main.Main;

@SuppressWarnings("serial")
public class ProgressDialog extends JDialog {
	
	private JProgressBar progressBar;
	private JLabel messageLabel;
	
	public ProgressDialog(String title, String message) {
		super(Main.mainScreen, "Progresso", true);
		
		setUndecorated(true);
		setResizable(true);
		setPreferredSize(new Dimension(300,150));
		setMinimumSize(new Dimension(300,100));
		
		JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
		mainPanel.setBorder(new CompoundBorder(new LineBorder(Main.firstColor), new EmptyBorder(20, 10, 20, 10)));
		
		mainPanel.add(Box.createVerticalGlue());
		
		String[] partes = title.split(";");
		for (String string : partes) {
			JLabel titleLabel = new JLabel(string);
			titleLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
			JPanel titlePanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 0, 0));
			titlePanel.add(titleLabel);
			mainPanel.add(titlePanel);
		}
		
		progressBar = new JProgressBar(0, 100);
		progressBar.setPreferredSize(new Dimension(150, 30));
		progressBar.setValue(0);
		progressBar.setAlignmentX(Component.CENTER_ALIGNMENT);
		
		messageLabel = new JLabel(message);
		messageLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		JPanel messagePanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 0, 0));
		messagePanel.add(messageLabel);
		
		mainPanel.add(Box.createVerticalStrut(10));
		mainPanel.add(progressBar);
		mainPanel.add(Box.createVerticalStrut(10));
		mainPanel.add(messagePanel);
		mainPanel.add(Box.createVerticalGlue());
		
		setContentPane(mainPanel);
		pack();
		setLocationRelativeTo(null);
		
	}
	
	
	public void setProgress(int value) {
		this.progressBar.setValue(value);
		revalidate();
	}
	
	
	public void setMessage(String message) {
		this.messageLabel.setText(message);
		revalidate();
	}

}
