package com.protreino.services.utils;

import java.awt.Color;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Font;

import javax.swing.JLabel;
import javax.swing.JPanel;

@SuppressWarnings("serial")
public class PanelWithLabel extends JPanel {
	
	private JLabel label;
	
	/**
	 * Com alinhamento no centro, sem negrito e espaçamento igual a zero
	 * @param texto
	 */
	public PanelWithLabel(String texto){
		this(texto, FlowLayout.CENTER, false, 0, 0);
	}
	
	/**
	 * Sem negrito e espaçamento igual a zero
	 * @param texto
	 * @param aligment
	 */
	public PanelWithLabel(String texto, int aligment){
		this(texto, aligment, false, 0, 0);
	}
	
	/**
	 * Com espaçamento igual a zero
	 * @param texto
	 * @param aligment
	 */
	public PanelWithLabel(String texto, int aligment, boolean bold){
		this(texto, aligment, bold, 0, 0);
	}
	
	
	public PanelWithLabel(String texto, int aligment, boolean bold, int hGap, int vGap){
		label = new JLabel(texto);
		if (FlowLayout.LEFT == aligment) {
			label.setAlignmentX(Component.LEFT_ALIGNMENT);
			setLayout(new FlowLayout(FlowLayout.LEFT, hGap, vGap));
		}
		else if (FlowLayout.CENTER == aligment) {
			label.setAlignmentX(Component.CENTER_ALIGNMENT);
			setLayout(new FlowLayout(FlowLayout.CENTER, hGap, vGap));
		}
		if (bold) {
			Font font = label.getFont();
			Font boldFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
			label.setFont(boldFont);
		}
		add(label);
	}
	
	public void setText(String texto){
		label.setText(texto);
	}
	
	public void setLabelColor(Color color){
		label.setForeground(color);
	}
	
}
