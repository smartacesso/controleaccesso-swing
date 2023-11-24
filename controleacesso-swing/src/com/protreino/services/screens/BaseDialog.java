package com.protreino.services.screens;

import java.awt.Color;
import java.awt.Font;
import java.awt.Frame;

import javax.swing.JDialog;
import javax.swing.JLabel;

import com.protreino.services.main.Main;

@SuppressWarnings("serial")
public class BaseDialog extends JDialog {
	
	 public BaseDialog() {
		 
	 }
	
	 public BaseDialog(Frame owner, String title, boolean modal) {
		 super(owner, title, modal);
	 }
	
	
	protected void redAndBoldFont(JLabel label) {
		label.setForeground(Color.red);
		Font f = label.getFont();
		label.setFont(f.deriveFont(f.getStyle() | Font.BOLD));
	}
	
	protected void blackAndUnboldFont(JLabel label) {
		label.setForeground(Color.BLACK);
		Font f = label.getFont();
		label.setFont(f.deriveFont(f.getStyle() & ~Font.BOLD));
	}
	
	protected void setFirstColorFont(JLabel label) {
		label.setForeground(Main.firstColor);
	}

}
