package com.protreino.services.utils;

import java.awt.Graphics;
import java.awt.Image;

import javax.swing.JPanel;

@SuppressWarnings("serial")
public class BackgroundPanel extends JPanel {
	
	private Image bgImage;
	
	@Override
	  protected void paintComponent(Graphics g) {
	    super.paintComponent(g);
	        g.drawImage(bgImage, 0, 0, null);
	}

	public Image getBgImage() {
		return bgImage;
	}

	public void setBgImage(Image bgImage) {
		this.bgImage = bgImage;
	}
	
}