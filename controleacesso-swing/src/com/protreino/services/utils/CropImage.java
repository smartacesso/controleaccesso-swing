package com.protreino.services.utils;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JPanel;
import javax.swing.event.MouseInputListener;

@SuppressWarnings("serial")
public class CropImage extends JPanel implements MouseListener, MouseInputListener {
	
	private byte [] cropedImage;
	private BufferedImage bufferedImage;
	
	int dragStatus = 0, c1, c2, c3, c4;
	int t1, t2, t3, t4;

	public void start(String caminho) { 
		ImagePanel img = new ImagePanel(caminho);
		add(img);
		setSize(600, 600);
		setVisible(true);
		addMouseListener(this);
		addMouseMotionListener(this);
	}
	
	@Override
	public void mousePressed(MouseEvent e) {
		repaint();
		c1 = e.getX();
		c2 = e.getY();
		t1 = e.getXOnScreen();
		t2 = e.getYOnScreen();
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		repaint();
		if(dragStatus == 1){
			c3 = e.getX();
			c4 = e.getY();
			t3 = e.getXOnScreen();
			t4 = e.getYOnScreen();
			
			try {
				draggedScreen();
			} catch(Exception exception) {
				exception.printStackTrace();
			}
		}
	}
	
//	public void draggedScreen() throws Exception {
//		int width = t1 - t3;
//		int height = t2 - t4;
//		width = width * -1;
//		height = width;
//		Robot robot = new Robot();
//		bufferedImage = robot.createScreenCapture(new Rectangle(t1, t2, width, height));
//		
//		ByteArrayOutputStream baos = new ByteArrayOutputStream();
//		ImageIO.write(bufferedImage, "JPG", baos);
//		
//		cropedImage = baos.toByteArray();
//		
//		baos.close();
//	}
	
	public void draggedScreen() throws Exception {
	    // Calcular a largura e altura do corte
	    int width = Math.abs(c3 - c1);
	    int height = Math.abs(c4 - c2);
	    
	    // Certificar-se de que as coordenadas iniciais sejam menores que as finais
	    int x = Math.min(c1, c3);
	    int y = Math.min(c2, c4);
	    
	    // Garantir que o corte seja apenas da imagem exibida
	    ImagePanel imagePanel = (ImagePanel) getComponent(0); // Assumindo que é o primeiro componente
	    BufferedImage originalImage = imagePanel.getBufferedImage();
	    
	    if (x + width > originalImage.getWidth()) {
	        width = originalImage.getWidth() - x;
	    }
	    if (y + height > originalImage.getHeight()) {
	        height = originalImage.getHeight() - y;
	    }
	    
	    // Realizar o corte da imagem original
	    bufferedImage = originalImage.getSubimage(x, y, width, height);
	    
	    // Converter para byte array
	    ByteArrayOutputStream baos = new ByteArrayOutputStream();
	    ImageIO.write(bufferedImage, "JPG", baos);
	    cropedImage = baos.toByteArray();
	    baos.close();
	}


	@Override
	public void mouseDragged(MouseEvent e) {
		repaint();
		dragStatus = 1;
		c3 = e.getX();
		c4 = e.getY();
		t3 = e.getXOnScreen();
		t4 = e.getYOnScreen();
	}
	
	public void paint(Graphics graphic) {
		super.paint(graphic);
		
		int width = c1 - c3;
		int height = c2 - c4;
		width = width * -1;
		
		if(width < 0)
			width = width * -1;

		height = width;
		
		graphic.setColor(Color.BLUE);
		graphic.drawRect(c1, c2, width, height);
	}
		
	@Override
	public void mouseMoved(MouseEvent e) {}
	@Override
	public void mouseClicked(MouseEvent e) {}
	@Override
	public void mouseEntered(MouseEvent e) {}
	@Override
	public void mouseExited(MouseEvent e) {}

	public byte [] getCropedImage() {
		return cropedImage;
	}

	public BufferedImage getBufferedImage() {
		return bufferedImage;
	}
}

class ImagePanel extends JPanel {
	private static final long serialVersionUID = 1L;
	
	private Image img;
	
	public ImagePanel(String img) {
		this(new ImageIcon(img).getImage());
	}
	
	public ImagePanel(Image img) {
		this.img = img;
		Dimension size = new Dimension(500, 500);
		setPreferredSize(size);
		setMinimumSize(size);
		setMaximumSize(size);
		setSize(size);
		setLayout(null);
	}
	
	public void paintComponent(Graphics graphic) { 
		graphic.drawImage(img, 0, 0, null);
	}
	
	public BufferedImage getBufferedImage() {
	    // Converte a imagem carregada em BufferedImage
	    if (img instanceof BufferedImage) {
	        return (BufferedImage) img;
	    }
	    // Caso não seja BufferedImage, converte manualmente
	    BufferedImage bufferedImg = new BufferedImage(img.getWidth(null), img.getHeight(null), BufferedImage.TYPE_INT_RGB);
	    Graphics g = bufferedImg.createGraphics();
	    g.drawImage(img, 0, 0, null);
	    g.dispose();
	    return bufferedImg;
	}

}