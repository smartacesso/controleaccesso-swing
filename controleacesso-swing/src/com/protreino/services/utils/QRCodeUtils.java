package com.protreino.services.utils;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import javax.imageio.ImageIO;

import com.itextpdf.text.pdf.BarcodeQRCode;

public class QRCodeUtils {
	
	public static byte[] gerarImagemQRCode(String text) {
		BarcodeQRCode qrcode = new BarcodeQRCode(text, 1, 1, null);
		Image awtImage = qrcode.createAwtImage(Color.BLACK, Color.WHITE);

		BufferedImage bi = new BufferedImage(500, 500, BufferedImage.TYPE_INT_RGB);
		Graphics2D graphics2D = bi.createGraphics();
		graphics2D.drawImage(awtImage, 0, 0, 500, 500, null);

		ByteArrayOutputStream buff = new ByteArrayOutputStream();
		try {
			ImageIO.write(bi, "JPG", buff);
		} catch (IOException ex) {
			ex.printStackTrace();
		}

		return buff.toByteArray();
	}

}
