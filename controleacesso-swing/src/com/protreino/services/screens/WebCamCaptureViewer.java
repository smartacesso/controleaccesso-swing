package com.protreino.services.screens;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.awt.image.BufferedImage;
import java.lang.Thread.UncaughtExceptionHandler;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;

import com.github.sarxos.webcam.Webcam;
import com.github.sarxos.webcam.WebcamDiscoveryEvent;
import com.github.sarxos.webcam.WebcamDiscoveryListener;
import com.github.sarxos.webcam.WebcamEvent;
import com.github.sarxos.webcam.WebcamListener;
import com.github.sarxos.webcam.WebcamPanel;
import com.github.sarxos.webcam.WebcamPicker;
import com.github.sarxos.webcam.WebcamResolution;
import com.protreino.services.main.Main;

public class WebCamCaptureViewer extends JDialog implements WebcamListener, WindowListener,
		UncaughtExceptionHandler, ItemListener, WebcamDiscoveryListener {

	private static final long serialVersionUID = 1L;

	private Webcam webcam = null;
	private WebcamPanel panel = null;
	private WebcamPicker picker = null;

	private JPanel tirarFotoPanel;

	private JButton tirarFotoButton;

	public WebCamCaptureViewer() {
		Webcam.addDiscoveryListener(this);

		setTitle("Tirar Foto");
		setIconImage(Main.favicon);
		setModal(true);
		setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
		setPreferredSize(new Dimension(400, 400));
		
		addWindowListener(this);
		
		tirarFotoPanel = new JPanel();
		tirarFotoPanel.setLayout(new BoxLayout(tirarFotoPanel, BoxLayout.Y_AXIS));
		tirarFotoPanel.setBorder(new EmptyBorder(10, 10, 10, 10));

		picker = new WebcamPicker();
		picker.addItemListener(this);

		webcam = picker.getSelectedWebcam();

		if (webcam == null) {
			System.out.println("No webcams found...");
			return;
		}
		
		if(panel != null)
			panel.stop();

		if(webcam.isOpen())
			webcam.close();
		
		webcam.setViewSize(WebcamResolution.VGA.getSize());
		webcam.addWebcamListener(WebCamCaptureViewer.this);

		panel = new WebcamPanel(webcam, false);
		panel.setFPSDisplayed(true);
		
		tirarFotoButton = new JButton("Capturar");
		tirarFotoButton.setPreferredSize(new Dimension(160, 40));
		tirarFotoButton.setAlignmentX(Component.CENTER_ALIGNMENT);

		tirarFotoPanel.add(picker);
		tirarFotoPanel.add(Box.createVerticalStrut(10));
		tirarFotoPanel.add(panel);
		tirarFotoPanel.add(Box.createVerticalStrut(10));
		tirarFotoPanel.add(tirarFotoButton);
		
		getContentPane().add(tirarFotoPanel);
		pack();
		setLocationRelativeTo(null);
	}
	
	public void start() {
		Thread t = new Thread() {
			@Override
			public void run() {
				try {
					Thread.sleep(500);
					panel.start();
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
		};

		t.setName("example-starter");
		t.setDaemon(true);
		t.setUncaughtExceptionHandler(this);
		t.start();
		
		setVisible(true);
	}

	@Override
	public void webcamOpen(WebcamEvent webcamEvent) {
		System.out.println("webcam open");
	}

	@Override
	public void webcamClosed(WebcamEvent webcamEvent) {
		System.out.println("webcam closed");
	}

	@Override
	public void webcamDisposed(WebcamEvent webcamEvent) {
		System.out.println("webcam disposed");
	}

	@Override
	public void webcamImageObtained(WebcamEvent webcamEvent) {}

	@Override
	public void windowActivated(WindowEvent event) {}

	@Override
	public void windowClosed(WindowEvent event) {
		System.out.println("windowClosed");
		
		panel.stop();
	}

	@Override
	public void windowClosing(WindowEvent event) {
		System.out.println("windowClosing");

		panel.stop();
	}

	@Override
	public void windowOpened(WindowEvent event) {}

	@Override
	public void windowDeactivated(WindowEvent event) {}

	@Override
	public void windowDeiconified(WindowEvent event) {
		System.out.println("webcam viewer resumed");
		panel.resume();
	}

	@Override
	public void windowIconified(WindowEvent event) {
		System.out.println("webcam viewer paused");
		panel.pause();
	}

	@Override
	public void uncaughtException(Thread thread, Throwable e) {
		System.err.println(String.format("Exception in thread %s", thread.getName()));
		e.printStackTrace();
	}

	@Override
	public void itemStateChanged(ItemEvent event) {
		if (event.getItem() == webcam) {
			return;
		}

		if (webcam == null) {
			return;
		}

		panel.stop();

		tirarFotoPanel.remove(panel);

		webcam.removeWebcamListener(this);
		webcam.close();

		webcam = (Webcam) event.getItem();
		
		webcam.setViewSize(WebcamResolution.VGA.getSize());
		webcam.addWebcamListener(this);

		panel = new WebcamPanel(webcam, false);
		panel.setFPSDisplayed(true);

		tirarFotoPanel.add(panel, 1);
		pack();
		
		try {
			Thread.sleep(3000);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}

		start();
	}

	@Override
	public void webcamFound(WebcamDiscoveryEvent event) {
		if (picker != null) {
			picker.addItem(event.getWebcam());
		}
	}

	@Override
	public void webcamGone(WebcamDiscoveryEvent event) {
		if (picker != null) {
			picker.removeItem(event.getWebcam());
		}
	}

	public JButton getTirarFotoButton() {
		return tirarFotoButton;
	}

	public void setTirarFotoButton(JButton tirarFotoButton) {
		this.tirarFotoButton = tirarFotoButton;
	}

	public Webcam getWebcam() {
		return webcam;
	}
}
