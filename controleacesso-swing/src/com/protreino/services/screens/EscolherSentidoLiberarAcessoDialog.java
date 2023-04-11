package com.protreino.services.screens;

import java.awt.Component;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;

import com.protreino.services.constants.Tipo;
import com.protreino.services.devices.Device;
import com.protreino.services.devices.ServerDevice;
import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.enumeration.BroadcastMessageType;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.enumeration.NotificationType;
import com.protreino.services.enumeration.VerificationResult;
import com.protreino.services.main.Main;
import com.protreino.services.to.BroadcastMessageTO;
import com.protreino.services.utils.HibernateUtil;
import com.protreino.services.utils.Utils;

public class EscolherSentidoLiberarAcessoDialog{
	
	private LogPedestrianAccessEntity logAccess;
	private Device device;
	
	public static JDialog escolherSentidoDialog;
	
	public static boolean janelaLiberarAcessoAberta = false;

	public EscolherSentidoLiberarAcessoDialog(Device device, String motivoLiberacao, String idPedestre) {
		if(device == null)
			return;
		
		escolherSentidoDialog = new JDialog();
		escolherSentidoDialog.setIconImage(Main.favicon);
		escolherSentidoDialog.setModal(true);
		escolherSentidoDialog.setTitle("Escolher sentido");
		escolherSentidoDialog.setResizable(false);
		
		this.device = device;
		
		janelaLiberarAcessoAberta = true;
		
		logAccess = new LogPedestrianAccessEntity(Main.loggedUser.getId(), null, 
				"LIBERADO PELO SISTEMA", this.device.getLocation(), motivoLiberacao);
		
		if(idPedestre != null)
			logAccess.setIdPedestrian(Long.valueOf(idPedestre));
		
		JPanel escolherSentidoPanel = new JPanel();
		escolherSentidoPanel.setBorder(new EmptyBorder(20, 20, 20, 20));
		escolherSentidoPanel.setLayout(new BoxLayout(escolherSentidoPanel, BoxLayout.Y_AXIS));
		
		JButton escolherEntradaButton = new JButton("1 - Entrada");
		escolherEntradaButton.setBorder(new EmptyBorder(10, 20, 10, 20));
		escolherEntradaButton.setAlignmentX(Component.CENTER_ALIGNMENT);
		escolherEntradaButton.addActionListener(e -> {
			liberarEntradaAction();
			escolherSentidoDialog.dispose();
		});
		
		JButton escolherSaidaButton = new JButton("2 - Saída");
		escolherSaidaButton.setBorder(new EmptyBorder(10, 20, 10, 20));
		escolherSaidaButton.setAlignmentX(Component.CENTER_ALIGNMENT);
		escolherSaidaButton.addActionListener(e -> {
			liberarSaidaAction();
			escolherSentidoDialog.dispose();
		});
		
		escolherSentidoPanel.add(escolherEntradaButton);
		escolherSentidoPanel.add(Box.createVerticalStrut(10));
		escolherSentidoPanel.add(escolherSaidaButton);
		
		escolherSentidoDialog.addComponentListener(new ComponentAdapter() {
			@Override
			public void componentHidden(ComponentEvent e) {
				janelaLiberarAcessoAberta = false;
				escolherSentidoDialog = null;
			}
		});
		escolherSentidoDialog.getContentPane().add(escolherSentidoPanel);
		escolherSentidoDialog.pack();
		escolherSentidoDialog.setLocationRelativeTo(null);
		escolherSentidoDialog.setVisible(true);
	}
	
	private void liberarEntradaAction() {
		logAccess.setDirection(Tipo.ENTRADA);
		logAccess.setEquipament(this.device.getFullIdentifier());
		
		liberarCatraca();
		janelaLiberarAcessoAberta = false;
	}
	
	private void liberarSaidaAction() {
		logAccess.setDirection(Tipo.SAIDA);
		logAccess.setEquipament(this.device.getFullIdentifier());
		
		Main.apertouF10 = true;
		liberarCatraca();
		Main.apertouF10 = false;
		janelaLiberarAcessoAberta = false;
	}
	
	private void liberarCatraca() {
		if (Manufacturer.SERVER.equals(device.getManufacturer())) {
			ServerDevice serverDevice = (ServerDevice) device;
			serverDevice.setLogAccess(logAccess);
			serverDevice.allowAccess();
		
		} else {
			Utils.createNotification("Acesso liberado pelo sistema.", NotificationType.GOOD);
			HibernateUtil.save(LogPedestrianAccessEntity.class, logAccess);
			if (Main.broadcastServer != null)
				Main.broadcastServer.sendMessage(new BroadcastMessageTO(BroadcastMessageType.LOG_ACCESS, logAccess));
    		device.setVerificationResult(VerificationResult.ALLOWED);
			device.allowAccess();
		}
	}
}
