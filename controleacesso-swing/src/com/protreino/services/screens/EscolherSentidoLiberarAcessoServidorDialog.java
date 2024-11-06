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
import com.protreino.services.main.Main;
import com.protreino.services.repository.HibernateAccessDataFacade;

public class EscolherSentidoLiberarAcessoServidorDialog {

	public static JDialog escolherSentidoDialog;

	public static boolean janelaLiberarAcessoAberta = false;
	private String identificador;
	
	public EscolherSentidoLiberarAcessoServidorDialog(String identificador) {
		escolherSentidoDialog = new JDialog();
		escolherSentidoDialog.setIconImage(Main.favicon);
		escolherSentidoDialog.setModal(true);
		escolherSentidoDialog.setTitle("Escolher sentido");
		escolherSentidoDialog.setResizable(false);
		this.identificador = identificador;

		janelaLiberarAcessoAberta = true;

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

		JButton escolherSaidaButton = new JButton("2 - Saida");
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
		HibernateAccessDataFacade.liberarAcessoNoServidor(identificador, Tipo.ENTRADA);
		janelaLiberarAcessoAberta = false;
	}

	private void liberarSaidaAction() {
		HibernateAccessDataFacade.liberarAcessoNoServidor(identificador, Tipo.SAIDA);
		janelaLiberarAcessoAberta = false;
	}

}
