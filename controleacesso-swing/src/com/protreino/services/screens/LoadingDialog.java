package com.protreino.services.screens;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Window;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;

import com.protreino.services.main.Main;

@SuppressWarnings("serial")
public class LoadingDialog extends JDialog {

	public LoadingDialog(Window owner) {
		super(owner, "Smart Acesso", ModalityType.APPLICATION_MODAL);
		setUndecorated(isUndecorated());
		setResizable(Boolean.TRUE);

		JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
		mainPanel.setBorder(new CompoundBorder(new LineBorder(Main.firstColor), new EmptyBorder(20, 10, 20, 10)));

		mainPanel.add(Box.createVerticalGlue());

		JLabel titleLabel = new JLabel("Carregando...", SwingConstants.CENTER);
		titleLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		mainPanel.add(titleLabel);

		add(mainPanel);

		pack();
		setLocationRelativeTo(Main.mainScreen);
	}

	public LoadingDialog(AccessListPanel accessListPanel) {
		super(Main.mainScreen, "Smart Acesso", ModalityType.APPLICATION_MODAL);
		setUndecorated(isUndecorated());
		setResizable(Boolean.TRUE);
		setPreferredSize(new Dimension(280, 120));
		setMinimumSize(new Dimension(280, 120));

		JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
		mainPanel.setBorder(new CompoundBorder(new LineBorder(Main.firstColor), new EmptyBorder(20, 10, 20, 10)));

		mainPanel.add(Box.createVerticalGlue());

		JLabel titleLabel = new JLabel("Carregando...", SwingConstants.CENTER);
		titleLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		mainPanel.add(titleLabel);

		add(mainPanel);

		setLocationRelativeTo(Main.mainScreen);

		pack();
	}

	public void showLoadingDialog() {
		SwingUtilities.invokeLater(() -> setVisible(true));
	}

	public void hideLoadingDialog() {
		SwingUtilities.invokeLater(() -> dispose());
	}

}
