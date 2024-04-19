package com.protreino.services.screens.dialogs;

import java.awt.BorderLayout;
import java.awt.Component;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;

import com.protreino.services.main.Main;

@SuppressWarnings("serial")
public class SimpleMessageDialog extends JDialog {

	public SimpleMessageDialog(final String title, final String message, final String buttonText) {
        setIconImage(Main.favicon);
        setModal(true);
        setTitle(title);
        setResizable(false);
        setLayout(new BorderLayout());

        JPanel mainPanel = new JPanel();
        mainPanel.setBorder(new EmptyBorder(20, 20, 20, 20));
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

        JLabel mensagemLabel = new JLabel(message);
        mensagemLabel.setAlignmentX(Component.CENTER_ALIGNMENT);

        JButton button = new JButton(buttonText);
        button.setBorder(new EmptyBorder(10, 20, 10, 20));
        button.setAlignmentX(Component.CENTER_ALIGNMENT);
        button.addActionListener(e -> {
            dispose();
        });

        JPanel buttonPanel = new JPanel();
        buttonPanel.setBorder(new EmptyBorder(20, 20, 20, 20));
        buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.X_AXIS));
        buttonPanel.add(button);

        mainPanel.add(mensagemLabel);
        mainPanel.add(Box.createVerticalStrut(10));
        mainPanel.add(buttonPanel);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        pack();
        setLocationRelativeTo(null);
        setVisible(true);
	}
	
}
