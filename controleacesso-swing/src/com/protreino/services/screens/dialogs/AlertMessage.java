package com.protreino.services.screens.dialogs;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.border.EmptyBorder;

public class AlertMessage extends JDialog {

    public AlertMessage(Component parent, String titulo, String mensagem) {
        super((java.awt.Frame) SwingUtilities.getWindowAncestor(parent), "", true); // Retira o título do frame
        setUndecorated(true);
        setSize(500, 250);
        setLocationRelativeTo(parent);

        JPanel panel = new JPanel(new BorderLayout());
        panel.setBorder(new EmptyBorder(30, 30, 30, 30));
        panel.setBackground(new Color(245, 245, 245));

        // Título
        JLabel tituloLabel = new JLabel(titulo, SwingConstants.CENTER);
        tituloLabel.setFont(new Font("Arial", Font.BOLD, 28));
        tituloLabel.setForeground(new Color(50, 50, 50));
        panel.add(tituloLabel, BorderLayout.NORTH);

        // Mensagem
        JLabel mensagemLabel = new JLabel("<html><div style='text-align: center;'>" + mensagem + "</div></html>", SwingConstants.CENTER);
        mensagemLabel.setFont(new Font("Arial", Font.PLAIN, 22));
        mensagemLabel.setForeground(Color.DARK_GRAY);

        // Botão
        JButton okButton = new JButton("OK");
        okButton.setFont(new Font("Arial", Font.PLAIN, 22));
        okButton.setPreferredSize(new Dimension(100, 40));
        okButton.addActionListener(e -> dispose());

        JPanel buttonPanel = new JPanel();
        buttonPanel.setBackground(panel.getBackground());
        buttonPanel.add(okButton);

        panel.add(mensagemLabel, BorderLayout.CENTER);
        panel.add(buttonPanel, BorderLayout.SOUTH);

        setContentPane(panel);
    }

    public void mostrar() {
        setVisible(true);
    }
}

