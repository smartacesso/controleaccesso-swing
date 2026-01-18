package com.protreino.services.screens.dialogs;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;

public class LoadingDialog extends JDialog {

    private JLabel mensagemLabel;
    private JProgressBar progressBar;

    public LoadingDialog(Window parent, String mensagem) {
        super(parent, "Aguarde", ModalityType.APPLICATION_MODAL);

        mensagemLabel = new JLabel(mensagem, SwingConstants.CENTER);
        mensagemLabel.setBorder(new EmptyBorder(10, 10, 5, 10));
        mensagemLabel.setFont(new Font("Segoe UI", Font.PLAIN, 13));

        progressBar = new JProgressBar();
        progressBar.setIndeterminate(true);

        JPanel content = new JPanel(new BorderLayout(10, 10));
        content.setBorder(new EmptyBorder(10, 15, 15, 15));
        content.add(mensagemLabel, BorderLayout.NORTH);
        content.add(progressBar, BorderLayout.CENTER);

        setContentPane(content);
        setSize(320, 110);
        setResizable(false);
        setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
        setLocationRelativeTo(parent);
    }

    public void setMensagem(String mensagem) {
        mensagemLabel.setText(mensagem);
    }
}
