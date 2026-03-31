package com.protreino.services.screens;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class TecladoAlfabeticoPanel extends JPanel {

    private final JTextField target;

    public TecladoAlfabeticoPanel(JTextField target) {
        this.target = target;

        setLayout(new GridLayout(5, 6, 10, 10)); // 5 linhas x 6 colunas

        Font buttonFont = new Font("Arial", Font.BOLD, 24);

        String[] letras = {
            "A","B","C","D","E","F",
            "G","H","I","J","K","L",
            "M","N","O","P","Q","R",
            "S","T","U","V","W","X",
            "Y","Z"
        };

        // Letras
        for (String letra : letras) {
            addButton(letra, buttonFont);
        }

        // Botões especiais
        addButton("Espaço", buttonFont);
        addButton("←", buttonFont);
        addButton("Limpar", buttonFont);
    }

    private void addButton(String label, Font font) {
        JButton button = new JButton(label);
        button.setFont(font);
        button.setPreferredSize(new Dimension(120, 70));
        button.addActionListener(this::handleClick);
        add(button);
    }

    private void handleClick(ActionEvent e) {
        String cmd = ((JButton) e.getSource()).getText();

        switch (cmd) {
            case "←":
                String text = target.getText();
                if (!text.isEmpty()) {
                    target.setText(text.substring(0, text.length() - 1));
                }
                break;

            case "Limpar":
                target.setText("");
                break;

            case "Espaço":
                target.setText(target.getText() + " ");
                break;

            default:
                target.setText(target.getText() + cmd);
                break;
        }
    }
}