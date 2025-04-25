package com.protreino.services.screens;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class TecladoVirtualPanel extends JPanel{
	private final JTextField target;

    public TecladoVirtualPanel(JTextField target) {
        this.target = target;
        setLayout(new GridLayout(4, 3, 10, 10));

        Font buttonFont = new Font("Arial", Font.BOLD, 32);

        for (int i = 1; i <= 9; i++) {
            addButton(String.valueOf(i), buttonFont);
        }

        addButton("←", buttonFont); // Backspace
        addButton("0", buttonFont);
        addButton("Limpar", buttonFont);
    }

    private void addButton(String label, Font font) {
        JButton button = new JButton(label);
        button.setFont(font);
        button.setPreferredSize(new Dimension(200, 80)); // largura x altura
        button.addActionListener(this::handleClick);
        add(button);
    }

    private void handleClick(ActionEvent e) {
        String cmd = ((JButton) e.getSource()).getText();
        if ("←".equals(cmd)) {
            String text = target.getText();
            if (!text.isEmpty()) {
                target.setText(text.substring(0, text.length() - 1));
            }
        } else if ("Limpar".equals(cmd)) {
            target.setText("");
        } else {
            target.setText(target.getText() + cmd);
        }
    }

}
