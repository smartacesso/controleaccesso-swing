package com.protreino.services.screens;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Toolkit;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JWindow;

import com.protreino.services.constants.Configurations;
import com.protreino.services.main.Main;

@SuppressWarnings("serial") // Ignora aviso sobre serialização
public class SplashScreen extends JWindow {

    private ImageIcon logoImageIcon; // Armazena o icone do logotipo

    public SplashScreen() {
        // Configura o painel principal
        JPanel content = new JPanel();
        content.setLayout(new BoxLayout(content, BoxLayout.Y_AXIS)); // Layout vertical
        content.setBackground(new Color(245, 245, 245, 255)); // Cor de fundo 

        loadImages(); // Carrega o logotipo

        // Define o tamanho e posição da janela (centralizada na tela)
        int width = 400, height = 200;
        Dimension screen = Toolkit.getDefaultToolkit().getScreenSize();
        int x = (screen.width - width) / 2, y = (screen.height - height) / 2;
        setBounds(x, y, width, height);

        // Cria os componentes visuais
        JLabel icon = new JLabel(logoImageIcon); // Logotipo
        icon.setAlignmentX(Component.CENTER_ALIGNMENT);

        JLabel message = new JLabel(Main.nomeAplicacao + " Controle de Acesso", JLabel.CENTER); // Texto principal
        message.setFont(new Font("Sans-Serif", Font.BOLD, 13));
        message.setForeground(Main.firstColor);
        message.setAlignmentX(Component.CENTER_ALIGNMENT);

        JLabel carregando = new JLabel("Carregando...", JLabel.CENTER); // Texto Carregando
        carregando.setFont(new Font("Sans-Serif", Font.BOLD, 12));
        carregando.setForeground(Main.firstColor);
        carregando.setAlignmentX(Component.CENTER_ALIGNMENT);

        JLabel versao = new JLabel("Versao " + Configurations.VERSION, JLabel.CENTER); // Versão do sistema
        versao.setFont(new Font("Sans-Serif", Font.BOLD, 11));
        versao.setForeground(Main.firstColor);
        versao.setAlignmentX(Component.CENTER_ALIGNMENT);

        // Adiciona os componentes ao painel
        content.add(Box.createVerticalGlue()); // Espaço flexível
        content.add(icon); // Logotipo
        content.add(Box.createVerticalStrut(20)); // Espaço fixo
        content.add(message); // Mensagem principal
        content.add(versao); // Versão do sistema
        content.add(Box.createVerticalGlue());
        content.add(carregando); // Texto Carregando
        content.add(Box.createVerticalGlue());

        // Define bordas e margens do painel
        content.setBorder(BorderFactory.createCompoundBorder(
            BorderFactory.createLineBorder(Main.firstColor), // Borda externa
            BorderFactory.createEmptyBorder(20, 10, 20, 10) // Margem interna
        ));

        setContentPane(content); // Define o painel como conteúdo da janela
        setVisible(true); // Exibe a janela
        toFront(); // Traz a janela para frente
    }

    // Método para carregar o logotipo
    private void loadImages() {
        try {
            Toolkit toolkit = Toolkit.getDefaultToolkit();
            logoImageIcon = new ImageIcon(toolkit.getImage(Main.class.getResource(
                Configurations.IMAGE_FOLDER + Main.customImageFolder + "logo_grd003.png"
            )));
        } catch (Exception e) {
            e.printStackTrace(); // Mostra erro no console se falhar
        }
    }
}
