package com.protreino.services.screens; // Pacote da tela de captura da webcam

import java.awt.Component; // Classe base de todos os componentes gráficos
import java.awt.Dimension; // Usada para definir o tamanho de componentes
import java.awt.event.ItemEvent; // Evento gerado quando há alteração em componentes de item
import java.awt.event.ItemListener; // Interface para escutar mudanças de item
import java.awt.event.WindowEvent; // Evento gerado por interações com a janela
import java.awt.event.WindowListener; // Interface para escutar eventos de janela
import java.lang.Thread.UncaughtExceptionHandler; // Usada para tratar exceções não capturadas em threads
import java.awt.image.BufferedImage;  // Classe que representa imagens em memória

import javax.swing.Box; // Usado para criar layouts com componentes empilhados
import javax.swing.BoxLayout; // Gerencia o layout de componentes em uma Box
import javax.swing.JButton; // Classe para criar botões
import javax.swing.JDialog; // Classe para criar janelas modais
import javax.swing.JPanel; // Usado para agrupar componentes
import javax.swing.border.EmptyBorder; // Adiciona espaçamento vazio em torno de componentes

import com.github.sarxos.webcam.Webcam; // Classe para acessar e controlar webcams
import com.github.sarxos.webcam.WebcamDiscoveryEvent; // Evento gerado quando uma webcam é descoberta
import com.github.sarxos.webcam.WebcamDiscoveryListener; // Interface para escutar eventos de descoberta de webcam
import com.github.sarxos.webcam.WebcamEvent; // Evento gerado pela webcam
import com.github.sarxos.webcam.WebcamListener; // Interface para escutar eventos da webcam
import com.github.sarxos.webcam.WebcamPanel; // Classe para exibir vídeo da webcam em um painel
import com.github.sarxos.webcam.WebcamPicker; // Permite escolher a webcam usada
import com.github.sarxos.webcam.WebcamResolution; // Define a resolução da webcam

import com.protreino.services.main.Main; // Contém informações principais do aplicativo

public class WebCamCaptureViewer extends JDialog implements WebcamListener, WindowListener,
        UncaughtExceptionHandler, ItemListener, WebcamDiscoveryListener {

    private static final long serialVersionUID = 1L;

    private Webcam webcam = null;
    private WebcamPanel panel = null;
    private WebcamPicker picker = null;

    private JPanel tirarFotoPanel;
    private JButton tirarFotoButton;

    public WebCamCaptureViewer() {
        // Adiciona o listener para descoberta de webcams
        Webcam.addDiscoveryListener(this);

        // Configuração da janela
        setTitle("Tirar Foto");
        setIconImage(Main.favicon);
        setModal(true);
        setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
        setPreferredSize(new Dimension(400, 400));

        // Adiciona o listener para eventos de janela
        addWindowListener(this);

        // Inicializa o painel de captura de foto
        tirarFotoPanel = new JPanel();
        tirarFotoPanel.setLayout(new BoxLayout(tirarFotoPanel, BoxLayout.Y_AXIS));
        tirarFotoPanel.setBorder(new EmptyBorder(10, 10, 10, 10));

        // Cria o picker para selecionar a webcam
        picker = new WebcamPicker();
        picker.addItemListener(this);

        // Obtém a webcam selecionada
        webcam = picker.getSelectedWebcam();

        // Se nenhuma webcam for detectada
        if (webcam == null) {
            System.out.println("No webcams found...");
            return;
        }

        // Fecha qualquer webcam aberta anteriormente
        if (panel != null) {
            panel.stop();
        }

        if (webcam.isOpen()) {
            webcam.close();
        }

        // Configura a webcam
        webcam.setViewSize(WebcamResolution.VGA.getSize());
        webcam.addWebcamListener(WebCamCaptureViewer.this);

        // Cria o painel para exibição da webcam
        panel = new WebcamPanel(webcam, false);
        panel.setFPSDisplayed(true);
        panel.setDisplayDebugInfo(true);
        panel.setImageSizeDisplayed(true);

        // Cria o botão para capturar a foto
        tirarFotoButton = new JButton("Capturar");
        tirarFotoButton.setPreferredSize(new Dimension(160, 40));
        tirarFotoButton.setAlignmentX(Component.CENTER_ALIGNMENT);
        
        // Adiciona o botão de captura e o painel ao layout
        tirarFotoPanel.add(picker);
        tirarFotoPanel.add(Box.createVerticalStrut(10));
        tirarFotoPanel.add(panel);
        tirarFotoPanel.add(Box.createVerticalStrut(10));
        tirarFotoPanel.add(tirarFotoButton);

        // Adiciona o painel à janela
        getContentPane().add(tirarFotoPanel);
        pack();
        setLocationRelativeTo(null);

        // Adiciona ActionListener para capturar a foto ao clicar no botão
        tirarFotoButton.addActionListener(e -> capturePhoto());
    }

    // Método para iniciar o painel da webcam em uma thread separada
    public void start() {
        // Cria um thread para iniciar o painel da webcam
        Thread t = new Thread() {
            @Override
            public void run() {
                try {
                    // Espera um pouco antes de começar
                    Thread.sleep(500);
                    panel.start(); // Inicia o painel da webcam
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

    // Método de captura de foto
    private void capturePhoto() {
        if (webcam != null) {
            // Captura a imagem da webcam
            BufferedImage image = webcam.getImage();
            // Aqui você pode salvar ou processar a imagem
            System.out.println("Foto capturada!");
        }
    }

    @Override
    public void webcamOpen(WebcamEvent webcamEvent) {
        System.out.println("Webcam aberta");
    }

    @Override
    public void webcamClosed(WebcamEvent webcamEvent) {
        System.out.println("Webcam fechada");
    }

    @Override
    public void webcamDisposed(WebcamEvent webcamEvent) {
        System.out.println("Webcam descartada");
    }

    @Override
    public void webcamImageObtained(WebcamEvent webcamEvent) {
        // Não é necessário implementar
    }

    @Override
    public void windowActivated(WindowEvent event) {}

    @Override
    public void windowClosed(WindowEvent event) {
        System.out.println("Janela fechada");
        panel.stop();
    }

    @Override
    public void windowClosing(WindowEvent event) {
        System.out.println("Janela fechando");
        panel.stop();
    }

    @Override
    public void windowOpened(WindowEvent event) {}

    @Override
    public void windowDeactivated(WindowEvent event) {}

    @Override
    public void windowDeiconified(WindowEvent event) {
        System.out.println("Visualização da webcam retomada");
        panel.resume();
    }

    @Override
    public void windowIconified(WindowEvent event) {
        System.out.println("Visualização da webcam pausada");
        panel.pause();
    }

    @Override
    public void uncaughtException(Thread thread, Throwable e) {
        System.err.println(String.format("Exceção na thread %s", thread.getName()));
        e.printStackTrace();
    }

    @Override
    public void itemStateChanged(ItemEvent event) {
        if (event.getItem() == webcam) {
            return; // Não faz nada se for a webcam já selecionada
        }

        if (webcam != null) {
            // Para a webcam e remove o painel
            panel.stop();
            tirarFotoPanel.remove(panel);
            webcam.removeWebcamListener(this);
            webcam.close();
        }

        // Atualiza a webcam selecionada
        webcam = (Webcam) event.getItem();
        webcam.setViewSize(WebcamResolution.VGA.getSize());
        webcam.addWebcamListener(this);

        // Cria um novo painel para a nova webcam
        panel = new WebcamPanel(webcam, false);
        panel.setFPSDisplayed(true);
        tirarFotoPanel.add(panel, 1);
        pack();

        // Aguarda um pouco antes de iniciar a nova webcam
        try {
            Thread.sleep(3000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        start(); // Inicia a nova webcam
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
