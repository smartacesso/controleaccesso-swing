package com.protreino.services.screens;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.URISyntaxException;
import java.net.URL;
import java.text.DecimalFormat;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.SwingWorker;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;

import com.google.gson.Gson;
import com.protreino.services.constants.Configurations;
import com.protreino.services.enumeration.NotificationType;
import com.protreino.services.main.Main;
import com.protreino.services.to.VersionInfoTO;
import com.protreino.services.utils.Download;
import com.protreino.services.utils.HttpConnection;
import com.protreino.services.utils.Utils;

@SuppressWarnings("serial")
public class AboutScreen extends JFrame {
	
	private AboutScreen instance;
	private Font boldFont;
	private String fileLocation;
	private String fileName;
	
	public AboutScreen(boolean abreTela) {
		instance = this;
		
		Font font = new JLabel().getFont();
		boldFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
		
		setTitle("Sobre - " + Main.nomeAplicacao);
		setResizable(false);
		setIconImage(Main.favicon);
		setSize(400, 200);
		setMinimumSize(getSize());
		
		JPanel panel = new JPanel();
		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
		panel.setBorder(new EmptyBorder(10, 10, 10, 10));
		
		JLabel aboutLabel = new JLabel(Main.nomeAplicacao + " - Aplicativo de controle de acesso");
		aboutLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		aboutLabel.setFont(boldFont);
		aboutLabel.setForeground(Main.firstColor);
		
		JLabel versaoLabel = new JLabel("Versão " + Configurations.VERSION);
		versaoLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		versaoLabel.setFont(boldFont);
		versaoLabel.setForeground(Main.firstColor);
		
		JButton procurarAtualizacaoButton = new JButton("Procurar atualizações");
		procurarAtualizacaoButton.setAlignmentX(Component.CENTER_ALIGNMENT);
		procurarAtualizacaoButton.setPreferredSize(new Dimension(150, 30));
		
		JButton okButton = new JButton("Ok");
		okButton.setAlignmentX(Component.CENTER_ALIGNMENT);
		okButton.setPreferredSize(new Dimension(60, 30));
		
		panel.add(aboutLabel);
		panel.add(Box.createVerticalStrut(10));
		panel.add(versaoLabel);
		panel.add(Box.createVerticalStrut(15));
		panel.add(procurarAtualizacaoButton);
		panel.add(Box.createVerticalStrut(30));
		panel.add(okButton);
		panel.add(Box.createVerticalGlue());
		
		okButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				dispose();
			}
		});
		
		procurarAtualizacaoButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					procurarAtualizacoes();
				} 
				catch (Exception e2) {
					e2.printStackTrace();
				}
			}
		});
		
		getContentPane().add(panel);
		pack();
		setLocationRelativeTo(null);
		okButton.requestFocus();
		
		if(abreTela) {
			setVisible(true);
		}
	
	}
	
	
	public void procurarAtualizacoes(){
		try {
			System.out.println("Procurando atualizações...");
			setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
			
			HttpConnection con = new HttpConnection(Main.urlApplication + "/restful-services/access/getLastVersion");
			Integer responseCode = con.getResponseCode();
			
			if (responseCode != 200) {
				JOptionPane.showMessageDialog(null, "não foi possível procurar atualizações. Verifique sua conexão e tente novamente.",
	            		"Atualização " + Main.nomeAplicacao, JOptionPane.PLAIN_MESSAGE);
				return;
			}
			
			BufferedReader bufferedReader = con.getResponseReader();
			Gson gson = new Gson();
			VersionInfoTO versionInfo = gson.fromJson(bufferedReader, VersionInfoTO.class);
			if (Double.valueOf(Configurations.VERSION).doubleValue() == versionInfo.getVersion().doubleValue()) {
				JOptionPane.showMessageDialog(null, "Você já possui a última Versão instalada!",
	            		"Atualização " + Main.nomeAplicacao, JOptionPane.PLAIN_MESSAGE); 
			
			} else if (Double.valueOf(Configurations.VERSION).doubleValue() < versionInfo.getVersion().doubleValue()) {
				
				JPanel panel = new JPanel();
				panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
				
				JLabel label1= new JLabel("Há uma nova Versão disponível!");
				label1.setAlignmentX(Component.LEFT_ALIGNMENT);
				
				JLabel novaVersaoLabel = new JLabel("Versão " + versionInfo.getVersion().toString());
				novaVersaoLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
				novaVersaoLabel.setFont(boldFont);
				
				JLabel descricaoLabel = new JLabel("Detalhes da Versão: ");
				descricaoLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
				
				panel.add(label1);
				panel.add(Box.createVerticalStrut(5));
				panel.add(novaVersaoLabel);
				panel.add(Box.createVerticalStrut(10));
				panel.add(descricaoLabel);
				panel.add(Box.createVerticalStrut(5));
				
				String[] partes = versionInfo.getDescription().split(";");
				
				for (String parte : partes){
					JLabel label = new JLabel(parte);
					label.setAlignmentX(Component.LEFT_ALIGNMENT);
					label.setFont(boldFont);
					panel.add(label);
				}
				
				JLabel confirmacaoLabel = new JLabel("Deseja instalá-la?");
				confirmacaoLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
				
				panel.add(Box.createVerticalStrut(20));
				panel.add(confirmacaoLabel);
				panel.add(Box.createVerticalStrut(10));
				
				int option = JOptionPane.showConfirmDialog(null, panel, "Nova Versão disponível", 
						JOptionPane.YES_NO_OPTION, JOptionPane.PLAIN_MESSAGE); 
				if (option == 0){
					atualizar(versionInfo);
				}
			}
				
		} catch (Exception e){
			e.printStackTrace();
			JOptionPane.showMessageDialog(null, "não foi possível procurar atualizações. Verifique sua conexão e tente novamente.",
            		"Atualização " + Main.nomeAplicacao, JOptionPane.PLAIN_MESSAGE); 
		} finally {
			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
	}
	
	
	private void atualizar(VersionInfoTO versionInfo) {
		SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
		    @Override
		    public Void doInBackground() {
		    	JDialog downloadDialog = new JDialog(instance);
		    	try {
		    		
		    		fileLocation = Utils.getAppDataFolder() + "/new_version/";
		    		if (Boolean.TRUE.equals(versionInfo.getFullInstaller()))
		    			fileName = "Instalador_" + Main.nomeAplicacao + "_Controle_de_Acesso.exe";
		    		else
		    			fileName = Main.nomeAplicacao + " Controle de Acesso.jar";
		    		
			    	URL url = new URL(versionInfo.getDownloadUrl());
					Download download = new Download(url, fileLocation, fileName);
					
					downloadDialog.setUndecorated(true);
					downloadDialog.setPreferredSize(new Dimension(300, 180));
					downloadDialog.setResizable(false);
					
					JLabel label = new JLabel("Baixando Versão " + versionInfo.getVersion().toString());
					label.setAlignmentX(Component.CENTER_ALIGNMENT);
					
					JProgressBar progressBar = new JProgressBar(0, 100);
					progressBar.setAlignmentX(Component.CENTER_ALIGNMENT);
					
					JLabel progressLabel = new JLabel("Progresso: 0 %");
					progressLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
					
					JLabel downloadedLabel = new JLabel("- / -  MB");
					downloadedLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
					
					JButton cancelarButton = new JButton("Cancelar");
					cancelarButton.setAlignmentX(Component.CENTER_ALIGNMENT);
					cancelarButton.addActionListener(new ActionListener() {
						@Override
						public void actionPerformed(ActionEvent e) {
							download.cancel();
						}
					});
					
					JPanel contentPane = new JPanel();
					contentPane.setLayout(new BoxLayout(contentPane, BoxLayout.Y_AXIS));
					contentPane.setBorder(new CompoundBorder(BorderFactory.createLineBorder(Main.firstColor, 1), BorderFactory.createEmptyBorder(20,10,10,10)));
					contentPane.add(label);
					contentPane.add(Box.createVerticalStrut(10));
					contentPane.add(progressLabel);
					contentPane.add(Box.createVerticalStrut(10));
					contentPane.add(progressBar);
					contentPane.add(Box.createVerticalStrut(10));
					contentPane.add(downloadedLabel);
					contentPane.add(Box.createVerticalStrut(20));
					contentPane.add(cancelarButton);
					
					downloadDialog.setContentPane(contentPane);
					downloadDialog.pack();
					downloadDialog.setLocationRelativeTo(null);
					downloadDialog.setVisible(true);
					downloadDialog.toFront();
					
					download.start();
			    	
					DecimalFormat decimalFormat = new DecimalFormat("#0.00");
			    	System.out.println("Baixando...");
					while (download.getStatus() == 0) { // DOWNLOADING
						Integer size = download.getSize();
						Integer downloaded = download.getDownloaded();
						Float progresso = download.getProgress();
						if (size != -1){
							 progressLabel.setText("Progresso:  " + progresso.intValue() + " %");
							 downloadedLabel.setText(decimalFormat.format(downloaded/1048576d) + "/" + decimalFormat.format(size/1048576d) + " MB");
							 progressBar.setValue(progresso.intValue());
						}
						Utils.sleep(100);
					}
					downloadDialog.dispose();
					
					if (download.getStatus() == 2){
						Object[] options = {"OK"};
						int option = -1;
						if (Boolean.TRUE.equals(versionInfo.getFullInstaller()))
							option = JOptionPane.showOptionDialog(null, "Download completo! Clique em OK para iniciar a instalação.", "Atualização pronta!",
									JOptionPane.PLAIN_MESSAGE, JOptionPane.PLAIN_MESSAGE, null, options, options[0]);
			    		else
							option = JOptionPane.showOptionDialog(null, "Download completo! Clique em OK para reiniciar o aplicativo.", "Atualização pronta!",
									JOptionPane.PLAIN_MESSAGE, JOptionPane.PLAIN_MESSAGE, null, options, options[0]);
						if (option == 0) {
							setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
							finalizarAtualizacao(versionInfo);
						}	
					}
					else if (download.getStatus() == 3){
						JOptionPane.showMessageDialog(null, "Download cancelado.",
			            		"Atualização " + Main.nomeAplicacao, JOptionPane.PLAIN_MESSAGE); 
					}
					else if (download.getStatus() == 4){
						JOptionPane.showMessageDialog(null, "Ocorreu um erro durante a atualização. Verifique sua conexão e tente novamente.",
			            		"Atualização " + Main.nomeAplicacao, JOptionPane.PLAIN_MESSAGE);  
					}
			    }
				catch (Exception e){
					e.printStackTrace();
					if (downloadDialog != null && downloadDialog.isVisible())
						downloadDialog.dispose();
					JOptionPane.showMessageDialog(null, "Ocorreu um erro durante a atualização. Verifique sua conexão e tente novamente.",
		            		"Atualização " + Main.nomeAplicacao, JOptionPane.PLAIN_MESSAGE); 
				}
		    	finally {
		    		setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		    	}
		    	return null;
		    }
		};
		worker.execute();
	}
	
	
	private void finalizarAtualizacao(VersionInfoTO versionInfo) throws URISyntaxException, IOException {
		String folderPath = new File(Main.class.getProtectionDomain().getCodeSource().getLocation().toURI()).getParent();
		Thread thread = new Thread(new Runnable() {
			@Override
			public void run() {
				try {
					String comando = Boolean.TRUE.equals(versionInfo.getFullInstaller()) ? "\"update.lnk\"" 
							: "java -jar \"UpdateWorker.jar\" \"" + Main.nomeAplicacao +"\"";
					Process process = Runtime.getRuntime().exec(new String[] { "cmd" });
					PrintWriter stdin = new PrintWriter(process.getOutputStream());
					stdin.println("cd \"" + folderPath + "\"");
					stdin.println(comando);
					stdin.close();
				}
				catch (Exception e) {
                    e.printStackTrace();
                    Utils.createNotification("Erro durante a atualização: " + e.getMessage(), NotificationType.BAD);
                }
			}
		});
		thread.start();
		Main.mainScreen.setVisible(false);
		Utils.sleep(4000);
		Main.exit(false);
	}
	
}
