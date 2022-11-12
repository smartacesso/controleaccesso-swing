package com.protreino.services.screens;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.net.SocketException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;
import javax.swing.border.EmptyBorder;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonNull;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.protreino.services.constants.Configurations;
import com.protreino.services.devices.Device;
import com.protreino.services.entity.UserEntity;
import com.protreino.services.enumeration.NotificationType;
import com.protreino.services.main.Main;
import com.protreino.services.utils.HibernateUtil;
import com.protreino.services.utils.HttpConnection;
import com.protreino.services.utils.Utils;

@SuppressWarnings("serial")
public class LoginDialog extends JDialog {
	
	private ImageIcon logoIcon;
	private JTextField unidadeTextField;
	private JTextField usernameTextField;
	private JTextField passwordTextField;
	private JLabel loginMessageLabel;
	private JButton loginButton;
	private JButton cancelarButton;
	
	public LoginDialog(){
		super(Main.mainScreen, "Autenticação", true);
		
		loadImages();
		
		setIconImage(Main.favicon);
		setResizable(false);
		setLayout(new BorderLayout());
		setMinimumSize(new Dimension(400, 300));
		
		addWindowListener(new WindowAdapter() {
		    @Override
		    public void windowClosing(WindowEvent e) {
		    	dispose();
		    	Main.mainScreen.setVisible(false);
		    }
		});
		
		JLabel logoLabel = new JLabel(logoIcon);
		logoLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		
		JLabel unidadeLabel = new JLabel("Nome da unidade");
		unidadeLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		
		unidadeTextField = new JTextField();
		unidadeTextField.setColumns(20);
		unidadeTextField.setMaximumSize(unidadeTextField.getPreferredSize());
		unidadeTextField.setHorizontalAlignment(JTextField.CENTER);
		
		JLabel usernameLabel = new JLabel("Usuário");
		usernameLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		
		usernameTextField = new JTextField();
		usernameTextField.setColumns(20);
		usernameTextField.setMaximumSize(usernameTextField.getPreferredSize());
		usernameTextField.setHorizontalAlignment(JTextField.CENTER);
		
		JLabel passwordLabel = new JLabel("Senha");
		passwordLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		
		passwordTextField = new JPasswordField();
		passwordTextField.setColumns(20);
		passwordTextField.setMaximumSize(passwordTextField.getPreferredSize());
		passwordTextField.setHorizontalAlignment(JTextField.CENTER);
		
		if (Main.desenvolvimento) {
			unidadeTextField.setText("desenvolvimento");
			usernameTextField.setText("admin");
			passwordTextField.setText("123456");
		}
		
		cancelarButton = new JButton("Cancelar");
		cancelarButton.setAlignmentX(Component.CENTER_ALIGNMENT);
		cancelarButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				dispose();
				Main.mainScreen.setVisible(false);
			}
		});
		
		loginButton = new JButton("Logar");
		loginButton.setAlignmentX(Component.CENTER_ALIGNMENT);
		
		Action loginAction = new AbstractAction() {
			
		    @Override
			public void actionPerformed(ActionEvent e) {
				SwingUtilities.invokeLater(new Runnable() {	public void run() {
					loginMessageLabel.setText("Logando...");
					loginMessageLabel.setForeground(Main.firstColor);
					loginButton.setEnabled(false);
					usernameTextField.setEnabled(false);
					passwordTextField.setEnabled(false);
					unidadeTextField.setEnabled(false);
				}});
				SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
				    @Override
				    public Void doInBackground() {
				    	setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
				    	try {
				    		HttpConnection con = new HttpConnection(Main.urlApplication + "/restful-services/login/do?loginName=" 
				    				+ usernameTextField.getText() + "&passwd=" + passwordTextField.getText()
				    				+ "&unidadeName=" + unidadeTextField.getText());
							if (con.getResponseCode() == 200) { // Status.OK 
								JsonObject loginResponse = con.getResponseJsonObject();
								JsonObject jsonObject = loginResponse.get("object").getAsJsonObject();
								if (jsonObject.get("status") != JsonNull.INSTANCE && jsonObject.get("status").getAsString().equals("ATIVO")) {
									JsonObject jsonAcademy = jsonObject.get("cliente").getAsJsonObject();
									if (jsonAcademy.get("id") != JsonNull.INSTANCE) {
										Calendar calendar = Calendar.getInstance();
										calendar.setTimeInMillis(jsonObject.get("dataCriacao") != JsonNull.INSTANCE ? Utils.convertDataJson(jsonObject.get("dataCriacao")).getTime() : null);
										Date creationDate = calendar.getTime();
										Integer qtdDigitos = null;
										String chaveIntegracaoComtele = null;
										Boolean expedidora = false;
										try {
											qtdDigitos = (jsonObject.get("qtdePadraoDigitosCartao") != JsonNull.INSTANCE ? jsonObject.get("qtdePadraoDigitosCartao").getAsInt() : null);
											chaveIntegracaoComtele = (jsonObject.get("chaveIntegracaoComtele") != JsonNull.INSTANCE ? jsonObject.get("chaveIntegracaoComtele").getAsString() : null);
										} catch (Exception e) {}
										try {
											expedidora = (jsonObject.get("expedidora") != JsonNull.INSTANCE ? jsonObject.get("expedidora").getAsBoolean() : false);
										}catch (Exception e) {}
										Main.loggedUser = new UserEntity(
												jsonObject.get("id") != JsonNull.INSTANCE ? jsonObject.get("id").getAsLong() : null,
												jsonObject.get("login") != JsonNull.INSTANCE ? jsonObject.get("login").getAsString() : null,
												jsonObject.get("senha") != JsonNull.INSTANCE ? jsonObject.get("senha").getAsString() : null,
												jsonObject.get("status") != JsonNull.INSTANCE ? jsonObject.get("status").getAsString() : null,
												creationDate,
												jsonAcademy.get("id") != JsonNull.INSTANCE ? jsonAcademy.get("id").getAsString() : null,
												jsonAcademy.get("nome") != JsonNull.INSTANCE ? jsonAcademy.get("nome").getAsString() : null,
												null,qtdDigitos, unidadeTextField.getText(),
												chaveIntegracaoComtele, expedidora);
										Main.loggedUser.setUseBiometry(true);
										Main.loggedUser = (UserEntity) HibernateUtil.saveUser(UserEntity.class, Main.loggedUser)[0];
										Main.lastSync = 0l;
										Main.devicesList = new ArrayList<Device>();
										Main.releaseTicketGateMenuItem.setEnabled(true);
										Main.updateAccessListMenuItem.setEnabled(true);
										
										loginMessageLabel.setText("Coletando configurações e pedestres...");
										//tras dados gerais antes de abrir
										Main.syncUsersAccessList();
										Main.syncAthleteAccessList();
										Main.syncLogAthleteAccess();
										
										// Buscando backup de dispostivos e prefências
										con = new HttpConnection(Main.urlApplication + "/restful-services/access/getBackupByUser?idUser=" + Main.loggedUser.getId().toString());
										if (con.getResponseCode() == 200) {
											JsonObject backupResponse = con.getResponseJsonObject();
											String backupPreferences = backupResponse.get("backupPreferences") != JsonNull.INSTANCE && !"null".equals(backupResponse.get("backupPreferences").getAsString()) ? 
													backupResponse.get("backupPreferences").getAsString() : null;
											String backupDevices = backupResponse.get("backupDevices") != JsonNull.INSTANCE && !"null".equals(backupResponse.get("backupDevices").getAsString()) ? 
													backupResponse.get("backupDevices").getAsString() : null;
											Main.loggedUser.setBackupPreferences(backupPreferences);
											Main.loggedUser.setBackupDevices(backupDevices);
											Main.loggedUser.setBackupChanged(false);
											Main.loggedUser = (UserEntity) HibernateUtil.saveUser(UserEntity.class, Main.loggedUser)[0];
											Utils.importPreferences();
											Utils.importDevices();
										}
										
										while(Boolean.TRUE.equals(Main.updatingUsersAccessList)
												|| Boolean.TRUE.equals(Main.updatingAthleteAccessList)
													|| Boolean.TRUE.equals(Main.updatingLogAccessList)) {
											Utils.sleep(500);
										}
										
										Main.mainScreen.buildUI();
										Utils.createNotification("Usuário logado com sucesso!", NotificationType.GOOD);
										dispose();
									
									} else {
										setMessageErrorLogin("Usuário não autorizado!");
										Main.loggedUser = null;
									}
								
								} else {
									setMessageErrorLogin("Usuário inativo!");
									Main.loggedUser = null;
								}
							
							} else if (con.getResponseCode() == 500) { // Status.INTERNAL_SERVER_ERROR
								JsonParser parser = new JsonParser();
								JsonObject loginResponse = parser.parse(con.getErrorString()).getAsJsonObject();
								if(loginResponse.get("message").toString().equals("\"msgs.account.usuario.senha.invalida\"")){
									setMessageErrorLogin("Senha incorreta!");
								
								} else{
									setMessageErrorLogin("Usuário não encontrado!");
								}
							} else {
								setMessageErrorLogin("Sem conexão!");
							}
					    
				    	} catch (SocketException se){
				    		setMessageErrorLogin("Sem conexão.");
				    		Main.loggedUser = null;
				    	
				    	} catch (Throwable e) {
				    		e.printStackTrace();
				    		setMessageErrorLogin("Falha durante o login. " + e.getMessage());
				    		Main.loggedUser = null;
						
				    	} finally {
				    		setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
				    	}
						
				    	return null;
					}
				};
				worker.execute();
			}
		};
		
		loginButton.addActionListener(loginAction);
		unidadeTextField.addActionListener(loginAction);
		usernameTextField.addActionListener(loginAction);
		passwordTextField.addActionListener(loginAction);
		
		JPanel buttonsPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 5, 0));
		buttonsPanel.add(loginButton);
		buttonsPanel.add(cancelarButton);
		
		loginMessageLabel = new JLabel(" ");
		loginMessageLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		Font font = loginMessageLabel.getFont();
		Font boldFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
		loginMessageLabel.setFont(boldFont);
		
		JPanel mainPanel = new JPanel();
		mainPanel.setBorder(new EmptyBorder(10, 10, 10, 10));
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
		mainPanel.add(Box.createVerticalGlue());
		mainPanel.add(logoLabel);
		mainPanel.add(Box.createRigidArea(new Dimension(10,15)));
		mainPanel.add(unidadeLabel);
		mainPanel.add(unidadeTextField);
		mainPanel.add(Box.createRigidArea(new Dimension(10,10)));
		mainPanel.add(usernameLabel);
		mainPanel.add(usernameTextField);
		mainPanel.add(Box.createRigidArea(new Dimension(10,10)));
		mainPanel.add(passwordLabel);
		mainPanel.add(passwordTextField);
		mainPanel.add(Box.createRigidArea(new Dimension(10,20)));
		mainPanel.add(buttonsPanel);
		mainPanel.add(Box.createRigidArea(new Dimension(10,20)));
		mainPanel.add(loginMessageLabel);
		mainPanel.add(Box.createVerticalGlue());
		
		getContentPane().add(mainPanel, BorderLayout.CENTER);
		pack();
		setLocationRelativeTo(null);
		setVisible(true);
		
	}
	
	
	private void setMessageErrorLogin(String message){
		loginMessageLabel.setText(message);
		loginMessageLabel.setForeground(Color.RED);
		loginButton.setEnabled(true);
		usernameTextField.setEnabled(true);
		passwordTextField.setEnabled(true);
		unidadeTextField.setEnabled(true);
		Main.loggedUser = null;
	}
	
	
	private void loadImages() {
		try {
			Toolkit toolkit = Toolkit.getDefaultToolkit();
			logoIcon = new ImageIcon(toolkit.getImage(Main.class.
					getResource(Configurations.IMAGE_FOLDER + Main.customImageFolder + "logo_grd003.png")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}
		
}
