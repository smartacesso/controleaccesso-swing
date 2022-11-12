package com.protreino.services.screens;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.UnsupportedEncodingException;
import java.security.NoSuchAlgorithmException;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JProgressBar;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.SwingWorker;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;

import com.protreino.services.entity.PlanoEntity;
import com.protreino.services.entity.UserEntity;
import com.protreino.services.main.Main;
import com.protreino.services.utils.EncryptionUtils;
import com.protreino.services.utils.HibernateUtil;
import com.protreino.services.utils.HttpConnection;
import com.protreino.services.utils.Utils;

@SuppressWarnings("serial")
public class AutenticationDialog extends JDialog {

	public final String OK = "OK";
	public final String CANCEL = "CANCEL";
	
	private JPasswordField passwordField;
	private JLabel invalidCredentialsLabel;
	private JTextField loginField;
	private JButton okButton;
	private JButton cancelButton;
	private String mensagemProgressDialog;
	private Boolean retornoAuthentication;
	private UserEntity usuario;
	
	private String option = CANCEL;
	
	public AutenticationDialog(Frame owner, boolean useLogin, boolean usePassword){
		this(owner, useLogin, usePassword, false);
	}
	
	public AutenticationDialog(Frame owner, boolean useLogin, boolean usePassword, boolean loginInterno){
		super(owner, "Autenticação", true);
		
		setPreferredSize(new Dimension(300,260));
		setMinimumSize(getPreferredSize());
		setIconImage(Main.favicon);
		
		JLabel loginLabel = new JLabel("Login");
		loginLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		
		loginField = new JTextField(15);
		loginField.setHorizontalAlignment(SwingConstants.CENTER);
		loginField.setAlignmentX(Component.CENTER_ALIGNMENT);
		loginField.setMaximumSize(loginField.getPreferredSize());
		
		JLabel passwordLabel = new JLabel("Senha");
		passwordLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		
		passwordField = new JPasswordField(15);
		passwordField.setEchoChar('*');
		passwordField.setHorizontalAlignment(SwingConstants.CENTER);
		passwordField.setAlignmentX(Component.CENTER_ALIGNMENT);
		passwordField.setMaximumSize(passwordField.getPreferredSize());
		
		if (Main.desenvolvimento) {
			loginField.setText("admin");
			passwordField.setText("123456");
		}
		
		invalidCredentialsLabel = new JLabel(" ");
		invalidCredentialsLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		invalidCredentialsLabel.setForeground(Color.RED);
		
		okButton = new JButton("OK");
		okButton.setPreferredSize(new Dimension(100, 30));
		
		Action okAction = new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				if (Utils.isNullOrEmpty(loginField.getText())) {
					invalidCredentialsLabel.setText("Login inválido!");
					return;
				}
				if (passwordField.getPassword().length == 0) {
					invalidCredentialsLabel.setText("Senha inválida!");
					return;
				}
				
				//IDENTIFICAR QUEM ESTÁ CHAMANDO A TELA
				if(loginInterno) {
					PlanoEntity planoComMaiorVencimento = buscaPlanoAtivoComMaiorDataVencimento();
					
					if(planoComMaiorVencimento == null
							|| (planoComMaiorVencimento.getFim() != null
									&& planoComMaiorVencimento.getFim().before(new Date()))) {
						
						invalidCredentialsLabel.setText("Login não permitido. Verifique com o administrador do sistema.");
						return;
					}
					
					try {
						String senhaCripto = String.valueOf(passwordField.getPassword());
						senhaCripto = EncryptionUtils.encrypt(senhaCripto);
						usuario = HibernateUtil.buscaUsuarioPeloLogin(loginField.getText(), senhaCripto);
						
						if(usuario != null) {
							System.out.println("Usuario logado: " + usuario.getName());
							invalidCredentialsLabel.setText(" ");
							option = OK;
							setVisible(false);
						} else {
							invalidCredentialsLabel.setText("Dados invalidos!");
							return;
						}
					} catch (Exception e) {
						e.printStackTrace();
						invalidCredentialsLabel.setText("Falha ao criptografar senha!");
					}
					
				} else {
					invalidCredentialsLabel.setText(" ");
					option = OK;
					setVisible(false);
				}
			}

			@SuppressWarnings("unchecked")
			private PlanoEntity buscaPlanoAtivoComMaiorDataVencimento() {
				HashMap<String, Object> args = new HashMap<>();
				args.put("ID_CLIENTE", Main.loggedUser.getIdClient());
				
				List<PlanoEntity> planos = (List<PlanoEntity>) HibernateUtil
									.getResultListWithParams(PlanoEntity.class, "PlanoEntity.findMaiorDataVencimentoAndAtivo", args);
				
				if(planos != null)
					return planos.stream().findFirst().orElse(null);
				
				return null;
			}
		};
		
		okButton.addActionListener(okAction);
		loginField.addActionListener(okAction);
		passwordField.addActionListener(okAction);
		
		cancelButton = new JButton("Cancelar");
		cancelButton.setPreferredSize(new Dimension(100, 30));
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				option = CANCEL;
				setVisible(false);
			}
		});
		
		JPanel buttonsPanel = new JPanel();
		buttonsPanel.setLayout(new FlowLayout(FlowLayout.CENTER));
		buttonsPanel.add(okButton);
		buttonsPanel.add(cancelButton);
		
		JPanel panel = new JPanel();
		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
		panel.setBorder(new EmptyBorder(10, 10, 10, 10));
		
		if (useLogin) {
			panel.add(loginLabel);
			panel.add(Box.createVerticalStrut(10));
			panel.add(loginField);
			panel.add(Box.createVerticalStrut(10));
		}
		if (usePassword) {
			panel.add(passwordLabel);
			panel.add(Box.createVerticalStrut(10));
			panel.add(passwordField);
			panel.add(Box.createVerticalStrut(10));
		}
		panel.add(Box.createVerticalStrut(5));
		panel.add(invalidCredentialsLabel);
		panel.add(Box.createVerticalStrut(10));
		panel.add(buttonsPanel);
		
		setContentPane(panel);
		
		setLocationRelativeTo(null);
		pack();
		
	}
	
	public AutenticationDialog(Frame owner, String mensagemDialogSenha, String mensagemProgressDialog){
		super(owner, "Autenticação", true);
		this.mensagemProgressDialog = mensagemProgressDialog;
		
		setIconImage(Main.favicon);
		
		String[] mensagens = mensagemDialogSenha.split("\n");
		
		passwordField = new JPasswordField(20);
		passwordField.setEchoChar('*');
		passwordField.setHorizontalAlignment(SwingConstants.CENTER);
		passwordField.setAlignmentX(Component.CENTER_ALIGNMENT);
		passwordField.setMaximumSize(passwordField.getPreferredSize());
		
		if (Main.desenvolvimento)
			passwordField.setText("123456");
		
		invalidCredentialsLabel = new JLabel(" ");
		invalidCredentialsLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		invalidCredentialsLabel.setForeground(Color.RED);
		
		okButton = new JButton("OK");
		okButton.setPreferredSize(new Dimension(100, 30));
		
		Action okAction = new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (passwordField.getPassword().length == 0) {
					invalidCredentialsLabel.setText("Senha inválida!");
					return;
				}
				invalidCredentialsLabel.setText(" ");
				option = OK;
				setVisible(false);
			}
		};
		
		okButton.addActionListener(okAction);
		passwordField.addActionListener(okAction);
		
		cancelButton = new JButton("Cancelar");
		cancelButton.setPreferredSize(new Dimension(100, 30));
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				option = CANCEL;
				setVisible(false);
			}
		});
		
		JPanel buttonsPanel = new JPanel();
		buttonsPanel.setLayout(new FlowLayout(FlowLayout.CENTER));
		buttonsPanel.add(okButton);
		buttonsPanel.add(cancelButton);
		
		JPanel panel = new JPanel();
		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
		
		Font font = new JLabel().getFont();
		Font biggerFont = new Font(font.getFontName(), font.getStyle(), font.getSize()+1);
		
		for (String mensagem : mensagens) {
			JLabel label = new JLabel(mensagem);
			label.setAlignmentX(Component.CENTER_ALIGNMENT);
			label.setFont(biggerFont);
			panel.add(label);
		}
		panel.add(Box.createVerticalStrut(15));
		panel.add(passwordField);
		panel.add(Box.createVerticalStrut(5));
		panel.add(invalidCredentialsLabel);
		
		JPanel mainPanel = new JPanel(new BorderLayout(10, 10));
		mainPanel.setBorder(new EmptyBorder(20, 20, 10, 20));
		mainPanel.add(panel, BorderLayout.CENTER);
		mainPanel.add(buttonsPanel, BorderLayout.PAGE_END);
		
		setContentPane(mainPanel);
		
		pack();
		setLocationRelativeTo(null);
	}
	
	public Boolean authenticate() throws Exception {
		retornoAuthentication = false;
		final JDialog loadingDialog = new JDialog();
		try {
			setVisible(true);
			if ("CANCEL".equals(option)) {
				return null;
			}
			
			final String senha = new String(getPassword());
			if (senha.isEmpty())
				return null;
			
			JProgressBar progressBar = new JProgressBar();
			progressBar.setIndeterminate(true);
			JPanel panelLoading = new JPanel(new BorderLayout(0, 20));
		    panelLoading.setBorder(new CompoundBorder(new LineBorder(Main.firstColor), new EmptyBorder(20, 20, 20, 20)));
		    panelLoading.add(new JLabel(mensagemProgressDialog), BorderLayout.PAGE_START);
		    panelLoading.add(progressBar, BorderLayout.CENTER);
		    loadingDialog.setUndecorated(true);
		    loadingDialog.getContentPane().add(panelLoading);
		    loadingDialog.pack();
		    loadingDialog.setLocationRelativeTo(null);
		    loadingDialog.setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
		    loadingDialog.setModal(true);
			
			SwingWorker<Boolean, Void> worker = new SwingWorker<Boolean, Void>() {
			    @Override
			    public Boolean doInBackground() {
			    	try {
//			    		HttpConnection con = new HttpConnection(Main.urlApplication + "/restful-services/login/do?loginName=" 
//			    				+ Main.loggedUser.getLoginName() + "&passwd=" + senha + "&unidadeName="+Main.loggedUser.getUnitName());
//			    		if (con.getResponseCode() == 200)  // Status.OK 
//							return true;
			    		UserEntity user = HibernateUtil.buscaUsuarioPeloLogin(Main.loggedUser.getLoginName(), EncryptionUtils.encrypt(senha));
			    		
			    		if(user != null)
			    			return true;
			    	
			    	} catch (Exception e){
			    		e.printStackTrace();
			    	}
			    	
			    	return false;
			    }
			};
			worker.addPropertyChangeListener(new PropertyChangeListener() {
				@Override
				public void propertyChange(PropertyChangeEvent evt) {
					if (evt.getPropertyName().equals("state") 
							&& evt.getNewValue() == SwingWorker.StateValue.DONE) {
						try {
							retornoAuthentication = worker.get();
						} 
						catch (InterruptedException | ExecutionException e) {
							e.printStackTrace();
						}
						loadingDialog.dispose();
					}
				}
			});
		    worker.execute();
		    loadingDialog.setVisible(true);
		    return retornoAuthentication;
		}
		catch (Exception e){
			loadingDialog.dispose();
			throw e;
		}
	}
	
	
	public String getOption(){
		return option;
	}
	
	public String getLogin(){
		return loginField != null ? loginField.getText() : null;
	}
	
	public char[] getPassword(){
		return passwordField != null ? passwordField.getPassword() : null;
	}

	public UserEntity getUsuario() {
		return usuario;
	}
	
}
