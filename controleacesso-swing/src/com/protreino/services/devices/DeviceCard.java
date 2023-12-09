package com.protreino.services.devices;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingWorker;
import javax.swing.border.EmptyBorder;

import com.protreino.services.constants.Configurations;
import com.protreino.services.constants.Tipo;
import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.enumeration.BroadcastMessageType;
import com.protreino.services.enumeration.DeviceMode;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.enumeration.FieldType;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.enumeration.MessageType;
import com.protreino.services.enumeration.NotificationType;
import com.protreino.services.enumeration.VerificationResult;
import com.protreino.services.main.Main;
import com.protreino.services.screens.AttachedDevicesPanel;
import com.protreino.services.screens.AutenticationDialog;
import com.protreino.services.screens.EscolherSentidoLiberarAcessoDialog;
import com.protreino.services.screens.HikivisionAttachedDevicesPanel;
import com.protreino.services.screens.PedestrianScreen;
import com.protreino.services.screens.ProgressDialog;
import com.protreino.services.screens.RegisterUserDialog;
import com.protreino.services.screens.ReleaseReasonDialog;
import com.protreino.services.to.BroadcastMessageTO;
import com.protreino.services.to.ConfigurationGroupTO;
import com.protreino.services.to.ConfigurationTO;
import com.protreino.services.to.FieldTO;
import com.protreino.services.to.FieldTO.ComboBoxListener;
import com.protreino.services.utils.HibernateUtil;
import com.protreino.services.utils.PanelWithLabel;
import com.protreino.services.utils.SelectItem;
import com.protreino.services.utils.Utils;

@SuppressWarnings("serial")
public class DeviceCard extends JPanel {
	
	private Long serialId;
	
	private DeviceCard instance;
	private Device device;
	private JPanel namePanel;
	private JLabel nameLabel;
	private JLabel icon;
	private JLabel mensagemLabel;
	private JLabel defaultDeviceLabel;
	private ImageIcon catracaIcon;
	private ImageIcon connectedIcon;
	private ImageIcon disconnectedIcon;
	private Image configImage;
	private JLabel statusIcon;
	private JButton connectButton;
	private JButton disconnectButton;
	private JButton liberarAcessoButton;
	private JButton cadastrarFaceButton;
	private Boolean autoConnect = false; // indice que pode reconectar sem pedir senha
	private PanelWithLabel erroConfigurationLabel;
	private Boolean bloquearLiberarAcesso = false;
	private Boolean estadoDesejadoLiberarAcesso = false;
	private ImageIcon linkIcon;
	private JLabel linkIconLabel;
	private JLabel link;
	
	private LogPedestrianAccessEntity logAccess;
	
	public DeviceCard(Device device){
		this.instance = this;
		this.device = device;
		
		serialId = System.currentTimeMillis();
		Utils.sleep(1);
		
		loadImages();
		Font font = new JLabel().getFont();
		Font boldFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
		
		setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
		setBorder(BorderFactory.createLineBorder(Main.firstColor, 1, true));
		
		namePanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 5));
		namePanel.setBackground(Main.firstColor);
		nameLabel = new JLabel(device.getName());
		nameLabel.setFont(boldFont);
		nameLabel.setForeground(Main.secondColor);
		nameLabel.setToolTipText("Dois cliques para mudar o nome");
		namePanel.add(nameLabel);
		add(namePanel);
		
		nameLabel.addMouseListener(new java.awt.event.MouseAdapter() {
		    public void mouseEntered(java.awt.event.MouseEvent evt) {
		    	nameLabel.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
		    }
		    public void mouseExited(java.awt.event.MouseEvent evt) {
		    	nameLabel.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		    }
		});
		
		nameLabel.addMouseListener(new MouseAdapter(){
		    @Override
		    public void mouseClicked(MouseEvent event){
		    	if (event.getClickCount() == 2)
		    		changeName();
		    }
		});
		
		JPanel iconsPanel = new JPanel();
		iconsPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 5, 5));
		icon = new JLabel(catracaIcon);
		icon.setAlignmentY(Component.BOTTOM_ALIGNMENT);
		iconsPanel.add(icon);
		
		statusIcon = new JLabel(disconnectedIcon);
		statusIcon.setAlignmentY(Component.BOTTOM_ALIGNMENT);
		iconsPanel.add(statusIcon);
		add(iconsPanel);
		
		JPanel linkPanel = new JPanel();
		linkPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 0, 0));
		linkPanel.setPreferredSize(new Dimension(200, 16));
		linkPanel.setMinimumSize(linkPanel.getPreferredSize());
		linkIconLabel = new JLabel(" ");
		linkIconLabel.setAlignmentY(Component.BOTTOM_ALIGNMENT);
		linkPanel.add(linkIconLabel);
		link = new JLabel(" ");
		link.setAlignmentY(Component.BOTTOM_ALIGNMENT);
		linkPanel.add(link);
		add(linkPanel);
		add(Box.createVerticalStrut(2));
		
		JPanel mensagemPanel = new JPanel(); //new FlowLayout(FlowLayout.LEFT));
		mensagemPanel.setLayout(new BoxLayout(mensagemPanel, BoxLayout.X_AXIS));
		mensagemPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		mensagemPanel.setBackground(Main.firstColor);
		mensagemLabel = new JLabel(" ");
		mensagemLabel.setFont(boldFont);
		mensagemLabel.setForeground(Main.secondColor);
		mensagemPanel.add(mensagemLabel);
		defaultDeviceLabel = new JLabel("Padrão");
		defaultDeviceLabel.setFont(boldFont);
		defaultDeviceLabel.setForeground(Color.WHITE);
		if (!device.isDefaultDevice())
			defaultDeviceLabel.setVisible(false);
		mensagemPanel.add(Box.createHorizontalGlue());
		mensagemPanel.add(defaultDeviceLabel);
		add(mensagemPanel);
		
		JPanel actionPanel = new JPanel();
		actionPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 5, 5));
		Dimension buttonSize = new Dimension(120, 35);
		connectButton = new JButton("Conectar");
		connectButton.setPreferredSize(buttonSize);
		connectButton.addMouseListener(new java.awt.event.MouseAdapter() {
		    public void mouseEntered(java.awt.event.MouseEvent evt) {
		    	if (connectButton.isEnabled()) {
		    		connectButton.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
		    	}
		    }
		    public void mouseExited(java.awt.event.MouseEvent evt) {
		    	connectButton.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		    }
		});
		connectButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
					@Override
				    public Void doInBackground() {
				    	try {
				    		
				    		//Não faz nada caso esteja recuperando dados
				    		if(device.coletandoDadosOffLine)
				    			return null;
				    		
				    		device.setTentandoConectar(true);
				    		if (!autoConnect && (device.getManufacturer().usePassword() 
				    				|| device.getManufacturer().useLogin())) {
				    			AutenticationDialog autenticationDialog = new AutenticationDialog(Main.mainScreen, 
				    					device.getManufacturer().useLogin(), device.getManufacturer().usePassword());
								autenticationDialog.setVisible(true);
								String option = autenticationDialog.getOption();
								if ("OK".equals(option)) {
									device.setCredentials(autenticationDialog.getLogin(), new String(autenticationDialog.getPassword()));
				    			}
				    			else
				    				return null;
				    		}
				    		setMensagem(autoConnect ? "Reconectando..." : "Conectando...", MessageType.NORMAL);
				    		autoConnect = false;
				    		setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
				    		connectButton.setEnabled(false);
			    			device.setDesiredStatus(DeviceStatus.CONNECTED);
			    			device.connect();
			    			connectButton.setVisible(false);
			    			disconnectButton.setVisible(true);
			    			disconnectButton.setEnabled(true);
						}
						catch (Throwable t) {
							t.printStackTrace();
							device.setDesiredStatus(DeviceStatus.DISCONNECTED);
							setMensagem("Não foi possível conectar", MessageType.ERROR);
							Main.mainScreen.addEvento(device.getName() + ": " + t.getMessage());
							connectButton.setEnabled(true);
							connectButton.setVisible(true);
				    		disconnectButton.setVisible(false);
						}
						finally {
							setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
							revalidate();
							device.setTentandoConectar(false);
						}
				    	return null;
				    }
				};
				worker.execute();
			}
		});
		actionPanel.add(connectButton);
		
		disconnectButton = new JButton("Desconectar");
		disconnectButton.setVisible(false);
		disconnectButton.setPreferredSize(buttonSize);
		disconnectButton.addMouseListener(new java.awt.event.MouseAdapter() {
		    public void mouseEntered(java.awt.event.MouseEvent evt) {
		    	if (disconnectButton.isEnabled()) {
		    		disconnectButton.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
		    	}
		    }
		    public void mouseExited(java.awt.event.MouseEvent evt) {
		    	disconnectButton.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		    }
		});
		disconnectButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
					@Override
				    public Void doInBackground() {
				    	try {
				    		setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
				    		disconnectButton.setEnabled(false);
				    		device.setDesiredStatus(DeviceStatus.DISCONNECTED);
				    		setMensagem("Desconectando...", MessageType.NORMAL);
				    		device.disconnect();
				    		disconnectButton.setVisible(false);
			    			connectButton.setVisible(true);
			    			connectButton.setEnabled(true);
						
				    	} catch (Exception e) {
							e.printStackTrace();
							setMensagem("Não foi possível desconectar", MessageType.ERROR);
							Main.mainScreen.addEvento("Erro ao desconectar " + device.getName() + ": " + e.getMessage());
							disconnectButton.setEnabled(true);
							disconnectButton.setVisible(true);
				    		connectButton.setVisible(false);
						
						} finally {
							setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
							revalidate();
						}
				    	return null;
				    }
				};
				worker.execute();
			}
		});
		actionPanel.add(disconnectButton);
		
		liberarAcessoButton = new JButton("Liberar acesso");
		liberarAcessoButton.setPreferredSize(buttonSize);
		liberarAcessoButton.setEnabled(false);
		estadoDesejadoLiberarAcesso = false;
		liberarAcessoButton.addMouseListener(new java.awt.event.MouseAdapter() {
		    public void mouseEntered(java.awt.event.MouseEvent evt) {
		    	if (liberarAcessoButton.isEnabled()) {
		    		liberarAcessoButton.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
		    	}
		    }
		    public void mouseExited(java.awt.event.MouseEvent evt) {
		    	liberarAcessoButton.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		    }
		});
		liberarAcessoButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent event) {
				
				Boolean exigeSenha = Utils.getPreferenceAsBoolean("releaseAccessRequiresPassword");
				if (exigeSenha) {
					AutenticationDialog autenticationDialog = new AutenticationDialog(null, 
							"Digite a senha do usuário logado \npara liberar o acesso", 
							"Aguarde, verificando senha...");
					Boolean retornoAuthentication = null;
					try {
						retornoAuthentication = autenticationDialog.authenticate();
					}
					catch (Exception ex){
						ex.printStackTrace();
						JOptionPane.showMessageDialog(null, "Ocorreu uma falha ao validar a senha.", 
								"Erro na validação", JOptionPane.PLAIN_MESSAGE);
						return;
					}
					
					if (retornoAuthentication == null)
						return;
					if (!retornoAuthentication) {
						JOptionPane.showMessageDialog(null, "Não foi possível validar a senha, ou senha inválida",
								"Erro na validação", JOptionPane.PLAIN_MESSAGE);
						return;
					}
				}
				
				String motivos = Utils.getPreferenceWithNull("releaseAccessReason");
				String motivoLiberacao = null;
				if (!Utils.isNullOrEmpty(motivos)) {
					ReleaseReasonDialog releaseReasonDialog = new ReleaseReasonDialog(Main.mainScreen, motivos);
					Main.releaseReasonDialog = releaseReasonDialog;
					releaseReasonDialog.setVisible(true);
					Main.releaseReasonDialog = null;
					if ("CANCEL".equals(releaseReasonDialog.getOption()))
						return;
					motivoLiberacao = releaseReasonDialog.getReason();
					if (Utils.isNullOrEmpty(motivoLiberacao)) {
						Utils.createNotification("É necessário informar um motivo.", NotificationType.BAD);
						return;
					}
				}
				
				logAccess = new LogPedestrianAccessEntity(Main.loggedUser.getId(), null, 
						"LIBERADO PELO SISTEMA", device.getLocation(), motivoLiberacao);
				
				if(device.getConfigurationValueAsBoolean("Bloquear Saída")) {
					new EscolherSentidoLiberarAcessoDialog(device, motivoLiberacao, null);
				}
				else
					liberarCatraca();
			}

		});
		liberarAcessoButton.setVisible(!device.getConfigurationValueAsBoolean("Somente para cadastros"));
		actionPanel.add(liberarAcessoButton);
		
		cadastrarFaceButton = new JButton("Cadastrar face");
		cadastrarFaceButton.setPreferredSize(buttonSize);
		cadastrarFaceButton.setEnabled(false);
		cadastrarFaceButton.setVisible(device.getConfigurationValueAsBoolean("Somente para cadastros"));
		cadastrarFaceButton.addMouseListener(new java.awt.event.MouseAdapter() {
			public void mouseEntered(java.awt.event.MouseEvent evt) {
				if (cadastrarFaceButton.isEnabled()) {
					cadastrarFaceButton.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
				}
			}
			public void mouseExited(java.awt.event.MouseEvent evt) {
				cadastrarFaceButton.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
			}
		});
		
		JMenuItem registerUserMenuItem = new JMenuItem("Cadastro de biometrias");
		cadastrarFaceButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent event) {
				registerUserMenuItem.doClick();
			}
		});
		actionPanel.add(cadastrarFaceButton);
		
		add(actionPanel);
		
		JMenuItem defaultDeviceMenuItem = new JMenuItem("Definir como padrão");
		JCheckBoxMenuItem mirrorMenuItem = new JCheckBoxMenuItem("Catraca espelhada");
		JCheckBoxMenuItem syncUsersMenuItem = new JCheckBoxMenuItem("Sincronizar pedestres");
		JMenuItem syncUsersNowMenuItem = new JMenuItem("Sincronizar digitais agora");
		JMenuItem athleteScreenMenuItem = new JMenuItem("Abrir tela do pedestre");
		JMenuItem configMenuItem = new JMenuItem("Configurações");
		JMenuItem removeMenuItem = new JMenuItem("Remover dispositivo");
		JMenuItem syncMenuItem = new JMenuItem("Sincronizar catraca");
		
		if(Boolean.TRUE.equals(device.getConfigurationValueAsBoolean("Somente para cadastros"))) {
			defaultDeviceMenuItem.setVisible(false);
			athleteScreenMenuItem.setVisible(false);
		}
		
		JPopupMenu jPopup = new JPopupMenu();
		if (!Manufacturer.SERVER.equals(device.getManufacturer())) {
			jPopup.add(registerUserMenuItem);
			jPopup.add(defaultDeviceMenuItem);
			jPopup.add(athleteScreenMenuItem);
		}
		if (Manufacturer.CONTROL_ID.equals(device.getManufacturer())) {
			jPopup.add(mirrorMenuItem);
			mirrorMenuItem.setSelected(device.isMirrorDevice());
		}
		if (Manufacturer.CONTROL_ID_UHF.equals(device.getManufacturer())) {
			jPopup.remove(0);
		}
		if (Manufacturer.HENRY_7X.equals(device.getManufacturer())) {
			jPopup.add(syncUsersMenuItem);
			syncUsersMenuItem.setSelected(device.isSyncUsers());
			jPopup.add(syncUsersNowMenuItem);
		}
		if (Manufacturer.RWTECH.equals(device.getManufacturer()))
			jPopup.add(syncMenuItem);
		if (Manufacturer.FACIAL.equals(device.getManufacturer())) 
			registerUserMenuItem.setText("Cadastro de faces");
			
		if(Manufacturer.TOP_DATA.equals(device.getManufacturer())
				|| Manufacturer.TOP_DATA_ACESSO.equals(device.getManufacturer())) {
			if(device.isSyncUsers()) {
				jPopup.add(syncMenuItem);
				registerUserMenuItem.setVisible(false);
			}
			
		}
			
			
		jPopup.add(configMenuItem);
		jPopup.addSeparator();
		jPopup.add(removeMenuItem);
		
		registerUserMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
            	if (device.isConnected()) {
            		if (Manufacturer.FACIAL.equals(device.getManufacturer()))
            			device.setMode(DeviceMode.NONE);
	            	RegisterUserDialog registerUserDialog = new RegisterUserDialog(Main.mainScreen, device); 
	            	registerUserDialog.showScreen();
            	
            	} else
            		JOptionPane.showMessageDialog(Main.mainScreen, "Conecte o dispositivo primeiro!", "Dispositivo desconectado", JOptionPane.PLAIN_MESSAGE);
            }
        });
		
		syncMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
            	sync();
            }
        });
		
		defaultDeviceMenuItem.addActionListener( (e) -> device.setDefaultDevice(true) );
		athleteScreenMenuItem.addActionListener( (e) -> openAthleteScreen() );
		mirrorMenuItem.addActionListener( (e) -> device.setMirrorDevice(mirrorMenuItem.isSelected()) );
		syncUsersMenuItem.addActionListener( (e) -> { 
			device.setSyncUsers(syncUsersMenuItem.isSelected()); 
			syncUsersNowMenuItem.setEnabled(syncUsersMenuItem.isSelected());
		});
		syncUsersNowMenuItem.addActionListener( (e) -> ((TopDataDevice) device).atualizaDigitaisLFD(device.isConnected(), true, null) );
		configMenuItem.addActionListener( (e) -> showConfigurationDialog() );
		removeMenuItem.addActionListener( (e) -> showConfirmRemove() );
		setComponentPopupMenu(jPopup);
		
	}
	
	private void liberarCatraca() {
		logAccess.setDirection(Tipo.ENTRADA);
		logAccess.setEquipament(this.device.getName());
		
		if (Manufacturer.SERVER.equals(device.getManufacturer())) {
			ServerDevice serverDevice = (ServerDevice) device;
			serverDevice.setLogAccess(logAccess);
			serverDevice.allowAccess();
		
		} else {
			Utils.createNotification("Acesso liberado pelo sistema.", NotificationType.GOOD);
			HibernateUtil.save(LogPedestrianAccessEntity.class, logAccess);
			if (Main.broadcastServer != null)
				Main.broadcastServer.sendMessage(new BroadcastMessageTO(BroadcastMessageType.LOG_ACCESS, logAccess));
    		device.setVerificationResult(VerificationResult.ALLOWED);
			device.allowAccess();
		}
	}
	
	public void setMensagem(String novaMensagem, MessageType tipoMensagem){
		if (novaMensagem == null || "".equals(novaMensagem))
			novaMensagem = " ";
		mensagemLabel.setText(novaMensagem);
		if (MessageType.ERROR.equals(tipoMensagem))
			mensagemLabel.setForeground(Color.PINK);
		else
			mensagemLabel.setForeground(Main.secondColor);
		
		if(!device.coletandoDadosOffLine)
			connectButton.setEnabled(true);
	}
	
	public void setStatus(DeviceStatus status){
		if (DeviceStatus.CONNECTED.equals(status)){
			statusIcon.setIcon(connectedIcon);
			liberarAcessoButton.setEnabled(bloquearLiberarAcesso ? false : true);
			estadoDesejadoLiberarAcesso = true;
			connectButton.setVisible(false);
			disconnectButton.setVisible(true);
			disconnectButton.setEnabled(true);
			cadastrarFaceButton.setEnabled(true);
			setMensagem("Conectado", MessageType.NORMAL);
		}
		else {
			statusIcon.setIcon(disconnectedIcon);
			liberarAcessoButton.setEnabled(false);
			cadastrarFaceButton.setEnabled(false);
			estadoDesejadoLiberarAcesso = false;
			if (DeviceStatus.CONNECTED.equals(device.getDesiredStatus())) {
				setMensagem("Reconectando...", MessageType.ERROR);
				disconnectButton.setEnabled(true);
				disconnectButton.setVisible(true);
	    		connectButton.setVisible(false);
			}
			else {
				disconnectButton.setVisible(false);
	    		connectButton.setVisible(true);
	    		if(device.coletandoDadosOffLine) {
	    			setMensagem("Ainda trabalhando...", MessageType.NORMAL);
	    			connectButton.setEnabled(false);
	    		}else {
	    			setMensagem(" ", MessageType.NORMAL);
	    		}
			}
		}
		revalidate();
	}
	
	private void changeName(){
		namePanel.removeAll();
		
		JTextField newName = new JTextField(device.getName());
		newName.setPreferredSize(new Dimension(180, 25));
		newName.selectAll();
		namePanel.add(newName);
		
		JButton ok = new JButton("OK");
		namePanel.add(ok);
		ok.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (!newName.getText().isEmpty())
					device.setName(newName.getText());
				namePanel.removeAll();
				nameLabel.setText(device.getName());
				namePanel.add(nameLabel);
				revalidate();
				if (device.getCatracaVinculada() != null)
					device.getCatracaVinculada().getDeviceCard().setCatracaVinculada(true, device.getName());
				Utils.exportDevices();
			}
		});
		revalidate();
	}
	
	public void setDefaultLabel() {
		if (device.isDefaultDevice())
			defaultDeviceLabel.setVisible(true);
		else
			defaultDeviceLabel.setVisible(false);
	}
	
	private void showConfigurationDialog(){
		
		Boolean retornoAuthentication = false;
		try {
			AutenticationDialog autenticationDialog = new AutenticationDialog(null,
					"Digite a senha do usuário logado", "Aguarde, verificando a senha informada...");
			retornoAuthentication = autenticationDialog.authenticate();
		if (retornoAuthentication == null)
			return;
		
		} catch (Exception e2) {
			e2.printStackTrace();
		}
		
		if (retornoAuthentication) {
		
			final JDialog dialog = new JDialog(Main.mainScreen, "Configurações " + device.getName(), true);
			dialog.setIconImage(configImage);
			dialog.setResizable(true);
			dialog.setLayout(new BorderLayout());
			dialog.setMinimumSize(new Dimension(600,505));
			
			JPanel mainPanel = new JPanel(new BorderLayout());
			mainPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
			
			JTabbedPane tabbedPane = new JTabbedPane();
			tabbedPane.setPreferredSize(new Dimension(600,400));
			tabbedPane.setMaximumSize(new Dimension(600,600));
			mainPanel.add(tabbedPane, BorderLayout.CENTER);
			
			//if (Manufacturer.HENRY_7X.equals(device.getManufacturer()))
			//	((Henry7XDevice) device).receiveConfiguration();
			
			// Configuracoes de uso do equipamento
			Map<FieldTO, ConfigurationTO> mapaFieldConfiguration = new HashMap<FieldTO, ConfigurationTO>();
			if (device.getConfigurationGroups() != null) {
				for (ConfigurationGroupTO configGroup : device.getConfigurationGroups()) {
					JPanel innerPanel = new JPanel();
					innerPanel.setLayout(new BoxLayout(innerPanel, BoxLayout.Y_AXIS));
					innerPanel.setBorder(new EmptyBorder(10, 10, 10, 10));
					
					if (configGroup.getConfigurations() != null) {
						for (ConfigurationTO config : configGroup.getConfigurations()) {
							FieldTO field = new FieldTO(config, this);
							
							if(field.getName().equals("Tipo de leitor")) {
								field.setComboBoxListener(new ComboBoxListener() {
									@Override
									public void action(ItemEvent e) {
										SelectItem leitorSelecionado = (SelectItem) e.getItem();
										for (FieldTO f : mapaFieldConfiguration.keySet()) {
											if(f.getName().equals("Quantidade dígitos cartão")) {
												Vector<SelectItem> options = getOptions(leitorSelecionado.getLabel());
												if(!options.isEmpty()) 
													f.setOptions(options);
											}
										}
									}
								});
							} 
							
							if(field.getName().equals("Envia digitais para catraca")) {
								for (FieldTO f : mapaFieldConfiguration.keySet()) {
									if(f.getName().equals("Modo de trabalho")) {
										field.setVisible("naCatraca".equals(f.getValue()));
										break;
									}
								}
							}
							
							if(field.getName().equals("Modo de trabalho")) {
								field.setComboBoxListener(new ComboBoxListener() {
									@Override
									public void action(ItemEvent e) {
										SelectItem modoSelecionado = (SelectItem) e.getItem();
										for (FieldTO f : mapaFieldConfiguration.keySet()) {
											if(f.getName().equals("Envia digitais para catraca")) {
												f.getCurrentPanel().setVisible("naCatraca".equals(modoSelecionado.getValue()));
												innerPanel.updateUI();
												break;
											}
										}
									}
								});
							}
							
							if(field.getName().equals("Dois leitores")
									|| field.getName().equals("Dois leitores (usa para catracas com urna)")) {
								field.setActionListener(new ActionListener() {
									
									@Override
									public void actionPerformed(ActionEvent e) {
										JCheckBox check = (JCheckBox)e.getSource();
										if(check.isSelected()) {
											for (FieldTO f : mapaFieldConfiguration.keySet()) {
												if(f.getName().equals("Leitor 2")) {
													f.setValue("Entrada e Saída_3");
													innerPanel.updateUI();
													break;
												}
											}
										}
										
									}
								});
							} 
							
							JPanel fieldPanel = field.getPanel();
							if(field.getName().equals("Quantidade dígitos cartão")) {
								for (FieldTO f : mapaFieldConfiguration.keySet()) {
									if(f.getName().equals("Tipo de leitor")) {
										Vector<SelectItem> options;
										
										if(f.getValue().equals("3")) {
											options = getOptions("Proximidade Wiegand");
										} else if(f.getValue().equals("33")) {
											options = getOptions("Proximidade Wiegand FC");
										} else if(f.getValue().equals("6")) {
											options = getOptions("Proximidade Wiegand FC Sem Separador");
										} else {
											options = getOptions("");
										}
										field.setOptions(options);
									}
								}
							}
							innerPanel.add(fieldPanel);
							innerPanel.add(Box.createVerticalStrut(5));
							mapaFieldConfiguration.put(field, config);
						}
					}
					JScrollPane scrollPane = new JScrollPane(innerPanel, 
							ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, 
							ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
					scrollPane.getVerticalScrollBar().setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));
					tabbedPane.addTab(configGroup.getName(), scrollPane);
				}
			}
			
			// InformaÃ§Ãµes de ip, porta e localizacao
			JPanel deviceInfoPanel = new JPanel();
			deviceInfoPanel.setLayout(new BoxLayout(deviceInfoPanel, BoxLayout.Y_AXIS));
			deviceInfoPanel.setBorder(new EmptyBorder(10, 10, 10, 10));
			FieldTO nameField = new FieldTO(this, "Nome", FieldType.TEXT, device.getName());
			deviceInfoPanel.add(nameField.getPanel());
			deviceInfoPanel.add(Box.createVerticalStrut(5));
			FieldTO locationField = new FieldTO(this, "Localização", FieldType.TEXT, device.getLocation());
			deviceInfoPanel.add(locationField.getPanel());
			deviceInfoPanel.add(Box.createVerticalStrut(5));
			if (device.getManufacturer().getFields() != null) {
				String partes[] = device.getIdentifier().split(";");
				int i = 0;
				for (FieldTO field : device.getManufacturer().getFields()) {
					field.setDefaultValue(partes[i]);
					field.setEnabled(false);
					deviceInfoPanel.add(field.getPanel());
					deviceInfoPanel.add(Box.createVerticalStrut(5));
					i++;
				}
			}
			JScrollPane scrollPane = new JScrollPane(deviceInfoPanel, 
					ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, 
					ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
			scrollPane.getVerticalScrollBar().setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));
			tabbedPane.addTab("Informações do dispositivo", scrollPane);
			
			boolean exibeAbaCatracaVinculada = !(device instanceof FacialDevice)
													&& !(device instanceof ServerDevice);
			if(exibeAbaCatracaVinculada) {
				JPanel catracasPanel = new AttachedDevicesPanel(device);
				catracasPanel.setLayout(new BoxLayout(catracasPanel, BoxLayout.Y_AXIS));
				catracasPanel.setBorder(new EmptyBorder(10, 10, 10, 10));
				
				JScrollPane scrollCatracasPane = new JScrollPane(catracasPanel, 
						ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, 
						ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
				scrollCatracasPane.getVerticalScrollBar().setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));
				
				tabbedPane.addTab("Catracas vinculadas", scrollCatracasPane);
			}
			
			boolean exibeAbaCamerasHikivision = !(device instanceof FacialDevice)
					&& !(device instanceof ServerDevice)
					&& Utils.isHikivisionConfigValid();
			if(exibeAbaCamerasHikivision) {
				JPanel camerasHikivisionPanel = new HikivisionAttachedDevicesPanel(device);
				camerasHikivisionPanel.setLayout(new BoxLayout(camerasHikivisionPanel, BoxLayout.Y_AXIS));
				camerasHikivisionPanel.setBorder(new EmptyBorder(10, 10, 10, 10));
				
				JScrollPane scrollCamerasHikivisionPane = new JScrollPane(camerasHikivisionPanel, 
						ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, 
						ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
				scrollCamerasHikivisionPane.getVerticalScrollBar().setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));
				
				tabbedPane.addTab("Cameras Hikivision", scrollCamerasHikivisionPane);
			}
			
			erroConfigurationLabel = new PanelWithLabel(" ", FlowLayout.LEFT, true, 10, 0);
			erroConfigurationLabel.setLabelColor(Color.RED);
			
			JButton resetarButton = new JButton("Valores padrão");
			resetarButton.setPreferredSize(new Dimension(120, 30));
			JButton salvarButton = new JButton(device.isConnected() ? "Salvar e enviar" : "Salvar");
			salvarButton.setPreferredSize(new Dimension(device.isConnected() ? 120 : 80, 30));
			JButton cancelarButton = new JButton("Cancelar");
			cancelarButton.setPreferredSize(new Dimension(100, 30));
			
			JPanel buttonsPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 5, 5));
			buttonsPanel.add(resetarButton);
			buttonsPanel.add(salvarButton);
			buttonsPanel.add(cancelarButton);
			
			salvarButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent arg0) {
					try {
						dialog.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
						Set<FieldTO> fields = mapaFieldConfiguration.keySet();
						for (FieldTO field : fields) {
							String statusField = field.checkField();
							if (!"".equals(statusField)) {
								erroConfigurationLabel.setText(statusField);
								return;
							}
						}
						for (FieldTO field : fields) {
							ConfigurationTO config = mapaFieldConfiguration.get(field);
							config.setValue(field.getValue());
						}
						device.setLocation(locationField.getValue());
						device.setName(nameField.getValue());
						device.createConfigurationMap();
						if (device.isConnected())
							device.sendConfiguration();
						device.saveConfigurations();
						Utils.exportDevices();
						JOptionPane.showMessageDialog(Main.mainScreen, device.isConnected() ? "Configurações enviadas!" : "Configurações salvas!", 
								"Sucesso!", JOptionPane.PLAIN_MESSAGE);
						dialog.dispose();
						nameLabel.setText(device.getName());
						
						revalidate();
						Main.mainScreen.verificaCatracasVinculadas();
						Main.mainScreen.refresh();
					}
					catch (Exception e){
						e.printStackTrace();
						erroConfigurationLabel.setText("Erro ao " + (device.isConnected() ? "enviar" : "salvar") + " as Configurações. " + e.getMessage());
					}
					finally {
						dialog.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
					}
				}
			});
			
			resetarButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent arg0) {
					device.createDefaultConfiguration();
					dialog.dispose();
					showConfigurationDialog();
				}
			});
			
			cancelarButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent arg0) {
					dialog.dispose();
				}
			});
			
			dialog.getContentPane().add(mainPanel, BorderLayout.PAGE_START);
			dialog.getContentPane().add(erroConfigurationLabel, BorderLayout.CENTER);
			dialog.getContentPane().add(buttonsPanel, BorderLayout.PAGE_END);
			dialog.pack();
			dialog.setLocationRelativeTo(null);
			dialog.setVisible(true);
		
		} else {
			JOptionPane.showMessageDialog(null, "Não foi possível validar a senha, ou senha invÃ¡lida",
					"Erro na validaÃ§Ã£o", JOptionPane.PLAIN_MESSAGE);
		}
	}
	
	private void showConfirmRemove(){
		Object[] options = { DeviceStatus.CONNECTED.equals(device.getStatus()) ? "Desconectar e remover" : "Remover", "Cancelar"};
		int result = JOptionPane.showOptionDialog(null, "Deseja realmente remover este dispositivo?",
				"ConfirmaÃ§Ã£o", 0, JOptionPane.PLAIN_MESSAGE, null, options, null);
		if (result == JOptionPane.OK_OPTION) {
			SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
				@Override
			    public Void doInBackground() {
					try {
						setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
						if (DeviceStatus.CONNECTED.equals(device.getStatus())){
							disconnectButton.doClick();
							Long inicio = System.currentTimeMillis();
							while (DeviceStatus.CONNECTED.equals(device.getStatus()) 
									&& (System.currentTimeMillis() - inicio) < 10000)
								Utils.sleep(50);
						}
						if(instance.getDevice() instanceof ServerDevice) {
							Main.servidor = null;
						}
						Main.mainScreen.removeDeviceCard(instance);
					}
					catch (Exception e) {
						e.printStackTrace();
						setMensagem("Não foi possível remover.", MessageType.ERROR);
						Main.mainScreen.addEvento("Erro ao remover " + device.getName() + ": " + e.getMessage());
					}
					finally {
						setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
					}
			    	return null;
			    }
			};
			worker.execute();
		}
	}
	
	private void sync() {
		TopDataDevice topData = (TopDataDevice) device;
		if(topData.isConnected()) {
			JOptionPane.showMessageDialog(Main.mainScreen, 
					"A sincronizaÃ§Ã£o de todas as digitais sÃ³ pode ser feita com o dispositivo desconectado.",
					"Desconecte o dispositivo!",
					JOptionPane.PLAIN_MESSAGE);
			return;
		}
		
		SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
			@Override
		    public Void doInBackground() {
				connectButton.setEnabled(false);
		    	topData.atualizaDigitaisLFD(topData.isConnected(), true, null);
		    	connectButton.setEnabled(true);
				return null;
			}
		};
		worker.execute();
	}
	
	public void openAthleteScreen() {
		try {
			if (device.isConnected()) {
				if(device.getAthleteScreen() == null)
					device.setAthleteScreen(new PedestrianScreen(device, (device.isCatraca() ?  device : null),
							device.getDeviceEntity().getAthleteScreenConfig()));
				else
					device.getAthleteScreen().showScreen();
        	
			} else
        		JOptionPane.showMessageDialog(Main.mainScreen, "Conecte o dispositivo primeiro!", "Dispositivo desconectado", JOptionPane.PLAIN_MESSAGE);
		
		} catch (Throwable e){
			e.printStackTrace();
			setMensagem(e.getMessage(), MessageType.ERROR);
			Main.mainScreen.addEvento(device.getName() + ": " + e.getMessage());
		}
	}
	
	private Vector<SelectItem> getOptions(String leitor){
		Vector<SelectItem> options = new Vector<>();
		
		if(leitor.equals("Proximidade Wiegand")) {
			options.add(new SelectItem(String.valueOf(5), String.valueOf(5)));
		} else if(leitor.equals("Proximidade Wiegand FC")) {
			options.add(new SelectItem(String.valueOf(9), String.valueOf(9)));
		} else if(leitor.equals("Proximidade Wiegand FC Sem Separador")) {
			options.add(new SelectItem(String.valueOf(8), String.valueOf(8)));
		} else {
			for(int i = 1; i <= 16; i++) {
				options.add(new SelectItem(String.valueOf(i), String.valueOf(i)));
			}
		}
		
		return options;
	}

	private void loadImages(){
		Toolkit toolkit = Toolkit.getDefaultToolkit();
		configImage = toolkit.getImage(Main.class.getResource(Configurations.IMAGE_FOLDER + Main.customImageFolder + "configuracoes.png"));
		catracaIcon = new ImageIcon(toolkit.getImage(Main.class.getResource(Configurations.IMAGE_FOLDER + "thumbnails/" 
						+ device.getManufacturer().getIconName())));
		connectedIcon = new ImageIcon(toolkit.getImage(Main.class.getResource(Configurations.IMAGE_FOLDER + "comuns/ok.png")));
		disconnectedIcon = new ImageIcon(toolkit.getImage(Main.class.getResource(Configurations.IMAGE_FOLDER + "comuns/erro.png")));
		linkIcon = new ImageIcon(toolkit.getImage(Main.class.getResource(Configurations.IMAGE_FOLDER + "comuns/link.png")));
	}

	public Device getDevice() {
		return device;
	}

	public void setDevice(Device device) {
		this.device = device;
	}

	public JButton getConnectButton() {
		return connectButton;
	}

	public JButton getDisconnectButton() {
		return disconnectButton;
	}

	public void setAutoConnect(Boolean autoConnect) {
		this.autoConnect = autoConnect;
	}
	
	public void setErroConfiguration(String erro) {
		erroConfigurationLabel.setText(erro);
	}

	public Long getSerialId() {
		return serialId;
	}

	public void setCatracaVinculada(Boolean catracaVinculada, String nomeLeitor) {
		this.bloquearLiberarAcesso = catracaVinculada;
		if (catracaVinculada) {
			liberarAcessoButton.setEnabled(false);
			link.setText(nomeLeitor);
			linkIconLabel.setIcon(linkIcon);
		}
		else {
			liberarAcessoButton.setEnabled(estadoDesejadoLiberarAcesso);
			link.setText(" ");
			linkIconLabel.setIcon(null);
		}
	}
	
	public void setLinkText(String mensagem) {
		link.setText(mensagem);
	}
	
	public void setIcon(Image image){
		this.icon.setIcon(new ImageIcon(image));
	}
	
	public void resetIcon(){
		this.icon.setIcon(catracaIcon);
	}
	
}
