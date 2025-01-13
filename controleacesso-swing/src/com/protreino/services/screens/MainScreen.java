package com.protreino.services.screens;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowStateListener;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JToolBar;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.WindowConstants;
import javax.swing.border.BevelBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import com.protreino.services.constants.Configurations;
import com.protreino.services.devices.Device;
import com.protreino.services.devices.DeviceCard;
import com.protreino.services.devices.HikivisionDeviceCard;
import com.protreino.services.devices.LcDevice;
import com.protreino.services.devices.ServerDevice;
import com.protreino.services.entity.CartaoComandaEntity;
import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.enumeration.NotificationType;
import com.protreino.services.enumeration.PerfilAcesso;
import com.protreino.services.main.Main;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.usecase.HikivisionUseCases;
import com.protreino.services.usecase.ReleaseAccessUseCase;
import com.protreino.services.utils.PanelWithLabel;
import com.protreino.services.utils.Utils;
import com.protreino.services.utils.WrapLayout;

@SuppressWarnings("serial")
public class MainScreen extends JFrame {

	private MainScreen instance;
	private ImageIcon logoIcon;
	private ImageIcon setaUpIcon;
	private ImageIcon setaDownIcon;
	private Font font;
	private Font tabHeaderFont;
	private SimpleDateFormat sdf;

	private JMenuItem loginInternoMenuItem;
	private JMenuItem logoutInternoMenuItem;
	private JMenuItem liberarAcessoMenuItem;
	private JMenuItem preferenciasMenuItem;
	private JMenuItem hikivisionManualSyncMenuItem;
	private JMenuItem TopDataFacialMenuItem;
	private JMenuItem syncUsersMenuItem;
	private JMenuItem logsMenuItem;
	private JMenuItem procurarAtualizacaoMenuItem;
	private JMenuItem sobreMenuItem;
	private JMenuItem cadastrarPedestreMenuItem;
	private JMenuItem cadastrarVisitanteMenuItem;
	
	private JLabel eventosLabel;
	private JScrollPane listaEventosScrollPane;
	private JPanel listaEventosPanel;
	private PanelWithLabel connectionStatusLabel;
	private PanelWithLabel hikivisionConnectionStatusLabel;

	private Container mainContentPane;
	private JTabbedPane tabbedPane;
	private JPanel devicesPane;
	private JPanel hikivisionDevicesPane;
   // private LoginPanel loginPanel;
	private AccessListPanel listaAcessoPanel;
	private AccessHistoryPanel historicoAcessoPanel;
	private DevicefromServerPanel devicefromServerPanel;
	private AccessCardListPanel listaCartoesPanel;
	private TopDataErrorsScreen listaErrosPanel;
	private ImageIcon adicionarIcon;
	private ImageIcon liberarAcessoIcon;
	private ImageIcon atualizarListaAcessoIcon;
	private JButton liberarAcessoButton;
	private JButton toggleEventosButton;
	private JButton atualizarListaAcessoButton;

	private Dimension actualDimension;
	private Point actualPosition;
	private Integer actualState;

	private Device newDevice;
	private List<String> eventos;
	private JMenu menuCadastros;
	
	public RegisterVisitorDialog cadastroVisitante;
	public RegisterVisitorDialog cadastroPedestre;
	public CartaoComandaDialog cadastroCartao;
	
	private final HikivisionUseCases hikivisionUseCases = new HikivisionUseCases();
	private final ReleaseAccessUseCase releaseAccessUseCase = new ReleaseAccessUseCase();

	public MainScreen() {
		instance = this;

		loadImages();

		font = getDefaultFont();
		tabHeaderFont = new Font(font.getFontName(), Font.BOLD, font.getSize() + 1);
		sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");

		setTitle(Main.nomeAplicacao + " Desktop - v." + Configurations.VERSION);
		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		setLayout(new BorderLayout());
		setPreferredSize(System.getProperty("os.name").toLowerCase().contains("linux") 
				? new Dimension(900, 600) : new Dimension(920, 718));
		setMinimumSize(getPreferredSize());
		setResizable(true);
		setIconImage(Main.favicon);
		
		addComponentListener(new ComponentAdapter() {
			public void componentMoved(ComponentEvent e) {
				actualPosition = getLocation();
			}

			public void componentResized(ComponentEvent evt) {
				actualDimension = getSize();
				setPreferredSize(actualDimension);
			}
		});

		addWindowStateListener(new WindowStateListener() {
			public void windowStateChanged(WindowEvent arg0) {
				actualState = getExtendedState();
			}
		});

		buildUI();

		if (Main.desenvolvimento) {
			setVisible(true);
			toFront();
		}
	}
	
	private Font getDefaultFont() {
		
		Font font = new JLabel().getFont();
		//if(System.getProperty("os.name").toLowerCase().contains("linux"))
		//	font = new Font(font.getName(), font.getStyle(), font.getSize() - 1);
		
		return font;
	}

	public void showScreen() {
		//buildUI();
		setVisible(true);
		toFront();
		if(listaAcessoPanel != null)
			listaAcessoPanel.updateDateLastSync();
		if(historicoAcessoPanel != null)
			historicoAcessoPanel.updateDateLastSync();
		
		if (Main.loggedUser == null)
			new LoginDialog();
	}

	public void buildUI() {
		eventos = new ArrayList<String>();

		mainContentPane = new Container();
		mainContentPane.setLayout(new BorderLayout());

		tabbedPane = new JTabbedPane();
		mainContentPane.add(tabbedPane, BorderLayout.CENTER);

		JPanel panelCatracas = montarPanelCatracas();
		tabbedPane.addTab("Catracas e leitores", panelCatracas);
		JLabel label = new JLabel("Catracas e leitores");
		label.setPreferredSize(new Dimension(150, 25));
		label.setForeground(Main.firstColor);
		label.setFont(tabHeaderFont);
		tabbedPane.setTabComponentAt(0, label);
		
		int position = 1;
		if(Main.loggedUser != null && Boolean.TRUE.equals(Main.loggedUser.getExpedidora())) {
			listaCartoesPanel = new AccessCardListPanel();
			tabbedPane.addTab("Lista de cartoes", listaCartoesPanel);
			label = new JLabel("Lista de cartoes");
			label.setPreferredSize(new Dimension(150, 25));
			label.setForeground(Main.firstColor);
			tabbedPane.setTabComponentAt(position, label);
			position++;
		}
		
		listaAcessoPanel = new AccessListPanel();
		tabbedPane.addTab("Lista de acesso", listaAcessoPanel);
		label = new JLabel("Lista de acesso");
		label.setPreferredSize(new Dimension(150, 25));
		label.setForeground(Main.firstColor);
		tabbedPane.setTabComponentAt(position, label);
		position++;

		historicoAcessoPanel = new AccessHistoryPanel();
		tabbedPane.addTab("Historico de acesso", historicoAcessoPanel);
		label = new JLabel("Historico de acesso");
		label.setPreferredSize(new Dimension(150, 25));
		label.setForeground(Main.firstColor);
		tabbedPane.setTabComponentAt(position, label);
		position++;
		
		if(Utils.isHikivisionConfigValid() && hikivisionUseCases.getSystemInformation()) {
			JPanel panelDevicesHkivision = montarPanelCamerasHikivision();
			tabbedPane.addTab("Leitores faciais", panelDevicesHkivision);
			label = new JLabel("Leitores faciais");
			label.setPreferredSize(new Dimension(150, 25));
			label.setForeground(Main.firstColor);
			tabbedPane.setTabComponentAt(position, label);
			position++;
		}
		
		if(Main.temServidor()) {
			devicefromServerPanel = new DevicefromServerPanel();
			tabbedPane.addTab("Devices do servidor", devicefromServerPanel);
			label = new JLabel("Devices do servidor");
			label.setPreferredSize(new Dimension(150, 25));
			label.setForeground(Main.firstColor);
			tabbedPane.setTabComponentAt(position, label);
			position++;
		}
		
		listaErrosPanel = new TopDataErrorsScreen();
		tabbedPane.addTab("Faces erradas topdata", listaErrosPanel);
		label = new JLabel("Faces erradas TopData");
		label.setPreferredSize(new Dimension(150, 25));
		label.setForeground(Main.firstColor);
		tabbedPane.setTabComponentAt(position, label);
		position++;

		tabbedPane.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				for (int i = 0; i < tabbedPane.getTabCount(); i++) {
					((JLabel) tabbedPane.getTabComponentAt(i))
							.setFont((i == tabbedPane.getSelectedIndex()) ? tabHeaderFont : font);
				}
				int p = 1;
				if(Main.loggedUser != null && Boolean.TRUE.equals(Main.loggedUser.getExpedidora())) {
					if (tabbedPane.getSelectedIndex() == p) {
						if(!listaCartoesPanel.isLoad())
							listaCartoesPanel.cleanFilter();
					}
					p++;
				}
				if (tabbedPane.getSelectedIndex() == p)
					if(!listaAcessoPanel.isLoad())
						listaAcessoPanel.cleanFilter();
					else
						listaAcessoPanel.executeFilter();
				p++;
				if (tabbedPane.getSelectedIndex() == p)
					if(!historicoAcessoPanel.isLoad())
						historicoAcessoPanel.cleanFilter();
					else
						historicoAcessoPanel.executeFilter();
				p++;
			}
		});

		JPanel eventosStatus = montarPanelEventos();
		mainContentPane.add(eventosStatus, BorderLayout.PAGE_END);

		montarMenuBar();

		setContentPane(mainContentPane);
		revalidate();
		pack();

		if (actualState != null) {
			setExtendedState(actualState);
		}
		if (actualDimension != null) {
			setPreferredSize(actualDimension);
		}
		if (actualPosition != null) {
			setLocation(actualPosition);
		} else {
			setLocationRelativeTo(null);
		}
	}

	/**
	 * Exibe a janela para adicionar um novo dispositivo Adiciona um novo
	 * dispositivo na lista e chama o metodo para criar o deviceCard
	 */
	private void addNewDevice() {
		NewDeviceDialog newDeviceDialog = new NewDeviceDialog(instance);
		newDeviceDialog.setVisible(true);
		String option = newDeviceDialog.getOption();
		if ("OK".equals(option)) {
			newDevice = newDeviceDialog.getNewDevice();

			if(newDevice instanceof ServerDevice) {
				Main.addServidor((ServerDevice) newDevice);
			}
			
			if(newDevice instanceof LcDevice) {
				Main.possuiLeitorLcAdd = true;
			}
			
			if(Main.devicesList == null) {
				Main.devicesList = new ArrayList<Device>();
			}
				
			Main.devicesList.add(newDevice);
			addDeviceCard(newDevice);
			DeviceEntity deviceEntity = new DeviceEntity(newDevice);
			deviceEntity = (DeviceEntity) HibernateAccessDataFacade.save(DeviceEntity.class, deviceEntity)[0];
			newDevice.setDeviceEntity(deviceEntity);
			if (Main.devicesList.size() == 1) {
				newDevice.setDefaultDevice(true);
			}
			Utils.exportDevices();
			
			refreshAll();
		}
	}

	private void addHikivisionDeviceCard(com.protreino.services.to.hikivision.HikivisionDeviceTO.Device device) {
		HikivisionDeviceCard hikivisionDeviceCard = new HikivisionDeviceCard(device);
		hikivisionDeviceCard.setStatus(device.getDevStatus());
		hikivisionDevicesPane.add(hikivisionDeviceCard);
		hikivisionDevicesPane.revalidate();
	}
	
	/**
	 * Cria o deviceCard. Chamado quando um novo dispositivo é adicionado ou quando
	 * a tela exibida e há dispositivos salvos na lista
	 * 
	 * @param device
	 */
	private void addDeviceCard(Device device) {
		DeviceCard deviceCard = new DeviceCard(device);
		device.setDeviceCard(deviceCard);
		devicesPane.add(deviceCard);
		devicesPane.revalidate();
	}

	public void removeDeviceCard(DeviceCard deviceCard) {
		Main.devicesList.remove(deviceCard.getDevice());
		devicesPane.remove(deviceCard);
		devicesPane.revalidate();
		devicesPane.repaint();

		if(deviceCard.getDevice() instanceof LcDevice)
			Main.possuiLeitorLcAdd = false;
		
		HibernateAccessDataFacade.remove(deviceCard.getDevice().getDeviceEntity());
		if (deviceCard.getDevice().isDefaultDevice()) {
			if (!Main.devicesList.isEmpty())
				Main.devicesList.get(0).setDefaultDevice(true);
		}
		
		Utils.exportDevices();
		refreshAll();
	}

	public void addEvento(String mensagem) {
		eventosLabel.setText(sdf.format(new Date()) + " - " + mensagem);
		eventos.add(eventosLabel.getText());
		System.out.println("Evento adicionado: " + eventos.size());
		if (toggleEventosButton.getIcon().equals(setaDownIcon)) {
			listaEventosPanel.add(new JLabel(sdf.format(new Date()) + " - " + mensagem));
			listaEventosPanel.revalidate();
		}
	}

	public void doLogout(JDialog dialog) {
		try {
			AutenticationDialog autenticationDialog = new AutenticationDialog(null,true, true, true);
			Boolean retornoAuthentication = autenticationDialog.authenticate();
			if (retornoAuthentication == null)
				return;

			if (retornoAuthentication) {
				dialog.dispose();
				Main.finalizarSessao();
				buildUI();
				revalidate();
				new LoginDialog();
			} else {
				JOptionPane.showMessageDialog(null, "Nao foi possivel realizar o logout.", "Erro no logout",
						JOptionPane.PLAIN_MESSAGE);
			}
		
		} catch (Exception e) {
			e.printStackTrace();
			Utils.createNotification("Nao foi possivel realizar o logout", NotificationType.BAD);
		}
	}
	
	private void doLoginInterno() {
		AutenticationDialog autenticationDialog = new AutenticationDialog(this, true, true, true);
		autenticationDialog.setVisible(true);
		String option = autenticationDialog.getOption();
		if ("OK".equals(option)) {
			Main.internoLoggedUser = autenticationDialog.getUsuario();
			
			loginInternoMenuItem.setVisible(false);
			logoutInternoMenuItem.setVisible(true);
			
			menuCadastros.setVisible(true);
			
			if(Main.loggedUser != null 
					&& Boolean.TRUE.equals(Main.loggedUser.getExpedidora())) {
				listaCartoesPanel.addButton.setVisible(true);
				listaCartoesPanel.clearCardStateButton.setVisible(true);
				listaCartoesPanel.cleanFilter();
				listaCartoesPanel.updateUI();
			}
			
			listaAcessoPanel.cleanFilter();
			listaAcessoPanel.updateUI();
			
		}
	}
	
	private void doLogoutInterno() {
		Main.internoLoggedUser = null;
		loginInternoMenuItem.setVisible(true);
		logoutInternoMenuItem.setVisible(false);
		
		menuCadastros.setVisible(false);
		
		if(Main.loggedUser != null 
				&& Boolean.TRUE.equals(Main.loggedUser.getExpedidora())) {
			listaCartoesPanel.addButton.setVisible(false);
			listaCartoesPanel.clearCardStateButton.setVisible(false);
			listaCartoesPanel.cleanFilter();
			listaCartoesPanel.updateUI();
		}
		listaAcessoPanel.cleanFilter();
		listaAcessoPanel.updateUI();
	}

	private JMenuBar montarMenuBar() {
		JMenuBar menuBar = new JMenuBar();

		JMenu menuOpcoes = new JMenu("Arquivo");
		menuOpcoes.setMnemonic(KeyEvent.VK_O);
		menuBar.add(menuOpcoes);
		
		menuCadastros = new JMenu("Cadastros");
		menuCadastros.setMnemonic(KeyEvent.VK_0);
		menuCadastros.setVisible(Main.internoLoggedUser != null);
		menuBar.add(menuCadastros);
		
		JMenu menuConfiguracoes = new JMenu("Configuracoes");
		menuConfiguracoes.setMnemonic(KeyEvent.VK_O);
		menuBar.add(menuConfiguracoes);
		
		JMenu menuAjuda = new JMenu("Ajuda");
		menuAjuda.setMnemonic(KeyEvent.VK_O);
		menuBar.add(menuAjuda);

		if (Main.loggedUser != null) {
			liberarAcessoMenuItem = new JMenuItem("Liberar acesso (F9)/(F10)");
			liberarAcessoMenuItem.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					releaseAccessUseCase.execute(null, null);
				}
			});
			menuOpcoes.add(liberarAcessoMenuItem);

			preferenciasMenuItem = new JMenuItem("Preferencias");
			preferenciasMenuItem.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					try {
//						AutenticationDialog autenticationDialog = new AutenticationDialog(null,"Digite a senha do usuario logado", "Aguarde, verificando a senha informada...");
						AutenticationDialog autenticationDialog = new AutenticationDialog(null,true, true,true);
						Boolean retornoAuthentication = autenticationDialog.authenticate();
						if (retornoAuthentication == null) {
							return;
						}
						
						
						if (retornoAuthentication) {
							new PreferencesDialog();
							int index = tabbedPane.getSelectedIndex();
							buildUI();
							tabbedPane.setSelectedIndex(index);
						} else {
							JOptionPane.showMessageDialog(null, "Senha invalida ou sem permissao",
									"Erro na validacao", JOptionPane.PLAIN_MESSAGE);
						}
					} catch (Exception e2) {
						e2.printStackTrace();
						Utils.createNotification("Nao foi possivel abri as preferencias", NotificationType.BAD);
					}
				}

				
			});
			menuConfiguracoes.add(preferenciasMenuItem);

			hikivisionManualSyncMenuItem = new JMenuItem("Sincronismo manual de dispositivos");
			hikivisionManualSyncMenuItem.addActionListener(
				e -> {
					try {
	//					AutenticationDialog autenticationDialog = new AutenticationDialog(null,
//								"Digite a senha do usuario logado", "Aguarde, verificando a senha informada...");
						AutenticationDialog autenticationDialog = new AutenticationDialog(null,true, true, true);
						Boolean retornoAuthentication = autenticationDialog.authenticate();
						if (retornoAuthentication == null) {
							return;
						}
						if (retornoAuthentication) {
							new SincronizacaoManualDialog();
							int index = tabbedPane.getSelectedIndex();
							buildUI();
							tabbedPane.setSelectedIndex(index);
						} else {
							JOptionPane.showMessageDialog(null, "Senha invalida ou sem permissao",
									"Erro na validacao", JOptionPane.PLAIN_MESSAGE);
						}
					} catch (Exception e2) {
						e2.printStackTrace();
						Utils.createNotification("Nao foi possivel abri as preferencias", NotificationType.BAD);
					}
				}
			);

			if(Utils.isHikivisionConfigValid()) {
				menuConfiguracoes.add(hikivisionManualSyncMenuItem);
			}
			
			//
			TopDataFacialMenuItem = new JMenuItem("Sincronismo facial topdata");
			TopDataFacialMenuItem.addActionListener(
				e -> {
					try {
	//					AutenticationDialog autenticationDialog = new AutenticationDialog(null,
//								"Digite a senha do usuario logado", "Aguarde, verificando a senha informada...");
						AutenticationDialog autenticationDialog = new AutenticationDialog(null,true, true, true);
						Boolean retornoAuthentication = autenticationDialog.authenticate();
						if (retornoAuthentication == null) {
							return;
						}
						if (retornoAuthentication) {
							new TopDataFacialDialog();
							int index = tabbedPane.getSelectedIndex();
							buildUI();
							tabbedPane.setSelectedIndex(index);
						} else {
							JOptionPane.showMessageDialog(null, "Senha invalida ou sem permissao",
									"Erro na validacao", JOptionPane.PLAIN_MESSAGE);
						}
					} catch (Exception e2) {
						e2.printStackTrace();
						Utils.createNotification("Nao foi possivel abri as preferencias", NotificationType.BAD);
					}
				}
			);

			if(Utils.isTopDataFacialEnable()) {
				menuConfiguracoes.add(TopDataFacialMenuItem);
			}
			
			//
			
			syncUsersMenuItem = new JMenuItem("Sincronizar usuarios");
			syncUsersMenuItem.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					try {
//						AutenticationDialog autenticationDialog = new AutenticationDialog(null,
//								"Digite a senha do usuario logado", "Aguarde, verificando a senha informada...");
						AutenticationDialog autenticationDialog = new AutenticationDialog(null,true,true,true);
						Boolean retornoAuthentication = autenticationDialog.authenticate();
						if (retornoAuthentication == null)
							return;
						if (retornoAuthentication) {
							syncUsers();
						} else {
							JOptionPane.showMessageDialog(null, "Senha invalida ou sem permissao",
									"Erro na validacao", JOptionPane.PLAIN_MESSAGE);
						}
					} catch (Exception e2) {
						e2.printStackTrace();
						Utils.createNotification("Nao foi possivel sincronizar os usuarios", NotificationType.BAD);
					}
				}
			});
			menuConfiguracoes.add(syncUsersMenuItem);

			logsMenuItem = new JMenuItem("Abrir pasta de logs");
			logsMenuItem.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					abrirPastaLogs();
				}
			});
			menuConfiguracoes.add(logsMenuItem);
			
			procurarAtualizacaoMenuItem = new JMenuItem("Procurar atualizacoes");
			procurarAtualizacaoMenuItem.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					try {
						new AboutScreen(false).procurarAtualizacoes();
					} 
					catch (Exception e2) {
						e2.printStackTrace();
					}
				}
			});
			menuAjuda.add(procurarAtualizacaoMenuItem);

			sobreMenuItem = new JMenuItem("Sobre");
			sobreMenuItem.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					new AboutScreen(true);
				}
			});
			menuAjuda.add(sobreMenuItem);

			loginInternoMenuItem = new JMenuItem("Login interno");
			loginInternoMenuItem.addActionListener(e -> {
				doLoginInterno();
			});
			loginInternoMenuItem.setVisible(Main.internoLoggedUser == null);
			menuOpcoes.add(loginInternoMenuItem);
			
			logoutInternoMenuItem = new JMenuItem("Logout interno");
			logoutInternoMenuItem.addActionListener(e -> {
				doLogoutInterno();
			});
			logoutInternoMenuItem.setVisible(Main.internoLoggedUser != null);
			menuOpcoes.add(logoutInternoMenuItem);
			
			cadastrarPedestreMenuItem = new JMenuItem("Cadastrar pedestre (F7)");
			cadastrarPedestreMenuItem.addActionListener(e -> {
				abreCadastroPedestre(null);
			});
			menuCadastros.add(cadastrarPedestreMenuItem);
			
			cadastrarVisitanteMenuItem = new JMenuItem("Cadastrar visitante (F8)");
			cadastrarVisitanteMenuItem.addActionListener(e -> {
				abreCadastroVisitante(null);
			});
			menuCadastros.add(cadastrarVisitanteMenuItem);
		}

		JMenuItem sairMenuItem = new JMenuItem("Fechar");
		sairMenuItem.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				Main.exit(true);
			}
		});
		menuOpcoes.add(sairMenuItem);

		setJMenuBar(menuBar);

		return menuBar;
	}

	public void abreCadastroVisitante(PedestrianAccessEntity p) {
		PedestrianAccessEntity visitante = p;
		if(visitante == null) {
			visitante = new PedestrianAccessEntity();
			visitante.setTipo("VISITANTE");
		}
		if(!RegisterVisitorDialog.abertoPeloAtalho) {
			RegisterVisitorDialog.abertoPeloAtalho = true;
			cadastroVisitante = new RegisterVisitorDialog(visitante);
			SwingUtilities.invokeLater( 
			    new Runnable() { 
			        @Override public void run() {
			        	cadastroVisitante.setVisible(true);
			        }
			    }
			);
		}
	}

	public void abreCadastroPedestre(PedestrianAccessEntity p) {
		PerfilAcesso perfil = Main.internoLoggedUser.getPerfilAcesso();
	    // Verifica se o usuario tem um perfil de acesso
	    if(Objects.nonNull(perfil)) {	
	        if(perfil.equals(PerfilAcesso.PORTEIRO)) {
	            JOptionPane.showMessageDialog(null, "Usuario nao possui acesso");
	            return;
	        }
	    }

	    // Caso o perfil seja diferente de PORTEIRO, continua para abrir o cadastro
	    PedestrianAccessEntity pedestre = p;
	    if (pedestre == null) {
	        pedestre = new PedestrianAccessEntity();
	        pedestre.setTipo("PEDESTRE");
	    }

	    // Verifica se o cadastro ja foi aberto pelo atalho
	    if (!RegisterVisitorDialog.abertoPeloAtalho) {
	        RegisterVisitorDialog.abertoPeloAtalho = true;
	        cadastroPedestre = new RegisterVisitorDialog(pedestre);
	        
	        // Mostra a tela do cadastro em uma thread separada
	        SwingUtilities.invokeLater(new Runnable() { 
	            @Override
	            public void run() {
	                cadastroPedestre.setVisible(true);
	            }
	        });
	    }
	}
	
	public void abreCadastroCartoComanda(CartaoComandaEntity c) {
		CartaoComandaEntity cartao = c;
		if(cartao == null) {
			cartao = new CartaoComandaEntity();
		}
		if(!CartaoComandaDialog.abertoPeloAtalho) {
			CartaoComandaDialog.abertoPeloAtalho = true;
			cadastroCartao = new CartaoComandaDialog(cartao);
			SwingUtilities.invokeLater( 
			    new Runnable() { 
			        @Override public void run() {
			        	cadastroCartao.setVisible(true);
			        }
			    }
			);
		}
	}

	private JToolBar montarToolBarCatracas() {
		JToolBar toolBar = new JToolBar("Toolbar");
		toolBar.setFloatable(false);
		toolBar.setRollover(false);
		
		//toolBar.setPreferredSize(preferredSize);

		Dimension buttonSize = System.getProperty("os.name").toLowerCase().contains("linux") 
				? new Dimension(150, 80) : new Dimension(160, 80);

		JButton addDeviceButton = makeButton("Adicionar dispositivo", adicionarIcon, "Adicionar dispositivo",
				buttonSize);
		addDeviceButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				addNewDevice();
			}
		});
		toolBar.add(addDeviceButton);
		toolBar.addSeparator(new Dimension(5, 0));

		liberarAcessoButton = makeButton("Liberar acesso (F9)/(F10)", liberarAcessoIcon, "Liberar acesso", buttonSize);
		liberarAcessoButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				releaseAccessUseCase.execute(null, null);
			}
		});
		toolBar.add(liberarAcessoButton);
		toolBar.addSeparator(new Dimension(5, 0));

		atualizarListaAcessoButton = makeButton(System.getProperty("os.name").toLowerCase().contains("linux") 
				? "Atualizar lista" : "Atualizar lista de acesso", atualizarListaAcessoIcon,
				"Atualizar lista de acesso", buttonSize);
		atualizarListaAcessoButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				Main.sync();
			}
		});
		toolBar.add(atualizarListaAcessoButton);
		
		JPanel logoPanel = new JPanel(new FlowLayout(FlowLayout.TRAILING));
		logoPanel.add(new JLabel(logoIcon));
		toolBar.add(logoPanel);
		

		return toolBar;
	}

	private JPanel montarPanelCatracas() {
		JPanel panel = new JPanel();
		panel.setLayout(new BorderLayout());

		JToolBar toolBar = montarToolBarCatracas();
		panel.add(toolBar, BorderLayout.PAGE_START);

		devicesPane = new JPanel();
		devicesPane.setLayout(new WrapLayout(FlowLayout.LEFT, 30, 30));
		devicesPane.setBackground(Color.WHITE);

		JMenuItem refreshMenuItem = new JMenuItem("Atualizar");
		refreshMenuItem.addActionListener((e) -> refresh());

		JPopupMenu jPopup = new JPopupMenu();
		jPopup.add(refreshMenuItem);
		devicesPane.setComponentPopupMenu(jPopup);
		
		for (Device device : Main.devicesList) {
			addDeviceCard(device);
			device.getDeviceCard()
					.setStatus(device.isConnected() ? DeviceStatus.CONNECTED : DeviceStatus.DISCONNECTED);
		}
		
		/*List <DeviceTO> devicesFromServer = HibernateUtil.getListDeviceFromServer();
		if(devicesFromServer != null && !devicesFromServer.isEmpty()) {
			for (DeviceTO deviceTO : devicesFromServer) {
				Device device = null;

				if(Manufacturer.TOP_DATA.equals(deviceTO.getManufacturer())) {
					 device = new TopDataDevice(deviceTO.getIdentifier());
				}

				addDeviceCard(device);
				device.getDeviceCard()
						.setStatus(device.isConnected() ? DeviceStatus.CONNECTED : DeviceStatus.DISCONNECTED);
			}
		}
		*/
		
		// Habilita/desabilita o botao de liberar acesso nas catracas que estiverem
		// vinculadas
		verificaCatracasVinculadas();

		JScrollPane scrollPane = new JScrollPane(devicesPane, ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
				ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		scrollPane.getVerticalScrollBar().setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));
		panel.add(scrollPane, BorderLayout.CENTER);
		return panel;
	}
	
	private JPanel montarPanelCamerasHikivision() {
		JPanel panel = new JPanel();
		panel.setLayout(new BorderLayout());

		//JToolBar toolBar = montarToolBarCatracas();
		//panel.add(toolBar, BorderLayout.PAGE_START);
		// TODO: Montar toolbar para add device hikivision
		// TODO: Menu de contexto pra remover device

		hikivisionDevicesPane = new JPanel();
		hikivisionDevicesPane.setLayout(new WrapLayout(FlowLayout.LEFT, 30, 30));
		hikivisionDevicesPane.setBackground(Color.WHITE);

		JMenuItem refreshMenuItem = new JMenuItem("Atualizar");
		refreshMenuItem.addActionListener((e) -> refresh());

		JPopupMenu jPopup = new JPopupMenu();
		jPopup.add(refreshMenuItem);
		hikivisionDevicesPane.setComponentPopupMenu(jPopup);
		
		List<com.protreino.services.to.hikivision.HikivisionDeviceTO.Device> devices = hikivisionUseCases.listarDispositivos();
		if(Objects.nonNull(devices) && !devices.isEmpty()) {
			devices.forEach(device -> {
				addHikivisionDeviceCard(device);
			});
		}
		
		JScrollPane scrollPane = new JScrollPane(hikivisionDevicesPane, ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
				ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		scrollPane.getVerticalScrollBar().setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));
		panel.add(scrollPane, BorderLayout.CENTER);
		return panel;
	}

	public void verificaCatracasVinculadas() {
		Map<String, String> identificadoresCatracasVinculadas = new HashMap<String, String>();
		for (Device device : Main.devicesList) {
			if (device.isLeitorBiometrico()) {
				String identificadorCatracaVinculada = device.getConfigurationValue("Catraca vinculada");
				if (identificadorCatracaVinculada != null && !"NULL".equals(identificadorCatracaVinculada)
						&& !"COMM".equals(identificadorCatracaVinculada)
						&& !"USB".equals(identificadorCatracaVinculada)) {
					identificadoresCatracasVinculadas.put(identificadorCatracaVinculada.replace("$", ";"),
							device.getName());
				}
			}
		}
		for (Device device : Main.devicesList) {
			if (device.isCatraca()) {
				if (identificadoresCatracasVinculadas.containsKey(device.getIdentifier()))
					device.getDeviceCard().setCatracaVinculada(true,
							identificadoresCatracasVinculadas.get(device.getIdentifier()));
				else
					device.getDeviceCard().setCatracaVinculada(false, null);
			}
		}
	}
	
	private void syncUsers(){
		try {
			setCursor(new Cursor(Cursor.WAIT_CURSOR));
			if (!Main.getUpdatingUsersAccessList()) {
				Main.syncUsersAccessList();
				while (Main.getUpdatingUsersAccessList()) {
					Thread.sleep(100);
				}
			}

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		}
	}
	
	public void setConnectionStatusLabel(Boolean online) {
		String servidor = Main.urlApplication.replace("sistema", "");
		
		if(connectionStatusLabel == null)
			connectionStatusLabel = new PanelWithLabel(" ", FlowLayout.LEFT, true, 5, 0);
		
		if(online == null) {
			connectionStatusLabel.setLabelColor(Color.GRAY);
			connectionStatusLabel.setText("Verificando...");
			connectionStatusLabel.setToolTipText(servidor);
		}else if(online) {
			connectionStatusLabel.setLabelColor(new Color(90, 196, 126));
			connectionStatusLabel.setText("Online");
			connectionStatusLabel.setToolTipText("Conectado ao servidor: " + servidor);
		} else {
			connectionStatusLabel.setLabelColor(Color.RED);
			connectionStatusLabel.setText("Offline");
			connectionStatusLabel.setToolTipText("<html>Sem conexão com servidor " + servidor + " os dados nao estão sendo sincronizados."
					+ "<br/>Verifique sua internet ou verifique se o servidor está ligado!</html>" );
		}
	}

	public void setHikivisionConnectionStatusLabel(final Boolean online) {
		String servidor = Utils.getPreference("hikivisionServerRecognizerURL");

		if(hikivisionConnectionStatusLabel == null) {
			hikivisionConnectionStatusLabel = new PanelWithLabel(" ", FlowLayout.LEFT, true, 5, 0);
		}

		if(online == null) {
			hikivisionConnectionStatusLabel.setLabelColor(Color.GRAY);
			hikivisionConnectionStatusLabel.setText("Verificando Hikivision...");
			hikivisionConnectionStatusLabel.setToolTipText(servidor);
		} else if(online) {
			hikivisionConnectionStatusLabel.setLabelColor(new Color(90, 196, 126));
			hikivisionConnectionStatusLabel.setText("Hikivision: Online");
			hikivisionConnectionStatusLabel.setToolTipText("Conectado ao servidor Hikivision: " + servidor);
		} else {
			hikivisionConnectionStatusLabel.setLabelColor(Color.RED);
			hikivisionConnectionStatusLabel.setText("Hikivision: Offline");
			hikivisionConnectionStatusLabel.setToolTipText("<html>Sem conexao com servidor " + servidor + " os dados nao estáo sendo sincronizados."
					+ "<br/>Verifique sua internet ou verifique se o servidor está ligado!</html>" );
		}
	}
	
	private JPanel montarPanelEventos() {
		eventosLabel = new JLabel(" ");
		eventosLabel.setHorizontalAlignment(SwingConstants.LEFT);

		JPanel eventosPanel = new JPanel();
		eventosPanel.setLayout(new BoxLayout(eventosPanel, BoxLayout.X_AXIS));
		eventosPanel.setBorder(new BevelBorder(BevelBorder.LOWERED));
		eventosPanel.setPreferredSize(new Dimension(10000, 24));
		eventosPanel.add(eventosLabel);
		eventosPanel.add(Box.createHorizontalGlue());

		toggleEventosButton = new JButton(setaUpIcon);
		toggleEventosButton.setPreferredSize(new Dimension(24, 18));

		JLabel loggedUserLabel = new JLabel(
				Main.loggedUser != null ? Main.loggedUser.getName() + " - " + Main.loggedUser.getLoginName() : "");
		
		setConnectionStatusLabel(null);
		setHikivisionConnectionStatusLabel(null);

		JPanel toggleButtonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
		toggleButtonPanel.add(toggleEventosButton);
		toggleButtonPanel.add(Box.createHorizontalStrut(10));
		toggleButtonPanel.add(loggedUserLabel);

		if(Utils.isHikivisionConfigValid()) {
			toggleButtonPanel.add(Box.createHorizontalStrut(10));
			toggleButtonPanel.add(hikivisionConnectionStatusLabel);
		}

		toggleButtonPanel.add(Box.createHorizontalStrut(10));
		toggleButtonPanel.add(connectionStatusLabel);
		

		JPanel toggleButtonPanelContainer = new JPanel();
		toggleButtonPanelContainer.setLayout(new BorderLayout(0, 0));
		toggleButtonPanelContainer.setBorder(new EmptyBorder(0, 0, 3, 0));
		toggleButtonPanelContainer.add(toggleButtonPanel, BorderLayout.PAGE_END);

		JPanel bottomPanel = new JPanel();
		bottomPanel.setLayout(new BoxLayout(bottomPanel, BoxLayout.X_AXIS));
		bottomPanel.setBorder(new EmptyBorder(0, 3, 1, 3));
		bottomPanel.add(eventosPanel);
		bottomPanel.add(toggleButtonPanelContainer);

		toggleEventosButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (toggleEventosButton.getIcon().equals(setaUpIcon)) {
					toggleEventosButton.setIcon(setaDownIcon);
					listaEventosPanel = new JPanel();
					listaEventosPanel.setLayout(new BoxLayout(listaEventosPanel, BoxLayout.Y_AXIS));
					for (String evento : eventos)
						listaEventosPanel.add(new JLabel(evento));
					listaEventosScrollPane = new JScrollPane(listaEventosPanel,
							ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
							ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
					listaEventosScrollPane.getVerticalScrollBar()
							.setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));
					listaEventosScrollPane.setPreferredSize(new Dimension(10000, 300));
					eventosPanel.removeAll();
					eventosPanel.add(listaEventosScrollPane);
					eventosPanel.add(Box.createHorizontalGlue());
					eventosPanel.setPreferredSize(new Dimension(10000, 300));
					revalidate();
				} else {
					toggleEventosButton.setIcon(setaUpIcon);
					eventosPanel.removeAll();
					eventosPanel.add(eventosLabel);
					eventosPanel.add(Box.createHorizontalGlue());
					eventosPanel.setPreferredSize(new Dimension(10000, 24));
					revalidate();
				}
			}
		});

		return bottomPanel;
	}

	public void refresh() {
		int index = tabbedPane.getSelectedIndex();
		if (index == 1 && listaAcessoPanel != null && listaAcessoPanel.isFiltering()) {
			return;
		}
		buildUI();
		tabbedPane.setSelectedIndex(index);
	}
	
	public void refreshAll() {
		int index = tabbedPane.getSelectedIndex();
		buildUI();
		tabbedPane.setSelectedIndex(index);
	}

	private void abrirPastaLogs() {
		try {
//			Runtime.getRuntime().exec("explorer.exe /open," + Main.logPath);
			Runtime.getRuntime().exec("cmd /c start " + Main.logPath);
		} catch (Exception e) {
			e.printStackTrace();
			Main.mainScreen
					.addEvento("Nao foi possivel abrir a pasta de logs. Navegue até ela manualmente: " + Main.logPath);
		}
	}

	private JButton makeButton(String text, ImageIcon icon, String tooltipText, Dimension buttonSize) {
		JButton button = new JButton(text);
		button.setIcon(icon);
		button.setToolTipText(tooltipText);
		button.setVerticalTextPosition(JButton.BOTTOM);
		button.setHorizontalTextPosition(JButton.CENTER);
		button.setBorder(BorderFactory.createEtchedBorder());
		button.setContentAreaFilled(false);
		button.setMinimumSize(buttonSize);
		button.setMaximumSize(buttonSize);
		button.setPreferredSize(buttonSize);
		button.addMouseListener(new java.awt.event.MouseAdapter() {
			public void mouseEntered(java.awt.event.MouseEvent evt) {
				if (button.isEnabled()) {
					button.setForeground(Main.secondColor);
					button.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
				}
			}

			public void mouseExited(java.awt.event.MouseEvent evt) {
				button.setForeground(UIManager.getColor("control"));
				button.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
			}
		});
		return button;
	}

	private void loadImages() {
		try {
			Toolkit toolkit = Toolkit.getDefaultToolkit();
			logoIcon = new ImageIcon(toolkit.getImage(
					Main.class.getResource(Configurations.IMAGE_FOLDER + Main.customImageFolder + "logo_grd003.png")));
			adicionarIcon = new ImageIcon(toolkit
					.getImage(Main.class.getResource(Configurations.IMAGE_FOLDER + Main.customImageFolder + "novo.png")));
			liberarAcessoIcon = new ImageIcon(toolkit
					.getImage(Main.class.getResource(Configurations.IMAGE_FOLDER + Main.customImageFolder + "timer.png")));
			atualizarListaAcessoIcon = new ImageIcon(toolkit.getImage(
					Main.class.getResource(Configurations.IMAGE_FOLDER + Main.customImageFolder + "atualizar.png")));
			setaUpIcon = new ImageIcon(
					toolkit.getImage(Main.class.getResource(Configurations.IMAGE_FOLDER + "comuns/seta_up.png")));
			setaDownIcon = new ImageIcon(
					toolkit.getImage(Main.class.getResource(Configurations.IMAGE_FOLDER + "comuns/seta_down.png")));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public JMenuItem getLiberarAcessoMenuItem() {
		return liberarAcessoMenuItem;
	}

	public JButton getLiberarAcessoButton() {
		return liberarAcessoButton;
	}

	public AccessListPanel getListaAcessoPanel() {
		return listaAcessoPanel;
	}

	public AccessHistoryPanel getHistoricoAcessoPanel() {
		return historicoAcessoPanel;
	}

	public void setHistoricoAcessoPanel(AccessHistoryPanel historicoAcessoPanel) {
		this.historicoAcessoPanel = historicoAcessoPanel;
	}

}
