package com.protreino.services.screens;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Objects;
import java.util.Set;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.SwingWorker;
import javax.swing.WindowConstants;
import javax.swing.border.EmptyBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import com.protreino.services.constants.Configurations;
import com.protreino.services.devices.Device;
import com.protreino.services.devices.FacialDevice;
import com.protreino.services.entity.ParametroEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.enumeration.DeviceMode;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.main.Main;
import com.protreino.services.utils.HibernateUtil;
import com.protreino.services.utils.Utils;

@SuppressWarnings("serial")
public class RegisterUserDialog extends JDialog{
	
	private ImageIcon iconSucesso;
	private ImageIcon iconErro;
	private RegisterUserDialog instance;
	
	public JTable accessListTable;
	private List<PedestrianAccessEntity> listaAcesso;
	private String[] columns;
	private Integer[] columnWidths;
	
	private JTextField filtroIdTextField;
	private JTextField filtroNomeTextField;
	private JButton cleanButton;
	private JButton searchButton;
	public JButton registerUserButton;
	private JButton removeUserButton;
	private JLabel errorLabel;
	private JButton syncButton;
	private JButton getUsersOnDeviceButton;
	private Boolean alteradoComSucesso;
	private String mensagemErroRetorno;
	private Set<Integer> hashSetRegistradosNaCatraca;
	
	private Device device;
	
	protected JButton first = null;
	protected JButton back = null;
	protected JButton prox = null;
	protected JButton last = null;
	protected JLabel countLabel;
	
	protected int totalRegistros = 0;
	protected int totalPaginas = 10;
	protected int paginaAtual = 1;
	protected int inicioPagina = 0;
	protected int registrosPorPagina = 10;
	
	private HashMap<String, Object> args;
	
	public RegisterUserDialog(Frame owner, Device device){
		super(owner, "Cadastro de usuários " + (device.isCatraca() ? "na catraca " : (device.isLeitorBiometrico() ? "com o leitor " : "com a câmera ")) + device.getName(), true);
		this.device = device;
		this.instance = this;
		
		loadImages();
		
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
		setPreferredSize(new Dimension(750,600));
		setMinimumSize(getPreferredSize());
		setIconImage(Main.favicon);
		
		if (Manufacturer.COMPUTER_ID.equals(device.getManufacturer())
				|| Manufacturer.NITGEN.equals(device.getManufacturer())
				|| Manufacturer.TOP_DATA.equals(device.getManufacturer())
				|| Manufacturer.TOP_DATA_ACESSO.equals(device.getManufacturer())
				|| Manufacturer.LC_DEVICE.equals(device.getManufacturer())
				|| Manufacturer.FACIAL.equals(device.getManufacturer())) {
			columns = new String[] {"Código", 
									"Nome", 
									Manufacturer.FACIAL.equals(device.getManufacturer()) ? "Face coletada" : "Digitais coletadas", 
									"Acesso Permitido"};
			columnWidths = new Integer[] {50, 320, 120, 120};
		
		} else {
			columns = new String[] {"Código", "Nome", "Acesso Permitido"};
			columnWidths = new Integer[] {80, 400, 130};
		}
		
		Font font = new JLabel().getFont();
		Font boldFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
		
		JPanel filtroIdContainer= new JPanel();
		filtroIdContainer.setLayout(new BoxLayout(filtroIdContainer, BoxLayout.Y_AXIS));
		JLabel filtroIdLabel = new JLabel("Código");
		filtroIdLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroIdContainer.add(filtroIdLabel);
		filtroIdTextField = new JTextField("", 10);
		filtroIdTextField.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroIdContainer.add(filtroIdTextField);
		filtroIdContainer.setAlignmentX(Component.LEFT_ALIGNMENT);
		
		JPanel filtroNomeContainer= new JPanel();
		filtroNomeContainer.setLayout(new BoxLayout(filtroNomeContainer, BoxLayout.Y_AXIS));
		JLabel filtroNomeLabel = new JLabel("Nome");
		filtroNomeLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroNomeContainer.add(filtroNomeLabel);
		filtroNomeTextField = new JTextField("", 50);
		filtroNomeTextField.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroNomeContainer.add(filtroNomeTextField);
		filtroNomeContainer.setAlignmentX(Component.LEFT_ALIGNMENT);
		
		JPanel cleanButtonContainer= new JPanel();
		cleanButtonContainer.setLayout(new BoxLayout(cleanButtonContainer, BoxLayout.Y_AXIS));
		cleanButton = new JButton("Limpar filtros");
		cleanButtonContainer.add(new JLabel(" "));
		cleanButtonContainer.add(cleanButton);
		
		JPanel searchButtonPanel= new JPanel();
		searchButtonPanel.setLayout(new BoxLayout(searchButtonPanel, BoxLayout.Y_AXIS));
		searchButton = new JButton("Pesquisar");
		searchButtonPanel.add(new JLabel(" "));
		searchButtonPanel.add(searchButton);
		
		JPanel filterContainer = new JPanel();
		filterContainer.setLayout(new BoxLayout(filterContainer, BoxLayout.X_AXIS));
		filterContainer.setMaximumSize(new Dimension(750, 40));
		filterContainer.add(filtroIdContainer);
		filterContainer.add(Box.createHorizontalStrut(10));
		filterContainer.add(filtroNomeContainer);
		filterContainer.add(Box.createHorizontalStrut(10));
		filterContainer.add(cleanButtonContainer);
		filterContainer.add(Box.createHorizontalStrut(10));
		filterContainer.add(searchButtonPanel);
		filterContainer.add(Box.createHorizontalGlue());
		
		JPanel accessListTableContainer = new JPanel();
		accessListTableContainer.setLayout(new BoxLayout(accessListTableContainer, BoxLayout.Y_AXIS));
		accessListTable = new JTable(new DefaultTableModel(columns, 0));
		formatTable();
		accessListTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
		accessListTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		accessListTable.getTableHeader().setReorderingAllowed(false);
		accessListTable.getTableHeader().setOpaque(false);
		accessListTable.getTableHeader().setForeground(Main.firstColor);
		accessListTable.getTableHeader().setBackground(Main.secondColor);
		accessListTable.getTableHeader().setFont(boldFont);
		accessListTable.setRowHeight(30);
		accessListTable.setSelectionBackground(Main.firstColor);
		accessListTable.setSelectionForeground(Color.WHITE);
		TableCellRenderer rendererFromHeader = accessListTable.getTableHeader().getDefaultRenderer();
		JLabel headerLabel = (JLabel) rendererFromHeader;
		headerLabel.setHorizontalAlignment(JLabel.CENTER);
		JScrollPane scrollPane = new JScrollPane(accessListTable);
		scrollPane.getVerticalScrollBar().setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));
		accessListTableContainer.add(scrollPane);
		
		errorLabel = new JLabel(" ");
		errorLabel.setForeground(Color.RED);
		errorLabel.setFont(boldFont);
		JPanel errorLabelContainer = new JPanel();
		errorLabelContainer.setLayout(new BoxLayout(errorLabelContainer, BoxLayout.X_AXIS));
		errorLabelContainer.add(errorLabel);
		
		syncButton = new JButton("Atualizar lista com o servidor");
		syncButton.setBorder(new EmptyBorder(10,10,10,10));
		getUsersOnDeviceButton = new JButton("Buscar lista na catraca");
		getUsersOnDeviceButton.setBorder(new EmptyBorder(10,10,10,10));
		registerUserButton = new JButton(device.isCatraca() ? "Cadastrar usuário na catraca" : (device.isLeitorBiometrico() ? "Coletar biometria" : "Capturar face"));
		registerUserButton.setBorder(new EmptyBorder(10,10,10,10));
		removeUserButton = new JButton("Excluir usuário da catraca");
		removeUserButton.setBorder(new EmptyBorder(10,10,10,10));
		
		JPanel buttonsContainer = new JPanel();
		buttonsContainer.setLayout(new BoxLayout(buttonsContainer, BoxLayout.X_AXIS));
		buttonsContainer.add(syncButton);
		buttonsContainer.add(Box.createHorizontalStrut(10));
		if (device.getManufacturer().giveListRegisteredUsers())
			buttonsContainer.add(getUsersOnDeviceButton);
		buttonsContainer.add(Box.createHorizontalGlue());
		if (device.isLeitorBiometrico() || !device.isRegistrationProcessStartedOnDevice()) {
			removeUserButton.setText("Apagar todas digitais deste usuário");
		}
		if (device.isCamera()) {
			removeUserButton.setText("Apagar as faces deste usuário");
		}
		buttonsContainer.add(removeUserButton);
		buttonsContainer.add(Box.createHorizontalStrut(10));
		buttonsContainer.add(registerUserButton);
		
		JPanel paginatorPanel = createPaginatorControls();
		
		JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
		mainPanel.setBorder(BorderFactory.createEmptyBorder(10,10,10,10));
		mainPanel.add(filterContainer);
		mainPanel.add(Box.createRigidArea(new Dimension(0,5)));
		mainPanel.add(accessListTableContainer);
		mainPanel.add(Box.createRigidArea(new Dimension(0,5)));
		mainPanel.add(errorLabelContainer);
		mainPanel.add(Box.createRigidArea(new Dimension(0,5)));
		mainPanel.add(paginatorPanel);
		mainPanel.add(Box.createRigidArea(new Dimension(0,5)));
		mainPanel.add(buttonsContainer);
		
		Container contentPane = getContentPane();
		contentPane.add(mainPanel, BorderLayout.CENTER);
		
		cleanButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				cleanFilter();
			}
		});
		
		syncButton.setMultiClickThreshhold(1000l);
		syncButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				sync();
				cleanFilter();
			}
		});
		
		getUsersOnDeviceButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				recuperarListaCadastradosNaCatraca();
				cleanFilter();
			}
		});
		
		registerUserButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (device.isCatraca()) {
					// exibe apenas um pequeno dialog na tela. O processo é acompanhado no visor da catraca
					cadastrarOuRemoverUsuario("CADASTRO");
				
				} else {
					// exibe a tela de coleta de biometria
					coletar();
				}
			}
		});
		
		removeUserButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				cadastrarOuRemoverUsuario("EXCLUSAO");
			}
		});
		
		addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosing(WindowEvent e) {
				for(Device d : Main.devicesList) {
					if(d.isConnected())
						d.setMode(DeviceMode.VERIFICATION);
				}
			}
		});
		
		addComponentListener(new ComponentListener() {
			@Override
			public void componentShown(ComponentEvent e) {
				toFront();
				for (Device d : Main.devicesList) {
					if(d instanceof FacialDevice)
						d.setMode(DeviceMode.NONE);
				}
				// nao faz nada enquanto a janela estiver aberta sem capturar, e so retorna ao normal quando fechar
			}
			@Override
			public void componentResized(ComponentEvent e) {}
			@Override
			public void componentMoved(ComponentEvent e) {}
			@Override
			public void componentHidden(ComponentEvent e) {}
		});
		
		ActionListener search = new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				filterList();
			}
		};
		
		searchButton.addActionListener(search);
		filtroIdTextField.addActionListener(search);
		filtroNomeTextField.addActionListener(search);
		
		pack();
		//showScreen();
	}
	
	public void showScreen() {
		limparMensagemErro();
		if (device.getManufacturer().giveListRegisteredUsers())
			recuperarListaCadastradosNaCatraca();
		cleanFilter();
		setLocationRelativeTo(null);
		setVisible(true);
		toFront();
	}
	
	@SuppressWarnings("unchecked")
	private String buscaParametroPeloNome(String nome) {
		HashMap<String, Object> args = new HashMap<>();
		args.put("NOME_PARAM", nome);
		args.put("ID_CLIENTE", Main.loggedUser.getIdClient());
		
		List<ParametroEntity> parametros = (List<ParametroEntity>) HibernateUtil
									.getResultListWithParams(ParametroEntity.class, "ParametroEntity.findByName", args);

		if(parametros == null || parametros.isEmpty())
			return "";
		
		return parametros.get(0).getValor();
	}
	
	@SuppressWarnings("unchecked")
	private void filterList() {
		try {
			setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
			args = new HashMap<>();
			
			args.put("removido", false);
			
			if (filtroIdTextField.getText() != null && !"".equals(filtroIdTextField.getText())) {
				Long id = 0l;
				try {
					id = Long.valueOf(filtroIdTextField.getText());
				} catch (Exception e) {}
				args.put("id", id);
			}
			if (filtroNomeTextField.getText() != null && !"".equals(filtroNomeTextField.getText()))
				args.put("name", filtroNomeTextField.getText());
			
			String join = " left join obj.templates temp ";
			
			String groupBy = " group by obj.id, obj.name, obj.status, obj.cadastradoNaCatracaRWTech, obj.cardNumber, obj.cadastradoNoDesktop, obj.luxandIdentifier ";
			
			if(args.size() == 1) {
				cleanFilter();
			}else {
				paginaAtual = 1;
				inicioPagina = 0;
				totalRegistros =  HibernateUtil.
						getResultListWithDynamicParamsCount(PedestrianAccessEntity.class, null, join, groupBy, args);
				
				executeFilter();
			}
			
		
		} catch (Exception e) {
			e.printStackTrace();
		
		} finally {
			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
	}
	
	protected JPanel createPaginatorControls() {
		countLabel = new JLabel("Pág. ("+ paginaAtual + "/" + totalPaginas + ") do total: " + totalRegistros);
		first = new JButton("<<");
		first.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				paginaAtual = 1;
				inicioPagina = 0;
				executeFilter();
			}
		});
		back = new JButton("< ");
		back.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(paginaAtual > 1) {
					paginaAtual--;
					inicioPagina = inicioPagina - registrosPorPagina;
				}
				executeFilter();
			}
		});
		prox = new JButton(" >");
		prox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(paginaAtual < totalPaginas) {
					paginaAtual++;
					inicioPagina = inicioPagina + registrosPorPagina;
				}
				executeFilter();
			}
		});
		last = new JButton(">>");
		last.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				paginaAtual = totalPaginas;
				inicioPagina = (registrosPorPagina * totalPaginas) - registrosPorPagina ;
				executeFilter();
			}
		});
		
		JPanel paginatorPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
		paginatorPanel.setBorder(new EmptyBorder(10,0,10,15));
		paginatorPanel.setMaximumSize(new Dimension(Integer.MAX_VALUE, 100));
		paginatorPanel.add(first);
		paginatorPanel.add(Box.createHorizontalStrut(5));
		paginatorPanel.add(back);
		paginatorPanel.add(Box.createHorizontalStrut(5));
		paginatorPanel.add(prox);
		paginatorPanel.add(Box.createHorizontalStrut(5));
		paginatorPanel.add(last);
		paginatorPanel.add(Box.createHorizontalStrut(5));
		paginatorPanel.add(countLabel);
		paginatorPanel.add(Box.createHorizontalStrut(5));
		return paginatorPanel;
	}
	
	
	@SuppressWarnings("unchecked")
	protected void executeFilter() {
		
		calculaTamanhoPaginas();
		
		if(args == null)
			args = new HashMap<>();
		args.put("removido", false);
		
		
		String construtor = " com.protreino.services.entity.PedestrianAccessEntity(obj.id, obj.name, obj.status, "
				+ "count(temp.id), obj.cadastradoNaCatracaRWTech, obj.cardNumber, obj.cadastradoNoDesktop, obj.luxandIdentifier)  ";
		
		String join = " left join obj.templates temp ";
		
		String groupBy = " group by obj.id, obj.name, obj.status, obj.cadastradoNaCatracaRWTech, obj.cardNumber, obj.cadastradoNoDesktop, obj.luxandIdentifier ";
		
		listaAcesso = (List<PedestrianAccessEntity>) HibernateUtil.
				getResultListWithDynamicParams(PedestrianAccessEntity.class, construtor, join, groupBy, "name", args, inicioPagina, registrosPorPagina);
		
		verificarRegistradosNaCatraca();
		populateTable(listaAcesso);
		
		paginatorControl();
		
		
	}
	
	protected void calculaTamanhoPaginas() {
		totalPaginas = totalRegistros / registrosPorPagina;
		if(totalPaginas == 0) {
			totalPaginas = 1;
		}else {
			int rest = totalRegistros % registrosPorPagina;
			if(rest > 0)
				totalPaginas++;
		}
	}
	
	protected void paginatorControl() {
		
		if(totalPaginas == 1) {
			first.setEnabled(false);
			back.setEnabled(false);
			prox.setEnabled(false);
			last.setEnabled(false);
		}else{
			if(paginaAtual == 1) {
				first.setEnabled(false);
				back.setEnabled(false);
				prox.setEnabled(true);
				last.setEnabled(true);
			}else if(paginaAtual == totalPaginas) {
				first.setEnabled(true);
				back.setEnabled(true);
				prox.setEnabled(false);
				last.setEnabled(false);
			}else {
				first.setEnabled(true);
				back.setEnabled(true);
				prox.setEnabled(true);
				last.setEnabled(true);
			}
			
		}
		
	}

	@SuppressWarnings("unchecked")
	private void cleanFilter(){
		filtroIdTextField.setText("");
		filtroNomeTextField.setText("");
		
		totalRegistros =  HibernateUtil.
				getResultListCount(PedestrianAccessEntity.class, "PedestrianAccessEntity.countNaoRemovidosOrderedToRegisterUser");
		
		//calcula páginas
		calculaTamanhoPaginas();
		
		listaAcesso = (List<PedestrianAccessEntity>) HibernateUtil.
				getResultListLimited(PedestrianAccessEntity.class, "PedestrianAccessEntity.findAllNaoRemovidosOrderedToRegisterUser", (long)registrosPorPagina);
		verificarRegistradosNaCatraca();
		populateTable(listaAcesso);
		
		paginatorControl();
	}
	
	private void sync(){
		try {
			setCursor(new Cursor(Cursor.WAIT_CURSOR));
			if (!Main.updatingAthleteAccessList) {
				Main.syncAthleteAccessList();
				while (Main.updatingAthleteAccessList)
					Thread.sleep(100);
			}
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		finally {
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		}
	}
	
	/**
	 * Recupera a lista dos usuários cadastrados na catraca. 
	 * Para catracas TOP DATA pode ser um processo lento. 
	 * Para catracas RWTECH não ï¿½ possivel, ao inves disso ï¿½ feito uma busca no banco antigo.
	 */
	private void recuperarListaCadastradosNaCatraca() {
		return;
		// recupera a lista de usuarios cadastrados na catraca
		// mostra um dialog para nao ficar travado sem nada na tela
		
		/*JDialog dialog = new JDialog(null, "Aguarde", ModalityType.APPLICATION_MODAL);
		try {
			setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
			Main.mainScreen.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
			
			SwingWorker<Void, Void> mySwingWorker = new SwingWorker<Void, Void>(){
				@Override
				protected Void doInBackground() throws Exception {
					hashSetRegistradosNaCatraca = device.getRegisteredUserList();
					return null;
				}
			};
			
			mySwingWorker.addPropertyChangeListener(new PropertyChangeListener() {
				@Override
				public void propertyChange(PropertyChangeEvent evt) {
					if (evt.getPropertyName().equals("state") 
							&& evt.getNewValue() == SwingWorker.StateValue.DONE) {
						dialog.dispose();
					}
				}
			});
			mySwingWorker.execute();
			
			JProgressBar progressBar = new JProgressBar();
			progressBar.setIndeterminate(true);
			progressBar.setPreferredSize(new Dimension(150, 40));
			progressBar.setAlignmentX(Component.CENTER_ALIGNMENT);
			
			JLabel label = new JLabel("Buscando usuários na catraca...");
			label.setAlignmentX(Component.CENTER_ALIGNMENT);
			
			JPanel dialogPanel = new JPanel();
			dialogPanel.setLayout(new BoxLayout(dialogPanel, BoxLayout.Y_AXIS));
			dialogPanel.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(Main.firstColor, 1),
					new EmptyBorder(30,20,20,20)));
			dialogPanel.setPreferredSize(new Dimension(200, 100));
			dialogPanel.add(progressBar);
			dialogPanel.add(Box.createVerticalStrut(10));
			dialogPanel.add(label);
			dialog.setUndecorated(true);
			dialog.setLayout(new BorderLayout());
			dialog.add(dialogPanel);
			dialog.pack();
			dialog.setLocationRelativeTo(null);
			dialog.setVisible(true);
			
		}
		catch (Exception e) {
			e.printStackTrace();
			Utils.createNotification("Não foi possível recuperar os usuários da catraca.", NotificationType.BAD);
		}
		finally {
			setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
			Main.mainScreen.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
			if (dialog != null)
				dialog.dispose();
		}*/
	}
	
	
	private void verificarRegistradosNaCatraca(){
		if (Manufacturer.RWTECH.equals(device.getManufacturer())) {
			for (PedestrianAccessEntity acesso : listaAcesso)
				acesso.setCadastradoNaCatraca(acesso.getCadastradoNaCatracaRWTech());
			
		} else {
			for (PedestrianAccessEntity acesso : listaAcesso) {
				if (hashSetRegistradosNaCatraca != null) {
					if (hashSetRegistradosNaCatraca.contains(acesso.getId().intValue()))
						acesso.setCadastradoNaCatraca(true);
					else if (acesso.getCardNumber() != null && !acesso.getCardNumber().isEmpty()) {
						try {
							Integer cartao = Integer.valueOf(acesso.getCardNumber());
							if (hashSetRegistradosNaCatraca.contains(cartao))
								acesso.setCadastradoNaCatraca(true);
						} catch (Exception e){}
					
					} else
						acesso.setCadastradoNaCatraca(false);
				} else
					acesso.setCadastradoNaCatraca(null);
			}
		}
	}
	
	public void coletar(){
		this.coletar(null);
	}
	
	public void coletar(PedestrianAccessEntity pedestre){
		if (!device.isConnected()) {
			setMensagemErro("Dispositivo desconectado.");
			return;
		}
		
		Integer index = accessListTable.getSelectedRow();
		
		if (index == -1 && pedestre == null) {
			setMensagemErro("Selecione um usuário.");
			return;
		}
		
		PedestrianAccessEntity acessoSelecionado = pedestre != null 
												? pedestre 
												: (PedestrianAccessEntity) HibernateUtil
														.getSingleResultById(PedestrianAccessEntity.class, listaAcesso.get(index).getId());
		
		if(!Manufacturer.FACIAL.equals(device.getManufacturer())){
			String limite = buscaParametroPeloNome("Limite de cadastro de digitais");
			if(limite != null && !limite.equals("") 
					&& !limite.equals("Sem limite")
					&& !limite.equals("Sem limites")) {
				try {
					Long l = Long.valueOf(limite);
					if(acessoSelecionado.getTemplates() != null 
							&& !acessoSelecionado.getTemplates().isEmpty()
							&& acessoSelecionado.getTemplates().size() >= l) {
						setMensagemErro("Limite de digitais excedido para o usuário.");
						return;
					}
				}catch (Exception e) {
					//suprime para não mostrar o erro
				}
			}
		}else if(Manufacturer.FACIAL.equals(device.getManufacturer())
				&& acessoSelecionado.getLuxandIdentifier() != null 
				&& !"".equals(acessoSelecionado.getLuxandIdentifier())  ){
			setMensagemErro("Apague a face desse usuário antes de cadastrar uma nova.");
			return;
		}
		
		if (acessoSelecionado == null) {
			setMensagemErro("Selecione um usuário.");
			return;
		}
		if (acessoSelecionado.getCadastradoNoDesktop()) {
			setMensagemErro("Pedestre precisa ser sincronizado no desktop");
			return;
		}
		if (Manufacturer.FACIAL.equals(device.getManufacturer())) {
			if(Boolean.TRUE.equals(acessoSelecionado.getCadastradoNoDesktop())) {
				RegisterVisitorDialog.criarDialogoUsuarioNaoPermitidoCadastrarFace();
				
			} else {
				FacialDialog facialDialog = new FacialDialog(Main.mainScreen, device, acessoSelecionado);
				facialDialog.showScreen();
			}
		
		} else {
			
			BiometricDialog biometricDialog = new BiometricDialog(Main.mainScreen, device, acessoSelecionado);
			biometricDialog.showScreen();
		}
		filterList();
	}
	
	public void cadastrarOuRemoverUsuario(String tipoOperacao) { // tipoAlteracao = "CADASTRO" ou "EXCLUSAO"
		cadastrarOuRemoverUsuario(tipoOperacao, null);
	}
	
	// Metodo para cadastrar ou remover um usuário na catraca
	public void cadastrarOuRemoverUsuario(String tipoOperacao, PedestrianAccessEntity pedestre) { // tipoAlteracao = "CADASTRO" ou "EXCLUSAO"
		if (!device.isConnected()) {
			setMensagemErro("Dispositivo desconectado.");
			return;
		}
		
		Integer index = accessListTable.getSelectedRow();
		if (index == -1 && pedestre == null) {
			setMensagemErro("Selecione um usuário.");
			return;
		}
		boolean cadastroFeitoDiretamenteNaCatraca = device.isRegistrationProcessStartedOnDevice();
		
		if ("EXCLUSAO".equals(tipoOperacao)) {
			if (device.isLeitorBiometrico() || device.isCamera()) {
				cadastroFeitoDiretamenteNaCatraca = false;
				int option = JOptionPane.showConfirmDialog(instance, "Deseja realmente excluir todas as " 
						+ (device.isLeitorBiometrico() ? "digitais" : "faces") + " deste usuário?", 
						"Confirmar exclusão", JOptionPane.YES_NO_OPTION, JOptionPane.PLAIN_MESSAGE);
				if (option != JOptionPane.OK_OPTION)
					return;
			
			} else if (cadastroFeitoDiretamenteNaCatraca == false){
				int option = JOptionPane.showConfirmDialog(instance, "Deseja realmente excluir este usuário da catraca?", 
						"Confirmar exclusão", JOptionPane.YES_NO_OPTION, JOptionPane.PLAIN_MESSAGE);
				if (option != JOptionPane.OK_OPTION)
					return;
			}
		}
		
		PedestrianAccessEntity acessoSelecionado = pedestre != null 
								? pedestre 
								: (PedestrianAccessEntity) HibernateUtil
										.getSingleResultById(PedestrianAccessEntity.class, listaAcesso.get(index).getId());
		
		//verifica limite de digitais
		if(!"EXCLUSAO".equals(tipoOperacao)) {
			
			String limite = buscaParametroPeloNome("Limite de cadastro de digitais");
			if(limite != null && !limite.equals("") && !limite.equals("Sem limite")) {
				Long l = Long.valueOf(limite);
				if(acessoSelecionado.getTemplates() != null 
						&& !acessoSelecionado.getTemplates().isEmpty()
						&& acessoSelecionado.getTemplates().size() >= l) {
					setMensagemErro("Limite de digitais excedido para o usuário.");
					return;
				}
			}
		}
		
		limparMensagemErro();
		
		// Define as mensagens e informacoes que serao exibidas de acordo com o tipo de operacao
		// e de acordo com a catraca, se o cadastro é feito direto nela ou nao
		Dimension dialogDimension = new Dimension(300, 150);
		String tituloDialog = "CADASTRO".equals(tipoOperacao) ? "Cadastro de usuário" : "Exclusão de usuário";
		String mensagemDialogSucesso = "CADASTRO".equals(tipoOperacao) ? "Usuário cadastrado com sucesso!" : 
			(device.isLeitorBiometrico() ? "Digitais apagadas com sucesso!" 
					: (device.isCamera() ? "Faces apagadas com sucesso!" : "Usuário removido com sucesso!"));
		String tituloDialogSucesso = "CADASTRO".equals(tipoOperacao) ? "Novo cadastro realizado" : "Exclusão realizada";
		String mensagemDialogFalha = "CADASTRO".equals(tipoOperacao) ? "Novo cadastro não realizado." : "Exclusão não realizada.";
		String tituloDialogFalha = "CADASTRO".equals(tipoOperacao) ? "Erro no cadastro" : "Erro na exclusão";
		String instrucoesDialog1 = "";
		String instrucoesDialog2 = "";
		
		if (cadastroFeitoDiretamenteNaCatraca) {
			instrucoesDialog1 = "CADASTRO".equals(tipoOperacao) 
					? "Cadastre um novo usuário diretamente na catraca com o Código" 
							: "Remova diretamente da catraca o usuário de Código";
			instrucoesDialog2 = acessoSelecionado.getId().toString();
			dialogDimension = new Dimension(480, 160);
		
		} else {
			instrucoesDialog1 = "CADASTRO".equals(tipoOperacao) ? "Siga os passos na catraca" : "Aguarde!";
			instrucoesDialog2 = "CADASTRO".equals(tipoOperacao) ? "para realizar a coleta da digital" 
					: (device.isLeitorBiometrico() ? "Apagando digitais..." 
							: (device.isCamera() ? "Apagando faces..." : "Removendo usuário..."));
		}
		
		alteradoComSucesso = false;
		mensagemErroRetorno = "";
		
		// cria o dialog com as informações
		JDialog dialog = new JDialog(null, tituloDialog, ModalityType.APPLICATION_MODAL);
		Font font = new JLabel().getFont();
		Font boldFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
		JLabel instrucoesLabel1 = new JLabel(instrucoesDialog1);
		instrucoesLabel1.setAlignmentX(Component.CENTER_ALIGNMENT);
		instrucoesLabel1.setFont(boldFont);
		JLabel instrucoesLabel2 = new JLabel(instrucoesDialog2);
		instrucoesLabel2.setAlignmentX(Component.CENTER_ALIGNMENT);
		instrucoesLabel2.setFont(boldFont);
		JPanel dialogPanel = new JPanel();
		dialogPanel.setLayout(new BoxLayout(dialogPanel, BoxLayout.Y_AXIS));
		dialogPanel.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(Main.firstColor, 1),
				new EmptyBorder(30,20,20,20)));
		dialogPanel.setPreferredSize(dialogDimension);
		
		if (cadastroFeitoDiretamenteNaCatraca) {
			// adiciona um botao para indicar o fim do cadastro/exclusao quando o processo ï¿½ feito diretamente na catraca
			Font bigFont = new Font(font.getFontName(), Font.BOLD, font.getSize()+20);
			instrucoesLabel2.setFont(bigFont);
			JButton okButton = new JButton("OK");
			okButton.setAlignmentX(Component.CENTER_ALIGNMENT);
			okButton.setPreferredSize(new Dimension(80, 50));
			okButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					try {
						dialog.dispose();
						if (device.getManufacturer().giveListRegisteredUsers())
							recuperarListaCadastradosNaCatraca();
						filterList();
					}
					catch (Exception e1){
						e1.printStackTrace();
					}
				}
			});
			dialogPanel.add(instrucoesLabel1);
			dialogPanel.add(Box.createVerticalStrut(15));
			dialogPanel.add(instrucoesLabel2);
			dialogPanel.add(Box.createVerticalStrut(20));
//			if (Manufacturer.HENRY_7X.equals(device.getManufacturer())){
//				((Henry7XDevice) device).verificaSenha();
//				if (device.getPassword() != null) {
//					Font mediumFont = new Font(font.getFontName(), Font.BOLD, font.getSize() + 10);
//					dialogPanel.setPreferredSize(new Dimension(440, 200));
//					JLabel senhaLabel = new JLabel("Senha: " + device.getPassword());
//					senhaLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
//					senhaLabel.setFont(mediumFont);
//					senhaLabel.setForeground(Color.RED);
//					dialogPanel.add(senhaLabel);
//					dialogPanel.add(Box.createVerticalStrut(20));
//				}
//			}
			dialogPanel.add(okButton);
		
		} else {
			// inicia o processo de cadastro/exclusao atravï¿½s do device
			SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
				@Override
				protected Void doInBackground() throws Exception {
					try {
						String retorno = null;
						device.setCancelAction(false);
						if ("CADASTRO".equals(tipoOperacao))
							retorno = device.cadastrateUser(acessoSelecionado);
						else
							retorno = device.removeUser(acessoSelecionado);
						if ("".equals(retorno)) {
							alteradoComSucesso = true;
							PedestrianAccessEntity athleteAccessEntity = (PedestrianAccessEntity) HibernateUtil
									.getSingleResultById(PedestrianAccessEntity.class, acessoSelecionado.getId());
							athleteAccessEntity.setCadastradoNaCatraca("CADASTRO".equals(tipoOperacao) ? true : false);
							athleteAccessEntity.setLuxandIdentifier(acessoSelecionado.getLuxandIdentifier());
							HibernateUtil.update(PedestrianAccessEntity.class, athleteAccessEntity);
						
						} else {
							mensagemErroRetorno = retorno;
							if (retorno.contains("PRIMARY KEY")) {
								mensagemErroRetorno = "Código já está cadastrado.";
							}
						}
					} catch (Exception e) {
						mensagemErroRetorno = e.getMessage();
					}
					return null;
				}
			};
			
			// adiciona um listener para indiciar o fim do processo
			worker.addPropertyChangeListener(new PropertyChangeListener() {
				@Override
				public void propertyChange(PropertyChangeEvent evt) {
					if (evt.getPropertyName().equals("state")) {
						if (evt.getNewValue() == SwingWorker.StateValue.DONE) {
							try {
								dialog.dispose();
								if (device.getManufacturer().giveListRegisteredUsers()) // A RWTECH nao me retorna uma lista, sao usados flags
									recuperarListaCadastradosNaCatraca();
								filterList();
								if (alteradoComSucesso){
									JOptionPane.showMessageDialog(instance,
											mensagemDialogSucesso,
											tituloDialogSucesso,
										    JOptionPane.PLAIN_MESSAGE,
										    iconSucesso);
								} else {
									JOptionPane.showMessageDialog(instance,
											mensagemDialogFalha + " " + mensagemErroRetorno,
											tituloDialogFalha,
										    JOptionPane.PLAIN_MESSAGE,
										    iconErro);
								}
							} catch (Exception e){
								e.printStackTrace();
							}
						}
					}
				}
			});
			worker.execute();
			
			// adiciona um progressBar para compor o dialog
			JProgressBar progressBar = new JProgressBar();
			progressBar.setIndeterminate(true);
			progressBar.setPreferredSize(new Dimension(200, 30));
			progressBar.setAlignmentX(Component.CENTER_ALIGNMENT);
			
			// Adicionar um botao para cancelar a aï¿½ï¿½o quando for possível
			JButton cancelButton = new JButton("Cancelar");
			cancelButton.setAlignmentX(Component.CENTER_ALIGNMENT);
			cancelButton.setPreferredSize(new Dimension(100, 50));
			cancelButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					try {
						device.setCancelAction(true);
						dialog.dispose();
					}
					catch (Exception e1){
						e1.printStackTrace();
					}
				}
			});
			
			dialogPanel.add(progressBar);
			dialogPanel.add(Box.createVerticalStrut(10));
			dialogPanel.add(instrucoesLabel1);
			dialogPanel.add(Box.createVerticalStrut(5));
			dialogPanel.add(instrucoesLabel2);
			dialogPanel.add(Box.createVerticalStrut(20));
			dialogPanel.add(cancelButton);
			
		}
		
		dialog.setUndecorated(true);
		dialog.setLayout(new BorderLayout());
        dialog.add(dialogPanel);
		dialog.pack();
		dialog.setLocationRelativeTo(null);
		dialog.setVisible(true);
		
	}
	
	private void setMensagemErro(String msg){
		errorLabel.setText(msg);
	}
	
	private void limparMensagemErro(){
		errorLabel.setText(" ");
	}
	
	private void populateTable(List<PedestrianAccessEntity> listaAcesso){
		DefaultTableModel dataModel = new DefaultTableModel(columns, 0){
			public boolean isCellEditable(int rowIndex, int mColIndex) {
				return false;
			}
		};
		if (listaAcesso != null && !listaAcesso.isEmpty()){
			for (PedestrianAccessEntity acesso : listaAcesso) {
				Object[] obj = new Object[columns.length];
				if (obj.length == 3) {
					obj[0] = acesso.getId();
					obj[1] = acesso.getName();
					obj[2] = "ATIVO".equals(acesso.getStatus()) ? "SIM" : "NÃO";
				} else {
					obj[0] = acesso.getId();
					obj[1] = acesso.getName();
					obj[2] = Manufacturer.FACIAL.equals(device.getManufacturer()) 
							? (acesso.getLuxandIdentifier() != null && !"".equals(acesso.getLuxandIdentifier()) ? "1" : "0") 
							: (acesso.getTamanhoListaTemplates() == null ? "0" : acesso.getTamanhoListaTemplates());
					obj[3] = "ATIVO".equals(acesso.getStatus()) ? "SIM" : "NÃO";
				}
				dataModel.addRow(obj);
			}
		}
		accessListTable.setModel(dataModel);
		countLabel.setText("Pág. ("+ paginaAtual + "/" + totalPaginas + ") do total: " + totalRegistros);
		formatTable();
	}
	
	private void formatTable(){
		DefaultTableCellRenderer centerRenderer = new CellRenderer();
		centerRenderer.setHorizontalAlignment(JLabel.CENTER);
		for (int i = 0; i < accessListTable.getColumnCount(); i++){
			TableColumn column = accessListTable.getColumnModel().getColumn(i);
			column.setMinWidth(columnWidths[i]);
			column.setCellRenderer(centerRenderer);
		}
	}
	
	private class CellRenderer extends DefaultTableCellRenderer {
		@Override
		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
			super.getTableCellRendererComponent(table, value, isSelected, false, row, column);
			String str = Objects.toString(value, "");
			Font font = getFont();
			Font boldFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
			
			if (isSelected) {
				setForeground(Color.WHITE);
			} else {
				if (column == 2 || column == 3) {
					if ("Não".equals(str)
							|| "0".equals(str)){
						setForeground(Color.GRAY);
						setFont(font);
					
					} else {
						setForeground(Main.firstColor);
						setFont(boldFont);
					}
				} else {
					setForeground(Color.BLACK);
					setFont(font);
				}
			}
			
			return this;
		}
	}
	
	private void loadImages() {
		try {
			Toolkit toolkit = Toolkit.getDefaultToolkit();
			iconSucesso = new ImageIcon(toolkit.getImage(Main.class
					.getResource(Configurations.IMAGE_FOLDER + "comuns/ok.png")));
			iconErro = new ImageIcon(toolkit.getImage(Main.class
					.getResource(Configurations.IMAGE_FOLDER + "comuns/erro.png")));
		
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	
	public String getMensagemErroRetorno() {
		return mensagemErroRetorno;
	}

	public void setMensagemErroRetorno(String mensagemErroRetorno) {
		this.mensagemErroRetorno = mensagemErroRetorno;
	}
}
