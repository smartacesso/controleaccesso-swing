package com.protreino.services.screens;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;
import javax.swing.text.JTextComponent;
import javax.swing.text.MaskFormatter;

import com.protreino.services.entity.HikivisionIntegrationErrorEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.main.Main;
import com.protreino.services.to.hikivision.HikivisionDeviceTO;
import com.protreino.services.usecase.HikivisionUseCases;
import com.protreino.services.usecase.SincronismoHorariosHikivision;
import com.protreino.services.usecase.SyncPedestrianAccessListUseCase;
import com.protreino.services.repository.*;
import com.protreino.services.utils.Utils;

//Classe para exibir o diálogo de sincronização manual de dispositivos
@SuppressWarnings("serial") // Ignora avisos de serialização
public class SincronizacaoManualDialog extends BaseDialog {

	private Font font; //fonte padrão 
	private Font tabHeaderFont; //fonte do cabeçalho
	private Container mainContentPane; //container principal
	private final HikivisionUseCases hikivisionUseCases; //integração com hikivison
	private final HikivisionIntegrationErrorRepository hikivisionIntegrationErrorRepository = new HikivisionIntegrationErrorRepository(); // repositorio com erro
	private SyncPedestrianAccessListUseCase syncPedestrianAccessListUseCase;
	private String[] columns = { "Device Id", "Device Name", "Status", "Sincronizar" }; // colunas da tabela
	private Integer[] columnWidths = { 280, 200, 150, 80 }; // largura das colunas
	private JTable deviceListTable; //tabela de dispositivos

	private JButton syncAll; //botao de sincronização total

	private JButton syncByDate; //botao de sincronização por data
	
	private JButton syncHorario; //botao de sincronização por data

	private JButton addDevice; // botao de adicionar dispositivos
	private JButton syncCameraListners; // botao de sincronizar camera
	private JButton requestFotos;
	private SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm"); //formato por data

	private static final int CHECKBOX_COLUMN = 3; //indice de colunas com checkbox

	//Opção de sincronismo manual de dispositivos
	
	public SincronizacaoManualDialog() {
		setIconImage(Main.favicon); //define icone da janela 
		setModal(true);
		setTitle("Sincronismo Hikivision"); // decrição de qual janela esta
		setResizable(false);
		setLayout(new BorderLayout());
		setPreferredSize(new Dimension(920, 718)); // define o tamanho da janela
		setMinimumSize(getPreferredSize());

		this.hikivisionUseCases = new HikivisionUseCases();

		//configura fontes 
		font = new JLabel().getFont();
		Font font2 = font;
		tabHeaderFont = new Font(font2.getFontName(), Font.BOLD, font2.getSize() + 1);

		//tela principal
		mainContentPane = new Container();
		mainContentPane.setLayout(new BorderLayout());

		Font font = new JLabel().getFont();
		Font headerFont = new Font(font.getFontName(), Font.BOLD, font.getSize());

		// painel e configuração de tabela
		JPanel deviceListTablePanel = new JPanel();
		deviceListTablePanel.setLayout(new BoxLayout(deviceListTablePanel, BoxLayout.Y_AXIS));
		deviceListTablePanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		
		deviceListTable = new JTable(new DefaultTableModel(columns, 0)); // inicia a tabela
		formatTable();
		deviceListTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS); //redimenciona automaticamente 
		deviceListTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		deviceListTable.getTableHeader().setReorderingAllowed(false);
		deviceListTable.getTableHeader().setOpaque(false);
		deviceListTable.getTableHeader().setForeground(Main.firstColor);
		deviceListTable.getTableHeader().setBackground(Main.secondColor);
		deviceListTable.getTableHeader().setFont(headerFont);
		deviceListTable.setRowHeight(30);
		deviceListTable.setSelectionBackground(Main.firstColor);
		deviceListTable.setSelectionForeground(Color.WHITE);
		TableCellRenderer rendererFromHeader = deviceListTable.getTableHeader().getDefaultRenderer();
		JLabel headerLabel = (JLabel) rendererFromHeader;
		headerLabel.setHorizontalAlignment(JLabel.CENTER);
		JScrollPane scrollPane = new JScrollPane(deviceListTable);
		scrollPane.getVerticalScrollBar().setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));
		deviceListTablePanel.add(scrollPane);

		//configuraçoes de botoes
		syncAll = new JButton("Sincronizacao Total");
		syncAll.setBorder(new EmptyBorder(10, 15, 10, 15));
		syncAll.setPreferredSize(new Dimension(180, 40));
		syncAll.addActionListener(e -> {
			syncDevices(null, null);
		});

		syncByDate = new JButton("Sincronizacao por data");
		syncByDate.setBorder(new EmptyBorder(10, 15, 10, 15));
		syncByDate.setPreferredSize(new Dimension(180, 40));
		syncByDate.addActionListener(e -> {
			criarDialogoDeSincronizacaoPorData();
		});

		addDevice = new JButton("Adicionar Camera");
		addDevice.setBorder(new EmptyBorder(10, 15, 10, 15));
		addDevice.setPreferredSize(new Dimension(120, 40));
		addDevice.addActionListener(e -> {
			adicionarDevice();
		});
		
		syncHorario = new JButton("Sincronizar horarios");
		syncHorario.setBorder(new EmptyBorder(10, 15, 10, 15));
		syncHorario.setPreferredSize(new Dimension(120, 40));
		syncHorario.addActionListener(e -> {
			syncHorario();
		});
		
		

		syncCameraListners = new JButton("Sincronizar listeners");
		syncCameraListners.setBorder(new EmptyBorder(10, 15, 10, 15));
		syncCameraListners.setPreferredSize(new Dimension(140, 40));
		syncCameraListners.setVisible(!Main.temServidor());
		syncCameraListners.addActionListener(e -> {
			syncCameraListners();
		});
		
		
		requestFotos = new JButton("Baixar fotos");
		requestFotos.setBorder(new EmptyBorder(10, 15, 10, 15));
		requestFotos.setPreferredSize(new Dimension(140, 40));
		requestFotos.setVisible(!Main.temServidor());
		requestFotos.addActionListener(e -> {
			baixarFotos();
		});

		//ação dos botoes 
		JPanel actionsPanel = new JPanel();
		actionsPanel.setLayout(new BoxLayout(actionsPanel, BoxLayout.X_AXIS));
		actionsPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		actionsPanel.setMaximumSize(new Dimension(Integer.MAX_VALUE, 100));
		actionsPanel.add(addDevice);
		actionsPanel.add(Box.createHorizontalStrut(10));
		actionsPanel.add(syncCameraListners);
		actionsPanel.add(Box.createHorizontalGlue());
		actionsPanel.add(syncAll);
		actionsPanel.add(Box.createHorizontalStrut(10));
		actionsPanel.add(syncByDate);
		actionsPanel.add(Box.createHorizontalStrut(10));
		actionsPanel.add(syncHorario);
		actionsPanel.add(Box.createHorizontalStrut(10));
		actionsPanel.add(requestFotos);

		populateTable();

		// Parte visual do "Sincronismo manual de dispositivos"
		
		mainContentPane.add(deviceListTablePanel, BorderLayout.CENTER); // Campos da parte superior da tela
		mainContentPane.add(actionsPanel, BorderLayout.SOUTH); // Botões da parte de baixo da janela
		
		// adiciona container da janela
		getContentPane().add(mainContentPane, BorderLayout.CENTER); 
		pack();
		setLocationRelativeTo(null);
		setVisible(true);
	}

	private void baixarFotos() {
		// TODO Auto-generated method stubSync
		// fazer uma query de connt para contar quantos pedestres vao ser sincronizados.
		// fazer a busca paginada
		
		final int pageSize = 500;
		final Integer countPedestresParaSincronizar = countPesdestresParaSincronizar(null, null);
		if(Objects.isNull(countPedestresParaSincronizar) || countPedestresParaSincronizar.equals(0)) {
			return;
		}

		System.out.println("Pedestre encontrados: " + countPedestresParaSincronizar);
		int offset = 0;
		do {
			List<PedestrianAccessEntity> pedestresParaSicronizar = buscaPedestresParaSicronizar(null, null, offset, pageSize);
			
			try {
				syncPedestrianAccessListUseCase = new SyncPedestrianAccessListUseCase();
				syncPedestrianAccessListUseCase.buscaFotosTodosDosPedestres(pedestresParaSicronizar);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
			offset += pageSize;
		} while (offset < countPedestresParaSincronizar);
		
	}

	private void syncHorario() {
		// TODO Auto-generated method stub
		SincronismoHorariosHikivision sincronismoHorariosHikivision = new SincronismoHorariosHikivision();
		sincronismoHorariosHikivision.execute();
	}

	// janela adicionar camera dentro de sincronismo manual de dispositivos
	private void adicionarDevice() {
		JDialog adicionarDeviceDialog = new JDialog();
		adicionarDeviceDialog.setIconImage(Main.favicon);
		adicionarDeviceDialog.setModal(true);
		adicionarDeviceDialog.setTitle("Adicionar Device");
		adicionarDeviceDialog.setResizable(false);
		adicionarDeviceDialog.setLayout(new BorderLayout());

		JPanel mainPanel = new JPanel();
		mainPanel.setBorder(new EmptyBorder(20, 50, 20, 50));
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

		JLabel addressLabel = new JLabel("Ip Da Camera");
		addressLabel.setPreferredSize(new Dimension(120, 25));
		addressLabel.setForeground(Main.firstColor);
		addressLabel.setFont(tabHeaderFont);
		JFormattedTextField addressTextField = Utils.getNewJFormattedTextField(12);
		addressTextField.setFont(new Font("SansSerif", Font.PLAIN, 16));
		JPanel addressPanel = getNewMiniPanel(addressLabel, addressTextField);

		JLabel portLabel = new JLabel("Porta da Camera");
		portLabel.setPreferredSize(new Dimension(120, 25));
		portLabel.setForeground(Main.firstColor);
		portLabel.setFont(tabHeaderFont);
		JFormattedTextField portTextField = Utils.getNewJFormattedTextField(12);
		portTextField.setFont(new Font("SansSerif", Font.PLAIN, 16));
		JPanel portPanel = getNewMiniPanel(portLabel, portTextField);

		JLabel userLabel = new JLabel("Usuario da Camera");
		userLabel.setPreferredSize(new Dimension(120, 25));
		userLabel.setForeground(Main.firstColor);
		userLabel.setFont(tabHeaderFont);
		JFormattedTextField userTextField = Utils.getNewJFormattedTextField(12);
		userTextField.setFont(new Font("SansSerif", Font.PLAIN, 16));
		JPanel userPanel = getNewMiniPanel(userLabel, userTextField);

		JLabel passwordLabel = new JLabel("Senha da Camera");
		passwordLabel.setPreferredSize(new Dimension(120, 25));
		passwordLabel.setForeground(Main.firstColor);
		passwordLabel.setFont(tabHeaderFont);
		JFormattedTextField passwordTextField = Utils.getNewJFormattedTextField(12);
		passwordTextField.setFont(new Font("SansSerif", Font.PLAIN, 16));
		JPanel passwordPanel = getNewMiniPanel(passwordLabel, passwordTextField);

		JLabel deviceNameLabel = new JLabel("Nome da Camera");
		deviceNameLabel.setPreferredSize(new Dimension(120, 25));
		deviceNameLabel.setForeground(Main.firstColor);
		deviceNameLabel.setFont(tabHeaderFont);
		JFormattedTextField deviceNameTextField = Utils.getNewJFormattedTextField(12);
		deviceNameTextField.setFont(new Font("SansSerif", Font.PLAIN, 16));
		JPanel deviceNamedPanel = getNewMiniPanel(deviceNameLabel, deviceNameTextField);

		JButton confirmarButton = new JButton("Confirmar");
		confirmarButton.setBorder(new EmptyBorder(10, 20, 10, 20));
		confirmarButton.setAlignmentX(Component.CENTER_ALIGNMENT);

		confirmarButton.addActionListener(e -> {
			restauraFontLabel(addressLabel, portLabel, userLabel, passwordLabel, deviceNameLabel);

			boolean valido = true;

			if (!isValidIpAddress(addressTextField.getText())) {
				redAndBoldFont(addressLabel);
				valido = false;
			}

			if (!isValidPort(portTextField.getText())) {
				redAndBoldFont(portLabel);
				valido = false;
			}

			if ("".equals(userTextField.getText())) {
				redAndBoldFont(userLabel);
				valido = false;
			}

			if ("".equals(passwordTextField.getText())) {
				redAndBoldFont(passwordLabel);
				valido = false;
			}

			if ("".equals(deviceNameTextField.getText())) {
				redAndBoldFont(deviceNameLabel);
				valido = false;
			}

			if (!valido) {
				return;
			}

			hikivisionUseCases.adicionarDispositivoAndListener(addressTextField.getText(),
					Integer.parseInt(portTextField.getText()), userTextField.getText(), passwordTextField.getText(),
					deviceNameTextField.getText());

			populateTable();
			adicionarDeviceDialog.dispose();
		});

		mainPanel.add(addressPanel);
		mainPanel.add(Box.createVerticalStrut(10));
		mainPanel.add(portPanel);
		mainPanel.add(Box.createVerticalStrut(10));
		mainPanel.add(userPanel);
		mainPanel.add(Box.createVerticalStrut(10));
		mainPanel.add(passwordPanel);
		mainPanel.add(Box.createVerticalStrut(10));
		mainPanel.add(deviceNamedPanel);
		mainPanel.add(Box.createVerticalStrut(70));
		mainPanel.add(confirmarButton);

		adicionarDeviceDialog.getContentPane().add(mainPanel, BorderLayout.CENTER);
		adicionarDeviceDialog.pack();
		adicionarDeviceDialog.setLocationRelativeTo(null);
		adicionarDeviceDialog.setVisible(true);
	}

	//campo sincronizar listeners dentro de sincronismo manual de dispositivos
	private void syncCameraListners() {
		List<String> devicesToSync = getDevicesToSync();
		if (devicesToSync.isEmpty()) {
			return;
		}

		devicesToSync.forEach(deviceId -> {
			hikivisionUseCases.adicionarListnerParaCamera(deviceId);
		});
	}

	private boolean isValidPort(String port) {
		if (port == null || port.isEmpty()) {
			return false;
		}

		final String regex = "^([0-9]){1,5}$";
		Pattern p = Pattern.compile(regex);
		Matcher m = p.matcher(port);

		return m.matches();
	}

	private boolean isValidIpAddress(String ipAddress) {
		if (ipAddress == null || ipAddress.isEmpty()) {
			return false;
		}

		final String regex = "^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$";
		Pattern p = Pattern.compile(regex);
		Matcher m = p.matcher(ipAddress);

		return m.matches();
	}

	private void restauraFontLabel(JLabel addressLabel, JLabel portLabel, JLabel userLabel, JLabel passwordLabel,
			JLabel deviceNameLabel) {
		setFirstColorFont(addressLabel);
		setFirstColorFont(portLabel);
		setFirstColorFont(userLabel);
		setFirstColorFont(passwordLabel);
		setFirstColorFont(deviceNameLabel);
	}

	//campo de sincroziação por data dentro de sincronismo manual de dispositivos
	private void criarDialogoDeSincronizacaoPorData() {
		JDialog sincronizacaoPorDataDialog = new JDialog();
		sincronizacaoPorDataDialog.setIconImage(Main.favicon);
		sincronizacaoPorDataDialog.setModal(true);
		sincronizacaoPorDataDialog.setTitle("Confirmar");
		sincronizacaoPorDataDialog.setResizable(false);
		sincronizacaoPorDataDialog.setLayout(new BorderLayout());

		JPanel mainPanel = new JPanel();
		
		mainPanel.setBorder(new EmptyBorder(20, 20, 20, 20));
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

		JLabel dataInicioLabel = new JLabel("Data de Inicio");
		JFormattedTextField dataInicioTextField = Utils.getNewJFormattedTextField(12);
		dataInicioTextField.setFont(new Font("SansSerif", Font.PLAIN, 16));
		MaskFormatter mask = Utils.getNewMaskFormatter("##/##/#### ##:##");
		mask.install(dataInicioTextField);
		JPanel dataInicioPanel = getNewMiniPanel(dataInicioLabel, dataInicioTextField);

		JLabel dataFimLabel = new JLabel("Data de Fim");
		JFormattedTextField dataFimTextField = Utils.getNewJFormattedTextField(12);
		dataFimTextField.setFont(new Font("SansSerif", Font.PLAIN, 16));
		mask = Utils.getNewMaskFormatter("##/##/#### ##:##");
		mask.install(dataFimTextField);
		JPanel dataFinalPanel = getNewMiniPanel(dataFimLabel, dataFimTextField);

		JButton confirmarButton = new JButton("Confirmar");
		confirmarButton.setBorder(new EmptyBorder(10, 20, 10, 20));
		confirmarButton.setAlignmentX(Component.CENTER_ALIGNMENT);
		confirmarButton.addActionListener(e -> {

			Date dataInicio = null;
			Date dataFim = null;

			try {
				dataInicio = sdf.parse(dataInicioTextField.getText());
				dataFim = sdf.parse(dataFimTextField.getText());

			} catch (ParseException ex) {
				ex.printStackTrace();
			}
			syncDevices(dataInicio, dataFim);

			sincronizacaoPorDataDialog.dispose();
		});
		
		mainPanel.add(dataInicioPanel);
		mainPanel.add(Box.createVerticalStrut(10));
		mainPanel.add(dataFinalPanel);
		mainPanel.add(Box.createVerticalStrut(10));
		mainPanel.add(confirmarButton);
		sincronizacaoPorDataDialog.getContentPane().add(mainPanel, BorderLayout.CENTER);
		sincronizacaoPorDataDialog.pack();
		sincronizacaoPorDataDialog.setLocationRelativeTo(null);
		sincronizacaoPorDataDialog.setVisible(true);
	}

	//campo de sincronização de devices dentro de sincronismo manual de dispositivos
	private void syncDevices(final Date inicio, final Date fim) {
		// fazer uma query de connt para contar quantos pedestres vao ser sincronizados.
		// fazer a busca paginada
		List<String> devicesToSync = getDevicesToSync();
		if (devicesToSync.isEmpty()) {
			return;
		}
		
		final int pageSize = 500;
		final Integer countPedestresParaSincronizar = countPesdestresParaSincronizar(inicio, fim);
		if(Objects.isNull(countPedestresParaSincronizar) || countPedestresParaSincronizar.equals(0)) {
			return;
		}

		final List<String> errors = new ArrayList<>();

		System.out.println("Pedestre encontrados: " + countPedestresParaSincronizar);
		
		JDialog progressBarDialog = new JDialog();
		progressBarDialog.setIconImage(Main.favicon);

		progressBarDialog.setTitle("Sincronizando");
		progressBarDialog.setResizable(false);
		progressBarDialog.setLayout(new BorderLayout());

		JProgressBar progressBar = new JProgressBar(JProgressBar.HORIZONTAL, 0, countPedestresParaSincronizar);

		JPanel mainPanel = new JPanel();
		mainPanel.add(progressBar);

		mainPanel.setBorder(new EmptyBorder(20, 20, 20, 20));
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

		progressBarDialog.getContentPane().add(mainPanel, BorderLayout.CENTER);
		progressBarDialog.pack();
		progressBarDialog.setSize(500, 100);
		progressBarDialog.setLocationRelativeTo(null);
		progressBarDialog.setModal(true);

		Thread thread = new Thread(new Runnable() {
			@Override
			public void run() {
				int offset = 0;
				do {
					List<PedestrianAccessEntity> pedestresParaSicronizar = buscaPedestresParaSicronizar(inicio, fim, offset, pageSize);
					
					pedestresParaSicronizar.forEach(pedestre -> {
						try {
							hikivisionUseCases.syncronizarUsuarioInDevices(pedestre, devicesToSync);
							
						} catch(Exception ex) {
							System.out.println(ex.getMessage());
						}
						
						progressBar.setValue(progressBar.getValue() + 1);
					});
					
					offset += pageSize;
				} while (offset < countPedestresParaSincronizar);
				
				List<HikivisionIntegrationErrorEntity> hikivisionIntegrationErrors = hikivisionIntegrationErrorRepository.findLatest(30);
				if(Objects.nonNull(hikivisionIntegrationErrors) && !hikivisionIntegrationErrors.isEmpty()) {
					hikivisionIntegrationErrors.forEach(error -> errors.add(error.getMessage()));
				}
				
				if(errors.isEmpty()) {
					progressBarDialog.dispose();
					//exibir botao de ok e mensagem que sincronizacao acabou e fechar o dialogo quando clicar em ok
				} else {
					progressBar.setVisible(false);
					String[] columns = { "Erros"};
					DefaultTableModel dataModel = new DefaultTableModel(columns, 0) {};

					errors.forEach(erro -> {
						String[] erros = {erro};
						dataModel.addRow(erros);
					});
					
					JTable table = new JTable();
					table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
					table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
					table.getTableHeader().setReorderingAllowed(false);
					table.getTableHeader().setOpaque(false);
					table.getTableHeader().setForeground(Main.firstColor);
					table.getTableHeader().setBackground(Main.secondColor);
					table.setRowHeight(30);
					table.setSelectionBackground(Main.firstColor);
					table.setSelectionForeground(Color.WHITE);
					DefaultTableCellRenderer centerRenderer = new DefaultTableCellRenderer();
					centerRenderer.setHorizontalAlignment(JLabel.CENTER);
					table.setDefaultRenderer(String.class, centerRenderer);

					for (int i = 0; i < table.getColumnCount(); i++) {
						TableColumn column = table.getColumnModel().getColumn(i);
						column.setPreferredWidth(240);
					}
					
					table.setModel(dataModel);
					
					JScrollPane scrollPane = new JScrollPane(table);
					scrollPane.getVerticalScrollBar().setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));
					
					mainPanel.add(scrollPane);
					
					progressBarDialog.setSize(650, 500);
					progressBarDialog.setLocationRelativeTo(null);
					progressBarDialog.revalidate();

					mainPanel.revalidate();
					mainPanel.updateUI();
				}
			}
		});
		thread.setDaemon(true);
		thread.start();
		progressBarDialog.setVisible(true);
	}
	
	//toda parte de sincronização usada na sincronização total entre outros
	
	private Integer countPesdestresParaSincronizar(final Date inicio, final Date fim) {
		if(Objects.isNull(inicio) && Objects.isNull(fim)) {
			return HibernateAccessDataFacade.
	                getResultListWithParamsCount(PedestrianAccessEntity.class, "PedestrianAccessEntity.countAllWithHikiVisionImageOnRegistred", null);
		}
		
        HashMap<String, Object> args = new HashMap<String, Object>();
        args.put("INIT_DATE", inicio);
        args.put("END_DATE", fim);

        return HibernateAccessDataFacade.
                getResultListWithParamsCount(PedestrianAccessEntity.class, "PedestrianAccessEntity.countAllWithHikiVisionImageOnRegistredBeteenDate", args);
	}

	@SuppressWarnings("unchecked")
	private List<PedestrianAccessEntity> buscaPedestresParaSicronizar(Date inicio, Date fim, Integer offset, Integer pageSize) {
		if (inicio != null && fim != null) {
			HashMap<String, Object> args = new HashMap<>();
			args.put("INIT_DATE", inicio);
			args.put("END_DATE", fim);

			return (List<PedestrianAccessEntity>) HibernateAccessDataFacade.getResultListWithParams(PedestrianAccessEntity.class,
					"PedestrianAccessEntity.findAllWithHikiVisionImageOnRegistredBeteenDate", args, offset, pageSize);
		}

		return (List<PedestrianAccessEntity>) HibernateAccessDataFacade.getResultListWithParams(PedestrianAccessEntity.class,
				"PedestrianAccessEntity.findAllWithHikiVisionImageOnRegistred", null, offset, pageSize);
	}

	private List<String> getDevicesToSync() {
		TableModel model = deviceListTable.getModel();
		List<String> devicesToSync = new ArrayList<>();
		for (int i = 0; i < model.getRowCount(); i++) {
			if (Boolean.valueOf(model.getValueAt(i, 3).toString())) {
				devicesToSync.add(model.getValueAt(i, 0).toString());
			}
		}

		return devicesToSync;
	}

	private void formatTable() {
		DefaultTableCellRenderer centerRenderer = new DefaultTableCellRenderer();
		centerRenderer.setHorizontalAlignment(JLabel.CENTER);
		deviceListTable.setDefaultRenderer(String.class, centerRenderer);

		for (int i = 0; i < deviceListTable.getColumnCount(); i++) {
			TableColumn column = deviceListTable.getColumnModel().getColumn(i);
			column.setPreferredWidth(columnWidths[i]);
		}
	}

	public void populateTable() {
		DefaultTableModel dataModel = new DefaultTableModel(columns, 0) {

			@Override
			public Class<?> getColumnClass(int columnIndex) {
				Class<?> clazz = String.class;
				switch (columnIndex) {
				case CHECKBOX_COLUMN:
					clazz = Boolean.class;
					break;
				}
				return clazz;
			}

			@Override
			public boolean isCellEditable(int row, int column) {
				return column == CHECKBOX_COLUMN;
			}

			@Override
			public void setValueAt(Object aValue, int row, int column) {
				if (aValue instanceof Boolean && column == CHECKBOX_COLUMN) {
					Vector rowData = (Vector) getDataVector().get(row);
					rowData.set(CHECKBOX_COLUMN, (boolean) aValue);
					fireTableCellUpdated(row, column);
				}
			}
		};
		List<HikivisionDeviceTO.Device> devices = hikivisionUseCases.listarDispositivos();

		if (devices == null || devices.isEmpty()) {
			deviceListTable.setModel(dataModel);
			return;
		}

		for (HikivisionDeviceTO.Device device : devices) {
			Object[] obj = new Object[4];
			obj[0] = device.getDevIndex();
			obj[1] = device.getDevName();
			obj[2] = device.getDevStatus();
			obj[3] = true;

			dataModel.addRow(obj);
		}

		deviceListTable.setModel(dataModel);
	}

	private JPanel getNewMiniPanel(JLabel label, JTextComponent text) {
		JPanel panel = new JPanel();
		panel.setLayout(new GridBagLayout());

		GridBagConstraints c = new GridBagConstraints();

		c.fill = GridBagConstraints.HORIZONTAL;
		c.gridx = 0;
		c.gridy = 0;
		panel.add(label, c);

		c.gridx = 0;
		c.gridy = 1;
		panel.add(text, c);

		return panel;
	}

}
