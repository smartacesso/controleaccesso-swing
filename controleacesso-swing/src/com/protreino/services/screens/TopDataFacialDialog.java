package com.protreino.services.screens;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
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
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.text.JTextComponent;

import org.java_websocket.WebSocket;

import com.protreino.services.entity.TopdataFacialEntity;
import com.protreino.services.main.Main;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.utils.Utils;


//Classe para exibir o diálogo de sincronização manual de dispositivos
@SuppressWarnings("serial") // Ignora avisos de serialização
public class TopDataFacialDialog extends BaseDialog{
	
	private Font font; //fonte padrão 
	private Font tabHeaderFont; //fonte do cabeçalho
	private Container mainContentPane; //container principal
	private String[] columns = { "Device IP", "porta", "Sincronizar" }; // colunas da tabela
	private Integer[] columnWidths = { 280, 150, 80}; // largura das colunas
	private JTable deviceListTable; //tabela de dispositivos

	private JButton syncAll; //botao de sincronização total

	private JButton syncByDate; //botao de sincronização por data

	private JButton addDevice; // botao de adicionar dispositivos
	
	private JButton editDevice;
	
	private JButton removeDevice;

	private SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm"); //formato por data
	private DefaultTableModel dataModel;
	
	private JFormattedTextField addressTextField;
	private JFormattedTextField portTextField;
	private JFormattedTextField deviceNameTextField;

	private List<TopdataFacialEntity> devices;
	
	private static final int CHECKBOX_COLUMN = 2; //indice de colunas com checkbox
	
	public TopDataFacialDialog() {
		setIconImage(Main.favicon); //define icone da janela 
		setModal(true);
		setTitle("Topdata facial"); // decrição de qual janela esta
		setResizable(false);
		setLayout(new BorderLayout());
		setPreferredSize(new Dimension(920, 718)); // define o tamanho da janela
		setMinimumSize(getPreferredSize());

		//this.hikivisionUseCases = new HikivisionUseCases();

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
			//syncDevices(null, null);
		});

		syncByDate = new JButton("Sincronizacao por data");
		syncByDate.setBorder(new EmptyBorder(10, 15, 10, 15));
		syncByDate.setPreferredSize(new Dimension(180, 40));
		syncByDate.addActionListener(e -> {
			//criarDialogoDeSincronizacaoPorData();
		});

		addDevice = new JButton("Adicionar Camera");
		addDevice.setBorder(new EmptyBorder(10, 15, 10, 15));
		addDevice.setPreferredSize(new Dimension(120, 40));
		addDevice.addActionListener(e -> {
			adicionarDevice();
		});
		
		editDevice = new JButton("Editar camera");
		editDevice.setBorder(new EmptyBorder(10, 15, 10, 15));
		editDevice.setPreferredSize(new Dimension(120, 40));
		editDevice.addActionListener(e -> {
			editarDevide();
		});
		
		removeDevice = new JButton("remover camera");
		removeDevice.setBorder(new EmptyBorder(10, 15, 10, 15));
		removeDevice.setPreferredSize(new Dimension(120, 40));
		removeDevice.addActionListener(e -> {
			removeDevice();
		});

		//ação dos botoes 
		JPanel actionsPanel = new JPanel();
		actionsPanel.setLayout(new BoxLayout(actionsPanel, BoxLayout.X_AXIS));
		actionsPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		actionsPanel.setMaximumSize(new Dimension(Integer.MAX_VALUE, 100));
		actionsPanel.add(addDevice);
		actionsPanel.add(Box.createHorizontalStrut(10));
		actionsPanel.add(editDevice);
		actionsPanel.add(Box.createHorizontalStrut(10));
		actionsPanel.add(removeDevice);	
		actionsPanel.add(Box.createHorizontalStrut(200));
		actionsPanel.add(syncAll);
		actionsPanel.add(Box.createHorizontalStrut(10));
		actionsPanel.add(syncByDate);


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
	
	private void editarDevide() {
	    // Obtém a linha selecionada na tabela
	    int selectedRow = deviceListTable.getSelectedRow();

	    if (selectedRow == -1) {
	        JOptionPane.showMessageDialog(null, "Nenhum dispositivo selecionado.");
	        return;
	    }

	    // Obtém os valores da linha selecionada
	    String ipFacial = (String) dataModel.getValueAt(selectedRow, 0);
	    String nomeFacial = (String) dataModel.getValueAt(selectedRow, 1);
	    String portaFacial = (String) dataModel.getValueAt(selectedRow, 2);

	    JDialog EditarDeviceDialog = new JDialog();
	    EditarDeviceDialog.setIconImage(Main.favicon);
	    EditarDeviceDialog.setModal(true);
	    EditarDeviceDialog.setTitle("Editar Device");
	    EditarDeviceDialog.setResizable(false);
	    EditarDeviceDialog.setLayout(new BorderLayout());

	    JPanel mainPanel = new JPanel();
	    mainPanel.setBorder(new EmptyBorder(20, 50, 20, 50));
	    mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

	    JLabel addressLabel = new JLabel("Ip Da Camera");
	    addressLabel.setPreferredSize(new Dimension(120, 25));
	    addressLabel.setForeground(Main.firstColor);
	    addressLabel.setFont(tabHeaderFont);
	    addressTextField = Utils.getNewJFormattedTextField(12); // Use o atributo da classe
	    addressTextField.setFont(new Font("SansSerif", Font.PLAIN, 16));
	    JPanel addressPanel = getNewMiniPanel(addressLabel, addressTextField);

	    JLabel portLabel = new JLabel("Porta da Camera");
	    portLabel.setPreferredSize(new Dimension(120, 25));
	    portLabel.setForeground(Main.firstColor);
	    portLabel.setFont(tabHeaderFont);
	    portTextField = Utils.getNewJFormattedTextField(12); // Use o atributo da classe
	    portTextField.setFont(new Font("SansSerif", Font.PLAIN, 16));
	    JPanel portPanel = getNewMiniPanel(portLabel, portTextField);

	    JLabel deviceNameLabel = new JLabel("Nome da Camera");
	    deviceNameLabel.setPreferredSize(new Dimension(120, 25));
	    deviceNameLabel.setForeground(Main.firstColor);
	    deviceNameLabel.setFont(tabHeaderFont);
	    deviceNameTextField = Utils.getNewJFormattedTextField(12); // Use o atributo da classe
	    deviceNameTextField.setFont(new Font("SansSerif", Font.PLAIN, 16));
	    JPanel deviceNamedPanel = getNewMiniPanel(deviceNameLabel, deviceNameTextField);

	    JButton confirmarButton = new JButton("Confirmar");
	    confirmarButton.setBorder(new EmptyBorder(10, 20, 10, 20));
	    confirmarButton.setAlignmentX(Component.CENTER_ALIGNMENT);

	    confirmarButton.addActionListener(e -> {
	        restauraFontLabel(addressLabel, portLabel, deviceNameLabel);

	        boolean valido = true;

	        if (!isValidIpAddress(addressTextField.getText())) {
	            redAndBoldFont(addressLabel);
	            valido = false;
	        }

	        if (!isValidPort(portTextField.getText())) {
	            redAndBoldFont(portLabel);
	            valido = false;
	        }

	        if ("".equals(deviceNameTextField.getText())) {
	            redAndBoldFont(deviceNameLabel);
	            valido = false;
	        }

	        if (!valido) {
	            return;
	        }

	        //editar o dispositivo
	        TopdataFacialEntity dispositivo = devices.get(selectedRow);
	        dispositivo.setIpFacial(addressTextField.getText());
	        dispositivo.setPortaFacial(portTextField.getText());
	        dispositivo.setNomeFacial(deviceNameTextField.getText());

	        HibernateAccessDataFacade.update(TopdataFacialEntity.class, dispositivo);
	        
	        populateTable(); // Chama o método para atualizar a tabela
	        EditarDeviceDialog.dispose();
	    });
	    
	    
	    mainPanel.add(addressPanel);
	    mainPanel.add(Box.createVerticalStrut(10));
	    mainPanel.add(portPanel);
	    mainPanel.add(Box.createVerticalStrut(10));
	    mainPanel.add(deviceNamedPanel);
	    mainPanel.add(Box.createVerticalStrut(70));
	    mainPanel.add(confirmarButton);

	    EditarDeviceDialog.getContentPane().add(mainPanel, BorderLayout.CENTER);
	    EditarDeviceDialog.pack();
	    EditarDeviceDialog.setLocationRelativeTo(null);
	    EditarDeviceDialog.setVisible(true);
	    
	}
	
	private void removeDevice() {
	    // Obtém a linha selecionada na tabela
	    int selectedRow = deviceListTable.getSelectedRow();

	    if (selectedRow == -1) {
	        JOptionPane.showMessageDialog(null, "Nenhum dispositivo selecionado.");
	        return;
	    }
	    
	    JDialog removerDeviceDialog = new JDialog();
	    removerDeviceDialog.setIconImage(Main.favicon);
	    removerDeviceDialog.setModal(true);
	    removerDeviceDialog.setTitle("Remover Device");
	    removerDeviceDialog.setResizable(false);
	    removerDeviceDialog.setLayout(new BorderLayout());

	    JPanel mainPanel = new JPanel();
	    mainPanel.setBorder(new EmptyBorder(20, 50, 20, 50));
	    mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
	    
	    JButton confirmarButton = new JButton("Confirmar");
	    confirmarButton.setBorder(new EmptyBorder(10, 20, 10, 20));
	    confirmarButton.setAlignmentX(Component.CENTER_ALIGNMENT);

	    confirmarButton.addActionListener(e -> {
	        //remover o dispositivo
	        TopdataFacialEntity dispositivo = devices.get(selectedRow);
	        dispositivo.setRemoved(true);

	        HibernateAccessDataFacade.update(TopdataFacialEntity.class, dispositivo);
	        
	        populateTable(); // Chama o método para atualizar a tabela
	        removerDeviceDialog.dispose();
	    });
	    
	    mainPanel.add(Box.createVerticalStrut(10));
	    mainPanel.add(confirmarButton);

	    removerDeviceDialog.getContentPane().add(mainPanel, BorderLayout.CENTER);
	    removerDeviceDialog.pack();
	    removerDeviceDialog.setLocationRelativeTo(null);
	    removerDeviceDialog.setVisible(true);
	    
	}


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
	    addressTextField = Utils.getNewJFormattedTextField(12); // Use o atributo da classe
	    addressTextField.setFont(new Font("SansSerif", Font.PLAIN, 16));
	    JPanel addressPanel = getNewMiniPanel(addressLabel, addressTextField);

	    JLabel portLabel = new JLabel("Porta da Camera");
	    portLabel.setPreferredSize(new Dimension(120, 25));
	    portLabel.setForeground(Main.firstColor);
	    portLabel.setFont(tabHeaderFont);
	    portTextField = Utils.getNewJFormattedTextField(12); // Use o atributo da classe
	    portTextField.setFont(new Font("SansSerif", Font.PLAIN, 16));
	    JPanel portPanel = getNewMiniPanel(portLabel, portTextField);

	    JLabel deviceNameLabel = new JLabel("Nome da Camera");
	    deviceNameLabel.setPreferredSize(new Dimension(120, 25));
	    deviceNameLabel.setForeground(Main.firstColor);
	    deviceNameLabel.setFont(tabHeaderFont);
	    deviceNameTextField = Utils.getNewJFormattedTextField(12); // Use o atributo da classe
	    deviceNameTextField.setFont(new Font("SansSerif", Font.PLAIN, 16));
	    JPanel deviceNamedPanel = getNewMiniPanel(deviceNameLabel, deviceNameTextField);

	    JButton confirmarButton = new JButton("Confirmar");
	    confirmarButton.setBorder(new EmptyBorder(10, 20, 10, 20));
	    confirmarButton.setAlignmentX(Component.CENTER_ALIGNMENT);

	    confirmarButton.addActionListener(e -> {
	        restauraFontLabel(addressLabel, portLabel, deviceNameLabel);

	        boolean valido = true;

	        if (!isValidIpAddress(addressTextField.getText())) {
	            redAndBoldFont(addressLabel);
	            valido = false;
	        }

	        if (!isValidPort(portTextField.getText())) {
	            redAndBoldFont(portLabel);
	            valido = false;
	        }

	        if ("".equals(deviceNameTextField.getText())) {
	            redAndBoldFont(deviceNameLabel);
	            valido = false;
	        }

	        if (!valido) {
	            return;
	        }

	        // Adiciona o novo dispositivo ao banco de dados e à tabela
	        TopdataFacialEntity newDevice = new TopdataFacialEntity();
	        newDevice.setIpFacial(addressTextField.getText());
	        newDevice.setPortaFacial(portTextField.getText());
	        newDevice.setNomeFacial(deviceNameTextField.getText());

	        HibernateAccessDataFacade.saveUser(TopdataFacialEntity.class, newDevice);
	        
	        populateTable(); // Chama o método para atualizar a tabela
	        adicionarDeviceDialog.dispose();
	    });

	    mainPanel.add(addressPanel);
	    mainPanel.add(Box.createVerticalStrut(10));
	    mainPanel.add(portPanel);
	    mainPanel.add(Box.createVerticalStrut(10));
	    mainPanel.add(deviceNamedPanel);
	    mainPanel.add(Box.createVerticalStrut(70));
	    mainPanel.add(confirmarButton);

	    adicionarDeviceDialog.getContentPane().add(mainPanel, BorderLayout.CENTER);
	    adicionarDeviceDialog.pack();
	    adicionarDeviceDialog.setLocationRelativeTo(null);
	    adicionarDeviceDialog.setVisible(true);
	}
	
	private void restauraFontLabel(JLabel addressLabel, JLabel portLabel,JLabel deviceNameLabel) {
		setFirstColorFont(addressLabel);
		setFirstColorFont(portLabel);
		setFirstColorFont(deviceNameLabel);
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

	private void formatTable() {
		DefaultTableCellRenderer centerRenderer = new DefaultTableCellRenderer();
		centerRenderer.setHorizontalAlignment(JLabel.CENTER);
		deviceListTable.setDefaultRenderer(String.class, centerRenderer);

		for (int i = 0; i < deviceListTable.getColumnCount(); i++) {
			TableColumn column = deviceListTable.getColumnModel().getColumn(i);
			column.setPreferredWidth(columnWidths[i]);
		}
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
	
	@SuppressWarnings("unchecked")
	public void populateTable() {
	    // Criação de um modelo de tabela que permite checkbox na coluna 3
	    dataModel = new DefaultTableModel(columns, 0) {

	        @Override
	        public Class<?> getColumnClass(int columnIndex) {
	            return columnIndex == CHECKBOX_COLUMN ? Boolean.class : String.class;
	        }

	        @Override
	        public boolean isCellEditable(int row, int column) {
	            return column == CHECKBOX_COLUMN; // Apenas a coluna de checkbox é editável
	        }

	        @Override
	        public void setValueAt(Object aValue, int row, int column) {
	            if (aValue instanceof Boolean && column == CHECKBOX_COLUMN) {
	                Vector<Object> rowData = (Vector<Object>) getDataVector().get(row);
	                rowData.set(CHECKBOX_COLUMN, aValue);
	                fireTableCellUpdated(row, column);
	            }
	        }
	    };

	    // Obtém a lista de dispositivos do banco
	    devices = (List<TopdataFacialEntity>) HibernateAccessDataFacade
	            .getResultList(TopdataFacialEntity.class, "TopdataFacialEntity.findAllNaoRemovidosOrdered");
	    
	    List<WebSocket> allTopDataFacialDevicesConnected = Main.facialTopDataIntegrationService.getAllTopDataFacialDevicesConnected();

	    // Preenche a tabela com os dispositivos encontrados
	    if (allTopDataFacialDevicesConnected != null && !allTopDataFacialDevicesConnected.isEmpty()) {
	        for (WebSocket device : allTopDataFacialDevicesConnected) {
	            Object[] item = new Object[3];
	            item[0] = device.getRemoteSocketAddress().getHostString();   // IP do dispositivo
	            item[1] = device.getRemoteSocketAddress().getPort(); // Porta do dispositivo
	            item[2] = false;                  // Checkbox desmarcado inicialmente

	            dataModel.addRow(item);
	        }
	    }

	    // Atualiza o modelo da tabela com o novo conteúdo
	    deviceListTable.setModel(dataModel);
	}

}
