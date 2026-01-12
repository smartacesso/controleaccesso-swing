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
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultCellEditor;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.text.JTextComponent;

import org.hibernate.Hibernate;
import org.java_websocket.WebSocket;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.protreino.services.devices.Device;
import com.protreino.services.devices.TopDataDevice;
import com.protreino.services.entity.LocalEntity;
import com.protreino.services.main.Main;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.repository.HibernateLocalAccessData;
import com.protreino.services.to.AttachedTO;
import com.protreino.services.usecase.HikivisionUseCases;
import com.protreino.services.utils.HttpConnection;
import com.protreino.services.utils.Utils;

@SuppressWarnings("serial") // Ignora avisos de serialização
public class LocaisScreen extends BaseDialog {
	
	private Font font; // fonte padrão
	private Font tabHeaderFont; // fonte do cabeçalho
	private Container mainContentPane; // container principal
	private String[] columns = { "Nome", "EDITAR", "REMOVER", "UUID" }; // colunas da tabela
	private Integer[] columnWidths = { 280, 150, 80, 80 }; // largura das colunas
	private JTable deviceListTable; // tabela de dispositivos

	private JButton addLocal; 
	private JButton syncLocais; // botao de sincronização total

	private DefaultTableModel dataModel;

	private static final int CHECKBOX_COLUMN = 2; // indice de colunas com checkbox
	private JTextField nomeLocalTextField;
	private HikivisionLocaisAttachedPanel camerasHikivisionPanel;
	
	private static final SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss:sss");

	public LocaisScreen() {
		
		setIconImage(Main.favicon); //define icone da janela 
		setModal(true);
		setTitle("Configurar locais"); // decrição de qual janela esta
		setResizable(false);
		setLayout(new BorderLayout());
		setPreferredSize(new Dimension(920, 718)); // define o tamanho da janela
		setMinimumSize(getPreferredSize());

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
		syncLocais = new JButton("Sincronizar local com web");
		syncLocais.setBorder(new EmptyBorder(10, 15, 10, 15));
		syncLocais.setPreferredSize(new Dimension(180, 40));
		syncLocais.addActionListener(e -> {
			enviaLocais();
		});
		
		
		//configuraçoes de botoes
		addLocal = new JButton("Adicionar local");
		addLocal.setBorder(new EmptyBorder(10, 15, 10, 15));
		addLocal.setPreferredSize(new Dimension(180, 40));
		addLocal.addActionListener(e -> {
			adicionarDevices();
		});
		
		// ação dos botões 
		JPanel actionsPanel = new JPanel();
		actionsPanel.setLayout(new BoxLayout(actionsPanel, BoxLayout.X_AXIS));
		actionsPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		actionsPanel.setMaximumSize(new Dimension(Integer.MAX_VALUE, 100));

		actionsPanel.add(syncLocais);              // botão da esquerda
		actionsPanel.add(Box.createHorizontalGlue()); // empurra o próximo para a direita
		actionsPanel.add(addLocal);                // botão da direita
		
		if (deviceListTable.isEditing()) {
			deviceListTable.getCellEditor().stopCellEditing();
		}

		//preencher tabela
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
	
	private void adicionarDevices() {

		JDialog adicionarDeviceDialog = new JDialog();
		adicionarDeviceDialog.setIconImage(Main.favicon);
		adicionarDeviceDialog.setModal(true);
		adicionarDeviceDialog.setTitle("Adicionar locais");
		adicionarDeviceDialog.setResizable(false);
		adicionarDeviceDialog.setLayout(new BorderLayout());

		JPanel mainPanel = new JPanel();
		mainPanel.setBorder(new EmptyBorder(20, 50, 20, 50));
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

		JLabel nomeLocalLabel = new JLabel("Nome local");
		nomeLocalLabel.setPreferredSize(new Dimension(120, 25));
		nomeLocalLabel.setForeground(Main.firstColor);
		nomeLocalLabel.setFont(tabHeaderFont);
		nomeLocalTextField = getNewTextField(20);
		nomeLocalTextField.setFont(new Font("SansSerif", Font.PLAIN, 16));
		JPanel nomeLocalPanel = getNewMiniPanel(nomeLocalLabel, nomeLocalTextField);
		
		
		camerasHikivisionPanel = new HikivisionLocaisAttachedPanel();
		camerasHikivisionPanel.setLayout(new BoxLayout(camerasHikivisionPanel, BoxLayout.Y_AXIS));
		camerasHikivisionPanel.setBorder(new EmptyBorder(10, 10, 10, 10));
		
		JScrollPane scrollCamerasHikivisionPane = new JScrollPane(camerasHikivisionPanel, 
				ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, 
				ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		scrollCamerasHikivisionPane.getVerticalScrollBar().setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));
		
		
		JButton confirmarButton = new JButton("Confirmar");
		confirmarButton.setBorder(new EmptyBorder(10, 20, 10, 20));
		confirmarButton.setAlignmentX(Component.CENTER_ALIGNMENT);

		confirmarButton.addActionListener(e -> {
			
			List<AttachedTO> camerasSelecionadas = camerasHikivisionPanel.getCamerasSelecionadas();
			
			LocalEntity local = new LocalEntity();
			local.setNome(nomeLocalTextField.getText());
			local.setIdClient(Main.loggedUser.getIdClient());
			
			if (local.getUuid() == null) {
			    local.setUuid(UUID.randomUUID().toString());
			}

			List<String> equipamentosPermitidos = new ArrayList<String>();
			
			for (AttachedTO camera : camerasSelecionadas) {
			    System.out.println("Camera: " + camera.getNomeDevice() + " (ID: " + camera.getIdDevice() + ")");
			   
				equipamentosPermitidos.add(camera.getNomeDevice());
			}
			
			local.setHikivisionDeviceNames(equipamentosPermitidos);
			
			HibernateAccessDataFacade.save(LocalEntity.class, local);

			populateTable();
			adicionarDeviceDialog.dispose();
		});
		
		mainPanel.add(nomeLocalPanel);
		mainPanel.add(Box.createVerticalStrut(10));
		mainPanel.add("Cameras Hikivision", scrollCamerasHikivisionPane);
		mainPanel.add(Box.createVerticalStrut(70));
		mainPanel.add(confirmarButton);

		adicionarDeviceDialog.getContentPane().add(mainPanel, BorderLayout.CENTER);
		adicionarDeviceDialog.pack();
		adicionarDeviceDialog.setLocationRelativeTo(null);
		adicionarDeviceDialog.setVisible(true);
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
	    dataModel = new DefaultTableModel(columns, 0) {
	        @Override
	        public boolean isCellEditable(int row, int column) {
	            // Permite edição (clique) apenas nas colunas dos botões
	            return column == 1 || column == 2;
	        }
	    };

	    @SuppressWarnings("unchecked")
		List<LocalEntity> allLocaisCadastrados = (List<LocalEntity>) HibernateAccessDataFacade.getResultList(LocalEntity.class, "LocalEntity.findAllNaoRemovido");

	    if (Objects.isNull(allLocaisCadastrados) || allLocaisCadastrados.isEmpty()) {
	        deviceListTable.setModel(dataModel);
	        return;
	    }

	    for (LocalEntity local : allLocaisCadastrados) {
	        dataModel.addRow(new Object[] {
	            local.getNome(),
	            "Editar",
	            "Remover",
	            local.getUuid()
	        });
	    }

	    deviceListTable.setModel(dataModel);

	    deviceListTable.getColumn("EDITAR").setCellRenderer(new ButtonRenderer());
	    deviceListTable.getColumn("EDITAR").setCellEditor(new ButtonEditor(new JCheckBox()));
	    deviceListTable.getColumn("REMOVER").setCellRenderer(new ButtonRenderer());
	    deviceListTable.getColumn("REMOVER").setCellEditor(new ButtonEditor(new JCheckBox()));


	    formatTable();
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
	
	private JTextField getNewTextField(int columns) {
		JTextField textField = new JTextField();
		textField.setColumns(columns);
		textField.setMaximumSize(textField.getPreferredSize());

		return textField;
	}
	
	// Renderiza o botão
	class ButtonRenderer extends JButton implements TableCellRenderer {
		public ButtonRenderer() {
			setOpaque(true);
		}

		@Override
		public Component getTableCellRendererComponent(JTable table, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			setText(value != null ? value.toString() : "");
			return this;
		}
	}


	// Edita o botão (ação ao clicar)
	public class ButtonEditor extends DefaultCellEditor {
		private JButton button;
		private String label;
		private boolean isPushed;
		private int row;

		public ButtonEditor(JCheckBox checkBox) {
			super(checkBox);
			button = new JButton();
			button.setOpaque(true);

			button.addActionListener(e -> {
				if (row >= 0 && row < deviceListTable.getRowCount()) {
					String uuid = (String) deviceListTable.getValueAt(row, 3);
					if ("Editar".equals(label)) {
						editarLocal(uuid);
					} else if ("Remover".equals(label)) {
						int confirm = JOptionPane.showConfirmDialog(
							deviceListTable,
							"Deseja realmente remover o local?",
							"Confirmar remoção",
							JOptionPane.YES_NO_OPTION
						);
						if (confirm == JOptionPane.YES_OPTION) {
							removerLocal(uuid);
						}
					}
				}
				// Após ação, interrompe a edição
				fireEditingStopped();
			});
		}

		@Override
		public Component getTableCellEditorComponent(JTable table, Object value,
				boolean isSelected, int row, int column) {
			this.row = row;
			this.label = value != null ? value.toString() : "";
			button.setText(label);
			isPushed = true;
			return button;
		}

		@Override
		public Object getCellEditorValue() {
			isPushed = false;
			return label;
		}
	}


	
	private void editarDevices(LocalEntity local) {

		JDialog editarDeviceDialog = new JDialog();
		editarDeviceDialog.setIconImage(Main.favicon);
		editarDeviceDialog.setModal(true);
		editarDeviceDialog.setTitle("Editar local");
		editarDeviceDialog.setResizable(false);
		editarDeviceDialog.setLayout(new BorderLayout());

		JPanel mainPanel = new JPanel();
		mainPanel.setBorder(new EmptyBorder(20, 50, 20, 50));
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

		JLabel nomeLocalLabel = new JLabel("Nome local");
		nomeLocalLabel.setPreferredSize(new Dimension(120, 25));
		nomeLocalLabel.setForeground(Main.firstColor);
		nomeLocalLabel.setFont(tabHeaderFont);

		nomeLocalTextField = getNewTextField(20);
		nomeLocalTextField.setFont(new Font("SansSerif", Font.PLAIN, 16));
		nomeLocalTextField.setText(local.getNome()); // <-- preenchendo nome existente

		JPanel nomeLocalPanel = getNewMiniPanel(nomeLocalLabel, nomeLocalTextField);

		camerasHikivisionPanel = new HikivisionLocaisAttachedPanel();
		camerasHikivisionPanel.setLayout(new BoxLayout(camerasHikivisionPanel, BoxLayout.Y_AXIS));
		camerasHikivisionPanel.setBorder(new EmptyBorder(10, 10, 10, 10));

//		// Preenche seleção de câmeras (se o seu painel tiver método para isso)
		camerasHikivisionPanel.setCamerasSelecionadas(local.getHikivisionDeviceNames());

		JScrollPane scrollCamerasHikivisionPane = new JScrollPane(camerasHikivisionPanel,
				ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
				ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		scrollCamerasHikivisionPane.getVerticalScrollBar().setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));

		JButton confirmarButton = new JButton("Salvar alterações");
		confirmarButton.setBorder(new EmptyBorder(10, 20, 10, 20));
		confirmarButton.setAlignmentX(Component.CENTER_ALIGNMENT);

		confirmarButton.addActionListener(e -> {
			List<AttachedTO> camerasSelecionadas = camerasHikivisionPanel.getCamerasSelecionadas();
			List<String> equipamentosPermitidos = new ArrayList<>();

			for (AttachedTO camera : camerasSelecionadas) {
				System.out.println("Camera: " + camera.getNomeDevice() + " (ID: " + camera.getIdDevice() + ")");
				equipamentosPermitidos.add(camera.getNomeDevice());
			}

			local.setNome(nomeLocalTextField.getText());
			local.setHikivisionDeviceNames(equipamentosPermitidos);
			local.setDataAlteracao(new Date());
			
			HibernateAccessDataFacade.update(LocalEntity.class, local);

			populateTable();
			editarDeviceDialog.dispose();
		});

		mainPanel.add(nomeLocalPanel);
		mainPanel.add(Box.createVerticalStrut(10));
		mainPanel.add("Cameras Hikivision", scrollCamerasHikivisionPane);
		mainPanel.add(Box.createVerticalStrut(70));
		mainPanel.add(confirmarButton);

		editarDeviceDialog.getContentPane().add(mainPanel, BorderLayout.CENTER);
		editarDeviceDialog.pack();
		editarDeviceDialog.setLocationRelativeTo(null);
		editarDeviceDialog.setVisible(true);
	}

	
	private void editarLocal(String uuid) {
		HashMap<String, Object> args = new HashMap<String, Object>();
		args.put("UUID", uuid);

		@SuppressWarnings("unchecked")
		List<LocalEntity> local = (List<LocalEntity>) HibernateAccessDataFacade.getResultListWithParams(LocalEntity.class, "LocalEntity.findByUuid", args);
		
		if (local.get(0) != null) {
			editarDevices(local.get(0));
		}
	}

	private void removerLocal(String uuid) {
	    int confirm = JOptionPane.showConfirmDialog(this, "Deseja remover o local?", "Confirmação", JOptionPane.YES_NO_OPTION);
	    if (confirm == JOptionPane.YES_OPTION) {
	    	HashMap<String, Object> args = new HashMap<String, Object>();
			args.put("UUID", uuid);

			@SuppressWarnings("unchecked")
			List<LocalEntity> local = (List<LocalEntity>) HibernateAccessDataFacade.getResultListWithParams(LocalEntity.class, "LocalEntity.findByUuid", args);
			LocalEntity localSelecionado = local.get(0);
	        if (local.get(0) != null) {
	        	
	        	localSelecionado.setDataRemovido(new Date());
	        	localSelecionado.setRemoved(true);
	        	localSelecionado.setDataAlteracao(new Date());
	        	
	        	HibernateAccessDataFacade.update(LocalEntity.class, localSelecionado);
	        	
	            populateTable();
	        }
	    }
	}
	
	private void enviaLocais() {
		
		@SuppressWarnings("unchecked")
		List<LocalEntity> locais = (List<LocalEntity>) HibernateAccessDataFacade.getResultList(LocalEntity.class,
				"LocalEntity.findAll");

		if (locais == null || locais.isEmpty()) {
			System.out.println(sdf.format(new Date()) + "   LOCAIS DE ACESSO: sem registros para enviar");
			return;
		}

		JsonArray responseArray = new JsonArray();
		for (LocalEntity local : locais) {
			// Garante que sempre exista UUID
			if (local.getUuid() == null) {
				local.setUuid(UUID.randomUUID().toString());
				HibernateAccessDataFacade.update(LocalEntity.class, local);
			}

			JsonObject responseObj = getNewLocalResponseObj(local);
			responseArray.add(responseObj);
		}

		System.out.println("Enviando request com locais: " + responseArray.size());

		int responseCode;

		try {
			HttpConnection con = new HttpConnection(Main.urlApplication + "/restful-services/access/uploadLocais"); // fazer
																													// o
																													// endpoint
			responseCode = con.sendResponse(responseArray.toString());

			if (responseCode != 200) {
				System.out.println(sdf.format(new Date()) + "  ERRO AO ENVIAR LOCAIS: Response code: " + responseCode);
				System.out.println(
						sdf.format(new Date()) + "  ERRO AO ENVIAR LOCAIS: Error String: " + con.getErrorString());
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private JsonObject getNewLocalResponseObj(LocalEntity local) {
		JsonObject responseObj = new JsonObject();

		responseObj.addProperty("id", local.getId() != null ? local.getId().toString() : "");
		responseObj.addProperty("uuid", local.getUuid());
		responseObj.addProperty("idCliente", Main.loggedUser.getIdClient());
		responseObj.addProperty("nome", local.getNome() != null ? local.getNome() : "");
		responseObj.addProperty("removido", local.getRemoved() != null ? local.getRemoved().toString() : "");

		// Adiciona a lista de nomes das câmeras
		JsonArray deviceNamesArray = new JsonArray();
		if (local.getHikivisionDeviceNames() != null) {
			for (String name : local.getHikivisionDeviceNames()) {
				deviceNamesArray.add(name);
			}
		}
		responseObj.add("hikvisionDeviceNames", deviceNamesArray);

		return responseObj;
	}
	
}
