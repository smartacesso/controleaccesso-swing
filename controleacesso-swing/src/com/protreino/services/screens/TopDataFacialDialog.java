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
import java.util.Base64;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
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

import org.java_websocket.WebSocket;

import com.protreino.services.entity.HikivisionIntegrationErrorEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
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
	
	private DefaultTableModel dataModel;
	
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
			syncDevices();
		});

		//ação dos botoes 
		JPanel actionsPanel = new JPanel();
		actionsPanel.setLayout(new BoxLayout(actionsPanel, BoxLayout.X_AXIS));
		actionsPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		actionsPanel.setMaximumSize(new Dimension(Integer.MAX_VALUE, 100));
		actionsPanel.add(Box.createHorizontalStrut(725));
		actionsPanel.add(syncAll);
		
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
	
	private void syncDevices() {
		// fazer uma query de connt para contar quantos pedestres vao ser sincronizados.
		// fazer a busca paginada
		
	
		List<String> devicesToSync = getDevicesToSync();
		
		if (devicesToSync.isEmpty()) {
			return;
		}
		
		final int pageSize = 500;
		
		final Integer countPedestresParaSincronizar = countPesdestresParaSincronizar();
		System.out.println("Pedestre encontrados: " + countPedestresParaSincronizar);
		
		if(Objects.isNull(countPedestresParaSincronizar) || countPedestresParaSincronizar.equals(0)) {
			return;
		}

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
					List<PedestrianAccessEntity> pedestresParaSicronizar = buscaPedestresParaSicronizar(offset, pageSize);
					
					pedestresParaSicronizar.forEach(pedestre -> {
						try {
							Long id = Long.valueOf(pedestre.getCardNumber());
							String nome =  pedestre.getName();
							String foto =   Base64.getEncoder().encodeToString(pedestre.getFoto());
							
							Main.facialTopDataIntegrationService.cadastrarPedestreBySync(id, nome, foto, devicesToSync);
							
						} catch(Exception ex) {
							System.out.println(ex.getMessage());
						}
						
						progressBar.setValue(progressBar.getValue() + 1);
					});
					
					offset += pageSize;
				} while (offset < countPedestresParaSincronizar);	
				progressBarDialog.dispose();
			}
			
			
		});
		thread.setDaemon(true);
		thread.start();		
		progressBarDialog.setVisible(true);	
	}
	
	private List<String> getDevicesToSync() {
		TableModel model = deviceListTable.getModel();
		List<String> devicesToSync = new ArrayList<>();
		for (int i = 0; i < model.getRowCount(); i++) {
			if (Boolean.valueOf(model.getValueAt(i, 2).toString())) {
				devicesToSync.add(model.getValueAt(i, 0).toString());
			}
		}

		return devicesToSync;
	}
	
	private Integer countPesdestresParaSincronizar() {
		return HibernateAccessDataFacade.
	           getResultListWithParamsCount(PedestrianAccessEntity.class, "PedestrianAccessEntity.countAllSyncTopData", null);
	}	
	
	@SuppressWarnings("unchecked")
	private List<PedestrianAccessEntity> buscaPedestresParaSicronizar(Integer offset, Integer pageSize) {
		
		return (List<PedestrianAccessEntity>) HibernateAccessDataFacade.getResultListWithParams(PedestrianAccessEntity.class,
				"PedestrianAccessEntity.findAllSyncTopData", null, offset, pageSize);
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
    
	    List<WebSocket> allTopDataFacialDevicesConnected = Main.facialTopDataIntegrationService.getAllTopDataFacialDevicesConnected();
	    if(Objects.isNull(allTopDataFacialDevicesConnected) || allTopDataFacialDevicesConnected.isEmpty()) {
			deviceListTable.setModel(dataModel);
			return;
	    }

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
