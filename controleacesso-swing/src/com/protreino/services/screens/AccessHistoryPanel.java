package com.protreino.services.screens;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import org.jdatepicker.ComponentColorDefaults;
import org.jdatepicker.ComponentColorDefaults.Key;
import org.jdatepicker.JDatePicker;

import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.main.Main;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.utils.SelectItem;
import com.protreino.services.utils.Utils;

@SuppressWarnings("serial")
public class AccessHistoryPanel extends PaginedListPanel {
	
	private JTable accessHistoryTable;
	private List<LogPedestrianAccessEntity> historicoAcesso;
	private String[] columns = {"Código", "Cartão","Tipo", "Nome", "Data do acesso", "Tipo de acesso"};
	private Integer[] columnWidths = {50, 60, 60, 270, 120, 130};
	 
	private JComboBox<SelectItem> filtroTipoJComboBox;
	private JTextField filtroNomeTextField;
	private JDatePicker filtroDataInicioDatePicker;
	private JDatePicker filtroDataFimDatePicker;
	private SimpleDateFormat sdf;
	private JButton syncButton;
	private JLabel dateLastSync;
	private SimpleDateFormat sdfComHora = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
	
	private HashMap<String, Object> args;
	
	public AccessHistoryPanel(){
		
		Font font = new JLabel().getFont();
		Font headerFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
		sdf = new SimpleDateFormat("dd/MM/yyyy");
		
		ComponentColorDefaults.getInstance().setColor(Key.FG_MONTH_SELECTOR, Main.firstColor);
        ComponentColorDefaults.getInstance().setColor(Key.BG_MONTH_SELECTOR, Main.secondColor);
        ComponentColorDefaults.getInstance().setColor(Key.FG_GRID_TODAY, Main.secondColor);
        ComponentColorDefaults.getInstance().setColor(Key.FG_GRID_TODAY_SELECTED, Main.secondColor);
        
        JPanel filtroTipoPanel= new JPanel();
		filtroTipoPanel.setLayout(new BoxLayout(filtroTipoPanel, BoxLayout.Y_AXIS));
		JLabel filtroTipoLabel = new JLabel("Tipo");
		filtroTipoLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroTipoPanel.add(filtroTipoLabel);
		
		Vector<SelectItem> itens = new Vector<SelectItem>();
		itens.add(new SelectItem("Todos", "TODOS"));
		itens.add(new SelectItem("PEDESTRE", "PEDESTRE"));
		itens.add(new SelectItem("VISITANTE", "VISITANTE"));

		filtroTipoJComboBox =  new JComboBox<SelectItem>(itens);
		filtroTipoJComboBox.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroTipoPanel.add(filtroTipoJComboBox);
		filtroTipoPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		JLabel filtroNomeLabel = new JLabel("Nome");
		filtroNomeLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroNomeTextField = new JTextField("", System.getProperty("os.name").toLowerCase().contains("linux") ? 20 : 30);
		filtroNomeTextField.setAlignmentX(Component.LEFT_ALIGNMENT);
		JPanel filtroNomePanel= new JPanel();
		filtroNomePanel.setLayout(new BoxLayout(filtroNomePanel, BoxLayout.Y_AXIS));
		filtroNomePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroNomePanel.add(filtroNomeLabel);
		filtroNomePanel.add(filtroNomeTextField);
		
		JLabel filtroDataLabel = new JLabel("Data de acesso");
		filtroDataLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroDataInicioDatePicker = new JDatePicker(new Date());
		filtroDataInicioDatePicker.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroDataInicioDatePicker.setPreferredSize(System.getProperty("os.name").toLowerCase().contains("linux") 
				? new Dimension(120, 30)
				: new Dimension(100, 21));
		filtroDataFimDatePicker = new JDatePicker(new Date());
		filtroDataFimDatePicker.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroDataFimDatePicker.setPreferredSize(System.getProperty("os.name").toLowerCase().contains("linux") 
				? new Dimension(120, 30)
				: new Dimension(100, 22));
		JPanel datePickersPanel= new JPanel();
		datePickersPanel.setLayout(new BoxLayout(datePickersPanel, BoxLayout.X_AXIS));
		datePickersPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		datePickersPanel.add(filtroDataInicioDatePicker);
		datePickersPanel.add(new JLabel(" atÃ© "));
		datePickersPanel.add(filtroDataFimDatePicker);
		JPanel filtroDataPanel= new JPanel();
		filtroDataPanel.setLayout(new BoxLayout(filtroDataPanel, BoxLayout.Y_AXIS));
		filtroDataPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroDataPanel.add(filtroDataLabel);
		filtroDataPanel.add(datePickersPanel);
		
		JButton cleanButton = new JButton("Limpar filtros");
		JPanel cleanButtonPanel= new JPanel();
		cleanButtonPanel.setLayout(new BoxLayout(cleanButtonPanel, BoxLayout.Y_AXIS));
		cleanButtonPanel.add(new JLabel(" "));
		cleanButtonPanel.add(cleanButton);
		
		JButton searchButton = new JButton("Pesquisar");;
		JPanel searchButtonPanel= new JPanel();
		searchButtonPanel.setLayout(new BoxLayout(searchButtonPanel, BoxLayout.Y_AXIS));
		searchButtonPanel.add(new JLabel(" "));
		searchButtonPanel.add(searchButton);
		
		JPanel filterFlowPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
		filterFlowPanel.setMaximumSize(new Dimension(10000, 80));
		filterFlowPanel.add(filtroNomePanel);
		filterFlowPanel.add(Box.createHorizontalStrut(10));
		filterFlowPanel.add(filtroTipoPanel);
		filterFlowPanel.add(Box.createHorizontalStrut(10));
		filterFlowPanel.add(filtroDataPanel);
		filterFlowPanel.add(Box.createHorizontalStrut(10));
		filterFlowPanel.add(cleanButtonPanel);
		filterFlowPanel.add(Box.createHorizontalStrut(10));
		filterFlowPanel.add(searchButtonPanel);
		
		
		JPanel filterPanel= new JPanel();
		filterPanel.setLayout(new BoxLayout(filterPanel, BoxLayout.X_AXIS));
		filterPanel.add(filterFlowPanel);
		
		JPanel accessHistoryTablePanel = new JPanel();
		accessHistoryTablePanel.setLayout(new BoxLayout(accessHistoryTablePanel, BoxLayout.Y_AXIS));
		accessHistoryTable = new JTable(new DefaultTableModel(columns, 0));
		formatTable();
		accessHistoryTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
		accessHistoryTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		accessHistoryTable.getTableHeader().setReorderingAllowed(false);
		accessHistoryTable.getTableHeader().setOpaque(false);
		accessHistoryTable.getTableHeader().setForeground(Main.firstColor);
		if(!System.getProperty("os.name").toLowerCase().contains("linux"))
			accessHistoryTable.getTableHeader().setBackground(Main.secondColor);
		accessHistoryTable.getTableHeader().setFont(headerFont);
		accessHistoryTable.setRowHeight(30);
		accessHistoryTable.setSelectionBackground(Main.firstColor);
		accessHistoryTable.setSelectionForeground(Color.WHITE);
		TableCellRenderer rendererFromHeader = accessHistoryTable.getTableHeader().getDefaultRenderer();
		JLabel headerLabel = (JLabel) rendererFromHeader;
		headerLabel.setHorizontalAlignment(JLabel.CENTER);
		JScrollPane scrollPane = new JScrollPane(accessHistoryTable);
		scrollPane.getVerticalScrollBar().setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));
		accessHistoryTablePanel.add(scrollPane);
		
		dateLastSync = new JLabel(" ");
		syncButton = new JButton("Enviar lista para o servidor");
		syncButton.setBorder(new EmptyBorder(10,15,10,15));
		syncButton.setPreferredSize(new Dimension(180, 40));
		
		JPanel paginatorPanel = createPaginatorControls();
		
		JPanel statusPanel = new JPanel();
		statusPanel.setLayout(new BoxLayout(statusPanel, BoxLayout.X_AXIS));
		statusPanel.setMaximumSize(new Dimension(Integer.MAX_VALUE, 100));
		statusPanel.add(paginatorPanel);
		statusPanel.add(Box.createHorizontalGlue());
		statusPanel.add(dateLastSync);
		statusPanel.add(Box.createHorizontalStrut(10));
		statusPanel.add(syncButton);
		
		updateDateLastSync();
		
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		setAlignmentX(Component.LEFT_ALIGNMENT);
		setBorder(BorderFactory.createEmptyBorder(10,10,10,10));
		add(filterPanel);
		add(Box.createVerticalStrut(5));
		add(accessHistoryTablePanel);
		add(Box.createVerticalStrut(5));
		add(statusPanel);
		
		cleanButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				cleanFilter();
			}
		});
		
		syncButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Main.syncLogAthleteAccess();
			}
		});
		
		ActionListener search = new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				filterList();
			}
		};
		
		searchButton.addActionListener(search);
		filtroNomeTextField.addActionListener(search);
		filtroDataInicioDatePicker.addActionListener(search);
		filtroDataFimDatePicker.addActionListener(search);
		filtroTipoJComboBox.addItemListener(new ItemListener() {
			@Override
			public void itemStateChanged(ItemEvent e) {
				filterList();
			}
			
		});
		
	}
	
	private void filterList() {
		args = new HashMap<String, Object>();
		try {
			Calendar dataFim = Calendar.getInstance();
			dataFim.setTime(Utils.isNullOrEmpty(filtroDataFimDatePicker.getFormattedTextField().getText()) 
					? new Date() : sdf.parse(filtroDataFimDatePicker.getFormattedTextField().getText()));
			dataFim.set(Calendar.HOUR_OF_DAY, 23);
			dataFim.set(Calendar.MINUTE, 59);
			dataFim.set(Calendar.SECOND, 59);
			dataFim.set(Calendar.MILLISECOND, 999);
			
			args.put("DATA_INICIO", Utils.isNullOrEmpty(filtroDataInicioDatePicker.getFormattedTextField().getText()) 
					? new Date() : sdf.parse(filtroDataInicioDatePicker.getFormattedTextField().getText()));
			args.put("DATA_FIM", dataFim.getTime());
		
		} catch (Exception e){
			e.printStackTrace();
			args = new HashMap<>();
			args.put("DATA_INICIO", new Date());
			args.put("DATA_FIM", new Date());
		}
		
		SelectItem itemSelecionado = null;
		if(filtroTipoJComboBox.getSelectedItem() != null)
			itemSelecionado = (SelectItem) filtroTipoJComboBox.getSelectedItem();
		
		args.put("NOME", !"".equals(filtroNomeTextField.getText()) ? "%" + filtroNomeTextField.getText() + "%" : "vazio");
		args.put("TIPO", !"TODOS".equals(itemSelecionado.getValue()) ? "%" + itemSelecionado.getValue() + "%" : "vazio");
		
		
		paginaAtual = 1;
		inicioPagina = 0;
		totalRegistros =  HibernateAccessDataFacade.getResultListWithParamsCount(LogPedestrianAccessEntity.class, 
															"LogPedestrianAccessEntity.countByPeriod", args);
		executeFilter();
		
	}
	
	@SuppressWarnings("unchecked")
	@Override
	protected void executeFilter() {
		
		calculaTamanhoPaginas();
		
		historicoAcesso = (List<LogPedestrianAccessEntity>)
				HibernateAccessDataFacade.getResultListWithParams(LogPedestrianAccessEntity.class, 
							"LogPedestrianAccessEntity.findByPeriod", args, inicioPagina, registrosPorPagina);

		populateTable(historicoAcesso);
		
		paginatorControl();
		
	}
	
	public void cleanFilter(){
		filtroNomeTextField.setText("");
     	filtroTipoJComboBox.setSelectedIndex(0);
     	Calendar calendar = Calendar.getInstance();
		filtroDataInicioDatePicker.getJDateInstantPanel().getModel().setDay(calendar.get(Calendar.DAY_OF_MONTH));
		filtroDataInicioDatePicker.getJDateInstantPanel().getModel().setMonth(calendar.get(Calendar.MONTH));
		filtroDataInicioDatePicker.getJDateInstantPanel().getModel().setYear(calendar.get(Calendar.YEAR));
		filtroDataFimDatePicker.getJDateInstantPanel().getModel().setDay(calendar.get(Calendar.DAY_OF_MONTH));
		filtroDataFimDatePicker.getJDateInstantPanel().getModel().setMonth(calendar.get(Calendar.MONTH));
		filtroDataFimDatePicker.getJDateInstantPanel().getModel().setYear(calendar.get(Calendar.YEAR));
		
		filterList();
	}
	
	private void populateTable(List<LogPedestrianAccessEntity> historicoAcesso){
		DefaultTableModel dataModel = new DefaultTableModel(columns, 0) {
			public boolean isCellEditable(int rowIndex, int mColIndex) {
				return false;
			}
		};
		
		if (historicoAcesso != null && !historicoAcesso.isEmpty()){
			SimpleDateFormat sdfTime = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
			
			for (LogPedestrianAccessEntity logAcesso : historicoAcesso) {
				Object[] obj = new Object[6];
				if (logAcesso.getIdPedestrian() != null) {
			
					PedestrianAccessEntity pedestre = logAcesso.getPedestre();
					if(pedestre == null) continue;
					
					obj[0] = logAcesso.getIdPedestrian();
					obj[1] = (pedestre.getCardNumber() != null && !"".equals(pedestre.getCardNumber())) ? pedestre.getCardNumber() : "-";
					obj[2] = pedestre.getTipo() != null ? pedestre.getTipo() : "-";
					obj[3] = pedestre.getName();
					obj[4] = logAcesso.getAccessDate() != null ? sdfTime.format(logAcesso.getAccessDate()) : "-";
					obj[5] = montaTipoAcesso(logAcesso);

				} else {
					obj[0] = "-";
					obj[1] = "-";
					obj[2] = "-";
					obj[3] = "-";
					obj[4] = logAcesso.getAccessDate() != null ? sdfTime.format(logAcesso.getAccessDate()) : "-";
					obj[5] = "LIBERADO PELO SISTEMA";
				}
				dataModel.addRow(obj);
			}
		}
		accessHistoryTable.setModel(dataModel);
		//int numAcessos = (historicoAcesso != null ? historicoAcesso.size() : 0);
		//countLabel.setText("Número de registros: " + numAcessos);
		countLabel.setText("PÃ¡g. ("+ paginaAtual + "/" + totalPaginas + ") do total: " + totalRegistros);
		formatTable();
	}

	private Object montaTipoAcesso(LogPedestrianAccessEntity logAcesso) {
		
		String texto = "ATIVO".equals(logAcesso.getStatus()) 
							? "LIBERADO" 
							: ("INATIVO".equals(logAcesso.getStatus()) 
									? "BLOQUEADO" 
								    : ("INDEFINIDO".equals(logAcesso.getStatus()) 
								    		? "NÃƒO GIROU" 
								    		: "LIBERADO PELO SISTEMA"));
		if(logAcesso.getDirection() != null) {
			texto += " " + logAcesso.getDirection();
		}
		
		if(logAcesso.getReason() != null && !logAcesso.getReason().isEmpty()) {
			texto += " - " + logAcesso.getReason();
		}
		
		return texto;
	}
	
	private void formatTable(){
		DefaultTableCellRenderer centerRenderer = new DefaultTableCellRenderer();
		centerRenderer.setHorizontalAlignment(JLabel.CENTER);
		try {
			for (int i = 0; i < accessHistoryTable.getColumnCount(); i++){
				try {
					TableColumn column = accessHistoryTable.getColumnModel().getColumn(i);
					column.setPreferredWidth(columnWidths[i]);
					column.setCellRenderer(centerRenderer);
				}catch (Exception e) {
					
				}
			}
		} catch (Exception e) {
		}
	}

	public JButton getSyncButton() {
		return syncButton;
	}

	public void updateDateLastSync() {
		if (Main.lastSyncLog != null && Main.lastSyncLog > 0) {
			Date date = new Date(Main.lastSyncLog);
			dateLastSync.setText("Atualizado: " + sdfComHora.format(date));
		}
		else
			dateLastSync.setText(" ");
	}
	
	public boolean isLoad() {
		return historicoAcesso != null && !historicoAcesso.isEmpty();
	}
	
	}		



