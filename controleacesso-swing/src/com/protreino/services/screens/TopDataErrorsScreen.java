package com.protreino.services.screens;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
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
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.entity.TopdataFacialErrorEntity;
import com.protreino.services.main.Main;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.repository.HibernateServerAccessData;
import com.protreino.services.repository.TopDataFacialErrorRepository;
import com.protreino.services.usecase.ReleaseAccessUseCase;
import com.protreino.services.utils.SelectItem;
import com.protreino.services.utils.Utils;

@SuppressWarnings("serial")
public class TopDataErrorsScreen extends PaginedListPanel {

	private JTable ErrorListTable;
	private List<TopdataFacialErrorEntity> listaErrors;
	private String[] columns = {"Id Pedestre", "IP Facial", "Cartao", "Nome", "Data"};
	private Integer[] columnWidths = { 60, 70, 70, 280, 70};

	private JTextField filtroIdTextField;
	private JTextField filtroCartaoTextField;
	private JTextField filtroNomeTextField;
	private JComboBox<SelectItem> filtroTipoJComboBox;

	private JButton cleanButton;
	private JButton searchButton;
	
	private List<Integer> colunasComLink;

	private SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");


	public TopDataErrorsScreen(){
		
		
		colunasComLink = new ArrayList<Integer>();
		colunasComLink.add(3);
		
		Font font = new JLabel().getFont();
		Font headerFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
		
		JPanel cleanButtonPanel= new JPanel();
		cleanButtonPanel.setLayout(new BoxLayout(cleanButtonPanel, BoxLayout.Y_AXIS));
		cleanButton = new JButton("Limpar filtros");
		cleanButtonPanel.add(new JLabel(" "));
		cleanButtonPanel.add(cleanButton);
		
		JPanel searchButtonPanel= new JPanel();
		searchButtonPanel.setLayout(new BoxLayout(searchButtonPanel, BoxLayout.Y_AXIS));
		searchButton = new JButton("Pesquisar");
		searchButtonPanel.add(new JLabel(" "));
		searchButtonPanel.add(searchButton);
		
		JPanel filterPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 0, 0));
		filterPanel.setMaximumSize(new Dimension(10000, 150));
		filterPanel.add(cleanButtonPanel);
		filterPanel.add(Box.createHorizontalStrut(10));
		filterPanel.add(searchButtonPanel);
		
		JPanel accessListTablePanel = new JPanel();
		accessListTablePanel.setLayout(new BoxLayout(accessListTablePanel, BoxLayout.Y_AXIS));
		ErrorListTable = new JTable(new DefaultTableModel(columns, 0));
		formatTable();
		ErrorListTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
		ErrorListTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		ErrorListTable.getTableHeader().setReorderingAllowed(false);
		ErrorListTable.getTableHeader().setOpaque(false);
		ErrorListTable.getTableHeader().setForeground(Main.firstColor);
		if(!System.getProperty("os.name").toLowerCase().contains("linux"))
			ErrorListTable.getTableHeader().setBackground(Main.secondColor);
		ErrorListTable.getTableHeader().setFont(headerFont);
		ErrorListTable.setRowHeight(30);
		ErrorListTable.setSelectionBackground(Main.firstColor);
		ErrorListTable.setSelectionForeground(Color.WHITE);
		TableCellRenderer rendererFromHeader = ErrorListTable.getTableHeader().getDefaultRenderer();
		JLabel headerLabel = (JLabel) rendererFromHeader;
		headerLabel.setHorizontalAlignment(JLabel.CENTER);
		JScrollPane scrollPane = new JScrollPane(ErrorListTable);
		scrollPane.getVerticalScrollBar().setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));
		accessListTablePanel.add(scrollPane);
		
		JPanel paginatorPanel = createPaginatorControls();
		
		JPanel statusPanel = new JPanel();
		statusPanel.setLayout(new BoxLayout(statusPanel, BoxLayout.X_AXIS));
		statusPanel.setMaximumSize(new Dimension(Integer.MAX_VALUE, 100));
		statusPanel.add(paginatorPanel);
		statusPanel.add(Box.createHorizontalGlue());
		
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		setBorder(BorderFactory.createEmptyBorder(10,10,10,10));
		add(filterPanel);
		add(Box.createRigidArea(new Dimension(0,5)));
		add(accessListTablePanel);
		add(Box.createRigidArea(new Dimension(0,5)));
		add(statusPanel);
		
		cleanButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				cleanFilter();
			}
		});
				
		ActionListener search = new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				filterList();
			}
		};
		
		searchButton.addActionListener(search);
		
	}
	
	private void filterList() {
		
		paginaAtual = 1;
		inicioPagina = 0;
		
		executeFilter();
	}



	
	public void cleanFilter(){
				
		totalRegistros = 0;  

		//calcula páginas
		calculaTamanhoPaginas();
		
		listaErrors = null;
		
		//if(Main.internoLoggedUser != null)
			//colunasComLink.add(2);
		populateTable(listaErrors);
		
		paginatorControl();
	}
	

	
	@Override
	protected void executeFilter() {

		if(Main.temServidor()) {
			totalRegistros = HibernateServerAccessData.CountBuscaErrosTopDataServidor();
			listaErrors = HibernateServerAccessData.BuscaErrosTopDataServidor();
		}else {	
			totalRegistros = buscaQuantidadeErrors();
			listaErrors = buscaErrorsVisitante();
		}
		
		calculaTamanhoPaginas();
		populateTable(listaErrors);
		
		paginatorControl();
		
	}
	
	private Integer buscaQuantidadeErrors() {
		final TopDataFacialErrorRepository topDataFacialErrorRepository = new TopDataFacialErrorRepository();
		return topDataFacialErrorRepository.countFindAll();
	}
	
	private List<TopdataFacialErrorEntity> buscaErrorsVisitante() {
		final TopDataFacialErrorRepository topDataFacialErrorRepository = new TopDataFacialErrorRepository();
		return topDataFacialErrorRepository.findAll();
	}
	
	private void populateTable(List<TopdataFacialErrorEntity> listaErro){
		DefaultTableModel dataModel = new DefaultTableModel(columns, 0) {
			public boolean isCellEditable(int rowIndex, int mColIndex) {
				return false;
			}
		};
		if (listaErro != null && !listaErro.isEmpty()){

			for (TopdataFacialErrorEntity erro : listaErro) {
				Object[] obj = new Object[8];
				obj[0] =  Objects.nonNull(erro.getPedestre()) ? erro.getPedestre().getId() : ""; 
				obj[1] = erro.getIpFacial();
				obj[2] = erro.getCardNumber();
				obj[3] = Objects.nonNull(erro.getPedestre()) ? erro.getPedestre().getName() : "";
				obj[4] = sdf.format(erro.getErrorDate());
				dataModel.addRow(obj);
			}
		}
		ErrorListTable.setModel(dataModel);

		countLabel.setText("Pag. ("+ paginaAtual + "/" + totalPaginas + ") do total: " + totalRegistros);
		formatTable();
	}
	
	@SuppressWarnings("unused")
	private String montaLiberado(PedestrianAccessEntity acesso) {
		
		String texto = "--";
		
		if("VISITANTE".equals(acesso.getTipo())) {
			//visitante
			texto = acesso.getCardNumber() == null || acesso.getCardNumber().isEmpty() ? "--" : "Acesso unico";
			if(acesso.getQuantidadeCreditos() != null && acesso.getQuantidadeCreditos() > 1l) {
				texto = acesso.getQuantidadeCreditos() + "x creditos ";
				if(acesso.getValidadeCreditos() != null)
					texto += " ate " + new SimpleDateFormat("dd/MM/yyyy").format(acesso.getValidadeCreditos());
			
			} else if(acesso.getDataInicioPeriodo() != null && acesso.getDataFimPeriodo() != null) {
				texto = new SimpleDateFormat("dd/MM/yyyy").format(acesso.getDataInicioPeriodo());
				texto += " ate " + new SimpleDateFormat("dd/MM/yyyy").format(acesso.getDataFimPeriodo());

			} else if(acesso.getDataInicioPeriodo() != null) {
				texto = "Inicia em " + new SimpleDateFormat("dd/MM/yyyy").format(acesso.getDataInicioPeriodo());
			}
			
		} else{
			
			//pedestre
			if(acesso.getQuantidadeCreditos() != null) {
				texto = acesso.getQuantidadeCreditos() + "x creditos ";
				if(acesso.getValidadeCreditos() != null)
					texto += " ate " + new SimpleDateFormat("dd/MM/yyyy").format(acesso.getValidadeCreditos());
			} else if(acesso.getValidadeCreditos() != null) {
				texto += new SimpleDateFormat("dd/MM/yyyy").format(acesso.getValidadeCreditos());
			}
			
			if(acesso.getDataInicioPeriodo() != null && acesso.getDataFimPeriodo() != null) {
				texto = new SimpleDateFormat("dd/MM/yyyy").format(acesso.getDataInicioPeriodo());
				texto += " ate " + new SimpleDateFormat("dd/MM/yyyy").format(acesso.getDataFimPeriodo());

			} else if(acesso.getDataInicioPeriodo() != null) {
				texto = "Inicia em " + new SimpleDateFormat("dd/MM/yyyy").format(acesso.getDataInicioPeriodo());
			}
		}
		
		return texto;
	}

	private void formatTable(){
		UrlRenderer urlRenderer = new UrlRenderer(colunasComLink);
		urlRenderer.setHorizontalAlignment(JLabel.CENTER);
		ActionRenderer actionRenderer = new ActionRenderer(colunasComLink);
		actionRenderer.setHorizontalAlignment(JLabel.CENTER);
		EditVisitanteRenderer editVisitanteRenderer = null;
		if(Main.internoLoggedUser != null) {
			editVisitanteRenderer = new EditVisitanteRenderer(colunasComLink);
			editVisitanteRenderer.setHorizontalAlignment(JLabel.CENTER);
		}
		DefaultTableCellRenderer centerRenderer = new DefaultTableCellRenderer();
		centerRenderer.setHorizontalAlignment(JLabel.CENTER);
		
		while (ErrorListTable.getMouseListeners().length > 2) {
			ErrorListTable.removeMouseListener(ErrorListTable.
					getMouseListeners()[ErrorListTable.getMouseListeners().length-1]);			
		}
		
		if(Main.internoLoggedUser != null) {
			ErrorListTable.addMouseListener(editVisitanteRenderer);			
		}
		
		ErrorListTable.addMouseListener(urlRenderer);
		ErrorListTable.addMouseListener(actionRenderer);

		while (ErrorListTable.getMouseMotionListeners().length > 2) {
			ErrorListTable.removeMouseMotionListener(ErrorListTable.
					getMouseMotionListeners()[ErrorListTable.getMouseMotionListeners().length-1]);			
		}
		
		if(Main.internoLoggedUser != null) {
			ErrorListTable.addMouseMotionListener(editVisitanteRenderer);			
		}

		ErrorListTable.addMouseMotionListener(urlRenderer);
		ErrorListTable.addMouseMotionListener(actionRenderer);
		
		for (int i = 0; i < ErrorListTable.getColumnCount(); i++){
			TableColumn column = ErrorListTable.getColumnModel().getColumn(i);
			column.setPreferredWidth(columnWidths[i]);
			
			if (i == 2 && Main.internoLoggedUser != null)
				column.setCellRenderer(editVisitanteRenderer);
			else if (i == 4)
				column.setCellRenderer(urlRenderer);
			else if (i == 6)
				column.setCellRenderer(actionRenderer);
			else
				column.setCellRenderer(centerRenderer);	
		}
		
		//accessListTable.setRowHeight(34);
	}
	
	
	
	public boolean isFiltering(){
		return (filtroIdTextField.getText() != null && !filtroIdTextField.getText().isEmpty())
				|| (filtroCartaoTextField.getText() != null && !filtroCartaoTextField.getText().isEmpty())
				|| (filtroNomeTextField.getText() != null && !filtroNomeTextField.getText().isEmpty())
				|| (filtroTipoJComboBox.getSelectedItem() != null );
	}
	
	
	class ActionRenderer extends UrlRenderer {
	    
		private ReleaseAccessUseCase releaseAccessUseCase = new ReleaseAccessUseCase();
		
	    public ActionRenderer(List<Integer> colunasComLink) {
			super(colunasComLink);
		}
		@Override 
	    public void mouseClicked(MouseEvent e) {
	        JTable table = (JTable) e.getComponent();
	        Point pt = e.getPoint();
	        int ccol = table.columnAtPoint(pt);
	        if (ccol == 6) { // && pointInsidePrefSize(table, pt)) {
	            int crow = table.rowAtPoint(pt);
	            String idPedestre = String.valueOf(table.getValueAt(crow, 0));
	            
	            if(Main.getDefaultDevice() != null 
            		    && Boolean.TRUE.equals(Main.getDefaultDevice().isConnected())
            			&& Boolean.TRUE.equals(Main.getDefaultDevice().getConfigurationValueAsBoolean("Bloquear saida"))) {
	            	new EscolherSentidoLiberarAcessoDialog(Main.getDefaultDevice(), "Liberado pelo sistema", idPedestre);

	            } else {
	            	releaseAccessUseCase.execute(idPedestre, null);
	            }
	            
	        }
	    }
	    @Override public void mouseDragged(MouseEvent e) { /* not needed */ }
	    @Override public void mouseEntered(MouseEvent e) { /* not needed */ }
	    @Override public void mousePressed(MouseEvent e) { /* not needed */ }
	    @Override public void mouseReleased(MouseEvent e) { /* not needed */ }
	}
	
	class EditVisitanteRenderer extends UrlRenderer {

		public EditVisitanteRenderer(List<Integer> colunasComLink) {
			super(colunasComLink);
		}

		@Override
		public void mouseClicked(MouseEvent e) {
			JTable table = (JTable) e.getComponent();
			Point pt = e.getPoint();
			int ccol = table.columnAtPoint(pt);
			if (ccol == 3) {
				int crow = table.rowAtPoint(pt);
				String idVisitante = String.valueOf(table.getValueAt(crow, 0));

				HashMap<String, Object> args = new HashMap<>();
				args.put("ID", Long.valueOf(idVisitante));

				PedestrianAccessEntity visitante = (PedestrianAccessEntity) HibernateAccessDataFacade.getUniqueResultWithParams(
						PedestrianAccessEntity.class, "PedestrianAccessEntity.findById", args);
				if("VISITANTE".equals(visitante.getTipo())) {
					Main.mainScreen.abreCadastroVisitante(visitante);
				} else {
					Main.mainScreen.abreCadastroPedestre(visitante);
				}
				
			}
		}

		@Override
		public void mouseDragged(MouseEvent e) {}
		@Override
		public void mouseEntered(MouseEvent e) {}
		@Override
		public void mousePressed(MouseEvent e) {}
		@Override
		public void mouseReleased(MouseEvent e) {}
	}
	
	public boolean isLoad() {
		return listaErrors != null && !listaErrors.isEmpty();
	}
	
	
}
