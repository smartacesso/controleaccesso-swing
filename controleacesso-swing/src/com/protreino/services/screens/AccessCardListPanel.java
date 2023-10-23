package com.protreino.services.screens;

import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.border.EmptyBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import com.protreino.services.entity.CartaoComandaEntity;
import com.protreino.services.entity.LogCartaoComandaEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.enumeration.NotificationType;
import com.protreino.services.enumeration.StatusCard;
import com.protreino.services.main.Main;
import com.protreino.services.utils.HibernateUtil;
import com.protreino.services.utils.SelectItem;
import com.protreino.services.utils.Utils;

@SuppressWarnings("serial")
public class AccessCardListPanel extends PaginedListPanel {

	private JTable accessListTable;
	private List<CartaoComandaEntity> listaAcesso;
	private String[] columns = {"Código", "N�mero", "R�tulo", "Status", "A��es"};
	private Integer[] columnWidths = {60, 100, 100, 80, 80};
	
	private JTextField filtroIdTextField;
	private JTextField filtroNumeroTextField;
	private JTextField filtroNumeroAlternativoTextField;
	private JComboBox<SelectItem> filtroTipoJComboBox;

	private JButton searchButton;
	private JButton cleanButton;
	public  JButton addButton;
	public  JButton clearCardStateButton;
	private HashMap<String, Object> args;
	private List<Integer> colunasComLink;
	
	public AccessCardListPanel(){
		
		args = new HashMap<String, Object>();
		
		colunasComLink = new ArrayList<Integer>();
		colunasComLink.add(2);
		colunasComLink.add(4);
		
		Font font = new JLabel().getFont();
		Font headerFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
		
		JPanel filtroIdPanel= new JPanel();
		filtroIdPanel.setLayout(new BoxLayout(filtroIdPanel, BoxLayout.Y_AXIS));
		JLabel filtroIdLabel = new JLabel("Código");
		filtroIdLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroIdPanel.add(filtroIdLabel);
		filtroIdTextField = new JTextField("", 12);
		filtroIdTextField.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroIdPanel.add(filtroIdTextField);
		filtroIdPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		
		JPanel filtroNumeroPanel= new JPanel();
		filtroNumeroPanel.setLayout(new BoxLayout(filtroNumeroPanel, BoxLayout.Y_AXIS));
		JLabel filtroNumeroLabel = new JLabel("N�mero");
		filtroNumeroLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroNumeroPanel.add(filtroNumeroLabel);
		filtroNumeroTextField = new JTextField("", 12);
		filtroNumeroTextField.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroNumeroPanel.add(filtroNumeroTextField);
		filtroNumeroPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		
		JPanel filtroNumeroAlternativoPanel= new JPanel();
		filtroNumeroAlternativoPanel.setLayout(new BoxLayout(filtroNumeroAlternativoPanel, BoxLayout.Y_AXIS));
		JLabel filtroNumerlAlternativoLabel = new JLabel("R�tulo");
		filtroNumerlAlternativoLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroNumeroAlternativoPanel.add(filtroNumerlAlternativoLabel);
		filtroNumeroAlternativoTextField = new JTextField("", 12);
		filtroNumeroAlternativoTextField.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroNumeroAlternativoPanel.add(filtroNumeroAlternativoTextField);
		filtroNumeroAlternativoPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
	
		JPanel filtroTipoPanel= new JPanel();
		filtroTipoPanel.setLayout(new BoxLayout(filtroTipoPanel, BoxLayout.Y_AXIS));
		JLabel filtroTipoLabel = new JLabel("Status");
		filtroTipoLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroTipoPanel.add(filtroTipoLabel);
		
		Vector<SelectItem> itens = new Vector<SelectItem>();
		itens.add(new SelectItem("Todos", "TODOS"));
		itens.add(new SelectItem("LIBERADO", StatusCard.LIBERADO));
		itens.add(new SelectItem("BLOQUEADO", StatusCard.BLOQUEADO));
		
		filtroTipoJComboBox =  new JComboBox<SelectItem>(itens);
		filtroTipoJComboBox.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroTipoPanel.add(filtroTipoJComboBox);
		filtroTipoPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		
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
		
		JPanel filterPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
		filterPanel.setMaximumSize(new Dimension(10000, 40));
		filterPanel.add(filtroIdPanel);
		filterPanel.add(Box.createHorizontalStrut(10));
		filterPanel.add(filtroNumeroPanel);
		filterPanel.add(Box.createHorizontalStrut(10));
		filterPanel.add(filtroNumeroAlternativoPanel);
		filterPanel.add(Box.createHorizontalStrut(10));
		filterPanel.add(filtroTipoPanel);
		filterPanel.add(Box.createHorizontalStrut(10));
		filterPanel.add(cleanButtonPanel);
		filterPanel.add(Box.createHorizontalStrut(10));
		filterPanel.add(searchButtonPanel);
		
		JPanel accessListTablePanel = new JPanel();
		accessListTablePanel.setLayout(new BoxLayout(accessListTablePanel, BoxLayout.Y_AXIS));
		accessListTable = new JTable(new DefaultTableModel(columns, 0));
		formatTable();
		accessListTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
		accessListTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		accessListTable.getTableHeader().setReorderingAllowed(false);
		accessListTable.getTableHeader().setOpaque(false);
		accessListTable.getTableHeader().setForeground(Main.firstColor);
		if(!System.getProperty("os.name").toLowerCase().contains("linux"))
			accessListTable.getTableHeader().setBackground(Main.secondColor);
		accessListTable.getTableHeader().setFont(headerFont);
		accessListTable.setRowHeight(30);
		accessListTable.setSelectionBackground(Main.firstColor);
		accessListTable.setSelectionForeground(Color.WHITE);
		TableCellRenderer rendererFromHeader = accessListTable.getTableHeader().getDefaultRenderer();
		JLabel headerLabel = (JLabel) rendererFromHeader;
		headerLabel.setHorizontalAlignment(JLabel.CENTER);
		JScrollPane scrollPane = new JScrollPane(accessListTable);
		scrollPane.getVerticalScrollBar().setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));
		accessListTablePanel.add(scrollPane);
		
		addButton = new JButton("Novo cartão/comanda");
		addButton.setBorder(new EmptyBorder(10,15,10,15));
		addButton.setPreferredSize(new Dimension(150, 40));
		addButton.setVisible(Main.internoLoggedUser != null);
		
		clearCardStateButton = new JButton("Limpar estado cart�es");
		clearCardStateButton.setBorder(new EmptyBorder(10,15,10,15));
		clearCardStateButton.setPreferredSize(new Dimension(150, 40));
		clearCardStateButton.setVisible(Main.internoLoggedUser != null
				&& Main.internoLoggedUser.getId().equals(Main.loggedUser.getId()));
		
		JPanel paginatorPanel = createPaginatorControls();
		
		JPanel statusPanel = new JPanel();
		statusPanel.setLayout(new BoxLayout(statusPanel, BoxLayout.X_AXIS));
		statusPanel.setMaximumSize(new Dimension(Integer.MAX_VALUE, 100));
		statusPanel.add(paginatorPanel);
		statusPanel.add(Box.createHorizontalGlue());
		statusPanel.add(clearCardStateButton);
		statusPanel.add(Box.createHorizontalStrut(10));
		statusPanel.add(addButton);
		
		
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
		
		addButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Main.mainScreen.abreCadastroCartoComanda(null);
			}
		});
		
		
		clearCardStateButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				clearCardState();
			}
		});
		
		ActionListener search = new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				filterList();
			}
		};
		
		searchButton.addActionListener(search);
		filtroIdTextField.addActionListener(search);
		filtroNumeroTextField.addActionListener(search);
		filtroNumeroAlternativoTextField.addActionListener(search);
		filtroTipoJComboBox.addItemListener(new ItemListener() {
			@Override
			public void itemStateChanged(ItemEvent e) {
				filterList();
			}
			
		});
		
	}
	
	
	protected void clearCardState() {
		
		int dialogResult = JOptionPane.showConfirmDialog(null, "Essa a��o far� com que todos os cart�es/comandas voltem para o "
				+ "status de AGUARDANDO (dentro da urna expedidora). "
				+ "Tem certeza que deseja continuar?", "Confirma��o", 
				JOptionPane.YES_NO_OPTION, JOptionPane.PLAIN_MESSAGE);
		if (dialogResult == JOptionPane.YES_OPTION) {
			//apaga temb�m dados de giros anteriores n�o registrados
			HibernateUtil.resetStatusAllCards();
			Utils.createNotification("Cartões/Comandas atualizados com sucesso!", NotificationType.GOOD);
			
			cleanFilter();
		}
		
	}


	@SuppressWarnings("unchecked")
	private void filterList() {
		
		args = new HashMap<>();
		args.put("removido", false);
		
		if (filtroIdTextField.getText() != null && !"".equals(filtroIdTextField.getText())) {
			Long id = 0l;
			try {
				id = Long.valueOf(filtroIdTextField.getText());
			} catch (Exception e) {}
			args.put("id", id);
		}
		if (filtroNumeroTextField.getText() != null && !"".equals(filtroNumeroTextField.getText()))
			args.put("numeroReal", filtroNumeroTextField.getText());
		if (filtroNumeroAlternativoTextField.getText() != null && !"".equals(filtroNumeroAlternativoTextField.getText()))
			args.put("numeroAlternativo", filtroNumeroAlternativoTextField.getText());
		if (filtroTipoJComboBox.getSelectedItem() != null) {
			SelectItem itemSelecionado = (SelectItem)filtroTipoJComboBox.getSelectedItem();
			if(itemSelecionado.getValue() != null){
				if(itemSelecionado.getValue().equals("TODOS")) {
					args.remove("status");
				} else {
					args.put("status", itemSelecionado.getValue());
				}
			}
		} else {
			args.remove("status");
		}
		
		paginaAtual = 1;
		inicioPagina = 0;
		totalRegistros =  HibernateUtil.
				getResultListWithDynamicParamsCount(CartaoComandaEntity.class, null, null, null, args);
		
		executeFilter();

		
	}
	
	@SuppressWarnings("unchecked")
	@Override
	protected void executeFilter() {
		calculaTamanhoPaginas();
		
		if(args == null)
			args = new HashMap<>();
		args.put("removido", false);
		
		listaAcesso = (List<CartaoComandaEntity>) HibernateUtil.
				getResultListWithDynamicParams(CartaoComandaEntity.class, "numeroReal", args, inicioPagina, registrosPorPagina);
		populateTable(listaAcesso);
		
		paginatorControl();
		
	}
	
	@SuppressWarnings("unchecked")
	public void cleanFilter(){
		args = new HashMap<String, Object>();
		filtroIdTextField.setText("");
		filtroNumeroTextField.setText("");
		filtroNumeroAlternativoTextField.setText("");
		filtroTipoJComboBox.setSelectedIndex(0);
		
		totalRegistros =  HibernateUtil.
				getResultListCount(CartaoComandaEntity.class, 
						"CartaoComandaEntity.countNaoRemovidosOrdered");
		
		//calcula p�ginas
		calculaTamanhoPaginas();
		
		listaAcesso = (List<CartaoComandaEntity>) HibernateUtil.
				getResultListLimited(CartaoComandaEntity.class, 
						"CartaoComandaEntity.findAllNaoRemovidosOrdered", (long)registrosPorPagina);
		
		if(Main.internoLoggedUser != null)
			colunasComLink.add(2);
		populateTable(listaAcesso);
		
		paginatorControl();
		
	}
	
	public void sync(){
		try {
			setCursor(new Cursor(Cursor.WAIT_CURSOR));
			if (!Main.updatingAthleteAccessList) {
				Main.syncAthleteAccessList();
				while (Main.updatingAthleteAccessList)
					Thread.sleep(100);
				cleanFilter();
			}
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		finally {
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		}
	}
	
	
	private void populateTable(List<CartaoComandaEntity> listaAcesso){
		DefaultTableModel dataModel = new DefaultTableModel(columns, 0) {
			public boolean isCellEditable(int rowIndex, int mColIndex) {
				return false;
			}
		};
		if (listaAcesso != null && !listaAcesso.isEmpty()){
			for (CartaoComandaEntity acesso : listaAcesso) {
				Object[] obj = new Object[7];
				obj[0] = acesso.getId();
				obj[1] = acesso.getNumeroReal();
				obj[2] = (acesso.getNumeroAlternativo() != null && !"".equals(acesso.getNumeroAlternativo())) ? acesso.getNumeroAlternativo() : "----";
				obj[3] = acesso.getStatus() != null ? acesso.getStatus() : "----";
				obj[4] =  Main.internoLoggedUser != null ? "LIBERAR CART�O" : "----";
				dataModel.addRow(obj);
			}
		}
		accessListTable.setModel(dataModel);
		//int numAcessos = listaAcesso != null ? listaAcesso.size() : 0;
		//countLabel.setText("N�mero de registros: " + numAcessos);
		countLabel.setText("Pág. ("+ paginaAtual + "/" + totalPaginas + ") do total: " + totalRegistros);
		formatTable();
	}
	
	private void formatTable(){
		ActionRenderer actionRenderer = new ActionRenderer();
		actionRenderer.setHorizontalAlignment(JLabel.CENTER);
		EditCartoComandaRenderer edit = null;
		if(Main.internoLoggedUser != null) {
			edit = new EditCartoComandaRenderer();
			edit.setHorizontalAlignment(JLabel.CENTER);
		}
		DefaultTableCellRenderer centerRenderer = new DefaultTableCellRenderer();
		centerRenderer.setHorizontalAlignment(JLabel.CENTER);
		
		while (accessListTable.getMouseListeners().length > 2)
			accessListTable.removeMouseListener(accessListTable.
					getMouseListeners()[accessListTable.getMouseListeners().length-1]);
		
		if(Main.internoLoggedUser != null)
			accessListTable.addMouseListener(edit);
		accessListTable.addMouseListener(actionRenderer);

		while (accessListTable.getMouseMotionListeners().length > 1)
			accessListTable.removeMouseMotionListener(accessListTable.
					getMouseMotionListeners()[accessListTable.getMouseMotionListeners().length-1]);
		
		if(Main.internoLoggedUser != null)
			accessListTable.addMouseMotionListener(edit);
		accessListTable.addMouseMotionListener(actionRenderer);
		
		for (int i = 0; i < accessListTable.getColumnCount(); i++){
			TableColumn column = accessListTable.getColumnModel().getColumn(i);
			column.setPreferredWidth(columnWidths[i]);
			
			if (i == 2 && Main.internoLoggedUser != null)
				column.setCellRenderer(edit);
			else if (i == 4 && Main.internoLoggedUser != null)
				column.setCellRenderer(actionRenderer);
			else
				column.setCellRenderer(centerRenderer);
			
		}
	}

	public boolean isFiltering(){
		return (filtroIdTextField.getText() != null && !filtroIdTextField.getText().isEmpty())
				|| (filtroNumeroTextField.getText() != null && !filtroNumeroTextField.getText().isEmpty())
				|| (filtroNumeroAlternativoTextField.getText() != null && !filtroNumeroAlternativoTextField.getText().isEmpty())
				|| (filtroTipoJComboBox.getSelectedItem() != null );
	}
	
	public JButton getAddButton() {
		return addButton;
	}
	
	class UrlRenderer extends DefaultTableCellRenderer implements MouseListener, MouseMotionListener {
		protected int vrow = -1; // viewRowIndex
		protected int vcol = -1; // viewColumnIndex
		protected boolean isRollover;
	    
	    @Override
	    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
	        super.getTableCellRendererComponent(table, value, isSelected, false, row, column);
	        String str = Objects.toString(value, "");
	        String color = isSelected ? "white" : "#46b2ca";
	        if (isRolloverCell(table, row, column)) {
	            setText("<html><u><font color='" + color + "'>" + str);
	        }
	        else if (hasFocus) {
	            setText("<html><font color='" + color + "'>" + str);
	        }
	        else {
	            setText(str);
	        }
	        return this;
	    }
	    
	    protected boolean isRolloverCell(JTable table, int row, int column) {
	        return this.vrow == row && this.vcol == column && this.isRollover;
	    }

	    @Override 
	    public void mouseMoved(MouseEvent e) {
	        JTable table = (JTable) e.getComponent();
	        Point pt = e.getPoint();
	        final int prevRow = vrow;
	        final int prevCol = vcol;
	        final boolean prevRollover = isRollover;
	        vrow = table.rowAtPoint(pt);
	        vcol = table.columnAtPoint(pt);
	        if (colunasComLink.contains(vcol)) 
	        	Main.mainScreen.setCursor(new Cursor(Cursor.HAND_CURSOR));
	        else
	        	Main.mainScreen.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	        isRollover = colunasComLink.contains(vcol);
	        if (vrow == prevRow && vcol == prevCol && isRollover == prevRollover) {
	            return;
	        }
	        if (!isRollover && !prevRollover) {
	            return;
	        }
	        Rectangle repaintRect;
	        if (isRollover) {
	            Rectangle r = table.getCellRect(vrow, vcol, false);
	            repaintRect = prevRollover ? r.union(table.getCellRect(prevRow, prevCol, false)) : r;
	        }
	        else {
	            repaintRect = table.getCellRect(prevRow, prevCol, false);
	        }
	        table.repaint(repaintRect);
	    }
	    
	    @Override
	    public void mouseExited(MouseEvent e) {
	    	Main.mainScreen.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	        JTable table = (JTable) e.getComponent();
	        if (colunasComLink.contains(vcol)) {
	            table.repaint(table.getCellRect(vrow, vcol, false));
	            vrow = -1;
	            vcol = -1;
	            isRollover = false;
	        }
	    }
	    
	    @Override 
	    public void mouseClicked(MouseEvent e) {
	        JTable table = (JTable) e.getComponent();
	        Point pt = e.getPoint();
	        int ccol = table.columnAtPoint(pt);
	        if (ccol == 4) { // && pointInsidePrefSize(table, pt)) {
	            int crow = table.rowAtPoint(pt);
	            String idUser = String.valueOf(table.getValueAt(crow, 0));
				try {
					//open(Main.urlApplication + "/paginas/sistema/pedestres/cadastroPedestre.xhtml?id=" + idUser);
				}
				catch (Exception e1) {
					e1.printStackTrace();
				}
	        }
	    }
	    @Override public void mouseDragged(MouseEvent e) { /* not needed */ }
	    @Override public void mouseEntered(MouseEvent e) { /* not needed */ }
	    @Override public void mousePressed(MouseEvent e) { /* not needed */ }
	    @Override public void mouseReleased(MouseEvent e) { /* not needed */ }
	}
	
	class ActionRenderer extends UrlRenderer {
	    
	    @Override 
	    public void mouseClicked(MouseEvent e) {
	        JTable table = (JTable) e.getComponent();
	        Point pt = e.getPoint();
	        int ccol = table.columnAtPoint(pt);
	        if (ccol == 4 && Main.internoLoggedUser != null) {
	        	 int crow = table.rowAtPoint(pt);
		         String id = String.valueOf(table.getValueAt(crow, 0));
		         
		         //liberar comanda manual
		         CartaoComandaEntity cartao = (CartaoComandaEntity) 
							HibernateUtil.getSingleResultById(CartaoComandaEntity.class, Long.valueOf(id));
		         if(!StatusCard.LIBERADO.equals(cartao.getStatus())) {
		        	 cartao.setStatus(StatusCard.LIBERADO);
		        	 cartao.setDataAlteracao(new Date());
		        	 HibernateUtil.update(CartaoComandaEntity.class, cartao);
		        	 
		        	 //cria log de libera��o (sem sincroniza��o com web)
		        	 LogCartaoComandaEntity log = new LogCartaoComandaEntity(cartao);
		        	 log.setUsuario(Main.internoLoggedUser);
		        	 log.setTipoLiberacao("MANUAL_"+cartao.getStatus().name());
		        	 log.setOrigem("USUARIO");
		        	 log.setData(new Date());
		        	 HibernateUtil.save(LogCartaoComandaEntity.class, log);
		        	 
		        	 
		        	 Utils.createNotification("Cartão " + cartao.getNumeroReal() + "/" + cartao.getNumeroAlternativo() + " liberado.", NotificationType.GOOD);
		         }else {
		        	 Utils.createNotification("Cartão " + cartao.getNumeroReal() + "/" + cartao.getNumeroAlternativo() + " j� est� liberado.", NotificationType.BAD);
		         }
		         cleanFilter();
	        }
	    }
	    @Override public void mouseDragged(MouseEvent e) { /* not needed */ }
	    @Override public void mouseEntered(MouseEvent e) { /* not needed */ }
	    @Override public void mousePressed(MouseEvent e) { /* not needed */ }
	    @Override public void mouseReleased(MouseEvent e) { /* not needed */ }
	}
	
	class EditCartoComandaRenderer extends UrlRenderer {

		@Override
		public void mouseClicked(MouseEvent e) {
			JTable table = (JTable) e.getComponent();
			Point pt = e.getPoint();
			int ccol = table.columnAtPoint(pt);
			if (ccol == 2) {
				int crow = table.rowAtPoint(pt);
				String id = String.valueOf(table.getValueAt(crow, 0));
				CartaoComandaEntity cartao = (CartaoComandaEntity) 
						HibernateUtil.getSingleResultById(CartaoComandaEntity.class, Long.valueOf(id));
				if(cartao != null)
					Main.mainScreen.abreCadastroCartoComanda(cartao);
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
		return listaAcesso != null && !listaAcesso.isEmpty();
	}
	
}

