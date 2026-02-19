package com.protreino.services.screens;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseEvent;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Vector;
import java.util.concurrent.ConcurrentHashMap;

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
import javax.swing.SwingUtilities;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.entity.UserEntity;
import com.protreino.services.main.Main;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.repository.HibernateLocalAccessData;
import com.protreino.services.usecase.ReleaseAccessUseCase;
import com.protreino.services.usecase.SyncPedestrianAccessListUseCase;
import com.protreino.services.utils.SelectItem;
import com.protreino.services.utils.Utils;

@SuppressWarnings("serial")
public class AccessListPanel extends PaginedListPanel {

	private JTable accessListTable;
	private List<PedestrianAccessEntity> listaAcesso;
	private String[] columns = {"Codigo", "Cartao", "Cpf", "Nome", "Tipo", "Status", "Observacao", "Regra", "Liberar acesso", "Alterado por"};
	private Integer[] columnWidths = {130, 140, 150, 300, 100, 100 ,280, 190, 105, 100};
	
	private JTextField filtroIdTextField;
	private JTextField filtroCartaoTextField;
	private JTextField filtroCpfTextField;
	private JTextField filtroNomeTextField;
	private JComboBox<SelectItem> filtroTipoJComboBox;
	private List<UserEntity> usuarioDoSistema;

	private JButton cleanButton;
	private JButton searchButton;
	private JButton syncButton;
	private HashMap<String, Object> args;
	private List<Integer> colunasComLink;
	private JLabel dateLastSync;
	
	private SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
	private static final SyncPedestrianAccessListUseCase syncPedestrianAccessListUseCase = new SyncPedestrianAccessListUseCase();
	private static final Map<String, CachedCount> countCache = new ConcurrentHashMap<>();
	String construtor = " com.protreino.services.entity.PedestrianAccessEntity(obj.id, obj.cardNumber, obj.cpf ,obj.name, "
			  + "obj.tipo, obj.status, obj.observacoes, obj.quantidadeCreditos, obj.validadeCreditos, obj.dataInicioPeriodo, obj.dataFimPeriodo, "
			  + "obj.idUsuario) ";
	
	public AccessListPanel(){
		
		args = new HashMap<String, Object>();
		
		colunasComLink = new ArrayList<Integer>();
		colunasComLink.add(5);
		colunasComLink.add(8);
		
		Font font = new JLabel().getFont();
		Font headerFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
		
		JPanel filtroIdPanel= new JPanel();
		filtroIdPanel.setLayout(new BoxLayout(filtroIdPanel, BoxLayout.Y_AXIS));
		JLabel filtroIdLabel = new JLabel("Codigo");
		filtroIdLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroIdPanel.add(filtroIdLabel);
		filtroIdTextField = new JTextField("", 8);
		filtroIdTextField.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroIdPanel.add(filtroIdTextField);
		filtroIdPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		
		JPanel filtroCartaoPanel= new JPanel();
		filtroCartaoPanel.setLayout(new BoxLayout(filtroCartaoPanel, BoxLayout.Y_AXIS));
		JLabel filtroCartaoLabel = new JLabel("Cartao");
		filtroCartaoLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroCartaoPanel.add(filtroCartaoLabel);
		filtroCartaoTextField = new JTextField("", 8);
		filtroCartaoTextField.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroCartaoPanel.add(filtroCartaoTextField);
		filtroCartaoPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		
		JPanel filtroCpfPanel= new JPanel();
		filtroCpfPanel.setLayout(new BoxLayout(filtroCpfPanel, BoxLayout.Y_AXIS));
		JLabel filtroCpfLabel = new JLabel("Cpf");
		filtroCpfLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroCpfPanel.add(filtroCpfLabel);
		filtroCpfTextField = new JTextField("", 8);
		filtroCpfTextField.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroCpfPanel.add(filtroCpfTextField);
		filtroCpfPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		
		JPanel filtroNomePanel= new JPanel();
		filtroNomePanel.setLayout(new BoxLayout(filtroNomePanel, BoxLayout.Y_AXIS));
		JLabel filtroNomeLabel = new JLabel("Nome");
		filtroNomeLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroNomePanel.add(filtroNomeLabel);
		filtroNomeTextField = new JTextField("", System.getProperty("os.name").toLowerCase().contains("linux") ? 20 : 35);
		filtroNomeTextField.setAlignmentX(Component.LEFT_ALIGNMENT);
		filtroNomePanel.add(filtroNomeTextField);
		filtroNomePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
	
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
		filterPanel.setMaximumSize(new Dimension(10000, 150));
		filterPanel.add(filtroIdPanel);
		filterPanel.add(Box.createHorizontalStrut(10));
		filterPanel.add(filtroCartaoPanel);
		filterPanel.add(Box.createHorizontalStrut(10));
		filterPanel.add(filtroCpfPanel);
		filterPanel.add(Box.createHorizontalStrut(10));
		filterPanel.add(filtroNomePanel);
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
		
		dateLastSync = new JLabel(" ");
		syncButton = new JButton("Atualizar lista com o servidor");
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
		
		syncButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				syncPedestrianAccessListUseCase.syncPedestrianAccessList();
			}
		});
		
		ActionListener search = new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				filterList();
			}
		};
		
		searchButton.addActionListener(search);
		filtroIdTextField.addActionListener(search);
		filtroCartaoTextField.addActionListener(search);
		filtroCpfTextField.addActionListener(search);
		filtroNomeTextField.addActionListener(search);
		filtroTipoJComboBox.addItemListener(new ItemListener() {
			@Override
			public void itemStateChanged(ItemEvent e) {
				filterList();
			}
		});
		
	}
	
	private void filterList() {
		modoEstimado = true;
		fimDosDados = false;

		args = new HashMap<>();
		args.put("removido", false);
		//
		if (filtroIdTextField.getText() != null && !"".equals(filtroIdTextField.getText())) {
			Long id = 0l;
			try {
				id = Long.valueOf(filtroIdTextField.getText());
			} catch (Exception e) {
			}
			args.put("id", id);
		}
		if (filtroCartaoTextField.getText() != null && !"".equals(filtroCartaoTextField.getText()))
			args.put("cardNumber", filtroCartaoTextField.getText());
		if (filtroCpfTextField.getText() != null && !"".equals(filtroCpfTextField.getText()))
			args.put("cpf", filtroCpfTextField.getText());
		if (filtroNomeTextField.getText() != null && !"".equals(filtroNomeTextField.getText()))
			args.put("name", filtroNomeTextField.getText());
		if (filtroTipoJComboBox.getSelectedItem() != null) {
			SelectItem itemSelecionado = (SelectItem) filtroTipoJComboBox.getSelectedItem();
			if (itemSelecionado.getValue() != null) {
				if (itemSelecionado.getValue().equals("TODOS")) {
					args.remove("tipo");
				} else {
					args.put("tipo", itemSelecionado.getValue());
				}
			}
		} else {
			args.remove("tipo");
		}

		paginaAtual = 1;
		inicioPagina = 0;

		if (!modoEstimado) {
			// Executar contagem e usar cache
			// Geração de chave de cache baseada nos filtros
			String cacheKey = args.toString();

			// Verificar se há um valor em cache válido
			CachedCount cached = countCache.get(cacheKey);
			if (cached == null || cached.isExpired()) {
				// Executa a contagem em uma thread separada
				new Thread(() -> {
					int count = HibernateAccessDataFacade.getResultListWithDynamicParamsCount(
							PedestrianAccessEntity.class, construtor, null, null, args);
					countCache.put(cacheKey, new CachedCount(count, System.currentTimeMillis()));
					// Atualiza totalRegistros e recarrega dados na UI (caso necessário)
					SwingUtilities.invokeLater(() -> {
						totalRegistros = count;
						executeFilter(); // Atualiza a interface novamente com o total correto
					});
				}).start();

			} else {
				totalRegistros = cached.count;
				executeFilter();
			}
		} else {
			executeFilter();
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	protected void executeFilter() {
	    fimDosDados = false; // <-- garante que só será true se detectado no if abaixo
		
		if(args == null) {
			args = new HashMap<>();
		}
		args.put("removido", false);
		
		calculaTamanhoPaginas();
		
		listaAcesso = (List<PedestrianAccessEntity>) HibernateAccessDataFacade.
				getResultListWithDynamicParams(PedestrianAccessEntity.class, construtor, null, null, null, args, inicioPagina, registrosPorPagina);
		
		// Verifica se é o fim da lista (para modo estimado)
		if (modoEstimado && (listaAcesso == null || listaAcesso.size() < registrosPorPagina)) {
			fimDosDados = true;
		}
		
		if(usuarioDoSistema == null || usuarioDoSistema.isEmpty()) {
			usuarioDoSistema = (List<UserEntity>) HibernateAccessDataFacade.getResultList(UserEntity.class, "UserEntity.findAll");
		}
		
		for(PedestrianAccessEntity pedestre  : listaAcesso) {
			if(pedestre.getIdUsuario() == null)
				continue;
			for(UserEntity user : usuarioDoSistema) {
				if(pedestre.getIdUsuario().equals(user.getId())) {
					pedestre.setNomeUuarioQueCriou(user.getLoginName());

					break;
				}
			}
		}
		populateTable(listaAcesso);
		
		paginatorControl();
	}

	public void cleanFilter(){
		
		args = new HashMap<String, Object>();
		filtroIdTextField.setText("");
		filtroCartaoTextField.setText("");
		filtroCpfTextField.setText("");
		filtroNomeTextField.setText("");
		filtroTipoJComboBox.setSelectedIndex(0);
		
		if(Main.internoLoggedUser != null)
			colunasComLink.add(3);
		
		filterList();
	}
	
	private void populateTable(List<PedestrianAccessEntity> listaAcesso){
		DefaultTableModel dataModel = new DefaultTableModel(columns, 0) {
			public boolean isCellEditable(int rowIndex, int mColIndex) {
				return false;
			}
		};
		if (listaAcesso != null && !listaAcesso.isEmpty()){
			for (PedestrianAccessEntity acesso : listaAcesso) {
				Object[] obj = new Object[10];
				obj[0] = acesso.getId();
				obj[1] = (acesso.getCardNumber() != null && !"".equals(acesso.getCardNumber())) ? acesso.getCardNumber() : "-";
				obj[2] = (acesso.getCpf() != null && !"".equals(acesso.getCpf())) ? acesso.getCpf() : "-";
				obj[3] = acesso.getName();
				obj[4] = acesso.getTipo() != null ? acesso.getTipo() : "-";
				obj[5] = "ATIVO".equals(acesso.getStatus()) ? "LIBERADO" : "BLOQUEADO";
				obj[6] = (acesso.getObservacoes() != null && !"".equals(acesso.getObservacoes())) ? acesso.getObservacoes() : "-";
				obj[7] =  montaLiberado(acesso); //TODO : vericar quantidade de acessos
				obj[8] = "LIBERAR ACESSO";
				obj[9] = acesso.getNomeUuarioQueCriou();
				dataModel.addRow(obj);
			}
		}
		accessListTable.setModel(dataModel);
		countLabel.setText("Pag. ("+ paginaAtual + "/" + totalPaginas + ") do total: " + totalRegistros);
		formatTable();
	}
	
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
		
		while (accessListTable.getMouseListeners().length > 2) {
			accessListTable.removeMouseListener(accessListTable.
					getMouseListeners()[accessListTable.getMouseListeners().length-1]);			
		}
		
		if(Main.internoLoggedUser != null) {
			accessListTable.addMouseListener(editVisitanteRenderer);			
		}
		
		accessListTable.addMouseListener(urlRenderer);
		accessListTable.addMouseListener(actionRenderer);

		while (accessListTable.getMouseMotionListeners().length > 2) {
			accessListTable.removeMouseMotionListener(accessListTable.
					getMouseMotionListeners()[accessListTable.getMouseMotionListeners().length-1]);			
		}
		
		if(Main.internoLoggedUser != null) {
			accessListTable.addMouseMotionListener(editVisitanteRenderer);			
		}

		accessListTable.addMouseMotionListener(urlRenderer);
		accessListTable.addMouseMotionListener(actionRenderer);
		
		for (int i = 0; i < accessListTable.getColumnCount(); i++){
			TableColumn column = accessListTable.getColumnModel().getColumn(i);
			column.setPreferredWidth(columnWidths[i]);
			
			if (i == 3 && Main.internoLoggedUser != null)
				column.setCellRenderer(editVisitanteRenderer);
			else if (i == 5)
				column.setCellRenderer(urlRenderer);
			else if (i == 8)
				column.setCellRenderer(actionRenderer);
			else
				column.setCellRenderer(centerRenderer);	
		}
		
		//accessListTable.setRowHeight(34);
	}
	
	
	
	public boolean isFiltering(){
		return (filtroIdTextField.getText() != null && !filtroIdTextField.getText().isEmpty())
				|| (filtroCartaoTextField.getText() != null && !filtroCartaoTextField.getText().isEmpty())
				|| (filtroCpfTextField.getText() != null && !filtroCpfTextField.getText().isEmpty())
				|| (filtroNomeTextField.getText() != null && !filtroNomeTextField.getText().isEmpty())
				|| (filtroTipoJComboBox.getSelectedItem() != null );
	}
	
	public JButton getSyncButton() {
		return syncButton;
	}
	
	public void updateDateLastSync() {
		if (Objects.nonNull(SyncPedestrianAccessListUseCase.getLastSync()) && SyncPedestrianAccessListUseCase.getLastSync() > 0) {
			Date date = new Date(SyncPedestrianAccessListUseCase.getLastSync());
			dateLastSync.setText("Atualizado: " + sdf.format(date));
		} else {
			dateLastSync.setText(" ");
		}
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
	        if (ccol == 8) { // && pointInsidePrefSize(table, pt)) {
	            int crow = table.rowAtPoint(pt);
	            String idPedestre = String.valueOf(table.getValueAt(crow, 0));
	            
	            
	            if(Utils.registraLogLiberacaoManual()) {
					String idVisitante = String.valueOf(table.getValueAt(crow, 0));

					HashMap<String, Object> args = new HashMap<>();
					args.put("ID", Long.valueOf(idVisitante));

					PedestrianAccessEntity visitante = (PedestrianAccessEntity) HibernateAccessDataFacade.getUniqueResultWithParams(
							PedestrianAccessEntity.class, "PedestrianAccessEntity.findById", args);
					
					
					LogPedestrianAccessEntity logAccess = new LogPedestrianAccessEntity(Main.loggedUser.getId(), visitante.getId(),
							visitante.getStatus(), null, "liberado pelo sistema", null, "");

					logAccess.setStatus("ATIVO");
					logAccess.setAccessDate(new Date());
					
					HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, logAccess);
					return;
	            }
	            
	            if(Main.temServidor()) {
					String idVisitante = String.valueOf(table.getValueAt(crow, 0));

					HashMap<String, Object> args = new HashMap<>();
					args.put("ID", Long.valueOf(idVisitante));

					PedestrianAccessEntity visitante = (PedestrianAccessEntity) HibernateAccessDataFacade.getUniqueResultWithParams(
							PedestrianAccessEntity.class, "PedestrianAccessEntity.findById", args);
	            	
	            	RegisterVisitorDialog diolog = new RegisterVisitorDialog(visitante, false);
	            	diolog.abrirDialogoAgendamento(visitante);
	            }
	            
	            if(!Main.temServidor()) {
		            if(Main.getDefaultDevice() != null 
	            		    && Boolean.TRUE.equals(Main.getDefaultDevice().isConnected())
	            			&& Boolean.TRUE.equals(Main.getDefaultDevice().getConfigurationValueAsBoolean("Bloquear saida"))) {
		            	new EscolherSentidoLiberarAcessoDialog(Main.getDefaultDevice(), "Liberado pelo sistema", idPedestre);

		            } else {
		            	releaseAccessUseCase.execute(idPedestre, null);
		            }
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
		return listaAcesso != null && !listaAcesso.isEmpty();
	}
	
	
	private void buscarTotalAssincrono() {
		String cacheKey = args.toString();
		CachedCount cached = countCache.get(cacheKey);

		if (cached == null || cached.isExpired()) {
			new Thread(() -> {
				int count = HibernateAccessDataFacade.getResultListWithDynamicParamsCount(
					PedestrianAccessEntity.class, construtor, null, null, args);
				countCache.put(cacheKey, new CachedCount(count, System.currentTimeMillis()));

				SwingUtilities.invokeLater(() -> {
					totalRegistros = count;
					modoEstimado = false; // ativa modo tradicional se quiser
					calculaTamanhoPaginas();
					paginatorControl(); // atualiza botões e label
				});
			}).start();
		}
	}

	
}

