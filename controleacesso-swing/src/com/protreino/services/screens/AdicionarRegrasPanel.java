package com.protreino.services.screens;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableModel;
import javax.swing.text.MaskFormatter;

import com.protreino.services.entity.PedestreRegraEntity;
import com.protreino.services.entity.RegraEntity;
import com.protreino.services.enumeration.TipoPedestre;
import com.protreino.services.enumeration.TipoRegra;
import com.protreino.services.main.Main;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.utils.SelectItem;
import com.protreino.services.utils.Utils;

@SuppressWarnings("serial")
public class AdicionarRegrasPanel extends JPanel {

	private JTable pedestreRegrasListTable;
	private String[] columns = {"Id", "Nome", "Validade", "Tipo de Regra"};
	
	private Font headerFont;
	
	private JComboBox<SelectItem> regrasJComboBox;
	
	private JTextField qtdeCreditosTextField;
	private JTextField diasValidadeCreditosTextField;
	
	private JFormattedTextField dataInicioPeriodoTextField;
	private JFormattedTextField dataFimPeriodoTextField;

//	private JFormattedTextField dataInicioEscalaTextField;

	
	private JFormattedTextField validadePedestreRegraTextField;
	
	private DefaultTableModel dataModel;
	
	private List<PedestreRegraEntity> pedestresRegras;
	
	private JPanel qtdeDeCreditosPanel;
	private JPanel validadeCreditosPanel;
	private JPanel dataInicioPeriodoPanel;
	private JPanel dataFimPeriodoPanel;
	
//	private JPanel dataInicioEscalaPanel;

	
	public AdicionarRegrasPanel(TipoPedestre tipoPedestre) {
		if(this.pedestresRegras == null) {
			this.pedestresRegras = new ArrayList<>();
		}
		
		JPanel pedestreRegraListTablePanel = new JPanel();
		pedestreRegraListTablePanel.setLayout(new BoxLayout(pedestreRegraListTablePanel, BoxLayout.Y_AXIS));
		pedestreRegrasListTable = getPedestreRegraListTable();
		Utils.escondeColunaFromTable(pedestreRegrasListTable, 0);
		
		dataModel = new DefaultTableModel(columns, 0);
		
		Font font = new JLabel().getFont();
		headerFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
		
		// <INICIO COMBO REGRAS>
		JPanel regrasPanel = new JPanel();
		regrasPanel.setLayout(new BoxLayout(regrasPanel, BoxLayout.Y_AXIS));
		JLabel regrasLabel = new JLabel("Regra");
		regrasLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		regrasPanel.add(regrasLabel);
		regrasPanel.add(Box.createVerticalStrut(2));
		
		regrasJComboBox = new JComboBox<SelectItem>(getAllRegrasDisponiveis(tipoPedestre));
		regrasJComboBox.setAlignmentX(Component.LEFT_ALIGNMENT);
		regrasJComboBox.setPreferredSize(new Dimension(200, 25));
		
		regrasJComboBox.addActionListener(e -> {
			escondePanels();
			
			SelectItem selectedItem = (SelectItem) regrasJComboBox.getSelectedItem();
			
			RegraEntity regra = (RegraEntity) selectedItem.getValue();
			
			if(TipoRegra.ACESSO_CREDITO.equals(regra.getTipo())) {
				qtdeDeCreditosPanel.setVisible(true);
				validadeCreditosPanel.setVisible(true);
			
			} else if(TipoRegra.ACESSO_ESCALA.equals(regra.getTipo())) {
				dataInicioPeriodoPanel.setVisible(true);
			
			} else if(TipoRegra.ACESSO_PERIODO.equals(regra.getTipo())) {
				dataInicioPeriodoPanel.setVisible(true);
				dataFimPeriodoPanel.setVisible(true);
			}
//			else if(TipoRegra.ACESSO_ESCALA_3_3.equals(regra.getTipo())) {
//				dataInicioEscalaPanel.setVisible(true);
//			}
		});
		
		regrasPanel.add(regrasJComboBox);
		regrasPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		// <FIM COMBO REGRAS
		
		// <INICIO VALIDADE REGRA>
		JPanel validadePedestreRegraPanel = new JPanel();
		validadePedestreRegraPanel.setLayout(new BoxLayout(validadePedestreRegraPanel, BoxLayout.Y_AXIS));
		JLabel validadePedestreRegraLabel = new JLabel("Validade");
		validadePedestreRegraLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		validadePedestreRegraPanel.add(validadePedestreRegraLabel);
		validadePedestreRegraPanel.add(Box.createVerticalStrut(2));
		
		validadePedestreRegraTextField = Utils.getNewJFormattedTextField(5);
		MaskFormatter mask = Utils.getNewMaskFormatter("##/##/####");
		mask.install(validadePedestreRegraTextField);
		validadePedestreRegraPanel.add(validadePedestreRegraTextField);
		validadePedestreRegraPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		// <FIM VALIDADE REGRA>
		
		// <INICIO QTDE DE CREDITOS>
		qtdeDeCreditosPanel = new JPanel();
		qtdeDeCreditosPanel.setLayout(new BoxLayout(qtdeDeCreditosPanel, BoxLayout.Y_AXIS));
		qtdeDeCreditosPanel.setVisible(false);
		JLabel qtdeCreditosLabel = new JLabel("Qtde de créditos");
		qtdeCreditosLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		qtdeDeCreditosPanel.add(qtdeCreditosLabel);
		qtdeDeCreditosPanel.add(Box.createVerticalStrut(2));
		
		qtdeCreditosTextField = new JTextField();
		qtdeCreditosTextField.setAlignmentX(Component.LEFT_ALIGNMENT);
		qtdeCreditosTextField.setPreferredSize(new Dimension(100, 25));
		qtdeDeCreditosPanel.add(qtdeCreditosTextField);
		qtdeDeCreditosPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		// <FIM QTDE DE CREDITOS>
		
		// <INICIO VALIDADE CREDITOS>
		validadeCreditosPanel = new JPanel();
		validadeCreditosPanel.setLayout(new BoxLayout(validadeCreditosPanel, BoxLayout.Y_AXIS));
		validadeCreditosPanel.setVisible(false);
		JLabel validadeCreditosLabel = new JLabel("Validade dos créditos em dias");
		validadeCreditosLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		validadeCreditosPanel.add(validadeCreditosLabel);
		validadeCreditosPanel.add(Box.createVerticalStrut(2));
		
		diasValidadeCreditosTextField = new JTextField();
		diasValidadeCreditosTextField.setAlignmentX(Component.LEFT_ALIGNMENT);
		diasValidadeCreditosTextField.setPreferredSize(new Dimension(100, 25));
		validadeCreditosPanel.add(diasValidadeCreditosTextField);
		validadeCreditosPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		// <FIM VALIDADE CREDITOS>
		
		dataInicioPeriodoPanel = new JPanel();
		dataInicioPeriodoPanel.setLayout(new BoxLayout(dataInicioPeriodoPanel, BoxLayout.Y_AXIS));
		dataInicioPeriodoPanel.setVisible(false);
		JLabel dataInicioPeriodoLabel = new JLabel("Data inicio periodo");
		dataInicioPeriodoLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		dataInicioPeriodoPanel.add(dataInicioPeriodoLabel);
		dataInicioPeriodoPanel.add(Box.createVerticalStrut(2));
		
		dataInicioPeriodoTextField = Utils.getNewJFormattedTextField(5);
		mask = Utils.getNewMaskFormatter("##/##/####");
		mask.install(dataInicioPeriodoTextField);
		dataInicioPeriodoPanel.add(dataInicioPeriodoTextField);
		dataInicioPeriodoPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		// <FIM DATA INICIO PERIODO>
		
		
//		dataInicioEscalaPanel = new JPanel();
//		dataInicioEscalaPanel.setLayout(new BoxLayout(dataInicioEscalaPanel, BoxLayout.Y_AXIS));
//		dataInicioEscalaPanel.setVisible(false);
//		JLabel dataInicioEscalaLabel = new JLabel("Data inicio Escala");
//		dataInicioEscalaLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
//		dataInicioEscalaPanel.add(dataInicioEscalaLabel);
//		dataInicioEscalaPanel.add(Box.createVerticalStrut(2));
		
//		dataInicioEscalaTextField = Utils.getNewJFormattedTextField(5);
//		mask = Utils.getNewMaskFormatter("##/##/####");
//		mask.install(dataInicioEscalaTextField);
//		dataInicioEscalaPanel.add(dataInicioEscalaTextField);
//		dataInicioEscalaPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		// <INICIO DATA INICIO ESCALA>
				
		dataFimPeriodoPanel = new JPanel();
		dataFimPeriodoPanel.setLayout(new BoxLayout(dataFimPeriodoPanel, BoxLayout.Y_AXIS));
		dataFimPeriodoPanel.setVisible(false);
		JLabel dataFimPeriodoLabel = new JLabel("Data fim período");
		dataFimPeriodoLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		dataFimPeriodoPanel.add(dataFimPeriodoLabel);
		dataFimPeriodoPanel.add(Box.createVerticalStrut(2));
		
		dataFimPeriodoTextField = Utils.getNewJFormattedTextField(5);
		mask = Utils.getNewMaskFormatter("##/##/####");
		mask.install(dataFimPeriodoTextField);
		dataFimPeriodoPanel.add(dataFimPeriodoTextField);
		dataFimPeriodoPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		// <FIM DATA FIM PERIODO>
		
		JButton addPedestreRegraButton = getAddPedestreRegraButton();
		JButton removePedestreRegraButton = getRemovePedestreRegraButton();
		
		JPanel addPedestreRegraPanel = new JPanel();
		addPedestreRegraPanel.setLayout(new BoxLayout(addPedestreRegraPanel, BoxLayout.Y_AXIS));
		addPedestreRegraPanel.add(Box.createVerticalStrut(25));
		addPedestreRegraPanel.add(addPedestreRegraButton);
		
		JPanel removePedestreRegraPanel = new JPanel();
		removePedestreRegraPanel.setLayout(new BoxLayout(removePedestreRegraPanel, BoxLayout.Y_AXIS));
		removePedestreRegraPanel.add(Box.createVerticalStrut(25));
		removePedestreRegraPanel.add(removePedestreRegraButton);
		
		JPanel headerPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
		headerPanel.setMaximumSize(new Dimension(10000, 60));
		headerPanel.add(regrasPanel);
		headerPanel.add(Box.createHorizontalStrut(10));
		headerPanel.add(validadePedestreRegraPanel);
		headerPanel.add(Box.createHorizontalStrut(10));
		headerPanel.add(qtdeDeCreditosPanel);
		headerPanel.add(Box.createHorizontalStrut(10));
		headerPanel.add(validadeCreditosPanel);
		headerPanel.add(Box.createHorizontalStrut(10));
		headerPanel.add(dataInicioPeriodoPanel);
		headerPanel.add(Box.createHorizontalStrut(10));
		headerPanel.add(dataFimPeriodoPanel);
		headerPanel.add(Box.createHorizontalStrut(10));
//		headerPanel.add(dataInicioEscalaPanel);
//		headerPanel.add(Box.createHorizontalStrut(10));
		headerPanel.add(addPedestreRegraPanel);
		headerPanel.add(removePedestreRegraPanel);
		
		JScrollPane scrollPane = new JScrollPane(pedestreRegrasListTable);
		scrollPane.getVerticalScrollBar().setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));
		pedestreRegraListTablePanel.add(scrollPane);
		
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		setBorder(BorderFactory.createEmptyBorder(10,10,10,10));
		add(headerPanel);
		add(Box.createRigidArea(new Dimension(0,5)));
		add(pedestreRegraListTablePanel);
	}
	
	private JButton getRemovePedestreRegraButton() {
		JButton removeDocumentButton = new JButton("Remover");
		removeDocumentButton.setBorder(new EmptyBorder(5, 10, 5, 10));
		removeDocumentButton.setPreferredSize(new Dimension(100, 40));
		
		removeDocumentButton.addActionListener(e -> {
			if(pedestreRegrasListTable.getSelectedRow() < 0)
				return;
			
			Long idParaExcluir = (Long) dataModel.getValueAt(pedestreRegrasListTable.getSelectedRow(), 0);
			
			for(int i = 0; i < pedestresRegras.size(); i++) {
				if(!idParaExcluir.equals(pedestresRegras.get(i).getId()))
					continue;
				
				if(pedestresRegras.get(i).getCadastradoNoDesktop())
					pedestresRegras.remove(i);
				else
					pedestresRegras.get(i).setRemovidoNoDesktop(true);
			}
			
			dataModel.removeRow(pedestreRegrasListTable.getSelectedRow());
			pedestreRegrasListTable.setModel(dataModel);
			
			Utils.escondeColunaFromTable(pedestreRegrasListTable, 0);
		});
		
		return removeDocumentButton;
	}
	
	private void populateTable(RegraEntity regraSelecionada) {
		Object[] item = new Object[4];
		Long idTemp = Utils.getRandomNumber();
		
		item[0] = idTemp;
		item[1] = regraSelecionada.getNome();
		item[2] = validadePedestreRegraTextField.getText();
		item[3] = regraSelecionada.getTipo().getDescricao();
		
		PedestreRegraEntity pedestreRegra = new PedestreRegraEntity();
		pedestreRegra.setId(idTemp);
		pedestreRegra.setRegra(regraSelecionada);
		pedestreRegra.setCadastradoNoDesktop(true);

		try {
			pedestreRegra.setValidade(new SimpleDateFormat("dd/MM/yyyy").parse(String.valueOf(validadePedestreRegraTextField.getText())));
		} catch (Exception e) {}

		if(!qtdeCreditosTextField.getText().isEmpty()) {
			pedestreRegra.setQtdeTotalDeCreditos(Long.valueOf(qtdeCreditosTextField.getText()));
			pedestreRegra.setQtdeDeCreditos(Long.valueOf(qtdeCreditosTextField.getText()));
		}

		if(!diasValidadeCreditosTextField.getText().isEmpty())
			pedestreRegra.setDiasValidadeCredito(Long.valueOf(diasValidadeCreditosTextField.getText()));
		
		try {
			pedestreRegra.setDataInicioPeriodo(new SimpleDateFormat("dd/MM/yyyy").parse(String.valueOf(dataInicioPeriodoTextField.getText())));
		} catch (Exception e) {}
		
		try {
			pedestreRegra.setDataFimPeriodo(new SimpleDateFormat("dd/MM/yyyy").parse(String.valueOf(dataFimPeriodoTextField.getText())));
		} catch (Exception e) {}
		
		if(TipoRegra.ACESSO_ESCALA_3_3.equals(regraSelecionada.getTipo())) {
			try {
				pedestreRegra.setDataInicioPeriodo(regraSelecionada.getDataInicioPeriodo());
			} catch (Exception e) {}
			
		}

		
//		try {
//			pedestreRegra.setDataInicioEscala3_3(new SimpleDateFormat("dd/MM/yyyy").parse(String.valueOf(dataInicioEscalaTextField.getText())));
//		} catch (Exception e) {}
		
		
		pedestresRegras.add(pedestreRegra);
		
		dataModel.insertRow(0, item);
		pedestreRegrasListTable.setModel(dataModel);
		
		Utils.escondeColunaFromTable(pedestreRegrasListTable, 0);
	}
	
	private JButton getAddPedestreRegraButton() {
		JButton addDocumentButton = new JButton("Adicionar");
		addDocumentButton.setBorder(new EmptyBorder(5, 10, 5, 10));
		addDocumentButton.setPreferredSize(new Dimension(100, 40));
		
		addDocumentButton.addActionListener(e -> {
			SelectItem itemSelecionado = (SelectItem) regrasJComboBox.getSelectedItem();
			
			if(itemSelecionado.getValue() == null)
				return;
			
			int countRegrasNaoRemovidas = countRegrasNaoRemovidas();
			if(countRegrasNaoRemovidas >= 1) {
				criarDialogoPadrao("Somente uma regra ativa", "Você só pode ter uma regra ativa.");
				return;
			}

			RegraEntity regraSelecionada = (RegraEntity) itemSelecionado.getValue();
			
			populateTable(regraSelecionada);
			
			limpaCampos();
			escondePanels();
		});
		
		return addDocumentButton;
	}
	
	private int countRegrasNaoRemovidas() {
		int regrasAtivas = 0;
		
		for(PedestreRegraEntity p : pedestresRegras) {
			if(p.getRemovidoNoDesktop())
				continue;
			
			regrasAtivas++;
		}
		
		return regrasAtivas;
	}

	private void limpaCampos() {
		validadePedestreRegraTextField.setText("");
		qtdeCreditosTextField.setText("");
		diasValidadeCreditosTextField.setText("");
		dataInicioPeriodoTextField.setText("");
		dataFimPeriodoTextField.setText("");
		regrasJComboBox.setSelectedIndex(0);
		
	}
	
	private void escondePanels() {
		qtdeDeCreditosPanel.setVisible(false);
		validadeCreditosPanel.setVisible(false);
		dataInicioPeriodoPanel.setVisible(false);
		dataFimPeriodoPanel.setVisible(false);
	}

	private JTable getPedestreRegraListTable() {
		JTable equipamentosListTable = new JTable(new DefaultTableModel(columns, 0));
		equipamentosListTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
		equipamentosListTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		equipamentosListTable.getTableHeader().setReorderingAllowed(false);
		equipamentosListTable.getTableHeader().setOpaque(false);
		equipamentosListTable.getTableHeader().setForeground(Main.firstColor);
		if(!System.getProperty("os.name").toLowerCase().contains("linux"))
			equipamentosListTable.getTableHeader().setBackground(Main.secondColor);
		equipamentosListTable.getTableHeader().setFont(headerFont);
		equipamentosListTable.setRowHeight(30);
		equipamentosListTable.setSelectionBackground(Main.firstColor);
		equipamentosListTable.setSelectionForeground(Color.WHITE);
		
		return equipamentosListTable;
	}
	
	@SuppressWarnings("unchecked")
	private Vector<SelectItem> getAllRegrasDisponiveis(TipoPedestre tipoPedestre) {
		Vector<SelectItem> regrasDisponiveisItens = new Vector<SelectItem>();
		regrasDisponiveisItens.add(new SelectItem("Selecione", null));

		HashMap<String, Object> args = new HashMap<>();
		args.put("TIPO_PEDESTRE", tipoPedestre);
		
		List<RegraEntity> regras = (List<RegraEntity>) HibernateAccessDataFacade
									.getResultListWithParams(RegraEntity.class, "RegraEntity.findAllByTipoPedestre", args);
		
		if(regras == null || regras.isEmpty())
			return regrasDisponiveisItens;
		
		regras.forEach(regra -> {
			regrasDisponiveisItens.add(new SelectItem(regra.getNome(), regra));
		});
		
		return regrasDisponiveisItens;
	}

	public List<PedestreRegraEntity> getPedestresRegras() {
		return pedestresRegras;
	}

	public void setPedestresRegras(List<PedestreRegraEntity> pedestresRegras) {
		this.pedestresRegras = pedestresRegras;
		
		if(this.pedestresRegras == null || this.pedestresRegras.isEmpty()) {
			return;
		}
		
		this.pedestresRegras.stream()
				.filter(p -> p.getRemovidoNoDesktop() == null || p.getRemovidoNoDesktop() == false)
				.forEach(p -> {
					Object[] item = new Object[4];

					item[0] = p.getId();
					item[1] = p.getRegra() != null ? p.getRegra().getNome() : "";
					try {
						item[2] = new SimpleDateFormat("dd/MM/yyyy").format(p.getValidade());
					} catch (Exception e) {
						item[2] = "";
					}
					item[3] = p.getRegra() != null ? p.getRegra().getTipo().getDescricao() : "";

					dataModel.addRow(item);
				});
		
		pedestreRegrasListTable.setModel(dataModel);
		
		Utils.escondeColunaFromTable(pedestreRegrasListTable, 0);
	}
	
	private void criarDialogoPadrao(String title, String msg) {
		JDialog defaultDialog = new JDialog();
		defaultDialog.setIconImage(Main.favicon);
		defaultDialog.setModal(true);
		defaultDialog.setTitle(title);
		defaultDialog.setResizable(false);
		defaultDialog.setLayout(new BorderLayout());

		JPanel mainPanel = new JPanel();
		mainPanel.setBorder(new EmptyBorder(20, 20, 20, 20));
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

		JLabel mensagemLabel = new JLabel(msg);
		mensagemLabel.setForeground(Color.red);
		mensagemLabel.setAlignmentX(Component.CENTER_ALIGNMENT);

		JButton okButton = new JButton("OK");
		okButton.setBorder(new EmptyBorder(10, 20, 10, 20));
		okButton.setAlignmentX(Component.CENTER_ALIGNMENT);
		okButton.addActionListener(e -> {
			defaultDialog.dispose();
		});

		JPanel confirmarPanel = new JPanel();
		confirmarPanel.setBorder(new EmptyBorder(20, 20, 20, 20));
		confirmarPanel.setLayout(new BoxLayout(confirmarPanel, BoxLayout.X_AXIS));
		confirmarPanel.add(okButton);

		mainPanel.add(mensagemLabel);
		mainPanel.add(Box.createVerticalStrut(10));
		mainPanel.add(confirmarPanel);

		defaultDialog.getContentPane().add(mainPanel, BorderLayout.CENTER);
		defaultDialog.pack();
		defaultDialog.setLocationRelativeTo(null);
		defaultDialog.setVisible(true);
	}
	
}
