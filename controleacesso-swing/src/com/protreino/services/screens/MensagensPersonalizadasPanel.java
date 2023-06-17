package com.protreino.services.screens;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
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

import com.protreino.services.entity.PedestrianMessagesEntity;
import com.protreino.services.enumeration.Status;
import com.protreino.services.main.Main;
import com.protreino.services.utils.SelectItem;
import com.protreino.services.utils.Utils;

@SuppressWarnings("serial")
public class MensagensPersonalizadasPanel extends JPanel {

	private JTable mensagensListTable;
	private String[] columns = {"Id", "Nome", "Status", "Mensagem", "Quantidade", "Validade"};
	
	private Font headerFont;

	private JTextField nomeTextField;
	private JComboBox<SelectItem> statusMensagensPersonalizadasJComboBox;
	private JTextField mensagemTextField;
	private JTextField quantidadeMensagemTextField;
	private JFormattedTextField validadeMensagemTextField;
	
	private JButton addMessageButton;
	private JButton removeMessageButton;
	
	private DefaultTableModel dataModel;
	
	private List<PedestrianMessagesEntity> mensagens;
	
	public MensagensPersonalizadasPanel() {
		if(this.mensagens == null)
			this.mensagens = new ArrayList<>();
		
		JPanel mensagensListTablePanel = new JPanel();
		mensagensListTablePanel.setLayout(new BoxLayout(mensagensListTablePanel, BoxLayout.Y_AXIS));
		mensagensListTable = getMensagensListTable();
		Utils.escondeColunaFromTable(mensagensListTable, 0);
		
		dataModel = new DefaultTableModel(columns, 0);
		
		Font font = new JLabel().getFont();
		headerFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
		
		JPanel nomeMensagemPanel = new JPanel();
		nomeMensagemPanel.setLayout(new BoxLayout(nomeMensagemPanel, BoxLayout.Y_AXIS));
		JLabel nomeLabel = new JLabel("Nome");
		nomeLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		nomeMensagemPanel.add(nomeLabel);
		nomeMensagemPanel.add(Box.createVerticalStrut(2));
		
		nomeTextField = new JTextField();
		nomeTextField.setAlignmentX(Component.LEFT_ALIGNMENT);
		nomeTextField.setPreferredSize(new Dimension(150, 25));
		nomeMensagemPanel.add(nomeTextField);
		nomeMensagemPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		
		JPanel statusMensagemPanel = new JPanel();
		statusMensagemPanel.setLayout(new BoxLayout(statusMensagemPanel, BoxLayout.Y_AXIS));
		JLabel statusLabel = new JLabel("Status");
		statusLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		statusMensagemPanel.add(statusLabel);
		statusMensagemPanel.add(Box.createVerticalStrut(2));
		
		statusMensagensPersonalizadasJComboBox = new JComboBox<SelectItem>(getStatusComboBox());
		statusMensagensPersonalizadasJComboBox.setAlignmentX(Component.LEFT_ALIGNMENT);
		statusMensagensPersonalizadasJComboBox.setPreferredSize(new Dimension(100, 25));
		statusMensagemPanel.add(statusMensagensPersonalizadasJComboBox);
		statusMensagemPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		
		JPanel textoMensagemPanel = new JPanel();
		textoMensagemPanel.setLayout(new BoxLayout(textoMensagemPanel, BoxLayout.Y_AXIS));
		JLabel mensagemLabel = new JLabel("Mensagem");
		mensagemLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		textoMensagemPanel.add(mensagemLabel);
		textoMensagemPanel.add(Box.createVerticalStrut(2));
		
		mensagemTextField = new JTextField();
		mensagemTextField.setAlignmentX(Component.LEFT_ALIGNMENT);
		mensagemTextField.setPreferredSize(new Dimension(250, 25));
		textoMensagemPanel.add(mensagemTextField);
		textoMensagemPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		
		JPanel quantidadeMensagemPanel = new JPanel();
		quantidadeMensagemPanel.setLayout(new BoxLayout(quantidadeMensagemPanel, BoxLayout.Y_AXIS));
		JLabel qtdeMensagemLabel = new JLabel("Quantidade");
		qtdeMensagemLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		quantidadeMensagemPanel.add(qtdeMensagemLabel);
		quantidadeMensagemPanel.add(Box.createVerticalStrut(2));
		
		quantidadeMensagemTextField = new JTextField();
		quantidadeMensagemTextField.setAlignmentX(Component.LEFT_ALIGNMENT);
		quantidadeMensagemTextField.setPreferredSize(new Dimension(50, 25));
		quantidadeMensagemPanel.add(quantidadeMensagemTextField);
		quantidadeMensagemPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		
		JPanel validadeMensagemPanel = new JPanel();
		validadeMensagemPanel.setLayout(new BoxLayout(validadeMensagemPanel, BoxLayout.Y_AXIS));
		JLabel validadeMensagemLabel = new JLabel("Validade");
		validadeMensagemLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		validadeMensagemPanel.add(validadeMensagemLabel);
		validadeMensagemPanel.add(Box.createVerticalStrut(2));
		
		validadeMensagemTextField = Utils.getNewJFormattedTextField(10);
		MaskFormatter mask = Utils.getNewMaskFormatter("##/##/####");
		mask.install(validadeMensagemTextField);
		validadeMensagemPanel.add(validadeMensagemTextField);
		validadeMensagemPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		
		addMessageButton = getAddMensagemButton();
		removeMessageButton = getRemoveMessageButton();

		JPanel addMensagensPanel = new JPanel();
		addMensagensPanel.setLayout(new BoxLayout(addMensagensPanel, BoxLayout.Y_AXIS));
		addMensagensPanel.add(Box.createVerticalStrut(10));
		addMensagensPanel.add(addMessageButton);
		
		JPanel removeMensagensPanel = new JPanel();
		removeMensagensPanel.setLayout(new BoxLayout(removeMensagensPanel, BoxLayout.Y_AXIS));
		removeMensagensPanel.add(Box.createVerticalStrut(10));
		removeMensagensPanel.add(removeMessageButton);
		
		JPanel headerPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
		headerPanel.setMaximumSize(new Dimension(10000, 60));
		headerPanel.add(nomeMensagemPanel);
		headerPanel.add(Box.createHorizontalStrut(10));
		headerPanel.add(statusMensagemPanel);
		headerPanel.add(Box.createHorizontalStrut(10));
		headerPanel.add(textoMensagemPanel);
		headerPanel.add(Box.createHorizontalStrut(10));
		headerPanel.add(quantidadeMensagemPanel);
		headerPanel.add(Box.createHorizontalStrut(10));
		headerPanel.add(validadeMensagemPanel);
		headerPanel.add(Box.createHorizontalStrut(10));
		
		JPanel headerPanelButtons = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
		headerPanelButtons.setMaximumSize(new Dimension(10000, 60));
		headerPanelButtons.add(addMensagensPanel);
		headerPanelButtons.add(removeMensagensPanel);
		
		JScrollPane scrollPane = new JScrollPane(mensagensListTable);
		scrollPane.getVerticalScrollBar().setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));
		mensagensListTablePanel.add(scrollPane);
		
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		setBorder(BorderFactory.createEmptyBorder(10,10,10,10));
		add(headerPanel);
		add(headerPanelButtons);
		add(Box.createRigidArea(new Dimension(0,5)));
		add(mensagensListTablePanel);
	}
	
	private void populateTable(SelectItem statusSelecionado) {
		Object[] item = new Object[6];
		Long idTemp = Utils.getRandomNumber();
		
		item[0] = idTemp;
		item[1] = nomeTextField.getText();
		item[2] = statusSelecionado.getValue();
		item[3] = mensagemTextField.getText();
		item[4] = quantidadeMensagemTextField.getText();
		item[5] = validadeMensagemTextField.getText();
		
		PedestrianMessagesEntity pedestreMessage = new PedestrianMessagesEntity();
		pedestreMessage.setId(idTemp);
		pedestreMessage.setNome(nomeTextField.getText());
		pedestreMessage.setStatus(Status.valueOf(String.valueOf(statusSelecionado.getValue())));
		pedestreMessage.setMensagem(mensagemTextField.getText());
		pedestreMessage.setQuantidade(Long.valueOf(quantidadeMensagemTextField.getText()));
		pedestreMessage.setCadastradoNoDesktop(true);
		
		try {
			pedestreMessage.setValidade(new SimpleDateFormat("dd/MM/yyyy").parse(validadeMensagemTextField.getText()));
		} catch (Exception e) {}

		mensagens.add(pedestreMessage);
		
		dataModel.addRow(item);
		mensagensListTable.setModel(dataModel);
		
		Utils.escondeColunaFromTable(mensagensListTable, 0);
	}
	
	private JButton getAddMensagemButton() {
		JButton addMensageButton = new JButton("Adicionar");
		addMensageButton.setBorder(new EmptyBorder(5, 10, 5, 10));
		addMensageButton.setPreferredSize(new Dimension(150, 40));
		
		addMensageButton.addActionListener(e -> {
			SelectItem statusSelecionado = (SelectItem) statusMensagensPersonalizadasJComboBox.getSelectedItem();
			
			populateTable(statusSelecionado);
			
			limparCampos();
		});
		
		return addMensageButton;
	}

	private JButton getRemoveMessageButton() {
		JButton removeMessageButton = new JButton("Remover");
		removeMessageButton.setBorder(new EmptyBorder(5, 10, 5, 10));
		removeMessageButton.setPreferredSize(new Dimension(150, 40));
		
		removeMessageButton.addActionListener(e -> {
			if(mensagensListTable.getSelectedRow() < 0)
				return;
			
			Long idParaExcluir = (Long) dataModel.getValueAt(mensagensListTable.getSelectedRow(), 0);

			for(int i = 0; i < mensagens.size(); i++) {
				if(!idParaExcluir.equals(mensagens.get(i).getId()))
					continue;
				
				if(mensagens.get(i).getCadastradoNoDesktop())
					mensagens.remove(i);
				else
					mensagens.get(i).setRemovidoNoDesktop(true);
			}
			
			dataModel.removeRow(mensagensListTable.getSelectedRow());
			mensagensListTable.setModel(dataModel);
			
			Utils.escondeColunaFromTable(mensagensListTable, 0);
		});
		
		return removeMessageButton;
	}
	
	private void limparCampos() {
		nomeTextField.setText("");
		mensagemTextField.setText("");
		quantidadeMensagemTextField.setText("");
		validadeMensagemTextField.setText("");
	}
	
	private Vector<SelectItem> getStatusComboBox() {
		Vector<SelectItem> statusItens = new Vector<SelectItem>();
		statusItens.add(new SelectItem("ATIVO", "ATIVO"));
		statusItens.add(new SelectItem("INATIVO", "INATIVO"));
		
		return statusItens;
	}

	private JTable getMensagensListTable() {
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

	public List<PedestrianMessagesEntity> getMensagens() {
		return mensagens;
	}

	public void setMensagens(List<PedestrianMessagesEntity> mensagens) {
		this.mensagens = mensagens;
		
		if(this.mensagens != null && !this.mensagens.isEmpty()) {
			this.mensagens.forEach(m -> {
				Object[] item = new Object[6];
				item[0] = m.getId();
				item[1] = m.getNome();
				item[2] = m.getStatus();
				item[3] = m.getMensagem();
				item[4] = m.getQuantidade();
				try {
					item[5] = new SimpleDateFormat("dd/MM/yyyy").format(m.getValidade());
				} catch (Exception e) {
					item[5] = "";
				}
				
				dataModel.addRow(item);
			});
			
			mensagensListTable.setModel(dataModel);
			Utils.escondeColunaFromTable(mensagensListTable, 0);
		}
	}
	
}
