package com.protreino.services.screens;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
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
import javax.swing.ListSelectionModel;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableModel;
import javax.swing.text.MaskFormatter;

import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.entity.PedestrianEquipamentEntity;
import com.protreino.services.main.Main;
import com.protreino.services.utils.HibernateUtil;
import com.protreino.services.utils.SelectItem;
import com.protreino.services.utils.Utils;

@SuppressWarnings("serial")
public class AvailableDevicesPanel extends JPanel {
	
	private JTable equipamentosListTable;
	private String[] columns = {"Id", "Identificador", "Modelo", "Validade"};
	
	private Font headerFont;

	private JComboBox<SelectItem> equipamentosDisponiveisJComboBox;
	private JFormattedTextField validadeEquipamentoTextField;
	
	private JButton addDeviceButton;
	private JButton removeDeviceButton;
	
	private DefaultTableModel dataModel;
	
	private List<PedestrianEquipamentEntity> pedestresEquipamentos;
	
	public AvailableDevicesPanel() {
		if(this.pedestresEquipamentos == null)
			this.pedestresEquipamentos = new ArrayList<>();
		
		JPanel equipamentosDisponiveisListTablePanel = new JPanel();
		equipamentosDisponiveisListTablePanel.setLayout(new BoxLayout(equipamentosDisponiveisListTablePanel, BoxLayout.Y_AXIS));
		equipamentosListTable = getDevicesListTable();
		Utils.escondeColunaFromTable(equipamentosListTable, 0);

		dataModel = new DefaultTableModel(columns, 0);
		
		Font font = new JLabel().getFont();
		headerFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
		
		JPanel equipamentosPanel = new JPanel();
		equipamentosPanel.setLayout(new BoxLayout(equipamentosPanel, BoxLayout.Y_AXIS));
		JLabel devicesLabel = new JLabel("Equipamentos");
		devicesLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		equipamentosPanel.add(devicesLabel);
		equipamentosPanel.add(Box.createVerticalStrut(2));
		
		equipamentosDisponiveisJComboBox = new JComboBox<SelectItem>(getAllEquipamentosDisponiveis());
		equipamentosDisponiveisJComboBox.setAlignmentX(Component.LEFT_ALIGNMENT);
		equipamentosDisponiveisJComboBox.setPreferredSize(new Dimension(150, 25));
		equipamentosPanel.add(equipamentosDisponiveisJComboBox);
		equipamentosPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		
		JPanel validadeEquipamentoPanel = new JPanel();
		validadeEquipamentoPanel.setLayout(new BoxLayout(validadeEquipamentoPanel, BoxLayout.Y_AXIS));
		JLabel validadeEquipamentosLabel = new JLabel("Validade");
		validadeEquipamentosLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		validadeEquipamentoPanel.add(validadeEquipamentosLabel);
		validadeEquipamentoPanel.add(Box.createVerticalStrut(2));
		
		validadeEquipamentoTextField = Utils.getNewJFormattedTextField(10);
		MaskFormatter mask = Utils.getNewMaskFormatter("##/##/####");
		mask.install(validadeEquipamentoTextField);
		validadeEquipamentoPanel.add(validadeEquipamentoTextField);
		validadeEquipamentoPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		
		addDeviceButton = getAddEquipamentoButton();
		removeDeviceButton = getRemoveEquipamentoButton();

		JPanel addEquipamentosPanel = new JPanel();
		addEquipamentosPanel.setLayout(new BoxLayout(addEquipamentosPanel, BoxLayout.Y_AXIS));
		addEquipamentosPanel.add(Box.createVerticalStrut(25));
		addEquipamentosPanel.add(addDeviceButton);
		
		JPanel removeEquipamentosPanel = new JPanel();
		removeEquipamentosPanel.setLayout(new BoxLayout(removeEquipamentosPanel, BoxLayout.Y_AXIS));
		removeEquipamentosPanel.add(Box.createVerticalStrut(25));
		removeEquipamentosPanel.add(removeDeviceButton);
		
		JPanel headerPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
		headerPanel.setMaximumSize(new Dimension(10000, 60));
		headerPanel.add(equipamentosPanel);
		headerPanel.add(Box.createHorizontalStrut(10));
		headerPanel.add(validadeEquipamentoPanel);
		headerPanel.add(Box.createHorizontalStrut(20));
		headerPanel.add(addEquipamentosPanel);
		headerPanel.add(removeEquipamentosPanel);
		
		JScrollPane scrollPane = new JScrollPane(equipamentosListTable);
		scrollPane.getVerticalScrollBar().setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));
		equipamentosDisponiveisListTablePanel.add(scrollPane);
		
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		setBorder(BorderFactory.createEmptyBorder(10,10,10,10));
		add(headerPanel);
		add(Box.createRigidArea(new Dimension(0,5)));
		add(equipamentosDisponiveisListTablePanel);
	}
	
	private JTable getDevicesListTable() {
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
	private Vector<SelectItem> getAllEquipamentosDisponiveis() {
		Vector<SelectItem> equipamentosDisponiveisItens = new Vector<SelectItem>();
		equipamentosDisponiveisItens.add(new SelectItem("Selecione", null));
		
		List<DeviceEntity> devices = (List<DeviceEntity>) HibernateUtil.getResultList(DeviceEntity.class, "DeviceEntity.findAll");
		
		if(devices == null || devices.isEmpty())
			return equipamentosDisponiveisItens;
		
		devices.forEach(device -> {
			equipamentosDisponiveisItens.add(new SelectItem(device.getName(), device.getIdentifier()));
		});
		
		return equipamentosDisponiveisItens;
	}
	
	private JButton getRemoveEquipamentoButton() {
		JButton removeDeviceButton = new JButton("Remover");
		removeDeviceButton.setBorder(new EmptyBorder(5, 10, 5, 10));
		removeDeviceButton.setPreferredSize(new Dimension(150, 40));
		
		removeDeviceButton.addActionListener(e -> {
			if(equipamentosListTable.getSelectedRow() < 0)
				return;
			
			Long idParaExcluir = (Long) dataModel.getValueAt(equipamentosListTable.getSelectedRow(), 0);

			for(int i = 0; i < pedestresEquipamentos.size(); i++) {
				if(!idParaExcluir.equals(pedestresEquipamentos.get(i).getId()))
					continue;
				
				if(pedestresEquipamentos.get(i).getCadastradoNoDesktop())
					pedestresEquipamentos.remove(i);
				else
					pedestresEquipamentos.get(i).setRemovidoNoDesktop(true);
			}
			
			dataModel.removeRow(equipamentosListTable.getSelectedRow());
			equipamentosListTable.setModel(dataModel);
			
			Utils.escondeColunaFromTable(equipamentosListTable, 0);
		});
		
		return removeDeviceButton;
	}
	
	private void populateTable(SelectItem equipamento) {
		Object[] item = new Object[4];
		Long idTemp = Utils.getRandomNumber();
		
		item[0] = idTemp;
		item[1] = equipamento.getValue();
		item[2] = equipamento.getLabel();
		item[3] = validadeEquipamentoTextField.getText();
		
		PedestrianEquipamentEntity pedestreEquip = new PedestrianEquipamentEntity();
		pedestreEquip.setId(idTemp);
		pedestreEquip.setIdEquipamento(String.valueOf(equipamento.getValue()));
		pedestreEquip.setNomeEquipamento(String.valueOf(equipamento.getLabel()));
		pedestreEquip.setCadastradoNoDesktop(true);

		try {
			pedestreEquip.setValidadeEquipamento(new SimpleDateFormat("dd/MM/yyyy").parse(String.valueOf(validadeEquipamentoTextField.getText())));
		} catch (Exception e) {}

		pedestresEquipamentos.add(pedestreEquip);
		
		dataModel.addRow(item);
		equipamentosListTable.setModel(dataModel);
		
		Utils.escondeColunaFromTable(equipamentosListTable, 0);
	}
	
	private JButton getAddEquipamentoButton() {
		JButton addDeviceButton = new JButton("Adicionar");
		addDeviceButton.setBorder(new EmptyBorder(5, 10, 5, 10));
		addDeviceButton.setPreferredSize(new Dimension(150, 40));
		
		addDeviceButton.addActionListener(e -> {
			SelectItem itemSelecionado = (SelectItem) equipamentosDisponiveisJComboBox.getSelectedItem();
			if(itemSelecionado == null || itemSelecionado.getValue() == null)
				return;
			
			for(PedestrianEquipamentEntity p : pedestresEquipamentos) {
				if(itemSelecionado.getValue().equals(p.getIdEquipamento())) {
					return;
				}
			}
			
			populateTable(itemSelecionado);
			
			validadeEquipamentoTextField.setText("");
		});
		
		return addDeviceButton;
	}

	public DefaultTableModel getDataModel() {
		return dataModel;
	}

	public List<PedestrianEquipamentEntity> getPedestresEquipamentos() {
		return pedestresEquipamentos;
	}

	public void setPedestresEquipamentos(List<PedestrianEquipamentEntity> pedestresEquipamentos) {
		this.pedestresEquipamentos = pedestresEquipamentos;
		
		if(this.pedestresEquipamentos != null && !this.pedestresEquipamentos.isEmpty()) {
			this.pedestresEquipamentos.forEach(p -> {
				Object[] item = new Object[4];
				item[0] = p.getId();
				item[1] = p.getIdEquipamento();
				item[2] = p.getNomeEquipamento();
				try {
					item[3] = new SimpleDateFormat("dd/MM/yyyy").format(p.getValidadeEquipamento());
				} catch (Exception e) {
					item[3] = "";
				}
				
				dataModel.addRow(item);
			});
			equipamentosListTable.setModel(dataModel);
			
			Utils.escondeColunaFromTable(equipamentosListTable, 0);
		}
	}
}
