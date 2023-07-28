package com.protreino.services.screens;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.util.ArrayList;
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
import javax.swing.ListSelectionModel;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableModel;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;
import com.protreino.services.devices.Device;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.main.Main;
import com.protreino.services.to.AttachedTO;
import com.protreino.services.utils.SelectItem;
import com.protreino.services.utils.Utils;

@SuppressWarnings("serial")
public class AttachedDevicesPanel extends JPanel {

	private JTable devicesListTable;
	private String[] columns = {"Nome", "Localização"};
	private Integer[] columnWidths = {280, 280};
	
	private Font headerFont;
	private JComboBox<SelectItem> optionsAttachedDevicesComboBox;
	
	private JButton addDeviceButton;
	private JButton removeDeviceButton;
	
	private Device deviceAtual;
	
	private DefaultTableModel dataModel;
	
	public AttachedDevicesPanel(Device deviceAtual) {
		this.deviceAtual = deviceAtual;

		JPanel devicesListTablePanel = new JPanel();
		devicesListTablePanel.setLayout(new BoxLayout(devicesListTablePanel, BoxLayout.Y_AXIS));
		devicesListTable = getDevicesListTable();
		dataModel = new DefaultTableModel(columns, 0);
		
		if(deviceAtual.getAttachedDevices() != null && !deviceAtual.getAttachedDevices().isEmpty()) {
			for(AttachedTO to : deviceAtual.getAttachedDevices()) {
				dataModel.addRow(attachedToToObject(to));
				devicesListTable.setModel(dataModel);
			}
		} else {
			deviceAtual.setAttachedDevices(new ArrayList<>());
		}
		
		Font font = new JLabel().getFont();
		headerFont = new Font(font.getFontName(), Font.BOLD, font.getSize());

		JPanel devicesPanel = new JPanel();
		devicesPanel.setLayout(new BoxLayout(devicesPanel, BoxLayout.Y_AXIS));
		JLabel devicesLabel = new JLabel("Catracas");
		devicesLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		devicesPanel.add(devicesLabel);
		devicesPanel.add(Box.createVerticalStrut(2));
		
		Vector<SelectItem> itens = getOptionDevices();
		
		optionsAttachedDevicesComboBox = new JComboBox<SelectItem>(itens);
		optionsAttachedDevicesComboBox.setAlignmentX(Component.LEFT_ALIGNMENT);
		optionsAttachedDevicesComboBox.setPreferredSize(new Dimension(150, 25));
		devicesPanel.add(optionsAttachedDevicesComboBox);
		devicesPanel.setAlignmentX(Component.LEFT_ALIGNMENT);

		addDeviceButton = getAddDeviceButton();
		removeDeviceButton = getRemoveDeviceButton();

		JPanel addDevicesPanel = new JPanel();
		addDevicesPanel.setLayout(new BoxLayout(addDevicesPanel, BoxLayout.Y_AXIS));
		addDevicesPanel.add(Box.createVerticalStrut(10));
		addDevicesPanel.add(addDeviceButton);
		
		JPanel removeDevicesPanel = new JPanel();
		removeDevicesPanel.setLayout(new BoxLayout(removeDevicesPanel, BoxLayout.Y_AXIS));
		removeDevicesPanel.add(Box.createVerticalStrut(10));
		removeDevicesPanel.add(removeDeviceButton);
		
		JPanel headerPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
		headerPanel.setMaximumSize(new Dimension(10000, 60));
		headerPanel.add(devicesPanel);
		headerPanel.add(Box.createHorizontalStrut(20));
		headerPanel.add(addDevicesPanel);
		headerPanel.add(removeDevicesPanel);
		
		JScrollPane scrollPane = new JScrollPane(devicesListTable);
		scrollPane.getVerticalScrollBar().setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));
		devicesListTablePanel.add(scrollPane);
		
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		setBorder(BorderFactory.createEmptyBorder(10,10,10,10));
		add(headerPanel);
		add(Box.createRigidArea(new Dimension(0,5)));
		add(devicesListTablePanel);
	}
	
	private void populateTable(SelectItem item) {
		fora:
		for(Device device : Main.devicesList) {
			if(device.getIdentifier().equals(item.getValue())) {
				AttachedTO attachedTO = new AttachedTO();
				attachedTO.setNomeDevice(device.getName());
				attachedTO.setLocalDevice(device.getLocation() != null ? device.getLocation() : null);
				attachedTO.setIdDevice(device.getIdentifier());
				
				String catraca = "["+attachedTO.getNomeDevice()+", "+attachedTO.getLocalDevice()+"]";
				if(dataModel.getDataVector() != null) {
					for(Object o : dataModel.getDataVector()) {
						if(o.toString().equals(catraca)) {
							break fora;
						}
					}
				}
				dataModel.addRow(attachedToToObject(attachedTO));
				deviceAtual.getAttachedDevices().add(attachedTO);
			}
		}
		devicesListTable.setModel(dataModel);
	}
	
	private Object[] attachedToToObject(AttachedTO attachedTO) {
		Object[] obj = new Object[3];
		obj[0] = attachedTO.getNomeDevice();
		obj[1] = attachedTO.getLocalDevice();
		obj[2] = attachedTO.getIdDevice();
		
		return obj;
	}
	
	private JButton getAddDeviceButton() {
		JButton addDeviceButton = new JButton("Adicionar catraca");
		addDeviceButton.setBorder(new EmptyBorder(10,15,10,15));
		addDeviceButton.setPreferredSize(new Dimension(150, 40));
		
		addDeviceButton.addActionListener(e -> {
			SelectItem itemSelecionado = (SelectItem) optionsAttachedDevicesComboBox.getSelectedItem();
			if(itemSelecionado != null)
				populateTable(itemSelecionado);
		});
		
		return addDeviceButton;
	}
	
	private JButton getRemoveDeviceButton() {
		JButton removeDeviceButton = new JButton("Remover catraca");
		removeDeviceButton.setBorder(new EmptyBorder(10,15,10,15));
		removeDeviceButton.setPreferredSize(new Dimension(150, 40));
		
		removeDeviceButton.addActionListener(e -> {
			try {
				deviceAtual.getAttachedDevices().remove(devicesListTable.getSelectedRow());
				dataModel.removeRow(devicesListTable.getSelectedRow());
				devicesListTable.setModel(dataModel);
			} catch(Exception ex) {}
		});
		
		return removeDeviceButton;
	}
	
	private JTable getDevicesListTable() {
		devicesListTable = new JTable(new DefaultTableModel(columns, 0));
		devicesListTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
		devicesListTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		devicesListTable.getTableHeader().setReorderingAllowed(false);
		devicesListTable.getTableHeader().setOpaque(false);
		devicesListTable.getTableHeader().setForeground(Main.firstColor);
		if(!System.getProperty("os.name").toLowerCase().contains("linux"))
			devicesListTable.getTableHeader().setBackground(Main.secondColor);
		devicesListTable.getTableHeader().setFont(headerFont);
		devicesListTable.setRowHeight(30);
		devicesListTable.setSelectionBackground(Main.firstColor);
		devicesListTable.setSelectionForeground(Color.WHITE);
		
		return devicesListTable;
	}
	
	private Vector<SelectItem> getOptionDevices(){
		Vector<SelectItem> itens = new Vector<SelectItem>();

		for (Device device : Main.devicesList) {
			if (!device.isCatraca() || deviceAtual.equals(device))
				continue;

			if(Manufacturer.TOP_DATA.equals(deviceAtual.getManufacturer())
					|| Manufacturer.TOP_DATA_ACESSO.equals(deviceAtual.getManufacturer())) {
				if(!Manufacturer.TOP_DATA.equals(device.getManufacturer())
						&& !Manufacturer.TOP_DATA_ACESSO.equals(device.getManufacturer()))
					continue;
			
			} else {
				if(!device.getManufacturer().equals(deviceAtual.getManufacturer()))
					continue;
			}
				
			itens.add(new SelectItem(device.getName(), device.getIdentifier()));
		}
		
		return itens;
	}
}
