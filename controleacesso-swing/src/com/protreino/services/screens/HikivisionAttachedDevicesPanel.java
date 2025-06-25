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

import com.protreino.services.devices.Device;
import com.protreino.services.main.Main;
import com.protreino.services.to.AttachedTO;
import com.protreino.services.to.hikivision.HikivisionDeviceTO;
import com.protreino.services.usecase.HikivisionUseCases;
import com.protreino.services.utils.SelectItem;
import com.protreino.services.utils.Utils;

@SuppressWarnings("serial")
public class HikivisionAttachedDevicesPanel extends JPanel {

	protected  JTable hikivisionCamerasListTable;
	protected String[] columns = {"Nome", "Id"};
	private Integer[] columnWidths = {280, 280};
	
	private Font headerFont;
	protected  JComboBox<SelectItem> optionsAttachedHikivisionCamerasComboBox;
	
	protected  JButton addHikivisionCameraButton;
	protected  JButton removeHikivisionCameraButton;
	
	private Device deviceAtual;
	
	protected  DefaultTableModel dataModel;
	
	private final HikivisionUseCases hikivisionUseCases = new HikivisionUseCases();
	protected final List<HikivisionDeviceTO.Device> hikivisionDevices;
	
    // Construtor especial para uso em heranças que montam a UI diferente
    protected HikivisionAttachedDevicesPanel(boolean skipUIInit) {
        this.hikivisionDevices = hikivisionUseCases.listarDispositivos();
    }
	public HikivisionAttachedDevicesPanel(Device deviceAtual) {
		this.deviceAtual = deviceAtual;
		hikivisionDevices = hikivisionUseCases.listarDispositivos();

		JPanel hikivisionCamerasListTablePanel = new JPanel();
		hikivisionCamerasListTablePanel.setLayout(new BoxLayout(hikivisionCamerasListTablePanel, BoxLayout.Y_AXIS));
		hikivisionCamerasListTable = getHikivisionCamerasListTable();
		dataModel = new DefaultTableModel(columns, 0);
		
		if(deviceAtual.getAttachedHikivisionCameras() != null && !deviceAtual.getAttachedHikivisionCameras().isEmpty()) {
			for(AttachedTO to : deviceAtual.getAttachedHikivisionCameras()) {
				dataModel.addRow(attachedToToObject(to));
				hikivisionCamerasListTable.setModel(dataModel);
			}
		} else {
			deviceAtual.setAttachedHikivisionCameras(new ArrayList<>());
		}
		
		Font font = new JLabel().getFont();
		headerFont = new Font(font.getFontName(), Font.BOLD, font.getSize());

		JPanel hikivisionCamerasPanel = new JPanel();
		hikivisionCamerasPanel.setLayout(new BoxLayout(hikivisionCamerasPanel, BoxLayout.Y_AXIS));
		JLabel devicesLabel = new JLabel("Catracas");
		devicesLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		hikivisionCamerasPanel.add(devicesLabel);
		hikivisionCamerasPanel.add(Box.createVerticalStrut(2));
		
		Vector<SelectItem> itens = getOptionHikivisionCameras();
		
		optionsAttachedHikivisionCamerasComboBox = new JComboBox<SelectItem>(itens);
		optionsAttachedHikivisionCamerasComboBox.setAlignmentX(Component.LEFT_ALIGNMENT);
		optionsAttachedHikivisionCamerasComboBox.setPreferredSize(new Dimension(150, 25));
		hikivisionCamerasPanel.add(optionsAttachedHikivisionCamerasComboBox);
		hikivisionCamerasPanel.setAlignmentX(Component.LEFT_ALIGNMENT);

		addHikivisionCameraButton = getAddHikivisionCameraButton();
		removeHikivisionCameraButton = getRemoveHikivisionCameraButton();

		JPanel addDevicesPanel = new JPanel();
		addDevicesPanel.setLayout(new BoxLayout(addDevicesPanel, BoxLayout.Y_AXIS));
		addDevicesPanel.add(Box.createVerticalStrut(10));
		addDevicesPanel.add(addHikivisionCameraButton);
		
		JPanel removeDevicesPanel = new JPanel();
		removeDevicesPanel.setLayout(new BoxLayout(removeDevicesPanel, BoxLayout.Y_AXIS));
		removeDevicesPanel.add(Box.createVerticalStrut(10));
		removeDevicesPanel.add(removeHikivisionCameraButton);
		
		JPanel headerPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
		headerPanel.setMaximumSize(new Dimension(10000, 60));
		headerPanel.add(hikivisionCamerasPanel);
		headerPanel.add(Box.createHorizontalStrut(20));
		headerPanel.add(addDevicesPanel);
		headerPanel.add(removeDevicesPanel);
		
		JScrollPane scrollPane = new JScrollPane(hikivisionCamerasListTable);
		scrollPane.getVerticalScrollBar().setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));
		hikivisionCamerasListTablePanel.add(scrollPane);
		
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		setBorder(BorderFactory.createEmptyBorder(10,10,10,10));
		add(headerPanel);
		add(Box.createRigidArea(new Dimension(0,5)));
		add(hikivisionCamerasListTablePanel);
	}
	
	private void populateTable(SelectItem item) {
		if (Utils.isNullOrEmpty(hikivisionDevices)) {
            return;
        }
		
		fora:
		for (HikivisionDeviceTO.Device device : hikivisionDevices) {
			if(device.getDevIndex().equalsIgnoreCase(item.getValue().toString())) {
				AttachedTO attachedTO = new AttachedTO();
				attachedTO.setNomeDevice(device.getDevName());
				attachedTO.setIdDevice(device.getDevIndex());
				
				String deviceNameAndId = "["+attachedTO.getNomeDevice()+", "+attachedTO.getIdDevice()+"]";
				if(dataModel.getDataVector() != null) {
					for(Object o : dataModel.getDataVector()) {
						if(o.toString().equals(deviceNameAndId)) {
							break fora;
						}
					}
				}
				dataModel.addRow(attachedToToObject(attachedTO));
				deviceAtual.getAttachedHikivisionCameras().add(attachedTO);
			}
		}
		
		hikivisionCamerasListTable.setModel(dataModel);
	}
	
	protected Object[] attachedToToObject(AttachedTO attachedTO) {
		Object[] obj = new Object[2];
		obj[0] = attachedTO.getNomeDevice();
		obj[1] = attachedTO.getIdDevice();
		
		return obj;
	}
	
	private JButton getAddHikivisionCameraButton() {
		JButton addHikivisionCameraButton = new JButton("Adicionar camera");
		addHikivisionCameraButton.setBorder(new EmptyBorder(10,15,10,15));
		addHikivisionCameraButton.setPreferredSize(new Dimension(150, 40));
		
		addHikivisionCameraButton.addActionListener(e -> {
			SelectItem itemSelecionado = (SelectItem) optionsAttachedHikivisionCamerasComboBox.getSelectedItem();
			if(itemSelecionado != null) {
				populateTable(itemSelecionado);				
			}
		});
		
		return addHikivisionCameraButton;
	}
	
	private JButton getRemoveHikivisionCameraButton() {
		JButton removeHikivisionCameraButton = new JButton("Remover camera");
		removeHikivisionCameraButton.setBorder(new EmptyBorder(10,15,10,15));
		removeHikivisionCameraButton.setPreferredSize(new Dimension(150, 40));
		
		removeHikivisionCameraButton.addActionListener(e -> {
			try {
				deviceAtual.getAttachedHikivisionCameras().remove(hikivisionCamerasListTable.getSelectedRow());
				dataModel.removeRow(hikivisionCamerasListTable.getSelectedRow());
				hikivisionCamerasListTable.setModel(dataModel);
			} catch(Exception ex) {}
		});
		
		return removeHikivisionCameraButton;
	}
	
	protected JTable getHikivisionCamerasListTable() {
		hikivisionCamerasListTable = new JTable(new DefaultTableModel(columns, 0));
		hikivisionCamerasListTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
		hikivisionCamerasListTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		hikivisionCamerasListTable.getTableHeader().setReorderingAllowed(false);
		hikivisionCamerasListTable.getTableHeader().setOpaque(false);
		hikivisionCamerasListTable.getTableHeader().setForeground(Main.firstColor);
		if(!System.getProperty("os.name").toLowerCase().contains("linux")) {
			hikivisionCamerasListTable.getTableHeader().setBackground(Main.secondColor);			
		}
		hikivisionCamerasListTable.getTableHeader().setFont(headerFont);
		hikivisionCamerasListTable.setRowHeight(30);
		hikivisionCamerasListTable.setSelectionBackground(Main.firstColor);
		hikivisionCamerasListTable.setSelectionForeground(Color.WHITE);
		
		return hikivisionCamerasListTable;
	}
	
	protected  Vector<SelectItem> getOptionHikivisionCameras(){
		Vector<SelectItem> itens = new Vector<SelectItem>();

        if (Utils.isNullOrEmpty(hikivisionDevices)) {
            return itens;
        }
	        
		for (HikivisionDeviceTO.Device device : hikivisionDevices) {
            final String deviceId = device.getDevIndex();
            final String deviceName = device.getDevName();
            
            itens.add(new SelectItem(deviceName, deviceId));
        }
		
		return itens;
	}
}

