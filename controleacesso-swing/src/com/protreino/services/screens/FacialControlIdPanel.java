package com.protreino.services.screens;

import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;

import org.jdatepicker.ComponentColorDefaults;
import org.jdatepicker.ComponentColorDefaults.Key;

import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.main.Main;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.to.DeviceTO;

public class FacialControlIdPanel extends PaginedListPanel {

	private JTable facialControlTable;
	private String[] columns = { "Nome", "ID do Equipamento" };
	private Integer[] columnWidths = { 270, 130 };

	private JTextField nomeTextField;
	private JTextField idTextField;
	private JButton addButton;
	private final List<DeviceTO> devicies = new ArrayList<DeviceTO>();

	public FacialControlIdPanel() {

		Font font = new JLabel().getFont();
		Font headerFont = new Font(font.getFontName(), Font.BOLD, font.getSize());

		JPanel inputPanel = new JPanel();
		inputPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 10, 5));

		JLabel nomeLabel = new JLabel("Nome:");
		nomeTextField = new JTextField(20);

		JLabel idLabel = new JLabel("ID do Equipamento:");
		idTextField = new JTextField(10);

		addButton = new JButton("Adicionar");

		inputPanel.add(nomeLabel);
		inputPanel.add(nomeTextField);
		inputPanel.add(idLabel);
		inputPanel.add(idTextField);
		inputPanel.add(addButton);

		JPanel tablePanel = new JPanel();
		tablePanel.setLayout(new BoxLayout(tablePanel, BoxLayout.Y_AXIS));
		facialControlTable = new JTable(new DefaultTableModel(columns, 0));
		formatTable();

		facialControlTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
		facialControlTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		facialControlTable.getTableHeader().setReorderingAllowed(false);
		facialControlTable.getTableHeader().setOpaque(false);
		facialControlTable.getTableHeader().setForeground(Main.firstColor);
		if (!System.getProperty("os.name").toLowerCase().contains("linux"))
			facialControlTable.getTableHeader().setBackground(Main.secondColor);
		facialControlTable.getTableHeader().setFont(headerFont);
		facialControlTable.setRowHeight(30);
		facialControlTable.setSelectionBackground(Color.LIGHT_GRAY);

		ComponentColorDefaults.getInstance().setColor(Key.FG_MONTH_SELECTOR, Main.firstColor);
		ComponentColorDefaults.getInstance().setColor(Key.BG_MONTH_SELECTOR, Main.secondColor);
		ComponentColorDefaults.getInstance().setColor(Key.FG_GRID_TODAY, Main.secondColor);
		ComponentColorDefaults.getInstance().setColor(Key.FG_GRID_TODAY_SELECTED, Main.secondColor);

		JScrollPane scrollPane = new JScrollPane(facialControlTable);
		tablePanel.add(scrollPane);

		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

		add(inputPanel);
		add(Box.createVerticalStrut(5));
		add(tablePanel);

		populateTable();

		addButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				save();
				populateTable();
			}

			private void save() {

				String nome = nomeTextField.getText().trim();
				String id = idTextField.getText().trim();
				DeviceEntity currentDevice = new DeviceEntity();

				currentDevice.setName(nome);
				currentDevice.setIdentifier("1;3570;");
				currentDevice.setFacialControlId(id);
				currentDevice.setDesiredStatus(DeviceStatus.CONNECTED);
				currentDevice.setManufacturer(Manufacturer.CONTROL_ID_FACIAL);
				HibernateAccessDataFacade.save(DeviceEntity.class, currentDevice);
			}
		});
	}

	private void populateTable() {

		HashMap<String, Object> args = new HashMap<>();
		args.put("MANUFACTURER", Manufacturer.CONTROL_ID_FACIAL);

		@SuppressWarnings("unchecked")
		List<DeviceEntity> devices = (List<DeviceEntity>) HibernateAccessDataFacade
				.getResultListWithParams(DeviceEntity.class, "DeviceEntity.findByManufacturer", args);

		Optional.ofNullable(devices).orElse(Collections.emptyList()).stream().forEach(device -> {
			DefaultTableModel model = (DefaultTableModel) facialControlTable.getModel();
			model.addRow(new Object[] { device.getName(), device.getFacialControlId() });
		});
	}

	private void formatTable() {
		DefaultTableCellRenderer centerRenderer = new DefaultTableCellRenderer();
		centerRenderer.setHorizontalAlignment(JLabel.CENTER);

		for (int i = 0; i < facialControlTable.getColumnCount(); i++) {
			TableColumn column = facialControlTable.getColumnModel().getColumn(i);
			column.setPreferredWidth(columnWidths[i]);
			column.setCellRenderer(centerRenderer);
		}
	}

	@Override
	protected void executeFilter() {
		// TODO Auto-generated method stub

	}

}
