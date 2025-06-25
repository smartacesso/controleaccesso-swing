package com.protreino.services.screens;

import java.awt.FlowLayout;
import java.awt.Font;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.table.DefaultTableModel;

import com.protreino.services.to.AttachedTO;
import com.protreino.services.to.hikivision.HikivisionDeviceTO;
import com.protreino.services.to.hikivision.HikivisionDeviceTO.Device;
import com.protreino.services.utils.SelectItem;

public class HikivisionLocaisAttachedPanel extends HikivisionAttachedDevicesPanel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	private final List<AttachedTO> camerasSelecionadas = new ArrayList<>();

	public HikivisionLocaisAttachedPanel() {
		super(true); // carrega os dispositivos, sem montar UI padrão

		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

		JLabel label = new JLabel("Faciais:");
		label.setFont(new Font("Arial", Font.BOLD, 14));
		add(label);
		add(Box.createVerticalStrut(10));

		optionsAttachedHikivisionCamerasComboBox = new JComboBox<>(getOptionHikivisionCameras());
		add(optionsAttachedHikivisionCamerasComboBox);
		add(Box.createVerticalStrut(10));

		addHikivisionCameraButton = getAddButtonLocal();
		removeHikivisionCameraButton = getRemoveButtonLocal();

		JPanel botoesPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		botoesPanel.add(addHikivisionCameraButton);
		botoesPanel.add(removeHikivisionCameraButton);
		add(botoesPanel);
		add(Box.createVerticalStrut(10));

		dataModel = new DefaultTableModel(columns, 0);
		hikivisionCamerasListTable = getHikivisionCamerasListTable();
		hikivisionCamerasListTable.setModel(dataModel);

		JScrollPane scrollPane = new JScrollPane(hikivisionCamerasListTable);
		add(scrollPane);
	}

	private JButton getAddButtonLocal() {
		JButton botao = new JButton("Adicionar câmera");
		botao.addActionListener(e -> {
			SelectItem item = (SelectItem) optionsAttachedHikivisionCamerasComboBox.getSelectedItem();
			if (item == null)
				return;

			String id = (String) item.getValue();
			if (camerasSelecionadas.stream().anyMatch(cam -> cam.getIdDevice().equals(id))) {
				return;
			}

			HikivisionDeviceTO.Device device = buscarDispositivoPorId(id);
			if (device == null)
				return;

			AttachedTO to = new AttachedTO();
			to.setIdDevice(device.getDevIndex());
			to.setNomeDevice(device.getDevName());

			camerasSelecionadas.add(to);
			dataModel.addRow(attachedToToObject(to));
		});
		return botao;
	}

	private JButton getRemoveButtonLocal() {
		JButton botao = new JButton("Remover câmera");
		botao.addActionListener(e -> {
			int row = hikivisionCamerasListTable.getSelectedRow();
			if (row >= 0) {
				String nome = (String) dataModel.getValueAt(row, 0);
				camerasSelecionadas.removeIf(cam -> cam.getNomeDevice().equals(nome));
				dataModel.removeRow(row);
			}
		});
		return botao;
	}



	public List<AttachedTO> getCamerasSelecionadas() {
		return camerasSelecionadas;
	}


	public void setCamerasSelecionadas(List<String> hikivisionDeviceNames) {
	    camerasSelecionadas.clear();
	    dataModel.setRowCount(0); // Limpa a tabela

	    if (hikivisionDeviceNames == null || hikivisionDeviceNames.isEmpty())
	        return;

	    for (String nome : hikivisionDeviceNames) {
	        // Busca o dispositivo pelo nome
	        HikivisionDeviceTO.Device device = buscarDispositivoPorNome(nome);
	        if (device != null) {
	            AttachedTO to = new AttachedTO();
	            to.setIdDevice(device.getDevIndex());
	            to.setNomeDevice(device.getDevName());

	            camerasSelecionadas.add(to);
	            dataModel.addRow(attachedToToObject(to));
	        }
	    }
	}

	private Device buscarDispositivoPorNome(String nome) {
		if (hikivisionDevices == null)
			return null;
		for (HikivisionDeviceTO.Device d : hikivisionDevices) {
			if (d.getDevName().equals(nome))
				return d;
		}
		return null;
	}

	private HikivisionDeviceTO.Device buscarDispositivoPorId(String id) {
		if (hikivisionDevices == null)
			return null;
		for (HikivisionDeviceTO.Device d : hikivisionDevices) {
			if (d.getDevIndex().equals(id))
				return d;
		}
		return null;
	}

}
