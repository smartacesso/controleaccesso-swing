package com.protreino.services.screens;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.MouseEvent;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.ListSelectionModel;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.main.Main;
import com.protreino.services.screens.AccessListPanel.ActionRenderer;
import com.protreino.services.screens.UrlRenderer;
import com.protreino.services.to.DeviceTO;
import com.protreino.services.utils.HibernateUtil;
import com.protreino.services.utils.Utils;

public class DevicefromServerPanel extends JPanel {

	private List<DeviceTO> devices;
	private String[] columns = { "Nome", "Identificador", "Fabricante", "Conectado", "liberar Acesso" };
	private Integer[] columnWidths = { 270, 60, 60, 50, 120 };
	private JTable devicesListTable;
	private JButton syncButton;
	private List<Integer> colunasComLink;

	public DevicefromServerPanel() {
		
		Font font = new JLabel().getFont();
		Font headerFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
		colunasComLink = new ArrayList<Integer>();
		colunasComLink.add(4);

		JPanel devicesFromServerTablePanel = new JPanel();
		devicesFromServerTablePanel.setLayout(new BoxLayout(devicesFromServerTablePanel, BoxLayout.Y_AXIS));
		devicesListTable = new JTable(new DefaultTableModel(columns, 0));
		 //formatTable();

		devicesListTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
		devicesListTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		devicesListTable.getTableHeader().setReorderingAllowed(false);
		devicesListTable.getTableHeader().setOpaque(false);
		devicesListTable.getTableHeader().setForeground(Main.firstColor);
		devicesListTable.getTableHeader().setFont(headerFont);
		devicesListTable.getTableHeader().setBackground(Main.secondColor);
		devicesListTable.setRowHeight(30);
		devicesListTable.setSelectionBackground(Main.firstColor);
		devicesListTable.setSelectionForeground(Color.WHITE);
		TableCellRenderer rendererFromHeader = devicesListTable.getTableHeader().getDefaultRenderer();
		JLabel headerLabel = (JLabel) rendererFromHeader;
		headerLabel.setHorizontalAlignment(JLabel.CENTER);
		JScrollPane scrollPane = new JScrollPane(devicesListTable);
		scrollPane.getVerticalScrollBar().setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));
		devicesFromServerTablePanel.add(scrollPane);

		syncButton = new JButton("Atualizar lista com o servidor");
		syncButton.setBorder(new EmptyBorder(10, 15, 10, 15));
		syncButton.setPreferredSize(new Dimension(180, 40));

		JPanel statusPanel = new JPanel();
		statusPanel.setLayout(new BoxLayout(statusPanel, BoxLayout.X_AXIS));
		statusPanel.setMaximumSize(new Dimension(Integer.MAX_VALUE, 100));
		statusPanel.add(Box.createHorizontalStrut(10));
		statusPanel.add(syncButton);

		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		setBorder(BorderFactory.createEmptyBorder(10,10,10,10));
		add(Box.createRigidArea(new Dimension(0,5)));
		add(devicesFromServerTablePanel);
		add(Box.createRigidArea(new Dimension(0,5)));
		add(statusPanel);

		syncButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				// Main.syncAthleteAccessList();
				devices = HibernateUtil.getListDeviceFromServer();
				if (devices != null) {
					populateTable(devices);
				}
			}
		});
	}

	private void formatTable() {
		UrlRenderer urlRenderer = new UrlRenderer(colunasComLink);
		urlRenderer.setHorizontalAlignment(JLabel.CENTER);
		ActionRenderer actionRenderer = new ActionRenderer(colunasComLink);
		actionRenderer.setHorizontalAlignment(JLabel.CENTER);
		DefaultTableCellRenderer centerRenderer = new DefaultTableCellRenderer();
		centerRenderer.setHorizontalAlignment(JLabel.CENTER);

		while (devicesListTable.getMouseListeners().length > 1) {
			devicesListTable.removeMouseListener(devicesListTable.
					getMouseListeners()[devicesListTable.getMouseListeners().length-1]);			
		}
		
		devicesListTable.addMouseListener(actionRenderer);
		
		while (devicesListTable.getMouseMotionListeners().length > 1) {
			devicesListTable.removeMouseMotionListener(devicesListTable.
					getMouseMotionListeners()[devicesListTable.getMouseMotionListeners().length-1]);			
		}
		
		devicesListTable.addMouseMotionListener(actionRenderer);
		
		for (int i = 0; i < devicesListTable.getColumnCount(); i++) {
			TableColumn column = devicesListTable.getColumnModel().getColumn(i);
			column.setPreferredWidth(columnWidths[i]);
 
			if (i == 4) {
				column.setCellRenderer(actionRenderer);
			} else {
				column.setCellRenderer(centerRenderer);
			}
		}

	}

	private void populateTable(List<DeviceTO> devicesFromServer) {
		DefaultTableModel dataModel = new DefaultTableModel(columns, 0) {
			public boolean isCellEditable(int rowIndex, int mColIndex) {
				return false;
			}
		};
		if (devicesFromServer != null && !devicesFromServer.isEmpty()) {
			for (DeviceTO device : devicesFromServer) {
				Object[] obj = new Object[8];
				obj[0] = device.getName();
				obj[1] = device.getIdentifier();
				obj[2] = device.getManufacturer();
				obj[3] = device.isConnected() ? "SIM" : "NÃO";
				obj[4] = "LIBERAR ACESSO";
				dataModel.addRow(obj);
			}
		}

		devicesListTable.setModel(dataModel);
		formatTable();
	}

	class ActionRenderer extends UrlRenderer {

		public ActionRenderer(List<Integer> colunasComLink) {
			super(colunasComLink);
		}

		@Override
		public void mouseClicked(MouseEvent e) {
			JTable table = (JTable) e.getComponent();
			Point pt = e.getPoint();
			int ccol = table.columnAtPoint(pt);
			if (ccol == 4) { // && pointInsidePrefSize(table, pt)) {
				int crow = table.rowAtPoint(pt);
				String identifier = String.valueOf(table.getValueAt(crow, 1));
				boolean isConnected = "SIM".equals(table.getValueAt(crow, 3).toString()) ? true : false;
				
				if(isConnected) {
					new EscolherSentidoLiberarAcessoServidorDialog(identifier);
				
				} else {
					new DeviceNotConnectedDialog();
				}
			}
		}

		@Override
		public void mouseDragged(MouseEvent e) {
			/* not needed */ }

		@Override
		public void mouseEntered(MouseEvent e) {
			/* not needed */ }

		@Override
		public void mousePressed(MouseEvent e) {
			/* not needed */ }

		@Override
		public void mouseReleased(MouseEvent e) {
			/* not needed */ }
	}
	
	class DeviceNotConnectedDialog {
		public JDialog deviceNotConnectedDialog;
		
		public DeviceNotConnectedDialog() {
			deviceNotConnectedDialog = new JDialog();
			deviceNotConnectedDialog.setIconImage(Main.favicon);
			deviceNotConnectedDialog.setModal(true);
			deviceNotConnectedDialog.setTitle("Device não conectado");
			deviceNotConnectedDialog.setResizable(false);
			
			JPanel deviceNotConnectedPanel = new JPanel();
			deviceNotConnectedPanel.setBorder(new EmptyBorder(20, 20, 20, 20));
			deviceNotConnectedPanel.setLayout(new BoxLayout(deviceNotConnectedPanel, BoxLayout.Y_AXIS));
			
			JLabel label = new JLabel("Este equipamento não está conectado");
			
			deviceNotConnectedPanel.add(label);
			deviceNotConnectedPanel.add(Box.createVerticalStrut(10));

			deviceNotConnectedDialog.addComponentListener(new ComponentAdapter() {
				@Override
				public void componentHidden(ComponentEvent e) {
					deviceNotConnectedDialog = null;
				}
			});
			
			deviceNotConnectedDialog.getContentPane().add(deviceNotConnectedPanel);
			deviceNotConnectedDialog.pack();
			deviceNotConnectedDialog.setLocationRelativeTo(null);
			deviceNotConnectedDialog.setVisible(true);
		}
		
	}

}
