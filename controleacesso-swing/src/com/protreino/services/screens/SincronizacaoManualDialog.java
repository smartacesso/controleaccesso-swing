package com.protreino.services.screens;

import com.nitgen.SDK.BSP.NBioBSPJNI.DEVICE_ID;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.main.Main;
import com.protreino.services.to.hikivision.HikivisionDeviceTO;
import com.protreino.services.to.hikivision.HikivisionDeviceTO.Device;
import com.protreino.services.to.hikivision.HikivisionDeviceTO.MatchList;
import com.protreino.services.to.hikivision.HikivisionDeviceTO.SearchResult;
import com.protreino.services.utils.HikiVisionIntegrationService;
import com.protreino.services.utils.Utils;

import javax.swing.*;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;

import java.awt.*;
import java.util.List;

public class SincronizacaoManualDialog extends BaseDialog {

    private Font font;
    private Font tabHeaderFont;
    private Container mainContentPane;
    private JTabbedPane tabbedPane;
    private HikiVisionIntegrationService hikiVision;
    public String[] columns = {"Device Id", "Device Name", "Status", ""};
    private JTable accessListTable;

    private List<HikivisionDeviceTO> deviceList;

    public SincronizacaoManualDialog() {
        setIconImage(Main.favicon);
        setModal(true);
        setTitle("Sincronismo manual de dispositivos");
        setResizable(false);
        setLayout(new BorderLayout());
        hikiVision = HikiVisionIntegrationService.getInstace();

        font = new JLabel().getFont();
        Font font2 = font;
        tabHeaderFont = new Font(font2.getFontName(), Font.BOLD, font2.getSize() + 1);

        mainContentPane = new Container();
        mainContentPane.setLayout(new BorderLayout());

        tabbedPane = new JTabbedPane();

        JPanel devicesPanel = criarPanelDispositivos();
        tabbedPane.add("Lista de dispositivos", devicesPanel);
        JLabel label = new JLabel("Lista de dispositivos");
        label.setPreferredSize(new Dimension(120, 25));
        label.setForeground(Main.firstColor);
        label.setFont(tabHeaderFont);
        tabbedPane.setTabComponentAt(0, label);

        mainContentPane.add(tabbedPane, BorderLayout.CENTER);
        getContentPane().add(mainContentPane, BorderLayout.CENTER);
        pack();
        setLocationRelativeTo(null);
        Font font = new JLabel().getFont();
		Font headerFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
        
    	JPanel accessListTablePanel = new JPanel();
		accessListTablePanel.setLayout(new BoxLayout(accessListTablePanel, BoxLayout.Y_AXIS));
		accessListTable = new JTable(new DefaultTableModel(columns, 0));
		accessListTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
		accessListTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		accessListTable.getTableHeader().setReorderingAllowed(false);
		accessListTable.getTableHeader().setOpaque(false);
		accessListTable.getTableHeader().setForeground(Main.firstColor);
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
		
		add(Box.createRigidArea(new Dimension(0,5)));
		add(accessListTablePanel);
		add(Box.createRigidArea(new Dimension(0,5)));
		setVisible(true);
		pack();
		setLocationRelativeTo(null);
    }

    private JPanel criarPanelDispositivos() {
        JTable accessListTable;

        
        Integer[] columnWidths = {280, 200, 150, 80};


        return null;
    }
    
    public void populateTable() {
    	
    	DefaultTableModel dataModel = new DefaultTableModel(columns, 0) {
			public boolean isCellEditable(int rowIndex, int mColIndex) {
				return false;
			}
		};
    	HikivisionDeviceTO hikivisionDevice = hikiVision.listarDisposivos();
    	
    	if(hikivisionDevice.getSearchResult()!= null) {
    		return;
    	}
    	
		if(hikivisionDevice == null || hikivisionDevice.getSearchResult().getTotalMatches() == 0) {
			return;
		}
		
		for(MatchList matchList : hikivisionDevice.getSearchResult().getMatchList()) {
			Device device = matchList.getDevice();
    		Object[] obj = new Object[4];
    		obj[0] = device.getDevIndex(); 
    		obj[1] = device.getDevName();
    		obj[2] = device.getDevStatus();
    		//obj[3] =
    		dataModel.addRow(obj);
    		
    	}
		accessListTable.setModel(dataModel);
 
}
}
