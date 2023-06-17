package com.protreino.services.screens;

import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.main.Main;
import com.protreino.services.to.hikivision.HikivisionDeviceTO;

import javax.swing.*;
import java.awt.*;
import java.util.List;

public class SincronizacaoManualDialog extends BaseDialog  {

    private Font font;
    private Font tabHeaderFont;
    private Container mainContentPane;
    private JTabbedPane tabbedPane;

    private List<HikivisionDeviceTO> deviceList;

    public SincronizacaoManualDialog() {
        setIconImage(Main.favicon);
        setModal(true);
        setTitle("Sincronismo manual de dispositivos");
        setResizable(false);
        setLayout(new BorderLayout());

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
    }

    private JPanel criarPanelDispositivos() {
        JTable accessListTable;

        String[] columns = {"Device Id", "Device Name", "Status", ""};
        Integer[] columnWidths = {280, 200, 150, 80};


        return null;
    }
}
