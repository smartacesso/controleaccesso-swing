package com.protreino.services.screens;

import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.main.Main;
import com.protreino.services.to.hikivision.HikivisionDeviceTO;
import com.protreino.services.to.hikivision.HikivisionDeviceTO.Device;
import com.protreino.services.to.hikivision.HikivisionDeviceTO.MatchList;
import com.protreino.services.to.hikivision.HikivisionDeviceTO.SearchResult;
import com.protreino.services.utils.HibernateUtil;
import com.protreino.services.utils.HikiVisionIntegrationService;
import com.protreino.services.utils.Utils;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.table.*;
import javax.swing.text.JTextComponent;
import javax.swing.text.MaskFormatter;
import java.awt.*;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.*;

public class SincronizacaoManualDialog extends BaseDialog {

    private Font font;
    private Font tabHeaderFont;
    private Container mainContentPane;
    private JTabbedPane tabbedPane;
    private HikiVisionIntegrationService hikiVisionService;
    private String[] columns = {"Device Id", "Device Name", "Status", "Sincronizar"};
    private Integer[] columnWidths = {280, 200, 150, 80};
    private JTable deviceListTable;

    private JButton syncALl;

    private JButton syncByDate;

    private SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm");

    private static final int CHECKBOX_COLUMN = 3;

    private List<HikivisionDeviceTO> deviceList;

    public SincronizacaoManualDialog() {
        setIconImage(Main.favicon);
        setModal(true);
        setTitle("Sincronismo manual de dispositivos");
        setResizable(true);
        setLayout(new BorderLayout());
        setPreferredSize(new Dimension(920, 718));
        setMinimumSize(getPreferredSize());
        hikiVisionService = HikiVisionIntegrationService.getInstace();

        font = new JLabel().getFont();
        Font font2 = font;
        tabHeaderFont = new Font(font2.getFontName(), Font.BOLD, font2.getSize() + 1);

        mainContentPane = new Container();
        mainContentPane.setLayout(new BorderLayout());


        Font font = new JLabel().getFont();
        Font headerFont = new Font(font.getFontName(), Font.BOLD, font.getSize());

        JPanel deviceListTablePanel = new JPanel();
        deviceListTablePanel.setLayout(new BoxLayout(deviceListTablePanel, BoxLayout.Y_AXIS));
        deviceListTablePanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
        deviceListTable = new JTable(new DefaultTableModel(columns, 0));
        formatTable();
        deviceListTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        deviceListTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        deviceListTable.getTableHeader().setReorderingAllowed(false);
        deviceListTable.getTableHeader().setOpaque(false);
        deviceListTable.getTableHeader().setForeground(Main.firstColor);
        deviceListTable.getTableHeader().setBackground(Main.secondColor);
        deviceListTable.getTableHeader().setFont(headerFont);
        deviceListTable.setRowHeight(30);
        deviceListTable.setSelectionBackground(Main.firstColor);
        deviceListTable.setSelectionForeground(Color.WHITE);
        TableCellRenderer rendererFromHeader = deviceListTable.getTableHeader().getDefaultRenderer();
        JLabel headerLabel = (JLabel) rendererFromHeader;
        headerLabel.setHorizontalAlignment(JLabel.CENTER);
        JScrollPane scrollPane = new JScrollPane(deviceListTable);
        scrollPane.getVerticalScrollBar().setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));
        deviceListTablePanel.add(scrollPane);


        syncALl = new JButton("Sincronização Total");
        syncALl.setBorder(new EmptyBorder(10, 15, 10, 15));
        syncALl.setPreferredSize(new Dimension(180, 40));

        syncByDate = new JButton("Sincronização por data");
        syncByDate.setBorder(new EmptyBorder(10, 15, 10, 15));
        syncByDate.setPreferredSize(new Dimension(180, 40));
        syncByDate.addActionListener(e -> {
            criarDialogoDeSincronizacaoPorData();
        });

        JPanel actionsPanel = new JPanel();
        actionsPanel.setLayout(new BoxLayout(actionsPanel, BoxLayout.X_AXIS));
        actionsPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
        actionsPanel.setMaximumSize(new Dimension(Integer.MAX_VALUE, 100));
        actionsPanel.add(Box.createHorizontalGlue());
        actionsPanel.add(syncALl);
        actionsPanel.add(Box.createHorizontalStrut(10));
        actionsPanel.add(syncByDate);


        populateTable();

        mainContentPane.add(deviceListTablePanel, BorderLayout.CENTER);
        mainContentPane.add(actionsPanel, BorderLayout.SOUTH);
        getContentPane().add(mainContentPane, BorderLayout.CENTER);
        setVisible(true);
        pack();
        setLocationRelativeTo(null);


    }

    private void criarDialogoDeSincronizacaoPorData() {

        JDialog sincronizacaoPorDataDialog = new JDialog();
        sincronizacaoPorDataDialog.setIconImage(Main.favicon);
        sincronizacaoPorDataDialog.setModal(true);
        sincronizacaoPorDataDialog.setTitle("Confirmar");
        sincronizacaoPorDataDialog.setResizable(false);
        sincronizacaoPorDataDialog.setLayout(new BorderLayout());

        JPanel mainPanel = new JPanel();
        mainPanel.setBorder(new EmptyBorder(20, 20, 20, 20));
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

        JLabel dataInicioLabel = new JLabel("Data de Início");
        JFormattedTextField dataInicioTextField = Utils.getNewJFormattedTextField(12);
        dataInicioTextField.setFont(new Font("SansSerif", Font.PLAIN, 16));
        MaskFormatter mask = Utils.getNewMaskFormatter("##/##/#### ##:##");
        mask.install(dataInicioTextField);
        JPanel dataInicioPanel = getNewMiniPanel(dataInicioLabel, dataInicioTextField);


        JLabel dataFimLabel = new JLabel("Data de Fim");
        JFormattedTextField dataFimTextField = Utils.getNewJFormattedTextField(12);
        dataFimTextField.setFont(new Font("SansSerif", Font.PLAIN, 16));
        mask = Utils.getNewMaskFormatter("##/##/#### ##:##");
        mask.install(dataFimTextField);
        JPanel dataFinalPanel = getNewMiniPanel(dataFimLabel, dataFimTextField);


        JButton confirmarButton = new JButton("Confirmar");
        confirmarButton.setBorder(new EmptyBorder(10, 20, 10, 20));
        confirmarButton.setAlignmentX(Component.CENTER_ALIGNMENT);
        confirmarButton.addActionListener(e -> {

            Date dataInicio = null;
            Date dataFim = null;

            try {
                dataInicio = sdf.parse(dataInicioTextField.getText());
                dataFim = sdf.parse(dataFimTextField.getText());

            } catch (ParseException ex) {
                ex.printStackTrace();
            }
            syncDevices(dataInicio, dataFim);

            sincronizacaoPorDataDialog.dispose();
        });
        mainPanel.add(dataInicioPanel);
        mainPanel.add(Box.createVerticalStrut(10));
        mainPanel.add(dataFinalPanel);
        mainPanel.add(Box.createVerticalStrut(10));
        mainPanel.add(confirmarButton);
        sincronizacaoPorDataDialog.getContentPane().add(mainPanel, BorderLayout.CENTER);
        sincronizacaoPorDataDialog.pack();
        sincronizacaoPorDataDialog.setLocationRelativeTo(null);
        sincronizacaoPorDataDialog.setVisible(true);

    }

    private void syncDevices(final Date inicio, final Date fim) {
        TableModel model = deviceListTable.getModel();
        List<String> devicesToSync = new ArrayList<>();
        for (int i = 0; i < model.getRowCount(); i++) {
            System.out.println(model.getValueAt(i, 3));
            if (Boolean.valueOf(model.getValueAt(i, 3).toString())) {
                devicesToSync.add(model.getValueAt(i, 0).toString());
            }
        }
        if (devicesToSync.isEmpty()) {
            return;
        }
        List<PedestrianAccessEntity> pedestresParaSicronizar = buscaPedestresParaSicronizar(inicio, fim);

        devicesToSync.forEach(device -> {
            pedestresParaSicronizar.forEach(pedestre ->{
                if(pedestre.getRemovido() || pedestre.getFoto() == null) {
                    hikiVisionService.apagarUsuario(device, pedestre.getCardNumber());
                }
                else {
                    if(!hikiVisionService.isUsuarioJaCadastrado(device, pedestre.getCardNumber())) {
                        hikiVisionService.adicionarUsuario(device, pedestre.getCardNumber(), pedestre.getName());
                    }
                   if(hikiVisionService.isFotoUsuarioJaCadastrada(device, pedestre.getCardNumber())) {
                       hikiVisionService.apagarFotoUsuario(device, pedestre.getCardNumber());
                   }
                    hikiVisionService.adicionarFotoUsuario(device, pedestre.getCardNumber(), pedestre.getFoto());
                }
            });
        });
    }

    private List<PedestrianAccessEntity> buscaPedestresParaSicronizar(Date inicio, Date fim) {
        if (inicio != null && fim != null) {
            //query por data
            HashMap<String, Object> args = new HashMap<>();
            args.put("INIT_DATE", inicio);
            args.put("END_DATE", fim);

            return (List<PedestrianAccessEntity>) HibernateUtil.getResultListWithParams(
                    PedestrianAccessEntity.class, "PedestrianAccessEntity.findAllWithHikiVisionImageOnRegistredBeteenDate", args);
        }

        return (List<PedestrianAccessEntity>) HibernateUtil.getResultList(
                PedestrianAccessEntity.class, "PedestrianAccessEntity.findAllWithHikiVisionImageOnRegistred");
    }

    private void formatTable() {

        DefaultTableCellRenderer centerRenderer = new DefaultTableCellRenderer();
        centerRenderer.setHorizontalAlignment(JLabel.CENTER);
        deviceListTable.setDefaultRenderer(String.class, centerRenderer);


        for (int i = 0; i < deviceListTable.getColumnCount(); i++) {
            TableColumn column = deviceListTable.getColumnModel().getColumn(i);
            column.setPreferredWidth(columnWidths[i]);
        }
    }

    public void populateTable() {

        DefaultTableModel dataModel = new DefaultTableModel(columns, 0) {

            @Override
            public Class<?> getColumnClass(int columnIndex) {
                Class clazz = String.class;
                switch (columnIndex) {
                    case CHECKBOX_COLUMN:
                        clazz = Boolean.class;
                        break;
                }
                return clazz;
            }

            @Override
            public boolean isCellEditable(int row, int column) {
                return column == CHECKBOX_COLUMN;
            }

            @Override
            public void setValueAt(Object aValue, int row, int column) {
                if (aValue instanceof Boolean && column == CHECKBOX_COLUMN) {
                    Vector rowData = (Vector) getDataVector().get(row);
                    rowData.set(CHECKBOX_COLUMN, (boolean) aValue);
                    fireTableCellUpdated(row, column);
                }
            }
        };
        //HikivisionDeviceTO hikivisionDevice = hikiVision.listarDisposivos();
        HikivisionDeviceTO hikivisionDevice = new HikivisionDeviceTO();
        SearchResult searchResult = new SearchResult();
        List<MatchList> MatchList = new ArrayList<>();
        MatchList matchListcurrent = new MatchList();

        Device deviceCurrent = new Device();
        deviceCurrent.setDevIndex("1");
        deviceCurrent.setDevStatus("ativo");
        deviceCurrent.setDevName("camera 1");
        matchListcurrent.setDevice(deviceCurrent);
        MatchList.add(matchListcurrent);
        MatchList.add(matchListcurrent);
        searchResult.setMatchList(MatchList);
        searchResult.setNumOfMatches(1);
        searchResult.setTotalMatches(1);
        hikivisionDevice.setSearchResult(searchResult);


        if (hikivisionDevice == null || hikivisionDevice.getSearchResult() == null ||
                hikivisionDevice.getSearchResult().getTotalMatches() == 0) {
            deviceListTable.setModel(dataModel);
            return;
        }

        for (MatchList matchList : hikivisionDevice.getSearchResult().getMatchList()) {
            Device device = matchList.getDevice();
            Object[] obj = new Object[4];
            obj[0] = device.getDevIndex();
            obj[1] = device.getDevName();
            obj[2] = device.getDevStatus();
            obj[3] = true;

            dataModel.addRow(obj);

        }
        deviceListTable.setModel(dataModel);

    }

    private JPanel getNewMiniPanel(JLabel label, JTextComponent text) {
        JPanel panel = new JPanel();
        panel.setLayout(new GridBagLayout());

        GridBagConstraints c = new GridBagConstraints();

        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridx = 0;
        c.gridy = 0;
        panel.add(label, c);

        c.gridx = 0;
        c.gridy = 1;
        panel.add(text, c);

        return panel;
    }

    private GridBagConstraints getNewGridBag(int x, int y, int iY, int iX) {
        GridBagConstraints c = new GridBagConstraints();

        c.gridx = x;
        c.gridy = y;
        c.ipady = iY;
        c.ipadx = iX;
        c.anchor = GridBagConstraints.LINE_START;

        return c;
    }


}
