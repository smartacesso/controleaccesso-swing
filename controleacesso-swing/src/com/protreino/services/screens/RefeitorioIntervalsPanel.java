package com.protreino.services.screens;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;

import com.protreino.services.devices.Device;

public class RefeitorioIntervalsPanel extends JPanel {

    /**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private JTable intervalsTable;
    private DefaultTableModel dataModel;
    private JButton addButton;
    private JButton removeButton;

    private Device deviceAtual;

    public RefeitorioIntervalsPanel(Device deviceAtual) {
        this.deviceAtual = deviceAtual;

        // Inicializa lista no device, se estiver null
        if (deviceAtual.getAttachedRefeitorioIntervals() == null) {
            deviceAtual.setAttachedRefeitorioIntervals(new ArrayList<>());
        }

        // Se ainda não houver intervalos cadastrados, carrega os padrões
        if (deviceAtual.getAttachedRefeitorioIntervals().isEmpty()) {
            carregarIntervalosPadrao(deviceAtual.getAttachedRefeitorioIntervals());
        }

        // Painel da tabela
        JPanel intervalsListPanel = new JPanel();
        intervalsListPanel.setLayout(new BoxLayout(intervalsListPanel, BoxLayout.Y_AXIS));

        // Colunas
        String[] columns = {"Nome", "Início", "Fim"};
        dataModel = new DefaultTableModel(columns, 0);
        intervalsTable = new JTable(dataModel);
        intervalsTable.setRowHeight(30);

        // Preenche tabela com os intervalos do device
        for (IntervaloTO i : deviceAtual.getAttachedRefeitorioIntervals()) {
            dataModel.addRow(new Object[]{i.getNome(), i.getInicio(), i.getFim()});
        }

        JScrollPane scrollPane = new JScrollPane(intervalsTable);
        scrollPane.getVerticalScrollBar().setUnitIncrement(10);
        intervalsListPanel.add(scrollPane);

        // Painel dos botões
        addButton = new JButton("Adicionar intervalo");
        addButton.addActionListener(e -> adicionarIntervalo());

        removeButton = new JButton("Remover intervalo");
        removeButton.addActionListener(e -> removerIntervalo());

        JPanel buttonsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        buttonsPanel.add(addButton);
        buttonsPanel.add(removeButton);

        // Layout final
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
        add(buttonsPanel);
        add(Box.createRigidArea(new Dimension(0, 5)));
        add(intervalsListPanel);
    }

    private void carregarIntervalosPadrao(List<IntervaloTO> lista) {
        lista.add(new IntervaloTO("LANCHE", "09:00", "09:30"));
        lista.add(new IntervaloTO("ALMOÇO", "11:30", "13:00"));
        lista.add(new IntervaloTO("JANTAR", "19:00", "20:00"));
    }

    private void adicionarIntervalo() {
        String nome = JOptionPane.showInputDialog(this, "Nome do intervalo:");
        String inicio = JOptionPane.showInputDialog(this, "Hora início (HH:mm):");
        String fim = JOptionPane.showInputDialog(this, "Hora fim (HH:mm):");

        if (nome != null && inicio != null && fim != null) {
            IntervaloTO novo = new IntervaloTO(nome, inicio, fim);
            deviceAtual.getAttachedRefeitorioIntervals().add(novo); // salva direto no device
            dataModel.addRow(new Object[]{nome, inicio, fim});
        }
    }

    private void removerIntervalo() {
        int selectedRow = intervalsTable.getSelectedRow();
        if (selectedRow >= 0) {
            deviceAtual.getAttachedRefeitorioIntervals().remove(selectedRow); // remove do device
            dataModel.removeRow(selectedRow);
        }
    }

    // DTO para armazenar intervalos
    public static class IntervaloTO {
        private String nome;
        private String inicio;
        private String fim;

        public IntervaloTO(String nome, String inicio, String fim) {
            this.nome = nome;
            this.inicio = inicio;
            this.fim = fim;
        }

        public String getNome() { return nome; }
        public String getInicio() { return inicio; }
        public String getFim() { return fim; }
    }
}
