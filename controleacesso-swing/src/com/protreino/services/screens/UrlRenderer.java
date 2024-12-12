package com.protreino.services.screens;

// Importações necessárias para o funcionamento do renderer de célula e manipulação de eventos
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.net.URI;
import java.util.List;
import java.util.Objects;

import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;

import com.protreino.services.main.Main;

// Classe responsável por renderizar links em células de uma JTable.
// Implementa MouseListener e MouseMotionListener para gerenciar interação com o mouse.
public class UrlRenderer extends DefaultTableCellRenderer implements MouseListener, MouseMotionListener {

    // Índices da célula (linha e coluna) que estão sendo "roladas" com o mouse.
    protected int vrow = -1; // viewRowIndex
    protected int vcol = -1; // viewColumnIndex
    protected boolean isRollover; // Indica se o mouse está sobre uma célula com link.

    // Lista de colunas que possuem links.
    private List<Integer> colunasComLink;

    // Construtor que recebe a lista de colunas com links.
    public UrlRenderer(List<Integer> colunasComLink) {
        this.colunasComLink = colunasComLink;
    }

    @Override
    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
            int row, int column) {

        // Chamada ao método da superclasse para configuração inicial da célula.
        super.getTableCellRendererComponent(table, value, isSelected, false, row, column);

        // Obtem o texto a ser exibido na célula, ou uma string vazia se o valor for null.
        String str = Objects.toString(value, "");

        // Define a cor do texto dependendo se a célula está selecionada ou não.
        String color = isSelected ? "white" : "#46b2ca";

        // Se o mouse está sobre a célula (efeito "rollover"), adiciona um sublinhado ao texto.
        if (isRolloverCell(table, row, column)) {
            setText("<html><u><font color='" + color + "'>" + str);
        }
        // Caso contrário, exibe o texto normalmente, mas com a cor correspondente.
        else if (hasFocus) {
            setText("<html><font color='" + color + "'>" + str);
        } else {
            setText(str);
        }

        return this;
    }

    // Verifica se a célula atual é a que o mouse está sobrevoando.
    protected boolean isRolloverCell(JTable table, int row, int column) {
        return this.vrow == row && this.vcol == column && this.isRollover;
    }

    @Override
    public void mouseMoved(MouseEvent e) {
        JTable table = (JTable) e.getComponent();
        Point pt = e.getPoint();

        // Salva os índices anteriores da célula para possível atualização.
        final int prevRow = vrow;
        final int prevCol = vcol;
        final boolean prevRollover = isRollover;

        // Atualiza os índices da célula atual com base na posição do mouse.
        vrow = table.rowAtPoint(pt);
        vcol = table.columnAtPoint(pt);

        // Altera o cursor para "mão" se a célula contém link; caso contrário, volta ao padrão.
        if (colunasComLink.contains(vcol)) {
            Main.mainScreen.setCursor(new Cursor(Cursor.HAND_CURSOR));
        } else {
            Main.mainScreen.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        }

        // Atualiza o estado de "rollover".
        isRollover = colunasComLink.contains(vcol);

        // Se não houver mudanças relevantes, retorna.
        if (vrow == prevRow && vcol == prevCol && isRollover == prevRollover) {
            return;
        }

        // Define o retângulo a ser repintado na tabela.
        Rectangle repaintRect;
        if (isRollover) {
            Rectangle r = table.getCellRect(vrow, vcol, false);
            repaintRect = prevRollover ? r.union(table.getCellRect(prevRow, prevCol, false)) : r;
        } else {
            repaintRect = table.getCellRect(prevRow, prevCol, false);
        }

        // Solicita a repintura da tabela na área definida.
        table.repaint(repaintRect);
    }

    @Override
    public void mouseExited(MouseEvent e) {
        Main.mainScreen.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        JTable table = (JTable) e.getComponent();

        // Restaura o estado original quando o mouse sai da tabela.
        if (colunasComLink.contains(vcol)) {
            table.repaint(table.getCellRect(vrow, vcol, false));
            vrow = -1;
            vcol = -1;
            isRollover = false;
        }
    }

    @Override
    public void mouseClicked(MouseEvent e) {
        JTable table = (JTable) e.getComponent();
        Point pt = e.getPoint();

        // Obtém a coluna da célula clicada.
        int ccol = table.columnAtPoint(pt);

        // Verifica se a célula clicada está na coluna específica (neste caso, coluna 4).
        if (ccol == 4) {
            int crow = table.rowAtPoint(pt); // Obtém a linha clicada.
            String idUser = String.valueOf(table.getValueAt(crow, 0)); // Obtém o valor da primeira coluna (idUser).
            try {
                // Abre o link no navegador com base no ID do usuário.
                open(Main.urlApplication + "/paginas/sistema/pedestres/cadastroPedestre.xhtml?id=" + idUser);
            } catch (Exception e1) {
                e1.printStackTrace();
            }
        }
    }

    // Métodos de MouseListener não utilizados.
    @Override
    public void mouseDragged(MouseEvent e) { /* not needed */ }
    @Override
    public void mouseEntered(MouseEvent e) { /* not needed */ }
    @Override
    public void mousePressed(MouseEvent e) { /* not needed */ }
    @Override
    public void mouseReleased(MouseEvent e) { /* not needed */ }

    // Método para abrir um URL no navegador.
    private static void open(String url) {
        try {
            if (Desktop.isDesktopSupported()) {
                // Abre o link no navegador padrão.
                URI uri = new URI(url);
                Desktop.getDesktop().browse(uri);
            } else {
                // Alternativa para sistemas que não suportam Desktop.
                System.out.println("Desktop nao suportado. Tentando abrir executando a url.dll");
                Runtime rt = Runtime.getRuntime();
                rt.exec("rundll32 url.dll,FileProtocolHandler " + url);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
