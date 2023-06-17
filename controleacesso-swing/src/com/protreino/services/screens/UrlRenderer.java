package com.protreino.services.screens;

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

public class UrlRenderer extends DefaultTableCellRenderer implements MouseListener, MouseMotionListener {
	protected int vrow = -1; // viewRowIndex
	protected int vcol = -1; // viewColumnIndex
	protected boolean isRollover;
	private List<Integer> colunasComLink;

	public UrlRenderer(List<Integer> colunasComLink) {
		this.colunasComLink = colunasComLink;
	}
	@Override
	public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
			int row, int column) {
		
		super.getTableCellRendererComponent(table, value, isSelected, false, row, column);
		String str = Objects.toString(value, "");
		String color = isSelected ? "white" : "#46b2ca";
		if (isRolloverCell(table, row, column)) {
			setText("<html><u><font color='" + color + "'>" + str);
		} else if (hasFocus) {
			setText("<html><font color='" + color + "'>" + str);
		} else {
			setText(str);
		}
		return this;
	}


	protected boolean isRolloverCell(JTable table, int row, int column) {
		return this.vrow == row && this.vcol == column && this.isRollover;
	}

	@Override
	public void mouseMoved(MouseEvent e) {
		JTable table = (JTable) e.getComponent();
		Point pt = e.getPoint();
		final int prevRow = vrow;
		final int prevCol = vcol;
		final boolean prevRollover = isRollover;
		vrow = table.rowAtPoint(pt);
		vcol = table.columnAtPoint(pt);
		if (colunasComLink.contains(vcol))
			Main.mainScreen.setCursor(new Cursor(Cursor.HAND_CURSOR));
		else
			Main.mainScreen.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		isRollover = colunasComLink.contains(vcol);
		if (vrow == prevRow && vcol == prevCol && isRollover == prevRollover) {
			return;
		}
		if (!isRollover && !prevRollover) {
			return;
		}
		Rectangle repaintRect;
		if (isRollover) {
			Rectangle r = table.getCellRect(vrow, vcol, false);
			repaintRect = prevRollover ? r.union(table.getCellRect(prevRow, prevCol, false)) : r;
		} else {
			repaintRect = table.getCellRect(prevRow, prevCol, false);
		}
		table.repaint(repaintRect);
	}

	@Override
	public void mouseExited(MouseEvent e) {
		Main.mainScreen.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		JTable table = (JTable) e.getComponent();
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
		int ccol = table.columnAtPoint(pt);
		if (ccol == 4) { // && pointInsidePrefSize(table, pt)) {
			int crow = table.rowAtPoint(pt);
			String idUser = String.valueOf(table.getValueAt(crow, 0));
			try {
				open(Main.urlApplication + "/paginas/sistema/pedestres/cadastroPedestre.xhtml?id=" + idUser);
			} catch (Exception e1) {
				e1.printStackTrace();
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
	
	private static void open(String url) {
		try {
			if (Desktop.isDesktopSupported()) {
				URI uri = new URI(url);
				Desktop.getDesktop().browse(uri);
			}
			else {
				System.out.println("Desktop não suportado. Tentando abrir executando a url.dll");
				Runtime rt = Runtime.getRuntime();
				rt.exec("rundll32 url.dll,FileProtocolHandler " + url);
			}
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}
}
