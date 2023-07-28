package com.protreino.services.screens;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;

@SuppressWarnings("serial")
public abstract class PaginedListPanel extends JPanel{
	
	protected JButton first = null;
	protected JButton back = null;
	protected JButton prox = null;
	protected JButton last = null;
	protected JLabel countLabel;
	
	protected int totalRegistros = 0;
	protected int totalPaginas = 10;
	protected int paginaAtual = 1;
	protected int inicioPagina = 0;
	protected int registrosPorPagina = 15;
	
	protected JPanel createPaginatorControls() {
		countLabel = new JLabel("Pág. ("+ paginaAtual + "/" + totalPaginas + ") do total: " + totalRegistros);
		first = new JButton("<<");
		first.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				paginaAtual = 1;
				inicioPagina = 0;
				executeFilter();
			}
		});
		back = new JButton("< ");
		back.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(paginaAtual > 1) {
					paginaAtual--;
					inicioPagina = inicioPagina - registrosPorPagina;
				}
				executeFilter();
			}
		});
		prox = new JButton(" >");
		prox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(paginaAtual < totalPaginas) {
					paginaAtual++;
					inicioPagina = inicioPagina + registrosPorPagina;
				}
				executeFilter();
			}
		});
		last = new JButton(">>");
		last.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				paginaAtual = totalPaginas;
				inicioPagina = (registrosPorPagina * totalPaginas) - registrosPorPagina ;
				executeFilter();
			}
		});
		
		JPanel paginatorPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
		paginatorPanel.setBorder(new EmptyBorder(10,0,10,15));
		paginatorPanel.setMaximumSize(new Dimension(Integer.MAX_VALUE, 100));
		paginatorPanel.add(first);
		paginatorPanel.add(Box.createHorizontalStrut(5));
		paginatorPanel.add(back);
		paginatorPanel.add(Box.createHorizontalStrut(5));
		paginatorPanel.add(prox);
		paginatorPanel.add(Box.createHorizontalStrut(5));
		paginatorPanel.add(last);
		paginatorPanel.add(Box.createHorizontalStrut(5));
		paginatorPanel.add(countLabel);
		paginatorPanel.add(Box.createHorizontalStrut(5));
		return paginatorPanel;
	}
	
	protected abstract void executeFilter();
	
	protected void calculaTamanhoPaginas() {
		totalPaginas = totalRegistros / registrosPorPagina;
		if(totalPaginas == 0) {
			totalPaginas = 1;
		}else {
			int rest = totalRegistros % registrosPorPagina;
			if(rest > 0)
				totalPaginas++;
		}
	}
	
	protected void paginatorControl() {
		
		if(totalPaginas == 1) {
			first.setEnabled(false);
			back.setEnabled(false);
			prox.setEnabled(false);
			last.setEnabled(false);
		}else{
			if(paginaAtual == 1) {
				first.setEnabled(false);
				back.setEnabled(false);
				prox.setEnabled(true);
				last.setEnabled(true);
			}else if(paginaAtual == totalPaginas) {
				first.setEnabled(true);
				back.setEnabled(true);
				prox.setEnabled(false);
				last.setEnabled(false);
			}else {
				first.setEnabled(true);
				back.setEnabled(true);
				prox.setEnabled(true);
				last.setEnabled(true);
			}
			
		}
		
	}

}
