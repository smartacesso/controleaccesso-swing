package com.protreino.services.screens;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Enumeration;

import javax.swing.AbstractButton;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.border.EmptyBorder;

import com.protreino.services.main.Main;
import com.protreino.services.utils.Utils;

@SuppressWarnings("serial")
public class ReleaseReasonDialog extends JDialog {

	public final String OK = "OK";
	public final String CANCEL = "CANCEL";
	private String option = CANCEL;
	private String reason;
	private JTextField outroTextField;
	private JPanel outroTextFieldPanel;
	private ButtonGroup buttonGroup;
	private JButton okButton;
	
	public ReleaseReasonDialog(Frame owner, String motivos) {
		super(owner, "Motivo da liberacao", true);
		
		String[] options = motivos.split(",");
		
		Font font = new JLabel().getFont();
		Font boldFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
		
		setMinimumSize(new Dimension(450,100));
		setIconImage(Main.favicon);
		
		JPanel panel = new JPanel();
		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
		panel.setBorder(new EmptyBorder(10, 10, 10, 10));
		
		JLabel label = new JLabel("Selecione o motivo da liberacao");
		label.setAlignmentX(Component.CENTER_ALIGNMENT);
		label.setFont(boldFont);
		
		panel.add(label);
		panel.add(Box.createRigidArea(new Dimension(0,10)));
		
		buttonGroup = new ButtonGroup();
		JPanel radioPanel = new JPanel();
        radioPanel.setLayout(new GridLayout(options.length, 1));
        boolean containsOutro = false;
        boolean primeiraOpcao = true;
		for (String motivo : options) {
			if (motivo.toLowerCase().equals("outro")) {
				containsOutro = true;
				continue;
			}
	        JRadioButton radioButton = new JRadioButton(motivo);
	        buttonGroup.add(radioButton);
        	radioButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					reason = motivo;
					if (outroTextFieldPanel != null)
						outroTextFieldPanel.setVisible(false);
				}
			});
        	if (primeiraOpcao) {
        		radioButton.doClick();
        		primeiraOpcao = false;
        	}
        	radioPanel.add(radioButton);
		}
		if (containsOutro) {
			JRadioButton radioButton = new JRadioButton("Outro");
	        buttonGroup.add(radioButton);
        	radioButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					reason = "Outro: ";
					outroTextFieldPanel.setVisible(radioButton.isSelected());
				}
			});
        	
        	outroTextFieldPanel = new JPanel();
    		outroTextFieldPanel.setLayout(new BoxLayout(outroTextFieldPanel, BoxLayout.X_AXIS));
    		outroTextField = new JTextField();
    		outroTextFieldPanel.setVisible(false);
    		outroTextFieldPanel.add(outroTextField);
        	
        	JPanel outroPanel = new JPanel();
        	outroPanel.setLayout(new BoxLayout(outroPanel, BoxLayout.X_AXIS));
        	outroPanel.add(radioButton);
    		outroPanel.add(Box.createHorizontalStrut(10));
    		outroPanel.add(outroTextFieldPanel);
    		
        	radioPanel.add(outroPanel);
		}
		panel.add(radioPanel);
		panel.add(Box.createRigidArea(new Dimension(0,10)));
		
		JLabel labelErro = new JLabel(" ");
		labelErro.setFont(boldFont);
		labelErro.setForeground(Color.RED);
		JPanel labelErroPanel = new JPanel();
		labelErroPanel.setLayout(new BoxLayout(labelErroPanel, BoxLayout.X_AXIS));
		labelErroPanel.add(labelErro);
		panel.add(labelErroPanel);
		panel.add(Box.createRigidArea(new Dimension(0,10)));
		
		JButton cancelarButton = new JButton("Cancelar");
		cancelarButton.setAlignmentX(Component.CENTER_ALIGNMENT);
		cancelarButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				option = CANCEL;
				setVisible(false);
			}
		});
		
		okButton = new JButton("OK");
		okButton.setAlignmentX(Component.CENTER_ALIGNMENT);
		okButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (Utils.isNullOrEmpty(reason)){
					labelErro.setText("É necessário informar um motivo.");
					return;
				}
				if ("Outro: ".equals(reason)) {
					if (Utils.isNullOrEmpty(outroTextField.getText())){
						labelErro.setText("É necessário informar um motivo.");
						return;
					}
					reason = reason + outroTextField.getText();
				}
				option = OK;
				setVisible(false);
			}
		});
		
		JPanel buttonsPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 5, 0));
		buttonsPanel.add(okButton);
		buttonsPanel.add(cancelarButton);
		panel.add(buttonsPanel);
		
		setContentPane(panel);
		
		pack();
		setLocationRelativeTo(null);
		
		okButton.grabFocus();
	}
	
	public void pressionarTeclaAtalho(int numeroPressionado) {
		if (numeroPressionado > buttonGroup.getButtonCount())
			return;
		int cont = 1;
		AbstractButton button = null;
		Enumeration<AbstractButton> lista = buttonGroup.getElements();
		while (lista.hasMoreElements()) {
			button = lista.nextElement();
			System.out.println(button.getText());
			if (cont == numeroPressionado)
				break;
			else
				cont++;
		}
		System.out.println("Selecionado: " + button.getText());
		button.doClick();
		okButton.doClick();
	}
	
	public String getOption(){
		return option;
	}
	
	public String getReason(){
		return reason;
	}
}
