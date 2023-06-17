package com.protreino.services.screens;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.InputMap;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.UIManager;
import javax.swing.border.EmptyBorder;

import com.protreino.services.devices.Device;
import com.protreino.services.main.Main;

@SuppressWarnings("serial")
public class ReleaseAccessDialog extends JDialog {

	public final String OK = "OK";
	public final String CANCEL = "CANCEL";
	
	private Device selectedDevice;
	private String option = CANCEL;
	
	public ReleaseAccessDialog(Frame owner, List<Device> dispositivoConectados){
		super(owner, "Liberar acesso", true);
		
		Font font = new JLabel().getFont();
		Font boldFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
		
		setMinimumSize(new Dimension(350,100));
		setIconImage(Main.favicon);
		
		JPanel panel = new JPanel();
		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
		panel.setBorder(new EmptyBorder(10, 10, 10, 10));
		
		JLabel label = new JLabel("Selecione o dispositivo para liberar o acesso");
		label.setAlignmentX(Component.CENTER_ALIGNMENT);
		label.setFont(boldFont);
		
		panel.add(label);
		panel.add(Box.createRigidArea(new Dimension(0,10)));
		
		Dimension buttonSize = new Dimension(220, 40);
		
		int contador = 1;
		for (Device device : dispositivoConectados) {
			
			String key = String.valueOf(contador);
	        Action action = new AbstractAction() {  
	            public void actionPerformed(ActionEvent e) {     
	                 selectedDevice = device;
	                 option = OK;
	                 setVisible(false);
	            }
	        };
	        
	        JButton button = new JButton(device.getName() + "  (" + key + ")");
			button.setAlignmentX(Component.CENTER_ALIGNMENT);
			button.setBorder(BorderFactory.createEtchedBorder());
			button.setContentAreaFilled(false);
			button.setMinimumSize(buttonSize);
			button.setMaximumSize(buttonSize);
			button.setPreferredSize(buttonSize);
			button.addActionListener(action);
			button.addMouseListener(new java.awt.event.MouseAdapter() {
			    public void mouseEntered(java.awt.event.MouseEvent evt) {
			    	button.setForeground(Main.secondColor);
			    	button.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
			    }
			    public void mouseExited(java.awt.event.MouseEvent evt) {
			    	button.setForeground(UIManager.getColor("control"));
			    	button.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
			    }
			});
			
			panel.add(button);
			panel.add(Box.createRigidArea(new Dimension(0,10)));
			
			InputMap inputMap = panel.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
            inputMap.put(KeyStroke.getKeyStroke(key), key);
            inputMap.put(KeyStroke.getKeyStroke("NUMPAD" + key), key);
            panel.getActionMap().put(key, action);
			
			contador++;
		}
		
		setContentPane(panel);
		
		pack();
		setLocationRelativeTo(null);
	
	}
	
	public String getOption(){
		return option;
	}
	
	public Device getSelectedDevice(){
		return selectedDevice;
	}
	
}
