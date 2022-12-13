package com.protreino.services.screens;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.ParseException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.EmptyBorder;

import com.protreino.services.constants.Configurations;
import com.protreino.services.enumeration.FieldType;
import com.protreino.services.enumeration.PreferenceGroup;
import com.protreino.services.main.Main;
import com.protreino.services.to.FieldTO;
import com.protreino.services.to.PreferenceTO;
import com.protreino.services.utils.PanelWithLabel;
import com.protreino.services.utils.Utils;
import com.topdata.EasyInner;

@SuppressWarnings("serial")
public class PreferencesDialog extends JDialog {
	
	private Image configImage;
	private PanelWithLabel errosLabel;
	private Map<String, FieldTO> fieldMap;
	
	public PreferencesDialog() {
		super(Main.mainScreen, "Preferências", true);
		
		loadImages();
		
		setIconImage(configImage);
		setResizable(true);
		setLayout(new BorderLayout());
		setMinimumSize(new Dimension(750,550));
		
		JPanel mainPanel = new JPanel(new BorderLayout());
		mainPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		
		JTabbedPane tabbedPane = new JTabbedPane(JTabbedPane.TOP, JTabbedPane.SCROLL_TAB_LAYOUT);
		tabbedPane.setPreferredSize(new Dimension(750, 550));
		mainPanel.add(tabbedPane, BorderLayout.CENTER);
		
		// Cria as abas
		Map<PreferenceGroup, JPanel> groupMap = new HashMap<PreferenceGroup, JPanel>();
		for (PreferenceGroup group : PreferenceGroup.values()) {
			JPanel panel = new JPanel();
			panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
			panel.setBorder(new EmptyBorder(10, 10, 10, 10));
			groupMap.put(group, panel);
			
			JScrollPane scrollPane = new JScrollPane(panel, 
					ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, 
					ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
			scrollPane.getVerticalScrollBar().setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));
			tabbedPane.addTab(group.getName(), scrollPane);
		}
		
		// Cria os campos
		fieldMap = new HashMap<String, FieldTO>();
		List<PreferenceTO> defaultPreferencesList = Utils.getDefaultPreferencesList();
		for (PreferenceTO preference : defaultPreferencesList) {
			FieldType fieldType = preference.getFieldType();
			FieldTO field = null;
			if (FieldType.NUMERIC_LIST.equals(fieldType))
				field = new FieldTO(this, preference.getLabel(), fieldType, Utils.getPreference(preference.getKey()), 
						preference.getNumericListSequency());
			else if(FieldType.COMBOBOX.equals(fieldType)) {
				String[] options = preference.getValue().split(";");
				field = new FieldTO(preference.getLabel(), fieldType, Utils.getPreference(preference.getKey()),
						options);
			
			} else
				field = new FieldTO(this, preference.getLabel(), fieldType, Utils.getPreference(preference.getKey()));
			
			field.setNumeric(preference.getNumeric());
			field.setTextFieldSize(preference.getTextFieldSize());
			if ("messageAllowed".equals(preference.getKey())) {
				field.setMaxCharacteres(16);
				field.setTextFieldSize(12);
			}
			if ("cardMaster".equals(preference.getKey())) {
				field.setMaxCharacteres(16);
				field.setTextFieldSize(12);
			}
			if ("messageAccessAuthorized".equals(preference.getKey())) {
				field.setMaxCharacteres(16);
				field.setTextFieldSize(12);
			}
			if (preference.getKey().contains("PedestrianScreen")) {
				field.setMaxCharacteres(50);
				field.setTextFieldSize(25);
			}
			if ("athleteScreenBackgroundImage".equals(preference.getKey())) {
				field.setImageExtensions(new String[] {"jpg", "jpeg"});
				field.setFullSize(true);
			}
			if ("messageEntryAllowed".equals(preference.getKey())) {
				field.setMaxCharacteres(14);
				field.setTextFieldSize(12);
			}
			if ("messageExitAllowed".equals(preference.getKey())) {
				field.setMaxCharacteres(14);
				field.setTextFieldSize(12);
			}
			
			// ajusta visibilidade de alguns campos
			if ("restrictAccessDays".equals(preference.getKey())) {
				field.setVisible("true".equals(fieldMap.get("restrictAccess").getValue()));
			}
			if ("restrictAccess".equals(preference.getKey())) {
				field.setActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						fieldMap.get("restrictAccessDays").setVisible("true".equals(fieldMap.get("restrictAccess").getValue()));
					}
				});
			}
			if ("tcpServerSocketPort".equals(preference.getKey())) {
				field.setVisible("true".equals(fieldMap.get("enableTCPServer").getValue()));
			}
			if ("enableTCPServer".equals(preference.getKey())) {
				field.setActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						fieldMap.get("tcpServerSocketPort").setVisible("true".equals(fieldMap.get("enableTCPServer").getValue()));
					}
				});
			}
			if ("broadcastServerSocketPort".equals(preference.getKey())) {
				field.setVisible("true".equals(fieldMap.get("enableBroadcastServer").getValue()));
			}
			if ("enableBroadcastServer".equals(preference.getKey())) {
				field.setActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						fieldMap.get("broadcastServerSocketPort").setVisible("true".equals(fieldMap.get("enableBroadcastServer").getValue()));
					}
				});
			}
			if ("releaseAccessReason".equals(preference.getKey())
					|| "serverRecognizerIP".equals(preference.getKey())) {
				field.setRequired(false);
			}
			JPanel panel = groupMap.get(preference.getPreferenceGroup());
			JPanel fieldPanel = field.getPanel();
			fieldPanel.setMaximumSize(new Dimension(10000, (int) fieldPanel.getPreferredSize().getHeight()));
			panel.add(fieldPanel);
			panel.add(Box.createVerticalStrut(5));
			fieldMap.put(preference.getKey(), field);
		}
		
		for (PreferenceGroup group : PreferenceGroup.values()) {
			JPanel panel = groupMap.get(group);
			panel.add(Box.createGlue());
		}
		
		errosLabel = new PanelWithLabel(" ", FlowLayout.LEFT, true, 10, 0);
		errosLabel.setLabelColor(Color.RED);
		
		JButton zerarLastSyncButton = new JButton("Zerar marcador");
		zerarLastSyncButton.setPreferredSize(new Dimension(140, 30));
		JButton syncButton = new JButton("Sincronizar");
		syncButton.setPreferredSize(new Dimension(80, 30));
		JButton logoutButton = new JButton("Logout");
		logoutButton.setPreferredSize(new Dimension(80, 30));
		JButton executarButton = new JButton("Disparar tarefas");
		executarButton.setPreferredSize(new Dimension(120, 30));
		
		JButton resetarButton = new JButton("Valores padrão");
		resetarButton.setPreferredSize(new Dimension(120, 30));
		JButton salvarButton = new JButton("Salvar");
		salvarButton.setPreferredSize(new Dimension(80, 30));
		JButton cancelarButton = new JButton("Cancelar");
		cancelarButton.setPreferredSize(new Dimension(100, 30));
		
		JPanel buttonsPanel = new JPanel();
		buttonsPanel.setLayout(new BoxLayout(buttonsPanel, BoxLayout.X_AXIS));
		buttonsPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		buttonsPanel.add(zerarLastSyncButton);
		buttonsPanel.add(Box.createHorizontalGlue());
		buttonsPanel.add(syncButton);
		buttonsPanel.add(Box.createHorizontalStrut(5));
		buttonsPanel.add(logoutButton);
		buttonsPanel.add(Box.createHorizontalStrut(5));
		buttonsPanel.add(executarButton);
		buttonsPanel.add(Box.createHorizontalStrut(5));
		buttonsPanel.add(resetarButton);
		buttonsPanel.add(Box.createHorizontalStrut(5));
		buttonsPanel.add(salvarButton);
		buttonsPanel.add(Box.createHorizontalStrut(5));
		buttonsPanel.add(cancelarButton);
		
		logoutButton.addActionListener(e -> {
			Main.mainScreen.doLogout(this);
		});
		
		executarButton.addActionListener(e -> {
			Main.tasksOfDay(false);
		});
		

		syncButton.addActionListener(e -> {
			  JFrame jFrame = new JFrame();
		        String dateInitialMessage = JOptionPane.showInputDialog(jFrame, "Primeira data com o formato dd/MM/yyyy HH:mm");
		        String dateFinalgetMessage = JOptionPane.showInputDialog(jFrame, "Segunda data com o formato dd/MM/yyyy HH:mm");
		        try {
					Main.dateSync(dateInitialMessage, dateFinalgetMessage);
				} catch (ParseException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
//Amanh� fazer a configura��o da fun��o e terminar de executar tanto a requisi��o, quanto o procuramento Main 1896
		        JOptionPane.showMessageDialog(jFrame, "Datas : " +dateInitialMessage +"\n" + dateFinalgetMessage);
		});

		
		salvarButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				// Verifica campos de texto obrigatorios
				
				for (String key : fieldMap.keySet()) {
					FieldTO field = fieldMap.get(key);
					if (FieldType.TEXT.equals(field.getType())
							|| FieldType.MESSAGE_LINES.equals(field.getType())){
						String statusField = field.checkField();
						if (!"".equals(statusField)) {
							errosLabel.setText(statusField);
							return;
						}
					}
				}
				
				// Salva as novas preferencias
				for (String key : fieldMap.keySet()) {
					FieldTO field = fieldMap.get(key);
					Utils.setPreference(key, field.getValue());
				}
				
				if(Main.servidor == null)
					Utils.exportPreferences();
				
				JOptionPane.showMessageDialog(Main.mainScreen, "Preferências salvas!", "Sucesso!", JOptionPane.PLAIN_MESSAGE);
				dispose();
			}
		});
		
		resetarButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				for (String key : fieldMap.keySet()) {
					FieldTO field = fieldMap.get(key);
					field.setValue(Utils.getDefaultPreference(key));
				}
			}
		});
		
		cancelarButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				dispose();
			}
		});
		
		zerarLastSyncButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				int dialogResult = JOptionPane.showConfirmDialog(null, "Zerar marcador?", "Confirmação", 
						JOptionPane.YES_NO_OPTION, JOptionPane.PLAIN_MESSAGE);
				if (dialogResult == JOptionPane.YES_OPTION){
					Main.lastSync = 0l;
				}
			}
		});
		
		getContentPane().add(mainPanel, BorderLayout.PAGE_START);
		getContentPane().add(errosLabel, BorderLayout.CENTER);
		getContentPane().add(buttonsPanel, BorderLayout.PAGE_END);
		pack();
		setLocationRelativeTo(null);
		setVisible(true);
	}

	private void loadImages(){
		Toolkit toolkit = Toolkit.getDefaultToolkit();
		configImage = toolkit.getImage(Main.class.getResource(Configurations.IMAGE_FOLDER + Main.customImageFolder + "configuracoes.png"));
	}
	
	public void setErrorMessage(String message){
		errosLabel.setText(message);
	}

}
