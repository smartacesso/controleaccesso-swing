package com.protreino.services.screens;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import com.protreino.services.entity.CartaoComandaEntity;
import com.protreino.services.enumeration.StatusCard;
import com.protreino.services.main.Main;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.utils.Utils;

@SuppressWarnings("serial")
public class CartaoComandaDialog extends JDialog {
	
	private JLabel numeroLabel;
	private JLabel  numeroAlternativoLabel;
	
	private JTextField numeroTextField;
	private JTextField numeroAlternativoTextField;
	private JLabel messageLabel;
	private JButton salvarButton;
	private JButton removerButton;
	private JButton voltarButton;
	
	public static boolean abertoPeloAtalho = false;
	
	public CartaoComandaDialog(CartaoComandaEntity cartaoComanda){
		super(Main.mainScreen, cartaoComanda.getId() == null ? "Adicionar cart„o/comanda" : "Alterar cart„o/comanda", true);
		
		setIconImage(Main.favicon);
		setResizable(false);
		setLayout(new BorderLayout());
		setMinimumSize(new Dimension(300, 80));
		
		addWindowListener(new WindowAdapter() {
		    @Override
		    public void windowClosing(WindowEvent e) {
		    	abertoPeloAtalho = false;
		    	dispose();
		    }
		});
		
		numeroLabel = new JLabel("N˙mero");
		numeroTextField = new JTextField();
		numeroTextField.setColumns(20);
		numeroTextField.setMaximumSize(numeroTextField.getPreferredSize());
		numeroTextField.setHorizontalAlignment(JTextField.LEFT);
		
		numeroAlternativoLabel = new JLabel("R√≥tulo");
		numeroAlternativoTextField = new JTextField();
		numeroAlternativoTextField.setColumns(20);
		numeroAlternativoTextField.setMaximumSize(numeroAlternativoTextField.getPreferredSize());
		numeroAlternativoTextField.setHorizontalAlignment(JTextField.LEFT);
		
		if(cartaoComanda.getId() != null) {
			numeroTextField.setText(cartaoComanda.getNumeroReal());
			numeroAlternativoTextField.setText(cartaoComanda.getNumeroAlternativo());
		}
		
		removerButton = new JButton("Remover");
		removerButton.setAlignmentX(Component.LEFT_ALIGNMENT);
		removerButton.addActionListener(e -> {
			
			int dialogResult = JOptionPane.showConfirmDialog(null, "Tem certeza que deseja remover esse cart„o/comanda?", "Confirma√ß√£o", 
					JOptionPane.YES_NO_OPTION, JOptionPane.PLAIN_MESSAGE);
			if (dialogResult == JOptionPane.YES_OPTION) {
				
				CartaoComandaEntity cartao = (CartaoComandaEntity) 
						HibernateAccessDataFacade.getSingleResultById(CartaoComandaEntity.class, Long.valueOf(cartaoComanda.getId()));
				cartao.setRemovido(true);
				cartao.setDataAlteracao(new Date());
				
				HibernateAccessDataFacade.update(CartaoComandaEntity.class, cartao);
				
				abertoPeloAtalho = false;
				this.dispose();
				
				Main.mainScreen.refreshAll();
			}
			
			
		});
		
		voltarButton = new JButton("Cancelar");
		voltarButton.setAlignmentX(Component.LEFT_ALIGNMENT);
		voltarButton.addActionListener(e -> {
			abertoPeloAtalho = false;
			this.dispose();
		});
		
		salvarButton = new JButton("Salvar");
		salvarButton.setAlignmentX(Component.LEFT_ALIGNMENT);
		salvarButton.addActionListener(e -> {
			
			boolean valido = true;
	    	if ("".equals(numeroTextField.getText().trim())) {
				redAndBoldFont(numeroLabel);
				valido = false;
			}
	    	if ("".equals(numeroAlternativoTextField.getText().trim())) {
				redAndBoldFont(numeroAlternativoLabel);
				valido = false;
			}
	    	if (verificaNumeroDuplicado(cartaoComanda, numeroTextField.getText().trim(), "numeroReal")) {
	    		redAndBoldFont(numeroLabel);
				valido = false;
	    	}
	    	if (verificaNumeroDuplicado(cartaoComanda, numeroAlternativoTextField.getText().trim(), "numeroAlternativo")) {
	    		redAndBoldFont(numeroAlternativoLabel);
				valido = false;
	    	}
	    	
	    	if(valido) {
	    		boolean usaCadastroLote = false;
	    		if(cartaoComanda.getId() == null)
	    			usaCadastroLote = true;
		    	cartaoComanda.setNumeroReal(numeroTextField.getText());
		    	cartaoComanda.setNumeroAlternativo(numeroAlternativoTextField.getText());
		    	cartaoComanda.setStatus(StatusCard.AGUARDANDO);
		    	cartaoComanda.setDataAlteracao(new Date());
		    	if(cartaoComanda.getId() == null) {
		    		cartaoComanda.setDataCriacao(new Date());
		    		HibernateAccessDataFacade.save(CartaoComandaEntity.class, cartaoComanda);
		    	} else {
		    		HibernateAccessDataFacade.update(CartaoComandaEntity.class, cartaoComanda);
		    	}
		    	
		    	limparTodosOsCampos();
				this.dispose();
				
				if(usaCadastroLote && Utils.getPreferenceAsBoolean("pedestrianAlwaysOpen")) {
					Main.mainScreen.refreshAll();
					new Thread() {
						public void run() {
							Utils.sleep(500);
							Main.mainScreen.abreCadastroCartoComanda(null);
						}
					}.run();
				}else {
					Main.mainScreen.refreshAll();
				}
			
	    	}
		});
		
		JPanel buttonsPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 5, 0));
		buttonsPanel.add(voltarButton);
		if(cartaoComanda.getId() != null)
			buttonsPanel.add(removerButton);
		buttonsPanel.add(salvarButton);
		
		
		messageLabel = new JLabel(" ");
		salvarButton.setAlignmentX(Component.RIGHT_ALIGNMENT);
		Font font = salvarButton.getFont();
		Font boldFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
		salvarButton.setFont(boldFont);
		
		JPanel mainPanel = new JPanel();
		mainPanel.add(numeroLabel);
		mainPanel.add(numeroTextField);
		mainPanel.add(Box.createRigidArea(new Dimension(10,10)));
		mainPanel.add(numeroAlternativoLabel);
		mainPanel.add(numeroAlternativoTextField);
		mainPanel.add(Box.createRigidArea(new Dimension(10,10)));
		mainPanel.add(buttonsPanel);
		mainPanel.add(Box.createRigidArea(new Dimension(10,20)));
		mainPanel.add(messageLabel);
		
		getContentPane().add(mainPanel, BorderLayout.CENTER);
		pack();
		setLocationRelativeTo(null);
		
	}
	
	

	@SuppressWarnings("unchecked")
	private boolean verificaNumeroDuplicado(CartaoComandaEntity cartao, String numero, String campo) {
		
		HashMap<String, Object> args = new HashMap<>();
		args.put(campo, numero);
		args.put("removido", false);
		List<CartaoComandaEntity> list = (List<CartaoComandaEntity>) HibernateAccessDataFacade
				.getResultListWithDynamicParams(CartaoComandaEntity.class, null, args);
		
		if(list != null && !list.isEmpty()) {
			if(cartao.getId() == null
					|| !cartao.getId().equals(list.get(0).getId()))
				return true;
		}
		
		//n„o tem c√≥digo
		return false;
	}


	protected void limparTodosOsCampos() {
		numeroTextField.setText("");
		numeroAlternativoTextField.setText("");
		abertoPeloAtalho = false;
	}

	protected void redAndBoldFont(JLabel label) {
		label.setForeground(Color.red);
		Font f = label.getFont();
		label.setFont(f.deriveFont(f.getStyle() | Font.BOLD));
	}
	
	protected void blackAndUnboldFont(JLabel label) {
		label.setForeground(Color.BLACK);
		Font f = label.getFont();
		label.setFont(f.deriveFont(f.getStyle() & ~Font.BOLD));
	}
	
	
}
