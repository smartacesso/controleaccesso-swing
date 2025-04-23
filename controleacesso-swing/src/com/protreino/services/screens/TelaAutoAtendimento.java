package com.protreino.services.screens;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GraphicsEnvironment;
import java.awt.GridLayout;
import java.util.Objects;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.border.EmptyBorder;

import com.protreino.services.entity.EmpresaEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.screens.dialogs.AlertMessage;
import com.protreino.services.screens.dialogs.SimpleMessageDialog;

public class TelaAutoAtendimento extends BaseDialog {
	/**
	 * 
	 */
	private static final long serialVersionUID = 3346594914933582211L;
	private JTextField cpfField;
	private JButton buscarButton;
	public RegisterVisitorDialog cadastroVisitante;

	public TelaAutoAtendimento(Frame owner) {
		super(owner, "Autoatendimento", true);
		initComponents();
	}

	private void initComponents() {
		// Remove bordas e coloca fullscreen
		setUndecorated(true);
		setBounds(GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds());

		setLayout(new BorderLayout());

		JLabel cpfLabel = new JLabel("Digite seu CPF:");
		cpfLabel.setFont(new Font("Arial", Font.BOLD, 48));
		cpfLabel.setHorizontalAlignment(SwingConstants.CENTER);

		// CPF Field precisa ser criado antes do teclado
		cpfField = new JTextField();
		cpfField.setFont(new Font("Arial", Font.PLAIN, 40));
		cpfField.setHorizontalAlignment(JTextField.CENTER);

		// Agora sim pode criar o teclado
		TecladoVirtualPanel teclado = new TecladoVirtualPanel(cpfField);
		
		// Envolve o teclado em um painel com margem inferior
		JPanel tecladoWrapper = new JPanel(new BorderLayout());
		tecladoWrapper.setBorder(new EmptyBorder(50, 300, 50, 300)); // top, left, bottom, right
		tecladoWrapper.add(teclado, BorderLayout.CENTER);

		add(tecladoWrapper, BorderLayout.SOUTH);


		buscarButton = new JButton("Buscar");
		buscarButton.setFont(new Font("Arial", Font.BOLD, 36));
		buscarButton.setPreferredSize(new Dimension(300, 80));
		buscarButton.addActionListener(e -> buscarCadastro());

		JPanel centerPanel = new JPanel(new GridLayout(3, 1, 30, 30));
		centerPanel.setBorder(BorderFactory.createEmptyBorder(100, 300, 100, 300));
		centerPanel.add(cpfLabel);
		centerPanel.add(cpfField);
		centerPanel.add(buscarButton);

		add(centerPanel, BorderLayout.CENTER);
	}

	private void buscarCadastro() {
		String cpf = cpfField.getText().trim();

		if (cpf.isEmpty()) {
			new AlertMessage(this, "Atenção", "Por favor, digite o CPF.").mostrar();
			return;
		}

		// Aqui você chama seu serviço ou DAO que busca o visitante/pedestre pelo CPF
		PedestrianAccessEntity visitante = buscarVisitantePorCpf(cpf);

		if (visitante != null) {
			cadastroVisitante = new RegisterVisitorDialog(visitante);
			EmpresaEntity empresa = cadastroVisitante.mostrarDialogoEscolherEmpresaTouch( (Frame) SwingUtilities.getWindowAncestor(this));
			
			if(empresa == null) {
				return;
			}
			
			if(empresa.autoAtendimentoLiberado()) {
			    cadastroVisitante.mostrarMiniPerfilVisitante(
			            (Frame) SwingUtilities.getWindowAncestor(this), // ou (Frame) this se for JFrame
			            visitante,
			            null
			        );
			}else {
				new AlertMessage(this, "Auto atendimento bloqueado", "Sem auto atendimento. Procure um atendente.").mostrar();
			}
		    cpfField.setText("");
		} else {
			new AlertMessage(this, "Não encontrado", "Não encontrado. Procure a recpção.").mostrar();
		}
	}

	// Simulação de busca (substitua pela lógica real)
	private PedestrianAccessEntity buscarVisitantePorCpf(String cpf) {
		// TODO: implementar busca no banco ou serviço
		PedestrianAccessEntity existente = (PedestrianAccessEntity) HibernateAccessDataFacade
				.getSingleResultByCPF(PedestrianAccessEntity.class, cpf);
		if (Objects.nonNull(existente)) {
			return existente;
		} else {
			return null;
		}
	}
}
