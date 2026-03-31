package com.protreino.services.screens;

import java.awt.*;
import java.util.List;
import java.util.Objects;

import javax.swing.*;
import javax.swing.border.EmptyBorder;

import com.protreino.services.entity.EmpresaEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.screens.dialogs.AlertMessage;
import com.protreino.services.utils.Utils;

public class TelaAutoAtendimento extends BaseDialog {

    private static final long serialVersionUID = 1L;

    private JPanel contentPanel;
    private JTextField cpfField;

    private PedestrianAccessEntity cadastro;

    enum Etapa {
        CPF,
        NOME,
        EMPRESA,
        OBS
    }

    private Etapa etapaAtual;

    public TelaAutoAtendimento(Frame owner, PedestrianAccessEntity cadastro) {
        super(owner, "Autoatendimento", true);
        setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
        this.cadastro = cadastro;

        setUndecorated(false);
        setBounds(GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds());
        setLayout(new BorderLayout());

        contentPanel = new JPanel(new BorderLayout());
        add(contentPanel, BorderLayout.CENTER);

        
        if(Objects.nonNull(cadastro.getId())) {
        	mostrarTelaNome();
        }else {
        	mostrarTelaCPF();
        }
    }

    // =========================================================
    // CPF
    // =========================================================
    private void mostrarTelaCPF() {
        etapaAtual = Etapa.CPF;

        JPanel panel = new JPanel(new BorderLayout());

        JPanel center = new JPanel(new GridLayout(3, 1, 30, 30));
        center.setBorder(BorderFactory.createEmptyBorder(100, 300, 50, 300));

        JLabel label = new JLabel("Digite seu CPF:");
        label.setFont(new Font("Arial", Font.BOLD, 48));
        label.setHorizontalAlignment(SwingConstants.CENTER);

        cpfField = new JTextField();
        cpfField.setFont(new Font("Arial", Font.PLAIN, 40));
        cpfField.setHorizontalAlignment(JTextField.CENTER);

        JButton btn = new JButton("Continuar");
        btn.setFont(new Font("Arial", Font.BOLD, 36));

		btn.addActionListener(e -> {
			String cpf = cpfField.getText().trim().replaceAll("\\D", "");

			if (cpf.isEmpty()) {
				new AlertMessage(this, "Atenção", "Digite o CPF").mostrar();
				return;
			}

			if (Utils.ativarValidarCpf() && !Utils.isCpfValido(cpf)) {
				new AlertMessage(this, "Atenção", "Digite o CPF correto").mostrar();
				return;
			}

			cadastro.setCpf(cpf);

			PedestrianAccessEntity visitante = buscarVisitantePorCpf(cpf);

			if (visitante != null) {
				cadastro = visitante;
			}
			mostrarTelaNome();
		});

        center.add(label);
        center.add(cpfField);
        center.add(btn);

        TecladoVirtualPanel teclado = new TecladoVirtualPanel(cpfField);

        JPanel tecladoWrapper = new JPanel(new BorderLayout());
        tecladoWrapper.setBorder(new EmptyBorder(20, 300, 50, 300));
        tecladoWrapper.add(teclado, BorderLayout.CENTER);

        panel.add(center, BorderLayout.CENTER);
        panel.add(tecladoWrapper, BorderLayout.SOUTH);
        
        trocarTela(panel);
    }

    // =========================================================
    // NOME
    // =========================================================
    private void mostrarTelaNome() {
        etapaAtual = Etapa.NOME;

        JPanel panel = new JPanel(new BorderLayout());

        JPanel center = new JPanel(new GridLayout(3, 1, 30, 30));
        center.setBorder(BorderFactory.createEmptyBorder(100, 300, 100, 300));

        JLabel label = new JLabel("Digite seu Nome:");
        label.setFont(new Font("Arial", Font.BOLD, 48));
        label.setHorizontalAlignment(SwingConstants.CENTER);

        JTextField nomeField = new JTextField();
        nomeField.setFont(new Font("Arial", Font.PLAIN, 40));
        nomeField.setHorizontalAlignment(JTextField.CENTER);

        // Preenche se já existir
        if (cadastro.getName() != null && !cadastro.getName().isEmpty()) {
            nomeField.setText(cadastro.getName());
            nomeField.setCaretPosition(nomeField.getText().length());
        }

        JButton btn = new JButton("Continuar");
        btn.setFont(new Font("Arial", Font.BOLD, 36));

        btn.addActionListener(e -> {
            String nome = nomeField.getText().trim();

            if (nome.isEmpty()) {
                new AlertMessage(this, "Atenção", "Digite o nome").mostrar();
                return;
            }

            cadastro.setName(nome);
            mostrarTelaEmpresa();
        });

        center.add(label);
        center.add(nomeField);
        center.add(btn);
        
        TecladoAlfabeticoPanel teclado = new TecladoAlfabeticoPanel(nomeField);

        JPanel tecladoWrapper = new JPanel(new BorderLayout());
        tecladoWrapper.setBorder(new EmptyBorder(20, 300, 50, 300));
        tecladoWrapper.add(teclado, BorderLayout.CENTER);


        panel.add(center, BorderLayout.CENTER);
        
        panel.add(tecladoWrapper, BorderLayout.SOUTH);
        
        JPanel bottom = new JPanel(new BorderLayout());

        bottom.add(tecladoWrapper, BorderLayout.CENTER);
        bottom.add(criarFooterVoltar(), BorderLayout.SOUTH);

        panel.add(bottom, BorderLayout.SOUTH);

        trocarTela(panel);
    }

    // =========================================================
    // EMPRESA
    // =========================================================
    
    private void mostrarTelaEmpresa() {
        etapaAtual = Etapa.EMPRESA;

        @SuppressWarnings("unchecked")
        List<EmpresaEntity> empresas = (List<EmpresaEntity>) HibernateAccessDataFacade
                .getResultList(EmpresaEntity.class, "EmpresaEntity.findAllActive");

        if (empresas == null || empresas.isEmpty()) {
            new AlertMessage(this, "Aviso", "Nenhuma empresa encontrada").mostrar();
            mostrarTelaCPF();
            return;
        }

        if (Utils.isAutoAtendimentoHabilitado()) {
            mostrarTelaEmpresaTouch(empresas);
        } else {
            mostrarTelaEmpresaDesktop(empresas);
        }
    }
    
    
    private void mostrarTelaEmpresaTouch(List<EmpresaEntity> empresas) {
        JPanel main = new JPanel(new BorderLayout());

        JLabel titulo = new JLabel("Selecione a Empresa");
        titulo.setFont(new Font("Arial", Font.BOLD, 36));
        titulo.setHorizontalAlignment(SwingConstants.CENTER);
        titulo.setBorder(new EmptyBorder(30, 0, 30, 0));

        main.add(titulo, BorderLayout.NORTH);

        JPanel grid = new JPanel(new GridBagLayout());
        grid.setBackground(Color.WHITE);

        JScrollPane scroll = new JScrollPane(grid);
        scroll.setBorder(null);

        main.add(scroll, BorderLayout.CENTER);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.insets = new Insets(20, 20, 20, 20);

        for (int i = 0; i < empresas.size(); i++) {
            EmpresaEntity empresa = empresas.get(i);

            JButton botao = new JButton("<html><center>" + empresa.getNome() + "</center></html>");
            botao.setFont(new Font("Arial", Font.PLAIN, 22));
            botao.setPreferredSize(new Dimension(300, 150));
            botao.setBackground(new Color(230, 230, 250));

            botao.addActionListener(e -> finalizarFluxo(empresa));

            gbc.gridx = i % 3;
            gbc.gridy = i / 3;

            grid.add(botao, gbc);
        }

        main.add(criarFooterVoltar(), BorderLayout.SOUTH);

        trocarTela(main);
    }
    
    private void mostrarTelaEmpresaDesktop(List<EmpresaEntity> empresas) {
        JPanel main = new JPanel(new BorderLayout());

        JLabel titulo = new JLabel("Selecione a Empresa");
        titulo.setFont(new Font("Arial", Font.BOLD, 28));
        titulo.setHorizontalAlignment(SwingConstants.CENTER);
        titulo.setBorder(new EmptyBorder(30, 0, 30, 0));

        main.add(titulo, BorderLayout.NORTH);

        JPanel center = new JPanel(new GridLayout(3, 1, 20, 20));
        center.setBorder(new EmptyBorder(100, 300, 100, 300));

        JComboBox<EmpresaEntity> combo = new JComboBox<>();

        for (EmpresaEntity emp : empresas) {
            combo.addItem(emp);
        }

        combo.setFont(new Font("Arial", Font.PLAIN, 40));

        JButton confirmar = new JButton("Confirmar");
        confirmar.setFont(new Font("Arial", Font.BOLD, 22));

        confirmar.addActionListener(e -> {
            EmpresaEntity selecionada = (EmpresaEntity) combo.getSelectedItem();
            if (selecionada != null) {
                mostrarTelaObservacao(selecionada);
            } else {
                new AlertMessage(this, "Atenção", "Selecione uma empresa").mostrar();
            }
        });

        center.add(new JLabel()); // espaçamento
        center.add(combo);
        center.add(confirmar);

        main.add(center, BorderLayout.CENTER);
        main.add(criarFooterVoltar(), BorderLayout.SOUTH);

        trocarTela(main);
    }
    
    // =========================================================
    // OBSERVACAO
    // =========================================================
    private void mostrarTelaObservacao(EmpresaEntity empresaSelecionada) {
        etapaAtual = Etapa.OBS;

        JPanel panel = new JPanel(new BorderLayout());

        JPanel center = new JPanel(new GridLayout(3, 1, 30, 30));
        center.setBorder(BorderFactory.createEmptyBorder(100, 300, 100, 300));

        JLabel label = new JLabel("Digite observação:");
        label.setFont(new Font("Arial", Font.BOLD, 48));
        label.setHorizontalAlignment(SwingConstants.CENTER);

        JTextField obsField = new JTextField();
        obsField.setFont(new Font("Arial", Font.PLAIN, 40));
        obsField.setHorizontalAlignment(JTextField.CENTER);

        // Preenche se já existir
        if (cadastro.getObservacoes() != null && !cadastro.getObservacoes().isEmpty()) {
            obsField.setText(cadastro.getObservacoes());
            obsField.setCaretPosition(obsField.getText().length());
        }

        JButton btn = new JButton("Continuar");
        btn.setFont(new Font("Arial", Font.BOLD, 36));

        btn.addActionListener(e -> {
            String obs = obsField.getText().trim();

            // Observação opcional
            cadastro.setObservacoes(obs);

            finalizarFluxo(empresaSelecionada);
        });

        center.add(label);
        center.add(obsField);
        center.add(btn);

        panel.add(center, BorderLayout.CENTER);
        panel.add(criarFooterVoltar(), BorderLayout.SOUTH);

        trocarTela(panel);
    }
    
    // =========================================================
    // FINAL
    // =========================================================
    private void finalizarFluxo(EmpresaEntity empresa) {

        if (empresa == null) {
            new AlertMessage(this, "Atenção", "Selecione uma empresa").mostrar();
            return;
        }

        if (Objects.isNull(empresa.getAutoAtendimentoLiberado())
                || empresa.autoAtendimentoLiberado()) {

            RegisterVisitorDialog cadastroVisitante =
                    new RegisterVisitorDialog(cadastro, true);

            cadastroVisitante.mostrarMiniPerfilVisitante(
                    (Frame) SwingUtilities.getWindowAncestor(this),
                    cadastro,
                    null
            );
        }

        // FINALIZA TUDO
        this.dispose();
    }

    // =========================================================
    // UTILS
    // =========================================================
    private void trocarTela(JPanel novaTela) {
        contentPanel.removeAll();
        contentPanel.add(novaTela, BorderLayout.CENTER);
        contentPanel.revalidate();
        contentPanel.repaint();
    }

    private JPanel criarFooterVoltar() {
        JButton voltar = new JButton("Voltar");
        voltar.setFont(new Font("Arial", Font.PLAIN, 20));

        voltar.addActionListener(e -> {
            switch (etapaAtual) {
                case CPF:
                    // primeira tela → pode fechar
                    dispose();
                    break;

                case NOME:
                    mostrarTelaCPF();
                    break;

                case EMPRESA:
                    mostrarTelaNome();
                    break;

                case OBS:
                    mostrarTelaEmpresa();
                    break;
            }
        });

        JPanel footer = new JPanel();
        footer.add(voltar);

        return footer;
    }
    
    private PedestrianAccessEntity buscarVisitantePorCpf(String cpf) {
        return (PedestrianAccessEntity)
                HibernateAccessDataFacade.getSingleResultByCPF(
                        PedestrianAccessEntity.class, cpf);
    }

}