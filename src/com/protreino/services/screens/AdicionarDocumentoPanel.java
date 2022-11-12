package com.protreino.services.screens;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FileDialog;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;

import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFormattedTextField;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableModel;
import javax.swing.text.MaskFormatter;

import com.protreino.services.entity.DocumentoEntity;
import com.protreino.services.main.Main;
import com.protreino.services.utils.Utils;

@SuppressWarnings("serial")
public class AdicionarDocumentoPanel extends JPanel {

	private JTable documentosListTable;
	private String[] columns = {"Id", "Nome", "Vencimento"};
	
	private Font headerFont;

	private JTextField nomeArquivoTextField;
	private JFormattedTextField validadeDocumentoTextField;
	
	private JLabel mensagemErrorCameraLabel;
	
	private byte[] arquivoDocumento;
	
	private DefaultTableModel dataModel;
	
	public JLabel sampleLabel;
	private BufferedImage bufferedImage;
	
	private List<DocumentoEntity> documentos;

	public AdicionarDocumentoPanel() {
		if(this.documentos == null)
			documentos = new ArrayList<>();
		
		JPanel documentosListTablePanel = new JPanel();
		documentosListTablePanel.setLayout(new BoxLayout(documentosListTablePanel, BoxLayout.Y_AXIS));
		documentosListTable = getDocumentosListTable();
		Utils.escondeColunaFromTable(documentosListTable, 0);
		
		dataModel = new DefaultTableModel(columns, 0);
		
		Font font = new JLabel().getFont();
		headerFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
		
		JPanel escolherTirarFotoPanel = new JPanel();
		escolherTirarFotoPanel.setLayout(new BoxLayout(escolherTirarFotoPanel, BoxLayout.Y_AXIS));
		JLabel escolherTirarFotoLabel = new JLabel("Tirar foto com a câmera");
		escolherTirarFotoLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		escolherTirarFotoPanel.add(escolherTirarFotoLabel);
		escolherTirarFotoPanel.add(Box.createVerticalStrut(2));
		
		JButton escolhertirarFotoButton = new JButton("Tirar foto");
		escolhertirarFotoButton.setBorder(new EmptyBorder(5, 10, 5, 10));
		escolhertirarFotoButton.setAlignmentX(Component.LEFT_ALIGNMENT);
		escolhertirarFotoButton.addActionListener(e -> {
			criarDialogoTirarFoto();
		});
		
		escolherTirarFotoPanel.add(escolhertirarFotoButton);
		escolherTirarFotoPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		
		mensagemErrorCameraLabel = new JLabel();
		mensagemErrorCameraLabel.setForeground(Color.RED);
		escolherTirarFotoPanel.add(mensagemErrorCameraLabel);
		
		JPanel escolherDoArquivoPanel = new JPanel();
		escolherDoArquivoPanel.setLayout(new BoxLayout(escolherDoArquivoPanel, BoxLayout.Y_AXIS));
		JLabel escolherDoArquivoLabel = new JLabel("Escolher do arquivo");
		escolherDoArquivoLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		escolherDoArquivoPanel.add(escolherDoArquivoLabel);
		escolherDoArquivoPanel.add(Box.createVerticalStrut(2));
		
		JButton escolherArquivoButton = new JButton("Abrir arquivo");
		escolherArquivoButton.setBorder(new EmptyBorder(5, 10, 5, 10));
		escolherArquivoButton.setAlignmentX(Component.LEFT_ALIGNMENT);
		escolherArquivoButton.addActionListener(e -> {
			criarFileDialogEscolherDocumento();
		});
		
		escolherDoArquivoPanel.add(escolherArquivoButton);
		escolherDoArquivoPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		
		JPanel nomeArquivoPanel = new JPanel();
		nomeArquivoPanel.setLayout(new BoxLayout(nomeArquivoPanel, BoxLayout.Y_AXIS));
		JLabel nomeLabel = new JLabel("Nome");
		nomeLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		nomeArquivoPanel.add(nomeLabel);
		nomeArquivoPanel.add(Box.createVerticalStrut(2));
		
		nomeArquivoTextField = new JTextField();
		nomeArquivoTextField.setAlignmentX(Component.LEFT_ALIGNMENT);
		nomeArquivoTextField.setPreferredSize(new Dimension(150, 25));
		nomeArquivoPanel.add(nomeArquivoTextField);
		nomeArquivoPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		
		JPanel validadeDocumentoPanel = new JPanel();
		validadeDocumentoPanel.setLayout(new BoxLayout(validadeDocumentoPanel, BoxLayout.Y_AXIS));
		JLabel validadeDocumentoLabel = new JLabel("Validade");
		validadeDocumentoLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
		validadeDocumentoPanel.add(validadeDocumentoLabel);
		validadeDocumentoPanel.add(Box.createVerticalStrut(2));
		
		validadeDocumentoTextField = Utils.getNewJFormattedTextField(10);
		MaskFormatter mask = Utils.getNewMaskFormatter("##/##/####");
		mask.install(validadeDocumentoTextField);
		validadeDocumentoPanel.add(validadeDocumentoTextField);
		validadeDocumentoPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		
		JButton addDocumentButton = getAddDocumentoButton();
		JButton removeDocumentButton = getRemoveDocumentoButton();
		
		JPanel addDocumentosPanel = new JPanel();
		addDocumentosPanel.setLayout(new BoxLayout(addDocumentosPanel, BoxLayout.Y_AXIS));
		addDocumentosPanel.add(Box.createVerticalStrut(25));
		addDocumentosPanel.add(addDocumentButton);
		
		JPanel removeDocumentosPanel = new JPanel();
		removeDocumentosPanel.setLayout(new BoxLayout(removeDocumentosPanel, BoxLayout.Y_AXIS));
		removeDocumentosPanel.add(Box.createVerticalStrut(25));
		removeDocumentosPanel.add(removeDocumentButton);
		
		JPanel headerPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
		headerPanel.setMaximumSize(new Dimension(10000, 60));
		headerPanel.add(escolherTirarFotoPanel);
		headerPanel.add(Box.createHorizontalStrut(10));
		headerPanel.add(escolherDoArquivoPanel);
		headerPanel.add(Box.createHorizontalStrut(10));
		headerPanel.add(nomeArquivoPanel);
		headerPanel.add(Box.createHorizontalStrut(10));
		headerPanel.add(validadeDocumentoPanel);
		headerPanel.add(Box.createHorizontalStrut(10));
		headerPanel.add(addDocumentosPanel);
		headerPanel.add(removeDocumentosPanel);
		
		JScrollPane scrollPane = new JScrollPane(documentosListTable);
		scrollPane.getVerticalScrollBar().setUnitIncrement(Integer.valueOf(Utils.getPreference("scrollSpeed")));
		documentosListTablePanel.add(scrollPane);
		
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		setBorder(BorderFactory.createEmptyBorder(10,10,10,10));
		add(headerPanel);
		add(Box.createRigidArea(new Dimension(0,5)));
		add(documentosListTablePanel);
	}
	
	private JButton getRemoveDocumentoButton() {
		JButton removeDocumentButton = new JButton("Remover");
		removeDocumentButton.setBorder(new EmptyBorder(5, 10, 5, 10));
		removeDocumentButton.setPreferredSize(new Dimension(100, 40));
		
		removeDocumentButton.addActionListener(e -> {
			if(documentosListTable.getSelectedRow() < 0)
				return;
			
			Long idParaExcluir = (Long) dataModel.getValueAt(documentosListTable.getSelectedRow(), 0);
			
			for(int i = 0; i < documentos.size(); i++) {
				if(!idParaExcluir.equals(documentos.get(i).getId()))
					continue;

				if(documentos.get(i).getCadastradoNoDesktop())
					documentos.remove(i);
				else
					documentos.get(i).setRemovidoNoDesktop(true);
			}
			
			dataModel.removeRow(documentosListTable.getSelectedRow());
			documentosListTable.setModel(dataModel);
			
			Utils.escondeColunaFromTable(documentosListTable, 0);
		});
		
		return removeDocumentButton;
	}
	
	private void populateTable() {
		Object[] item = new Object[3];
		Long idTemp = Utils.getRandomNumber();
		
		item[0] = idTemp;
		item[1] = nomeArquivoTextField.getText();
		item[2] = validadeDocumentoTextField.getText();
		
		DocumentoEntity documento = new DocumentoEntity();
		documento.setId(idTemp);
		documento.setNome(nomeArquivoTextField.getText());
		documento.setArquivo(arquivoDocumento);
		documento.setCadastradoNoDesktop(true);
		
		try {
			documento.setValidade(new SimpleDateFormat("dd/MM/yyyy").parse(String.valueOf(validadeDocumentoTextField.getText())));
		} catch (Exception e) {}

		documentos.add(documento);
		
		dataModel.addRow(item);
		documentosListTable.setModel(dataModel);
		
		Utils.escondeColunaFromTable(documentosListTable, 0);
	}
	
	private JButton getAddDocumentoButton() {
		JButton addDocumentButton = new JButton("Adicionar");
		addDocumentButton.setBorder(new EmptyBorder(5, 10, 5, 10));
		addDocumentButton.setPreferredSize(new Dimension(100, 40));
		
		addDocumentButton.addActionListener(e -> {
			
			populateTable();
			
			nomeArquivoTextField.setText("");
			validadeDocumentoTextField.setText("");
			arquivoDocumento = null;
		});
		
		return addDocumentButton;
	}
	
	private void criarFileDialogEscolherDocumento() {
		FileDialog fileDialog = new FileDialog(new JFrame(), "Escolha um arquivo", FileDialog.LOAD);
		fileDialog.setDirectory("C:\\");
		fileDialog.setFile("*.jpg;*.jpeg;*.png;*.pdf");
		fileDialog.setVisible(true);

		String caminho = fileDialog.getDirectory() + fileDialog.getFile();

		if (caminho == null || caminho.isEmpty())
			return;
		
		if(caminho.endsWith(".png") || caminho.endsWith(".jpg") || caminho.endsWith(".jpeg") || caminho.endsWith(".pdf")) {
			String[] partesCaminho = caminho.split("\\\\");
			
			nomeArquivoTextField.setText(partesCaminho[partesCaminho.length - 1]);
			arquivoDocumento = getBytesFromFile(caminho);
		}
		
	}
	
	private void criarDialogoTirarFoto() {
		WebCamCaptureViewer webCamCaptureViewer = new WebCamCaptureViewer();

		webCamCaptureViewer.getTirarFotoButton().addActionListener(e -> {
			BufferedImage imageCaptured = webCamCaptureViewer.getWebcam().getImage();
			
			if(imageCaptured != null) {
				setBufferedImage(imageCaptured);
				salvarImagemCapturada();
				
				webCamCaptureViewer.dispose();
			}
		});
		
		webCamCaptureViewer.start();
	}

	public void salvarImagemCapturada() {
		try {
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			ImageIO.write(bufferedImage, "jpg", baos);
			baos.flush();
			arquivoDocumento = baos.toByteArray();
			nomeArquivoTextField.setText(Utils.getRandomName() + ".png");
			baos.close();

		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	private byte[] getBytesFromFile(String filePath) {
		try {
			return Files.readAllBytes(Paths.get(filePath));
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		return null;
	}
	
	private JTable getDocumentosListTable() {
		JTable equipamentosListTable = new JTable(new DefaultTableModel(columns, 0));
		equipamentosListTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
		equipamentosListTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		equipamentosListTable.getTableHeader().setReorderingAllowed(false);
		equipamentosListTable.getTableHeader().setOpaque(false);
		equipamentosListTable.getTableHeader().setForeground(Main.firstColor);
		if(!System.getProperty("os.name").toLowerCase().contains("linux"))
			equipamentosListTable.getTableHeader().setBackground(Main.secondColor);
		equipamentosListTable.getTableHeader().setFont(headerFont);
		equipamentosListTable.setRowHeight(30);
		equipamentosListTable.setSelectionBackground(Main.firstColor);
		equipamentosListTable.setSelectionForeground(Color.WHITE);
		
		return equipamentosListTable;
	}

	public List<DocumentoEntity> getDocumentos() {
		return documentos;
	}

	public void setDocumentos(List<DocumentoEntity> documentos) {
		this.documentos = documentos;
		
		if(this.documentos != null && !this.documentos.isEmpty()) {
			this.documentos.forEach(d -> {
				Object[] item = new Object[3];
				item[0] = d.getId();
				item[1] = d.getNome();
				try {
					item[2] = new SimpleDateFormat("dd/MM/yyyy").format(d.getValidade());
				} catch (Exception e) {
					item[2] = "";
				}
				
				dataModel.addRow(item);
			});
			documentosListTable.setModel(dataModel);
			
			Utils.escondeColunaFromTable(documentosListTable, 0);
		}
	}

	public BufferedImage getBufferedImage() {
		return bufferedImage;
	}

	public void setBufferedImage(BufferedImage bufferedImage) {
		this.bufferedImage = bufferedImage;
	}
}
