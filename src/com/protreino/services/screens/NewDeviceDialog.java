package com.protreino.services.screens;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.net.InetAddress;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JTextField;
import javax.swing.SwingWorker;
import javax.swing.border.EmptyBorder;

import com.protreino.services.constants.Configurations;
import com.protreino.services.devices.ComputerIdDevice;
import com.protreino.services.devices.ControlIdDevice;
import com.protreino.services.devices.Device;
import com.protreino.services.devices.LcDevice;
import com.protreino.services.devices.NitgenDevice;
import com.protreino.services.enumeration.FieldType;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.main.Main;
import com.protreino.services.to.FieldTO;
import com.protreino.services.utils.PanelWithLabel;
import com.protreino.services.utils.ScanDevices;
import com.protreino.services.utils.SelectItem;
import com.protreino.services.utils.Utils;

@SuppressWarnings("serial")
public class NewDeviceDialog extends JDialog{
	
	public final String OK = "OK";
	public final String CANCEL = "CANCEL";
	
	private ImageIcon searchIcon;
	private Font boldFont;
	
	private Device newDevice;
	private List<FieldTO> newDeviceFields;
	private List<FieldTO> cameraDeviceFiels;
	private JComboBox<SelectItem> foundDevicesComboBox;
	private JPanel mainPanel;
	private JComboBox<Manufacturer> fabricanteComboBox;
	
	private String option = CANCEL;
	private String ipEncontradoVarredura;
	private Boolean stopWorkerVarredura;
	
	private String tipoCamera;
	
	public NewDeviceDialog(Frame owner) {
		super(owner, "Novo dispositivo", true);
		
		loadImages();
		
		Font font = new JLabel().getFont();
		boldFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
		
		setResizable(false);
		setIconImage(Main.favicon);
		
		mainPanel = new JPanel();
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
		mainPanel.setBorder(new EmptyBorder(10, 10, 10, 10));
		
		JPanel fabricantePanel= new JPanel();
		fabricantePanel.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0));
		JLabel fabricanteLabel = new JLabel("Selecione o fabricante/modelo:");
		fabricanteLabel.setFont(boldFont);
		fabricantePanel.add(fabricanteLabel);
		fabricantePanel.add(Box.createHorizontalStrut(5));
		fabricanteComboBox = new JComboBox<Manufacturer>();
		for (Manufacturer manufacturer : Manufacturer.values()){
			if (!Manufacturer.COMM.equals(manufacturer)
					&& !Manufacturer.PROVEU.equals(manufacturer)
					&& !Manufacturer.USB.equals(manufacturer)
					&& !Manufacturer.HENRY_8X.equals(manufacturer)
					&& !Manufacturer.HENRY_7X.equals(manufacturer)
					&& !Manufacturer.TECNIBRA.equals(manufacturer)
					&& !Manufacturer.COMPUTER_ID.equals(manufacturer)
					&& !Manufacturer.RWTECH.equals(manufacturer)
					&& !Manufacturer.TOLETUS.equals(manufacturer)
					&& !Manufacturer.SYSTEMTEC.equals(manufacturer)) {
				
				if(Manufacturer.SERVER.equals(manufacturer) 
						&& Boolean.TRUE.equals(Main.servidor != null)) {
					continue;
				}
				if(Manufacturer.LC_DEVICE.equals(manufacturer)
						&& Boolean.TRUE.equals(Main.possuiLeitorLcAdd)) {
					continue;
				}
				
				if(Manufacturer.TOP_DATA_EXPEDIDORA.equals(manufacturer) && !Boolean.TRUE.equals(Main.loggedUser.getExpedidora())) {
					//só adiciona expedidora se usuário poder
					continue;
				}
				
				fabricanteComboBox.addItem(manufacturer);
			}
		}
		fabricanteComboBox.setPreferredSize(new Dimension(250, 25));
		fabricantePanel.add(fabricanteComboBox);
		
		JPanel mainNewDevicePanel = new JPanel();
		mainNewDevicePanel.setLayout(new BoxLayout(mainNewDevicePanel, BoxLayout.Y_AXIS));
		
		JPanel mensagensPanel= new JPanel();
		mensagensPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 20, 0));
		
		JLabel mensagem = new JLabel(" ");
		mensagem.setForeground(Color.RED);
		mensagem.setFont(boldFont);
		mensagensPanel.add(mensagem);
		
		JPanel botoesPanel= new JPanel();
		botoesPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 20, 0));
		
		JButton adicionarButton = new JButton("Adicionar");
		adicionarButton.setPreferredSize(new Dimension(150, 30));
		adicionarButton.setEnabled(false);
		botoesPanel.add(adicionarButton);
		
		JButton cancelarButton = new JButton("Cancelar");
		cancelarButton.setPreferredSize(new Dimension(150, 30));
		botoesPanel.add(cancelarButton);
		
		mainPanel.add(fabricantePanel);
		mainPanel.add(Box.createVerticalStrut(5));
		mainPanel.add(mainNewDevicePanel);
		mainPanel.add(Box.createVerticalGlue());
		mainPanel.add(Box.createVerticalStrut(10));
		mainPanel.add(mensagensPanel);
		mainPanel.add(Box.createVerticalStrut(5));
		mainPanel.add(botoesPanel);
		mainPanel.add(Box.createVerticalStrut(5));
		
		fabricanteComboBox.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
					private JPanel panelCamerasInterno;

					@Override
					protected Void doInBackground() throws Exception {
						try {
							mensagem.setText(" ");
							setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
							Manufacturer fabricante = (Manufacturer) fabricanteComboBox.getSelectedItem();
							mainNewDevicePanel.removeAll();
							if (!Manufacturer.NENHUM.equals(fabricante)){
								adicionarButton.setEnabled(true);
								newDeviceFields = fabricante.getFields();
								if (newDeviceFields != null) {
									JPanel dadosDispositivoPanel = new JPanel();
									dadosDispositivoPanel.setLayout(new BoxLayout(dadosDispositivoPanel, BoxLayout.Y_AXIS));
									for (FieldTO field : newDeviceFields) {
										field.setTextFieldSize(15);
										JPanel fieldPanel = field.getPanel();
										if ("Número IP do dispositivo".equals(field.getName())) {
											JTextField textField = (JTextField) field.getField()[0];
											Container container = textField.getParent();
											JButton button = new JButton(searchIcon);
											button.setPreferredSize(new Dimension(22, 22));
											button.addActionListener(new ActionListener() {
												@Override
												public void actionPerformed(ActionEvent e) {
													String retorno = fazerVarreduraIps();
													if (retorno != null) {
														mensagem.setText("IP encontrado: " + retorno);
														textField.setText(retorno);
													
													} else
														mensagem.setText("Nenhuma catraca encontrada");
												}
											});
											container.add(button);
											double width = fieldPanel.getMaximumSize().getWidth();
											double height = fieldPanel.getPreferredSize().getHeight();
											fieldPanel.setMaximumSize(new Dimension(Double.valueOf(width).intValue(), Double.valueOf(height).intValue()));
										
										} else if("Selecione o tipo de camêra".equals(field.getName())) {
											JComboBox<String> comboTipoCamera = (JComboBox<String>) field.getField()[0];
											
											comboTipoCamera.addActionListener(e -> {
												if(panelCamerasInterno != null)
													mainNewDevicePanel.remove(panelCamerasInterno);
												
												criarComboEscolherCamera(comboTipoCamera, fabricante);
												
												mainNewDevicePanel.add(panelCamerasInterno);
												mainPanel.revalidate();
												pack();
											});
											
											criarComboEscolherCamera(comboTipoCamera, fabricante);
										}
										
										dadosDispositivoPanel.add(fieldPanel);
										dadosDispositivoPanel.add(Box.createVerticalStrut(7));
									}
									
									PanelWithLabel label = new PanelWithLabel("Insira as informações do dispositivo:", FlowLayout.LEFT, true, 0, 10);
									mainNewDevicePanel.add(label);
									mainNewDevicePanel.add(dadosDispositivoPanel);
									
									if(panelCamerasInterno != null)
										mainNewDevicePanel.add(panelCamerasInterno);
								
								} else {
									PanelWithLabel label = new PanelWithLabel("Selecione um dispositivo na lista abaixo", FlowLayout.LEFT, true, 0, 10);
									mainNewDevicePanel.add(label);
									JPanel buscaDispositivosPanel = montarPanelBuscaDispositivos(fabricante);
									mainNewDevicePanel.add(buscaDispositivosPanel);
								}
							} else
								adicionarButton.setEnabled(false);
						} catch (Exception e) {
							e.printStackTrace();
						} finally {
							mainPanel.revalidate();
							pack();
							setLocationRelativeTo(null);
							setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
						}
						return null;
					}

					private void criarComboEscolherCamera(JComboBox<String> comboTipoCamera, Manufacturer fabricante) {
						panelCamerasInterno = new JPanel();
						panelCamerasInterno.setLayout(new BoxLayout(panelCamerasInterno, BoxLayout.Y_AXIS));
						
						tipoCamera = comboTipoCamera.getSelectedItem().toString();

						List<FieldTO> fieldsCameras = fabricante.getCameraFacialFields(tipoCamera);
						cameraDeviceFiels = new ArrayList<>();
						
						for(FieldTO fieldTOCamera : fieldsCameras) {
							JPanel fieldCamera = fieldTOCamera.getPanel();
							panelCamerasInterno.add(fieldCamera);
							panelCamerasInterno.add(Box.createVerticalStrut(7));
							cameraDeviceFiels.add(fieldTOCamera);
						}
					}
				};
				worker.execute();
			}
		});
		
		adicionarButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					Manufacturer fabricante = (Manufacturer) fabricanteComboBox.getSelectedItem();
					if (newDeviceFields != null) {
						// Cria um identificador com os dados inseridos e cria um device a partir desse identificador
						StringBuilder identifier = new StringBuilder();
						for (FieldTO field : newDeviceFields){
							if (field.getValue() != null && !"".equals(field.getValue()))
								identifier.append(field.getValue() + ";");
							else {
								mensagem.setText("Todos os campos são necessários");
								return;
							}
						}
						
						if(cameraDeviceFiels != null && Manufacturer.FACIAL.equals(fabricante)) {
							for(FieldTO field : cameraDeviceFiels) {
								if("IP".equals(tipoCamera) 
										&& "URL da camêra".equalsIgnoreCase(field.getName())
											&& field.getValue().trim().isEmpty()) {
									mensagem.setText("A URL é obrigatória.");
									return;
								}
								String valor = field.getValue().isEmpty() ? " " : field.getValue();
								
								identifier.append(valor + ";");
							}
						}
						
						if (fabricante.useLogin()) // login será pedido quando conectar, mas precisar estar presente no identificador
							identifier.append(" ;");
						if (fabricante.usePassword()) // senha será pedida quando conectar, mas precisar estar presente no identificador
							identifier.append(" ;");
						newDevice = fabricante.getNewDevice(identifier.toString());
					
					} else {
						if (foundDevicesComboBox == null || foundDevicesComboBox.getSelectedItem() == null) {
							mensagem.setText("Selecione um dispositivo");
							return;
						}
						SelectItem item = (SelectItem) foundDevicesComboBox.getSelectedItem();
						if (item.getValue() == null) {
							mensagem.setText("Selecione um dispositivo válido");
							return;
						}
						newDevice = (Device) item.getValue();
					}
					for (Device device : Main.devicesList){
						if (device.isTheSame(newDevice)){
							mensagem.setText("Dispositivo já adicionado!");
							newDevice = null;
							return;
						}
					}
					option = OK;
					dispose();
				
				} catch (Exception e1) {
					e1.printStackTrace();
				}
			}
		});
		
		cancelarButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				option = CANCEL;
				dispose();
			}
		});
		
		getContentPane().add(mainPanel);
		pack();
		setLocationRelativeTo(null);
	}
	
	private JPanel montarPanelBuscaDispositivos(Manufacturer fabricante){
		
		JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0));
		
		Vector<SelectItem> itens = buscaDispositivos(fabricante);
		foundDevicesComboBox = new JComboBox<SelectItem>(itens);
		foundDevicesComboBox.setPreferredSize(new Dimension(400, 25));
		panel.add(foundDevicesComboBox);
		panel.add(Box.createHorizontalStrut(5));
		
		return panel;
	}
	
	private Vector<SelectItem> buscaDispositivos(Manufacturer fabricante){
		Vector<Device> dispositivosEncontrados = new Vector<Device>();
		Vector<SelectItem> itens = new Vector<SelectItem>();
		try {
			ScanDevices scanner = new ScanDevices(fabricante);
			scanner.execute();
			while (!scanner.isDone()){
				Utils.sleep(50);
			}
			dispositivosEncontrados = scanner.get();
		
		} catch (Exception e){
			e.printStackTrace();
		}
		
		if (dispositivosEncontrados.isEmpty())
			itens.add(new SelectItem("Nenhum dispositivo encontrado", null));
		else 
			for (Device device : dispositivosEncontrados) {
				String identifier = device.getIdentifier();
				if (Manufacturer.COMPUTER_ID.equals(device.getManufacturer()))
					identifier = ((ComputerIdDevice) device).getSerialNumber();
				if (Manufacturer.NITGEN.equals(device.getManufacturer()))
					identifier = ((NitgenDevice) device).getNameIdAndInstance();
				if(Manufacturer.LC_DEVICE.equals(device.getManufacturer()))
					identifier = ((LcDevice) device).getIdentifier();
				itens.add(new SelectItem(device.getName() + "(" + identifier + ")" , device));
			}
		
		return itens;
	}
	
	private String fazerVarreduraIps(){
		
		// cria o dialog com as informações
		JDialog dialog = new JDialog(null, "Procurando a catraca na rede...", ModalityType.APPLICATION_MODAL);
		Font font = new JLabel().getFont();
		Font boldFont = new Font(font.getFontName(), Font.BOLD, font.getSize());
		JLabel instrucoesLabel1 = new JLabel("Procurando a catraca na rede: ");
		instrucoesLabel1.setAlignmentX(Component.CENTER_ALIGNMENT);
		instrucoesLabel1.setFont(boldFont);
		JPanel dialogPanel = new JPanel();
		dialogPanel.setLayout(new BoxLayout(dialogPanel, BoxLayout.Y_AXIS));
		dialogPanel.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(Main.firstColor, 1),
				new EmptyBorder(30,20,20,20)));
		dialogPanel.setPreferredSize(new Dimension(350, 150));
		
		ipEncontradoVarredura = null;
		stopWorkerVarredura = false;
		
		// inicia o processo de varredura
		SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
			@Override
			protected Void doInBackground() throws Exception {
				
				Manufacturer fabricante = (Manufacturer) fabricanteComboBox.getSelectedItem();
				
				// Inicia o nucle RWTECH para ja ficar pronto e nao ter q iniciá-lo toda vez
//				if (Manufacturer.RWTECH.equals(fabricante)){
//					RWTechDevice deviceTeste = new RWTechDevice(5000, " ; ;");
//					deviceTeste.iniciarNucleoIntegracao();
//					Utils.sleep(10000);
//				}
				
				// Recupera IP digitado para ver qual sub-rede usar
				String subrede = null;
				for (FieldTO field : newDeviceFields) {
					if ("Número IP do dispositivo".equals(field.getName())
							&& !field.getValue().isEmpty()
							&& field.getValue().split("\\.").length >= 2){
						String[] partes = field.getValue().split("\\.");
						subrede = partes[2];
						break;
					}
				}
				if (subrede == null)
					return null;
				
				for (int i = 2; i <= 254; i++) {
					if (stopWorkerVarredura)
						return null;
					
					String ip = "192.168." + subrede + "." + String.valueOf(i);
					instrucoesLabel1.setText("Procurando a catraca na rede: " + ip);
					
					// ping no ip e vê se tem alguma resposta, se responder é um possivel ip de catraca, e ai tenta conectar nele
					if (!ping(ip))
						continue;
					
					System.out.println("IP respondeu ao ping: " + ip);
					
					try {
						// Cria um identificador com os dados inseridos e cria um device a partir desse identificador
						StringBuilder identifier = new StringBuilder();
						for (FieldTO field : newDeviceFields) 
							identifier.append(("Número IP do dispositivo".equals(field.getName()) ? ip : field.getValue()) + ";");
						
						Boolean retorno = false;
//						if (Manufacturer.HENRY_8X.equals(fabricante)) {
//							Henry8XDevice deviceTeste = new Henry8XDevice(5000, identifier.toString());
//							retorno = deviceTeste.quickConnect();
//						}
//						else if (Manufacturer.TECNIBRA.equals(fabricante)){
//							TecnibraDevice deviceTeste = new TecnibraDevice(5000, identifier.toString());
//							retorno = deviceTeste.quickConnect();
//						}
//						else 
						if (Manufacturer.CONTROL_ID.equals(fabricante)){
							identifier.append("admin;admin");
							ControlIdDevice deviceTeste = new ControlIdDevice(5000, identifier.toString());
							retorno = deviceTeste.quickConnect();
						}
//						else if (Manufacturer.RWTECH.equals(fabricante)){
//							RWTechDevice deviceTeste = new RWTechDevice(5000, identifier.toString());
//							retorno = deviceTeste.quickConnect();
//						}
//						else if (Manufacturer.TOLETUS.equals(fabricante)){
//							ToletusDevice deviceTeste = new ToletusDevice(3000, identifier.toString());
//							retorno = deviceTeste.quickConnect();
//						}
						
						if (retorno) {
							ipEncontradoVarredura = ip;
							return null;
						}
					}
					catch (Exception e){}
				}
				return null;
			}
		};
		
		// adiciona um listener para indiciar o fim do processo
		worker.addPropertyChangeListener(new PropertyChangeListener() {
			@Override
			public void propertyChange(PropertyChangeEvent evt) {
				if (evt.getPropertyName().equals("state")
						&& evt.getNewValue() == SwingWorker.StateValue.DONE) {
					try {
						dialog.dispose();
					}
					catch (Exception e){
						e.printStackTrace();
					}
				}
			}
		});
		worker.execute();
		
		// adiciona um progressBar para compor o dialog
		JProgressBar progressBar = new JProgressBar();
		progressBar.setIndeterminate(true);
		progressBar.setPreferredSize(new Dimension(200, 30));
		progressBar.setAlignmentX(Component.CENTER_ALIGNMENT);
		
		// Adicionar um botao para cancelar a ação quando for possível
		JButton cancelButton = new JButton("Cancelar");
		cancelButton.setAlignmentX(Component.CENTER_ALIGNMENT);
		cancelButton.setPreferredSize(new Dimension(100, 50));
		cancelButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					stopWorkerVarredura = true;
				}
				catch (Exception e1){
					e1.printStackTrace();
				}
			}
		});
		
		dialogPanel.add(progressBar);
		dialogPanel.add(Box.createVerticalStrut(10));
		dialogPanel.add(instrucoesLabel1);
		dialogPanel.add(Box.createVerticalStrut(20));
		dialogPanel.add(cancelButton);
		dialog.setUndecorated(true);
		dialog.setLayout(new BorderLayout());
        dialog.add(dialogPanel);
		dialog.pack();
		dialog.setLocationRelativeTo(null);
		dialog.setVisible(true);
		
		return ipEncontradoVarredura;
	}

	private boolean ping(String ip) {
		try{
            InetAddress address = InetAddress.getByName(ip);
            boolean reachable = address.isReachable(1000);
            return reachable;
        } catch (Exception e){
            e.printStackTrace();
            return false;
        }
	}
	
	private void loadImages() {
		try {
			Toolkit toolkit = Toolkit.getDefaultToolkit();
			searchIcon = new ImageIcon(toolkit.getImage(Main.class.getResource(Configurations.IMAGE_FOLDER + "comuns/buscar_min.png")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public Device getNewDevice() {
		return newDevice;
	}
	
	public String getOption() {
		return option;
	}
	
}
