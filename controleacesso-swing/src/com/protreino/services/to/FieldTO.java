package com.protreino.services.to;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Graphics2D;
import java.awt.GridLayout;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.util.Vector;

import javax.imageio.ImageIO;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JColorChooser;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.filechooser.FileFilter;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.DocumentFilter;
import javax.swing.text.PlainDocument;

import org.apache.commons.codec.binary.Base64;

import com.protreino.services.devices.DeviceCard;
import com.protreino.services.enumeration.FieldType;
import com.protreino.services.main.Main;
import com.protreino.services.screens.PreferencesDialog;
import com.protreino.services.utils.PanelWithLabel;
import com.protreino.services.utils.SelectItem;
import com.protreino.services.utils.Utils;

public class FieldTO {
	
	private DeviceCard deviceCard;
	private PreferencesDialog preferencesDialog;
	
	private String name;
	private FieldType type;
	private String defaultValue;
	private Vector<SelectItem> options;
	private Long inicio, passo, fim;
	private Integer maxCharacteres;
	private Integer minCharacteres;
	private Boolean numeric = false;
	private Boolean required = true;
	private Boolean enabled = true;
	private String[] imageExtensions;
	private Boolean fullSize;
	private Boolean visible = true;
	private ActionListener actionListener;
	
	private JTextField textField;
	private JTextField textField2;
	private JCheckBox checkBox;
	private JComboBox<SelectItem> comboBox;
	private JButton loadImageButton;
	private JButton removeImageButton;
	private JLabel imageLabel;
	private String imageBase64;
	private Integer textFieldSize = 25;
	private ColorChooserButton colorChooser;
	
	private JPanel mainPanel;
	
	private ComboBoxListener comboBoxListener;
	
	/**
	 * Construtor simples restrito para campos dos tipos TEXT ou CHECKBOX
	 * @param name
	 * @param type
	 * @param defaultValue
	 */
	public FieldTO(String name, FieldType type, String defaultValue) {
		this.name = name;
		this.type = type;
		this.defaultValue = defaultValue;
	}
	
	/**
	 * Construtor para o deviceCard restrito para campos que nao sejam COMBOBOX
	 * @param name
	 * @param type
	 * @param defaultValue
	 */
	public FieldTO(DeviceCard deviceCard, String name, FieldType type, String defaultValue) {
		this.deviceCard = deviceCard;
		this.name = name;
		this.type = type;
		this.defaultValue = defaultValue;
	}
	
	/**
	 * Construtor para a tela de preferencias para campos que nao sejam COMBOBOX
	 * @param name
	 * @param type
	 * @param defaultValue
	 */
	public FieldTO(PreferencesDialog preferencesDialog, String name, FieldType type, String defaultValue) {
		this.preferencesDialog = preferencesDialog;
		this.name = name;
		this.type = type;
		this.defaultValue = defaultValue;
	}
	
	/**
	 * Construtor para a tela de preferencias para campos do tipo NUMERIC_LIST
	 * @param name
	 * @param type
	 * @param defaultValue
	 */
	public FieldTO(PreferencesDialog preferencesDialog, String name, FieldType type, 
			String defaultValue, String numericListSequency) {
		this.preferencesDialog = preferencesDialog;
		this.name = name;
		this.type = type;
		this.defaultValue = defaultValue;
		String[] itens = numericListSequency.split(";");
		inicio = Long.valueOf(itens[0]);
		passo = Long.valueOf(itens[1]);
		fim = Long.valueOf(itens[2]);
	}
	
	/**
	 * Construtor restrito para campos do tipo COMBOBOX
	 * @param name
	 * @param type
	 * @param defaultValue
	 * @param options
	 */
	public FieldTO(String name, FieldType type, String defaultValue, String[] options) {
		this.name = name;
		this.type = type;
		this.defaultValue = defaultValue;
		this.options = new Vector<SelectItem>();
		if (options != null) {
			for (String item : options){
				if (item.contains("_"))
					this.options.add(new SelectItem(item.split("_")[0], item.split("_")[1]));
				else
					this.options.add(new SelectItem(item));
			}
		}
	}
	
	public FieldTO(ConfigurationTO config, DeviceCard deviceCard) {
		this(config);
		this.deviceCard = deviceCard;
	}
	
	/**
	 * Construtor para todos os tipos de campo
	 * @param config
	 */
	public FieldTO(ConfigurationTO config) {
		this.name = config.getNameAux();
		this.type = config.getType();
		this.defaultValue = config.getValue();
		this.options = new Vector<SelectItem>();
		if (config.getComboboxValues() != null && !config.getComboboxValues().isEmpty()) {
			String[] itens = config.getComboboxValues().split(";");
			if (FieldType.NUMERIC_LIST.equals(type)){
				inicio = Long.valueOf(itens[0]);
				passo = Long.valueOf(itens[1]);
				fim = Long.valueOf(itens[2]);
			
			} else if (FieldType.COMBOBOX.equals(type)){
				for (String item : itens) {
					if (item.contains("_"))
						this.options.add(new SelectItem(item.split("_")[0], item.split("_")[1]));
					else
						this.options.add(new SelectItem(item));
				}
			}
		}
		this.maxCharacteres = config.getMaxCharacteres();
		this.minCharacteres = config.getMinCharacteres();
		this.numeric = config.getNumeric();
		this.required = config.getRequired();
	}
	
	public JPanel getCurrentPanel(){
		return mainPanel;
	}
	
	public JPanel getPanel(){
		
		mainPanel = new JPanel();
		mainPanel.setLayout(new GridLayout(1, 2, 20, 5));
		
		PanelWithLabel label = new PanelWithLabel(name, FlowLayout.LEFT);
		mainPanel.add(label);
		
		JPanel fieldPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
		
		if (FieldType.TEXT.equals(type)){
			if (maxCharacteres != null && maxCharacteres > 0)
				textField = new JTextFieldLimit(textFieldSize, maxCharacteres);
			else
				textField = new JTextField(textFieldSize);
			textField.setText(defaultValue);
			if (numeric){
				PlainDocument doc = (PlainDocument) textField.getDocument();
				doc.setDocumentFilter(new MyIntFilter());
			}
			textField.setAlignmentX(Component.LEFT_ALIGNMENT);
			fieldPanel.add(textField);
		
		} else if (FieldType.CHECKBOX.equals(type)){
			checkBox = new JCheckBox();
			if (!Utils.isNullOrEmpty(defaultValue))
				checkBox.setSelected(Boolean.valueOf(defaultValue));
			if (actionListener != null)
				checkBox.addActionListener(actionListener);
			fieldPanel.add(checkBox);
		
		} else if (FieldType.COMBOBOX.equals(type)){
			comboBox = new JComboBox<SelectItem>(options);
			if(maxCharacteres != null && maxCharacteres > 0) {
				comboBox.setPreferredSize(new Dimension(maxCharacteres, 25));
			} else {
				comboBox.setPreferredSize(new Dimension(250, 25));
			}
			comboBox.setAlignmentX(Component.LEFT_ALIGNMENT);
			if (!Utils.isNullOrEmpty(defaultValue))
				comboBox.setSelectedItem(new SelectItem(defaultValue));
			
			comboBox.addItemListener(new ItemListener() {
				
				@Override
				public void itemStateChanged(ItemEvent e) {
					if(comboBoxListener != null)
						comboBoxListener.action(e);
				}
			});
			fieldPanel.add(comboBox);
		
		} else if (FieldType.YES_NO_COMBOBOX.equals(type)){
			Vector<SelectItem> options = new Vector<>();
			options.add(new SelectItem("Sim", "true"));
			options.add(new SelectItem("Não", "false"));
			comboBox = new JComboBox<SelectItem>(options);
			comboBox.setAlignmentX(Component.LEFT_ALIGNMENT);
			if (!Utils.isNullOrEmpty(defaultValue)) {
				if (!defaultValue.contains("_")) 
					defaultValue = Boolean.valueOf(defaultValue) ? "Sim_true" : "Não_false";
				comboBox.setSelectedItem(new SelectItem(defaultValue));
			}
			fieldPanel.add(comboBox);
		
		} else if (FieldType.MESSAGE_LINES.equals(type)){
			if (maxCharacteres == null)
				maxCharacteres = 16;
			textField = new JTextFieldLimit(12, maxCharacteres);
			textField.setAlignmentX(Component.LEFT_ALIGNMENT);
			textField2 = new JTextFieldLimit(12, maxCharacteres);
			textField2.setAlignmentX(Component.LEFT_ALIGNMENT);
			if (!Utils.isNullOrEmpty(defaultValue)){
				String[] partes = defaultValue.split(";");
				textField.setText(partes[0]);
				textField2.setText(partes.length > 1 ? partes[1] : "");
			}
			fieldPanel.add(textField);
			fieldPanel.add(Box.createHorizontalStrut(5));
			fieldPanel.add(textField2);
		
		} else if (FieldType.NUMERIC_LIST.equals(type)){
			Vector<SelectItem> options = new Vector<>();
			for (long i = inicio; i <= fim; i = i + passo)
				options.add(new SelectItem(String.valueOf(i), String.valueOf(i)));
			comboBox = new JComboBox<SelectItem>(options);
			comboBox.setPreferredSize(new Dimension(60, 25));
			comboBox.setAlignmentX(Component.LEFT_ALIGNMENT);
			if (!Utils.isNullOrEmpty(defaultValue))
				comboBox.setSelectedItem(new SelectItem(defaultValue));
			fieldPanel.add(comboBox);
		
		} else if (FieldType.IMAGE.equals(type)){
			imageLabel = new JLabel("Sem imagem");
			imageLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
			loadImageButton = new JButton("Carregar imagem");
			removeImageButton = new JButton("Remove imagem");
			JPanel buttonsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
			buttonsPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
			buttonsPanel.add(loadImageButton);
			buttonsPanel.add(Box.createHorizontalStrut(5));
			buttonsPanel.add(removeImageButton);
			fieldPanel.setLayout(new BoxLayout(fieldPanel, BoxLayout.Y_AXIS));
			fieldPanel.add(imageLabel);
			fieldPanel.add(buttonsPanel);
			loadImageButton.addActionListener((e) -> loadImage());
			removeImageButton.addActionListener((e) -> removeImage());
			if (!Utils.isNullOrEmpty(defaultValue)) {
				imageBase64 = defaultValue;
				defineImage();
			}
		
		} else if (FieldType.COLOR_CHOOSER.equals(type)) {
			String partes[] = (!Utils.isNullOrEmpty(defaultValue) ? defaultValue : (name.contains("primária") ? "39;57;74" : "70;178;202")).split(";");
			Color color = new Color(Integer.valueOf(partes[0]), Integer.valueOf(partes[1]), Integer.valueOf(partes[2]));
			colorChooser = new ColorChooserButton(color);
			fieldPanel.add(colorChooser);
		}
		mainPanel.add(fieldPanel);
		double width = mainPanel.getMaximumSize().getWidth();
		double height = mainPanel.getPreferredSize().getHeight();
		mainPanel.setMaximumSize(new Dimension(Double.valueOf(width).intValue(), Double.valueOf(height).intValue()));
		mainPanel.setVisible(visible);
		return mainPanel;
	}
	
	private void loadImage(){
		JFileChooser fileChooser = new JFileChooser();
		if (imageExtensions == null)
			imageExtensions = new String[] {"jpeg", "jpg", "png"};
		fileChooser.addChoosableFileFilter(new ImageFilter(imageExtensions));
		fileChooser.setAcceptAllFileFilterUsed(false);
		int returnVal = fileChooser.showDialog(Main.mainScreen, "Carregar imagem");
		if (returnVal == JFileChooser.APPROVE_OPTION) {
            try {
            	File file = fileChooser.getSelectedFile();
            	String extension = getExtension(file);
            	BufferedImage img = ImageIO.read(file);
            	if (Boolean.TRUE.equals(fullSize)) {
    				ByteArrayOutputStream baos = new ByteArrayOutputStream();
    				ImageIO.write(img, extension, baos );
    				imageBase64 = Base64.encodeBase64String(baos.toByteArray());
            	
            	} else {
            		int imageType = "png".equals(extension) ? BufferedImage.TYPE_INT_ARGB : BufferedImage.OPAQUE;
            		Image scaledImage = getScaledImage(img, 250, 250, imageType);
    				ByteArrayOutputStream baos = new ByteArrayOutputStream();
    				ImageIO.write((RenderedImage) scaledImage, extension, baos );
    				imageBase64 = Base64.encodeBase64String(baos.toByteArray());
            	}
            	//imageBase64 = imageBase64 + ";" + extension;
				defineImage();
				if (deviceCard != null)
					deviceCard.setErroConfiguration("");
				if (preferencesDialog != null)
					preferencesDialog.setErrorMessage(" ");
			
            } catch (Exception e) {
				e.printStackTrace();
				if (deviceCard != null)
					deviceCard.setErroConfiguration("Erro ao carregar imagem. " + e.getMessage());
				if (preferencesDialog != null)
					preferencesDialog.setErrorMessage("Erro ao carregar imagem. Verifique o tamanho e o formato da imagem.");
			}
        } 
	}
	
	private void removeImage(){
		defaultValue = "";
		imageBase64 = "";
		imageLabel.setIcon(null);
		imageLabel.setText("Sem imagem");
	}
	
	private Image getScaledImage(Image srcImg, int w, int h, int imageType){
	    BufferedImage resizedImg = new BufferedImage(w, h, imageType);
	    Graphics2D g2 = resizedImg.createGraphics();
	    g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
	    g2.drawImage(srcImg, 0, 0, w, h, null);
	    g2.dispose();
	    return resizedImg;
	}
	
	private void defineImage() {
		try {
			byte[] bytes = Base64.decodeBase64(imageBase64);
			BufferedImage img = ImageIO.read(new ByteArrayInputStream(bytes));
			if (img.getWidth() > 250 || img.getHeight() > 250) {
				Integer larguraAtual = img.getWidth();
				Integer alturaAtual = img.getHeight();
				int largura = 250;
				int altura = 250;
				if (larguraAtual >= alturaAtual)
					altura = Double.valueOf(250 / (larguraAtual.doubleValue() / alturaAtual)).intValue();
				else
					largura = Double.valueOf(250 / (alturaAtual.doubleValue() / larguraAtual)).intValue();
				int imageType = BufferedImage.TYPE_INT_ARGB == img.getType() ? BufferedImage.TYPE_INT_ARGB : BufferedImage.OPAQUE;
				img = (BufferedImage) getScaledImage(img, largura, altura, imageType);
			}
			imageLabel.setIcon(new ImageIcon(img));
			imageLabel.setText("");
			mainPanel.revalidate();
			double width = mainPanel.getMaximumSize().getWidth();
			double height = mainPanel.getPreferredSize().getHeight();
			mainPanel.setMaximumSize(new Dimension(Double.valueOf(width).intValue(), Double.valueOf(height).intValue()));
		
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	
	public String getName(){
		return this.name;
	}
	
	/**
	 * Método que retorno uma String representando o valor do campo. 
	 * 
	 * @ Para campos com valor booleano (CHECKBOX e YES_NO_COMBOBOX),
	 * é retornado "true" ou "false"
	 * 
	 * @ Para um campo do tipo MESSAGE_LINES, que é composto por dois TextFields, 
	 * é retornada uma String com um ";" separando os valores de cada TextField. Ex: "texto1;texto2"
	 */
	public String getValue() {
		if (FieldType.TEXT.equals(type)){
			return textField.getText().trim();
		
		} else if (FieldType.CHECKBOX.equals(type)){
			return checkBox.isSelected() ? "true" : "false";
		
		} else if (FieldType.COMBOBOX.equals(type)){
			return (String) ((SelectItem) comboBox.getSelectedItem()).getValue();
		
		} else if (FieldType.YES_NO_COMBOBOX.equals(type)){
			return (String) ((SelectItem) comboBox.getSelectedItem()).getValue();
		
		} else if (FieldType.MESSAGE_LINES.equals(type)) {
			String texto1 = textField.getText().trim();
			String texto2 = textField2.getText().trim();
			if (maxCharacteres > 0) {
				if (texto1.length() > maxCharacteres)
					texto1 = texto1.substring(0, maxCharacteres);
				if (texto2.length() > maxCharacteres)
					texto2 = texto2.substring(0, maxCharacteres);
			}
			return texto1 + ";" + texto2;
		
		} else if (FieldType.NUMERIC_LIST.equals(type)){
			String retorno = (String) ((SelectItem) comboBox.getSelectedItem()).getValue();
			if (minCharacteres != null){
				while (retorno.length() < minCharacteres)
					retorno = "0" + retorno;
			}
			return retorno;
		
		} else if (FieldType.IMAGE.equals(type)){
			return imageBase64;
		
		} else if (FieldType.COLOR_CHOOSER.equals(type)){
			Color cor = colorChooser.getSelectedColor();
			return cor.getRed() + ";" + cor.getGreen() + ";" + cor.getBlue();
		}
		return "";
	}
	
	
	public void setValue(String value){
		if (FieldType.TEXT.equals(type)){
			textField.setText(value);
		
		} else if (FieldType.CHECKBOX.equals(type)){
			if (!Utils.isNullOrEmpty(value))
				checkBox.setSelected(Boolean.valueOf(value));
		
		} else if (FieldType.COMBOBOX.equals(type)){
			if (!Utils.isNullOrEmpty(value))
				comboBox.setSelectedItem(new SelectItem(value));
		
		} else if (FieldType.YES_NO_COMBOBOX.equals(type)){
			if (!Utils.isNullOrEmpty(value)) {
				if (!value.contains("_")) 
					value = Boolean.valueOf(value) ? "Sim_true" : "Não_false";
				comboBox.setSelectedItem(new SelectItem(value));
			}
		
		} else if (FieldType.MESSAGE_LINES.equals(type)){
			if (!Utils.isNullOrEmpty(value)){
				String[] partes = value.split(";");
				textField.setText(partes[0]);
				textField2.setText(partes.length > 1 ? partes[1] : "");
			}
		
		} else if (FieldType.NUMERIC_LIST.equals(type)){
			if (!Utils.isNullOrEmpty(value))
				comboBox.setSelectedItem(new SelectItem(value));
		
		} else if (FieldType.IMAGE.equals(type)){
			imageBase64 = value;
			if (!Utils.isNullOrEmpty(imageBase64))
				defineImage();
			else
				removeImage();
		
		} else if (FieldType.COLOR_CHOOSER.equals(type)){
			String partes[] = value.split(";");
			Color cor = new Color(Integer.valueOf(partes[0]), Integer.valueOf(partes[1]), Integer.valueOf(partes[2]));
			colorChooser.setSelectedColor(cor);
		}
	}
	
	/**
	 * Verifica se um campos do tipo TEXT e MESSAGE_LINES está de acordo com as definições, isto é
	 * o tamanho não excede os limites mínimo e máximo de caracteres, e caso seja obrigatório, não pode ser nulo ou vazio.
	 * Por padrão o campos é obrigatório, mas pode ser alterado para deixar de ser.
	 * @return
	 */
	public String checkField(){
		if (!required)
			return "";
		
		if (FieldType.TEXT.equals(type)) {
			if (Utils.isNullOrEmpty(textField.getText()))
				return "O campo " + name.toUpperCase() + " é obrigatório.";
			if (maxCharacteres != null && textField.getText().trim().length() > maxCharacteres)
				return "O campo " + name.toUpperCase() + " é maior que o limite. Limite: " + maxCharacteres;
			if (minCharacteres != null && textField.getText().trim().length() < minCharacteres)
				return "O campo " + name.toUpperCase() + " é menor que o limite. Limite: " + minCharacteres;
		
		} else if (FieldType.MESSAGE_LINES.equals(type)) {
			if (Utils.isNullOrEmpty(textField.getText()))
				return "O campo " + name.toUpperCase() + " é obrigatório.";
			if (maxCharacteres != null 
					&& (textField.getText().trim().length() > maxCharacteres 
							|| textField2.getText().trim().length() > maxCharacteres)) {
				return "O campo " + name.toUpperCase() + " é maior que o limite. Limite: " + maxCharacteres;
			}
			if (minCharacteres != null 
					&& (textField.getText().trim().length() < minCharacteres 
							|| textField2.getText().trim().length() < minCharacteres)) {
				return "O campo " + name.toUpperCase() + " é menor que o limite. Limite: " + minCharacteres;
			}
		}
		
		return "";
	}
	
	
	public Component[] getField() {
		if (FieldType.TEXT.equals(type)){
			return new Component[] { textField };
		
		} else if (FieldType.CHECKBOX.equals(type)){
			return new Component[] { checkBox };
		
		} else if (FieldType.COMBOBOX.equals(type)
				|| FieldType.YES_NO_COMBOBOX.equals(type)
				|| FieldType.NUMERIC_LIST.equals(type)){
			return new Component[] { comboBox };
		
		} else if (FieldType.MESSAGE_LINES.equals(type)) {
			return new Component[] { textField, textField2 };
		
		} else if (FieldType.COLOR_CHOOSER.equals(type)){
			return new Component[] { colorChooser };
		}
		return null;
	}
	

	@SuppressWarnings("serial")
	private class JTextFieldLimit extends JTextField {
	    private int limit;

	    public JTextFieldLimit(int columns, int limit) {
	        super(columns);
	        this.limit = limit;
	    }

	    @Override
	    protected Document createDefaultModel() {
	        return new LimitDocument();
	    }

	    private class LimitDocument extends PlainDocument {

	        @Override
	        public void insertString( int offset, String  str, AttributeSet attr ) throws BadLocationException {
	            if (str == null) return;

	            if ((getLength() + str.length()) <= limit) {
	                super.insertString(offset, str, attr);
	            }
	        }       

	    }

	}
	
	
	private class MyIntFilter extends DocumentFilter {
		@Override
		public void insertString(FilterBypass fb, int offset, String string, AttributeSet attr)
				throws BadLocationException {

			Document doc = fb.getDocument();
			StringBuilder sb = new StringBuilder();
			sb.append(doc.getText(0, doc.getLength()));
			sb.insert(offset, string);

			if (test(sb.toString())) {
				super.insertString(fb, offset, string, attr);
			} else {
				// warn the user and don't allow the insert
			}
		}

		private boolean test(String text) {
			try {
				if (text.isEmpty())
					return true;
				Integer.parseInt(text);
				return true;
			} catch (NumberFormatException e) {
				return false;
			}
		}

		@Override
		public void replace(FilterBypass fb, int offset, int length, String text, AttributeSet attrs)
				throws BadLocationException {

			Document doc = fb.getDocument();
			
			if (maxCharacteres != null && maxCharacteres > 0){
				if ((doc.getLength() + text.length()) > maxCharacteres) {
					text = text.substring(0, (maxCharacteres - doc.getLength()));
				}
			}
			
			StringBuilder sb = new StringBuilder();
			sb.append(doc.getText(0, doc.getLength()));
			sb.replace(offset, offset + length, text);

			if (test(sb.toString())) {
				super.replace(fb, offset, length, text, attrs);
			} else {
				// warn the user and don't allow the insert
			}

		}

		@Override
		public void remove(FilterBypass fb, int offset, int length) throws BadLocationException {
			Document doc = fb.getDocument();
			StringBuilder sb = new StringBuilder();
			sb.append(doc.getText(0, doc.getLength()));
			sb.delete(offset, offset + length);

			if (test(sb.toString())) {
				super.remove(fb, offset, length);
			} else {
				// warn the user and don't allow the insert
			}

		}
	}

	public String getDefaultValue() {
		return defaultValue;
	}

	public void setDefaultValue(String defaultValue) {
		this.defaultValue = defaultValue;
	}
	
	public void setEnabled(Boolean enabled){
		this.enabled = enabled;
		enableDisableField();
	}
	
	public void enableDisableField(){
		if (textField != null)
			textField.setEnabled(enabled);
		if (textField2 != null)
			textField2.setEnabled(enabled);
		if (checkBox != null)
			checkBox.setEnabled(enabled);
		if (comboBox != null)
			comboBox.setEnabled(enabled);
	}
	
	private class ImageFilter extends FileFilter {
		
		public String[] imageExtensions;
	    
	    public ImageFilter(String[] imageExtensions){
	    	this.imageExtensions = imageExtensions;
	    }
	    
		@Override
		public boolean accept(File f) {
			if (f.isDirectory()) {
		        return true;
		    }

		    String extension = getExtension(f);
		    if (extension != null) {
		    	for (String s : imageExtensions) {
		    		if (s.equals(extension))
		    			return true;
		    	}
		    	return false;
		    }

		    return false;
		}

		@Override
		public String getDescription() {
			String descricao = "";
			String separador = "";
			for (String s : imageExtensions) {
				descricao = separador + s.toUpperCase();
				separador = ", ";
			}
		    return descricao + " images";
		}
		
	}
	
	public String getExtension(File f) {
        String ext = null;
        String s = f.getName();
        int i = s.lastIndexOf('.');

        if (i > 0 &&  i < s.length() - 1) {
            ext = s.substring(i+1).toLowerCase();
        }
        return ext;
    }
	
	@SuppressWarnings("serial")
	public class ColorChooserButton extends JButton {

        private Color current;

        public ColorChooserButton(Color c) {
            setSelectedColor(c); 
            addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    Color newColor = JColorChooser.showDialog(null, "Selecione uma cor", current);
                    setSelectedColor(newColor);
                }
            });
        }

        public Color getSelectedColor() {
            return current;
        }

        public void setSelectedColor(Color newColor) {
            if (newColor == null) return;
            current = newColor;
            setIcon(createIcon(current, 16, 16));
            repaint();

        }

        public ImageIcon createIcon(Color main, int width, int height) {
            BufferedImage image = new BufferedImage(width, height, java.awt.image.BufferedImage.TYPE_INT_RGB);
            Graphics2D graphics = image.createGraphics();
            graphics.setColor(main);
            graphics.fillRect(0, 0, width, height);
            graphics.setXORMode(Color.DARK_GRAY);
            graphics.drawRect(0, 0, width-1, height-1);
            image.flush();
            ImageIcon icon = new ImageIcon(image);
            return icon;
        }
    }
	
	public static class ComboBoxListener{
		public void action(ItemEvent e) {}
	}
	
	public FieldType getType() {
		return type;
	}

	public void setRequired(Boolean required) {
		if (required != null)
			this.required = required;
	}

	public void setNumeric(Boolean numeric){
		if (numeric != null)
			this.numeric = numeric;
	}
	
	public void setTextFieldSize(Integer textFieldSize) {
		if (textFieldSize != null)
			this.textFieldSize = textFieldSize;
	}

	public void setMaxCharacteres(Integer maxCharacteres) {
		this.maxCharacteres = maxCharacteres;
	}

	public void setMinCharacteres(Integer minCharacteres) {
		this.minCharacteres = minCharacteres;
	}

	public void setImageExtensions(String[] imageExtensions) {
		this.imageExtensions = imageExtensions;
	}

	public void setFullSize(Boolean fullSize) {
		this.fullSize = fullSize;
	}

	public void setVisible(Boolean visible){
		this.visible = visible;
		if (mainPanel != null)
			mainPanel.setVisible(visible);
	}

	public void setActionListener(ActionListener actionListener) {
		this.actionListener = actionListener;
	}

	public ComboBoxListener getComboBoxListener() {
		return comboBoxListener;
	}

	public void setComboBoxListener(ComboBoxListener comboBoxListener) {
		this.comboBoxListener = comboBoxListener;
	}

	public void setOptions(Vector<SelectItem> options) {
		
		if (!FieldType.NUMERIC_LIST.equals(type))
			return;
		
		this.options = options;
		if(mainPanel != null) {
			JPanel fieldPanel = (JPanel) mainPanel.getComponent(1);
			fieldPanel.removeAll();
			if (FieldType.NUMERIC_LIST.equals(type)){
				comboBox = new JComboBox<SelectItem>(options);
				comboBox.setPreferredSize(new Dimension(60, 25));
				comboBox.setAlignmentX(Component.LEFT_ALIGNMENT);
				if (!Utils.isNullOrEmpty(defaultValue))
					comboBox.setSelectedItem(new SelectItem(defaultValue));
				fieldPanel.add(comboBox);
			}
			fieldPanel.updateUI();
		}
	}

	public JCheckBox getCheckBox() {
		return checkBox;
	}

	public void setCheckBox(JCheckBox checkBox) {
		this.checkBox = checkBox;
	}
}