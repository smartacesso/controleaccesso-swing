package com.protreino.services.utils;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Map;

import javax.swing.AbstractAction;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.JWindow;
import javax.swing.KeyStroke;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.main.Main;

public class AutoCompleteOld {

	private final JTextField textField;
	private final Window container;
	private JPanel suggestionsPanel;
	private JWindow autoSuggestionPopUpWindow;
	private String typedWord;
	private final ArrayList<String> dictionary = new ArrayList<>();
	private int tW, tH;
	private Font biggerFont;
	private Map<String, PedestrianAccessEntity> map;
	private Method setAcesso;
	private JComponent nextComponent;
	private Boolean enabled = true;

	private DocumentListener documentListener = new DocumentListener() {
		@Override
		public void insertUpdate(DocumentEvent de) {
			if (enabled) {
				setAcesso(null);
				checkForAndShowSuggestions();
			}
		}

		@Override
		public void removeUpdate(DocumentEvent de) {
			if (enabled) {
				setAcesso(null);
				checkForAndShowSuggestions();
			}
		}

		@Override
		public void changedUpdate(DocumentEvent de) {
			if (enabled) {
				setAcesso(null);
				checkForAndShowSuggestions();
			}
		}
	};

	public AutoCompleteOld(JTextField textField, Window mainWindow, ArrayList<String> words, 
			Map<String, PedestrianAccessEntity> map, Method setAcesso, JComponent nextComponent) {
		this.textField = textField;
		this.container = mainWindow;
		this.textField.getDocument().addDocumentListener(documentListener);
		this.map = map;
		this.setAcesso = setAcesso;
		this.nextComponent = nextComponent;
		
		setDictionary(words);

		typedWord = "";
		tW = 0;
		tH = 0;

		autoSuggestionPopUpWindow = new JWindow(mainWindow);

		suggestionsPanel = new JPanel();
		suggestionsPanel.setLayout(new GridLayout(0, 1));
		suggestionsPanel.setBackground(Color.WHITE);
		suggestionsPanel.setBorder(new CompoundBorder(new LineBorder(Color.GRAY), new EmptyBorder(3, 3, 3, 3)));

		addKeyBindingToRequestFocusInPopUpWindow();

		Font font = new JLabel().getFont();
		biggerFont = new Font(font.getName(), font.getStyle(), font.getSize() + 2);
		
		mainWindow.addComponentListener(new ComponentListener() {
			@Override
			public void componentShown(ComponentEvent e) {
				autoSuggestionPopUpWindow.setVisible(false);
			}
			
			@Override
			public void componentResized(ComponentEvent e) {
				autoSuggestionPopUpWindow.setVisible(false);
			}
			
			@Override
			public void componentMoved(ComponentEvent e) {
				autoSuggestionPopUpWindow.setVisible(false);
			}
			
			@Override
			public void componentHidden(ComponentEvent e) {
				autoSuggestionPopUpWindow.setVisible(false);
			}
		});
	}
	
	
	public void refreshData(ArrayList<String> words, Map<String, PedestrianAccessEntity> map) {
		this.map = map;
		setDictionary(words);
	}

	@SuppressWarnings("serial")
	private void addKeyBindingToRequestFocusInPopUpWindow() {
		textField.getInputMap(JComponent.WHEN_FOCUSED).put(KeyStroke.getKeyStroke(KeyEvent.VK_DOWN, 0, true),
				"Down released");
		textField.getActionMap().put("Down released", new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent ae) {
				for (int i = 0; i < suggestionsPanel.getComponentCount(); i++) {
					if (suggestionsPanel.getComponent(i) instanceof SuggestionLabel) {
						((SuggestionLabel) suggestionsPanel.getComponent(i)).setFocused(true);
						autoSuggestionPopUpWindow.toFront();
						autoSuggestionPopUpWindow.requestFocusInWindow();
						suggestionsPanel.requestFocusInWindow();
						suggestionsPanel.getComponent(i).requestFocusInWindow();
						break;
					}
				}
			}
		});
		suggestionsPanel.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
				.put(KeyStroke.getKeyStroke(KeyEvent.VK_DOWN, 0, true), "Down released");
		suggestionsPanel.getActionMap().put("Down released", new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent ae) {
				ArrayList<SuggestionLabel> sls = getAddedSuggestionLabels();
				int max = sls.size();
				if (max > 1) {
					for (int i = 0; i < max; i++) {
						SuggestionLabel label = sls.get(i);
						if (label.isFocused() && i < (max-1)) {
							label.setFocused(false);
							label = sls.get(i+1);
							label.setFocused(true);
							autoSuggestionPopUpWindow.toFront();
							autoSuggestionPopUpWindow.requestFocusInWindow();
							suggestionsPanel.requestFocusInWindow();
							suggestionsPanel.getComponent(i+1).requestFocusInWindow();
							break;
						}
					}
				} 
				else {
					autoSuggestionPopUpWindow.setVisible(false);
					setFocusToTextField();
					checkForAndShowSuggestions();
				}
			}
		});
		
		
		suggestionsPanel.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
				.put(KeyStroke.getKeyStroke(KeyEvent.VK_UP, 0, true), "Up released");
		suggestionsPanel.getActionMap().put("Up released", new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent ae) {
				ArrayList<SuggestionLabel> sls = getAddedSuggestionLabels();
				int max = sls.size();
				if (max > 1) {
					for (int i = 0; i < max; i++) {
						SuggestionLabel label = sls.get(i);
						if (label.isFocused() && i > 0) {
							label.setFocused(false);
							label = sls.get(i-1);
							label.setFocused(true);
							autoSuggestionPopUpWindow.toFront();
							autoSuggestionPopUpWindow.requestFocusInWindow();
							suggestionsPanel.requestFocusInWindow();
							suggestionsPanel.getComponent(i-1).requestFocusInWindow();
							break;
						}
					}
				} 
				else {
					autoSuggestionPopUpWindow.setVisible(false);
					setFocusToTextField();
					checkForAndShowSuggestions();
				}
			}
		});
		
	}

	private void setFocusToTextField() {
		container.toFront();
		container.requestFocusInWindow();
		textField.requestFocusInWindow();
	}

	public ArrayList<SuggestionLabel> getAddedSuggestionLabels() {
		ArrayList<SuggestionLabel> sls = new ArrayList<>();
		for (int i = 0; i < suggestionsPanel.getComponentCount(); i++) {
			if (suggestionsPanel.getComponent(i) instanceof SuggestionLabel) {
				SuggestionLabel sl = (SuggestionLabel) suggestionsPanel.getComponent(i);
				sls.add(sl);
			}
		}
		return sls;
	}

	private void checkForAndShowSuggestions() {
		typedWord = textField.getText();
		suggestionsPanel.removeAll();

		// used to calcualte size of JWindow as new Jlabels are added
		tW = 0;
		tH = 0;

		boolean added = wordTyped(typedWord);

		if (!added) {
			if (autoSuggestionPopUpWindow.isVisible()) {
				autoSuggestionPopUpWindow.setVisible(false);
			}
		} else {
			showPopUpWindow();
			setFocusToTextField();
		}
	}

	protected void addWordToSuggestions(String word) {
		SuggestionLabel suggestionLabel = new SuggestionLabel(word, Main.secondColor, Main.firstColor, this);

		calculatePopUpWindowSize(suggestionLabel);

		suggestionsPanel.add(suggestionLabel);
	}

	private void calculatePopUpWindowSize(JLabel label) {
		// so we can size the JWindow correctly
		if (tW < label.getPreferredSize().width + 10) {
			tW = label.getPreferredSize().width + 10;
		}
		tH += label.getPreferredSize().height;
	}

	private void showPopUpWindow() {
		autoSuggestionPopUpWindow.getContentPane().add(suggestionsPanel);
		autoSuggestionPopUpWindow.setMinimumSize(new Dimension(textField.getWidth(), 30));
		autoSuggestionPopUpWindow.setSize(tW, tH + 6);
		autoSuggestionPopUpWindow.setVisible(true);

		int windowX = 0;
		int windowY = 0;

		windowX = container.getX() + textField.getX() + 14;
		if (suggestionsPanel.getHeight() > autoSuggestionPopUpWindow.getMinimumSize().height) {
			windowY = container.getY() + textField.getParent().getY() + textField.getHeight()
					+ autoSuggestionPopUpWindow.getMinimumSize().height + 8;
		} else {
			windowY = container.getY() + textField.getParent().getY() + textField.getHeight()
					+ autoSuggestionPopUpWindow.getHeight() + 8;
		}

		autoSuggestionPopUpWindow.setLocation(windowX, windowY);
		autoSuggestionPopUpWindow.setMinimumSize(new Dimension(textField.getWidth(), 30));
		autoSuggestionPopUpWindow.revalidate();
		autoSuggestionPopUpWindow.repaint();

	}

	public void setDictionary(ArrayList<String> words) {
		dictionary.clear();
		if (words == null) {
			return;// so we can call constructor with null value for dictionary
					// without exception thrown
		}
		for (String word : words) {
			dictionary.add(word);
		}
	}

	public JWindow getAutoSuggestionPopUpWindow() {
		return autoSuggestionPopUpWindow;
	}

	public Window getContainer() {
		return container;
	}

	public JTextField getTextField() {
		return textField;
	}

	public void addToDictionary(String word) {
		dictionary.add(word);
	}

	protected boolean wordTyped(String typedWord) {

		if (typedWord.isEmpty()) {
			return false;
		}

		boolean suggestionAdded = false;

		for (String word : dictionary) {
			boolean fullymatches = true;
			for (int i = 0; i < typedWord.length(); i++) {// each string in the
															// word
				if (!typedWord.toLowerCase().startsWith(String.valueOf(word.toLowerCase().charAt(i)), i)) {
					fullymatches = false;
					break;
				}
			}
			if (fullymatches) {
				addWordToSuggestions(word);
				suggestionAdded = true;
			}
		}
		return suggestionAdded;
	}
	
	public void hide(){
		autoSuggestionPopUpWindow.setVisible(false);
	}

	@SuppressWarnings("serial")
	class SuggestionLabel extends JLabel {

		private boolean focused = false;
		private final JWindow autoSuggestionsPopUpWindow;
		private final JTextField textField;
		private Color suggestionsTextColor;

		public SuggestionLabel(String string, final Color borderColor, Color suggestionsTextColor,
				AutoCompleteOld autoSuggestor) {
			super(string);
			super.setFont(biggerFont);
			this.suggestionsTextColor = suggestionsTextColor;
			this.textField = autoSuggestor.getTextField();
			this.autoSuggestionsPopUpWindow = autoSuggestor.getAutoSuggestionPopUpWindow();

			initComponent();
		}

		private void initComponent() {
			setFocusable(true);
			setForeground(suggestionsTextColor);

			addMouseListener(new MouseAdapter() {
				@Override
				public void mouseClicked(MouseEvent me) {
					super.mouseClicked(me);
					replaceWithSuggestedText();
					autoSuggestionsPopUpWindow.setVisible(false);
				}
			});

			getInputMap(JComponent.WHEN_FOCUSED).put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0, true),
					"Enter released");
			getActionMap().put("Enter released", new AbstractAction() {
				@Override
				public void actionPerformed(ActionEvent ae) {
					replaceWithSuggestedText();
					autoSuggestionsPopUpWindow.setVisible(false);
				}
			});
			
		}

		public void setFocused(boolean focused) {
			if (focused) {
				setBackground(Main.firstColor);
				setForeground(Main.secondColor);
			} else {
				setBackground(Color.WHITE);
				setForeground(Main.firstColor);
			}
			repaint();
			this.focused = focused;
		}

		public boolean isFocused() {
			return focused;
		}

		private void replaceWithSuggestedText() {
			String suggestedWord = getText();
			textField.setText(suggestedWord);
			setAcesso(map.get(suggestedWord));
			nextComponent.requestFocus();
		}
	}
	
	private void setAcesso(PedestrianAccessEntity acesso) {
		try {
			setAcesso.invoke(container, acesso);
		} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
			e.printStackTrace();
		}
	}
	
	public void setEnabled(Boolean enabled){
		this.enabled = enabled;
	}

}
