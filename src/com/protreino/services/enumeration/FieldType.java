package com.protreino.services.enumeration;

public enum FieldType {
	
	/**
	 * Um JTextField simples. 
	 * Podem ser definidos máximo e minimo de caracteres. Pode ser definido como campo numerico.
	 * O valor padrão pode ser uma string qualquer.
	 * Por padrão, é um campo obrigatório e portanto não pode ser nulo ou vazio. Pode ser alterado para deixar de ser obrigatório.
	 */
	TEXT,
	
	/**
	 * Um JCheckBox. 
	 * Os valores booleanos são representados pelas strings "true" e "false". 
	 * O valor padrão deve ser definido como "true" ou "false".
	 */
	CHECKBOX,
	
	/**
	 * Um JComboBox de SelectItem. 
	 * Deve ser passada uma string com os valores da lista do combobox no formato "LABEL_VALUE;LABEL_VALUE". 
	 * Exemplo: "Horário_clockwise;Antihorário_anticlockwise;Ambos_both".
	 * Podem ser passados quantos valores LABEL_VALUE forem necessários.
	 * O valor padrão deve ser definido no padrão "LABEL_VALUE".
	 */
	COMBOBOX,
	
	/**
	 * Um JComboBox predefinido com os dois valores booleanos "Sim_true;Não_false". 
	 * O valor padrão pode ser definido como "Sim_true" ou "Não_false", ou apenas como "true" ou "false".
	 */
	YES_NO_COMBOBOX,
	
	/**
	 * Um campo composto por dois JTextField.
	 * Podem ser definidos máximo e minimo de caracteres.
	 * O retorno e o valor padrão seguem o formato "TEXTO1;TEXTO2".
	 */
	MESSAGE_LINES,
	
	/**
	 * Um JComboBox com uma sequência numérica, apenas para números INTEIROS.
	 * Os valores da lista devem ser definidos no formato "INICIO;PASSO;FIM". Por exemplo, a string "1;1;9" resulta na lista 1 2 3 4 5 6 7 8 9. 
	 * O valor padrão pode ser definido apenas como "VALOR".
	 * Por padrão, é um campo obrigatório e portanto não pode ser nulo ou vazio. Pode ser alterado para deixar de ser obrigatório.
	 */
	NUMERIC_LIST,
	
	/**
	 * Um botão para carregar uma imagem e um imageView para visualizar a imagem carregada.
	 * A imagem é convertida para uma string base64.
	 * Não é obrigatório.
	 */
	IMAGE,
	
	/**
	 * Um botao que abre um color picker. 
	 */
	COLOR_CHOOSER;

}
