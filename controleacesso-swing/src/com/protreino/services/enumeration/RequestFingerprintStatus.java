package com.protreino.services.enumeration;
public enum RequestFingerprintStatus {

	REQUESTED, PROCESSING, FINISHED, CANCELED;

	public String toString() {
		if (this.equals(REQUESTED))
			return "Requisitado";
		else if (this.equals(PROCESSING))
			return "Processando";
		else if (this.equals(FINISHED))
			return "Concluído";
		else if (this.equals(CANCELED))
			return "Cancelado";
		else
			return "";
	}

	public static RequestFingerprintStatus valueFromImport(String importValue) {
		if (importValue.equals("Requisitado"))
			return RequestFingerprintStatus.REQUESTED;
		else if (importValue.equals("Processando"))
			return RequestFingerprintStatus.PROCESSING;
		else if (importValue.equals("Concluído"))
			return RequestFingerprintStatus.FINISHED;
		else if (importValue.equals("Cancelado"))
			return RequestFingerprintStatus.CANCELED;
		else
			return null;
	}

}