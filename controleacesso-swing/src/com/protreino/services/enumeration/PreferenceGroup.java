package com.protreino.services.enumeration;

public enum PreferenceGroup {

	GENERAL, MESSAGES, ATHLETE_SCREEN, FACE_RECOGNIZER, HIKIVISION_FACE_RECOGONIZER, TOPDATA_FACE_RECOGONIZER,
	CONTROLID_FACE_RECOGONIZER, CONTROLID_FACE_IDENTIFIER;

	public String getName() {
		if (this.equals(GENERAL)) {
			return "Geral";
		}
		if (this.equals(MESSAGES)) {
			return "Mensagens";
		}
		if (this.equals(ATHLETE_SCREEN)) {
			return "Tela do pedestre";
		}
		if (this.equals(FACE_RECOGNIZER)) {
			return "Reconhecimento facial";
		}
		if (this.equals(HIKIVISION_FACE_RECOGONIZER)) {
			return "Reconhecimento Facial HikiVision";
		}
		if (this.equals(TOPDATA_FACE_RECOGONIZER)) {
			return "Reconhecimento Facial Topdata";
		}
		if (this.equals(CONTROLID_FACE_RECOGONIZER)) {
			return "Reconhecimento Facial Control ID";
		}
		if (this.equals(CONTROLID_FACE_IDENTIFIER)) {
			return "Reconhecimento Facial Control ID";
		}
		return "";
	}

}
