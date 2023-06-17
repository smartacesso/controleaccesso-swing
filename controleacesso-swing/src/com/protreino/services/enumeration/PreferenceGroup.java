package com.protreino.services.enumeration;

public enum PreferenceGroup {
	
	GENERAL,
	MESSAGES,
	ATHLETE_SCREEN,
	FACE_RECOGNIZER,
	HIKIVISION_FACE_RECOGONIZER;
	

	public String getName(){
		if(this.equals(GENERAL)) {
			return "Geral";			
		}
		if(this.equals(MESSAGES)) {
			return "Mensagens";			
		}
		if(this.equals(ATHLETE_SCREEN)) {
			return "Tela do pedestre";			
		}
		if(this.equals(FACE_RECOGNIZER)) {
			return "Reconhecimento facial";			
		}
		if(this.equals(HIKIVISION_FACE_RECOGONIZER)) {
			return "Reconhecimento Facial HikiVision";
		}
		return "";
	}

}
