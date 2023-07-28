package com.protreino.services.enumeration;
public enum Finger {
	
	RIGHT_INDEX,
	RIGHT_THUMB,
	RIGHT_MIDDLE,
	RIGHT_RING,
	RIGHT_LITTLE,
	LEFT_INDEX,
	LEFT_THUMB,
	LEFT_MIDDLE,
	LEFT_RING,
	LEFT_LITTLE;
	
	public String toString() {
		
		if(this.equals(RIGHT_INDEX))
			return "Indicador direito";
		else if(this.equals(RIGHT_THUMB))
			return "Polegar direito";
		else if(this.equals(RIGHT_MIDDLE))
			return "M�dio direito";
		else if(this.equals(RIGHT_RING))
			return "Anelar direito";
		else if(this.equals(RIGHT_LITTLE))
			return "M�nimo direito";
		else if(this.equals(LEFT_INDEX))
			return "Indicador esquerdo";
		else if(this.equals(LEFT_THUMB))
			return "Polegar esquerdo";
		else if(this.equals(LEFT_MIDDLE))
			return "M�dio esquerdo";
		else if(this.equals(LEFT_RING))
			return "Anelar esquerdo";
		else if(this.equals(LEFT_LITTLE))
			return "M�nimo esquerdo";
		return "";
	}
	
	public static Finger valueFromImport(String importValue) {
		if(importValue.equals("Indicador direito")){
			return Finger.RIGHT_INDEX;
		}else if(importValue.equals("Polegar direito")){
			return Finger.RIGHT_THUMB;
		}else if(importValue.equals("M�dio direito")){
			return Finger.RIGHT_MIDDLE;
		}else if(importValue.equals("Anelar direito")){
			return Finger.RIGHT_RING;
		}else if(importValue.equals("M�nimo direito")){
			return Finger.RIGHT_LITTLE;
		}else if(importValue.equals("Indicador esquerdo")){
			return Finger.LEFT_INDEX;
		}else if(importValue.equals("Polegar esquerdo")){
			return Finger.LEFT_THUMB;
		}else if(importValue.equals("M�dio esquerdo")){
			return Finger.LEFT_MIDDLE;
		}else if(importValue.equals("Anelar esquerdo")){
			return Finger.LEFT_RING;
		}else if(importValue.equals("M�nimo esquerdo")){
			return Finger.LEFT_LITTLE;
		}
		else
			return null;
	}
}
