package com.protreino.services.enumeration;
public enum Finger {
	
	RIGHT_INDEX(1),
	RIGHT_THUMB(2),
	RIGHT_MIDDLE(3),
	RIGHT_RING(4),
	RIGHT_LITTLE(5),
	LEFT_INDEX(6),
	LEFT_THUMB(7),
	LEFT_MIDDLE(8),
	LEFT_RING(9),
	LEFT_LITTLE(10);
	
	private Integer position;
	
	private Finger(Integer position) {
		this.position = position;
	}
	
	
	
	
	public String toString() {
		
		if(this.equals(RIGHT_INDEX))
			return "Indicador direito";
		else if(this.equals(RIGHT_THUMB))
			return "Polegar direito";
		else if(this.equals(RIGHT_MIDDLE))
			return "Médio direito";
		else if(this.equals(RIGHT_RING))
			return "Anelar direito";
		else if(this.equals(RIGHT_LITTLE))
			return "Mínimo direito";
		else if(this.equals(LEFT_INDEX))
			return "Indicador esquerdo";
		else if(this.equals(LEFT_THUMB))
			return "Polegar esquerdo";
		else if(this.equals(LEFT_MIDDLE))
			return "Medio esquerdo";
		else if(this.equals(LEFT_RING))
			return "Anelar esquerdo";
		else if(this.equals(LEFT_LITTLE))
			return "Minimo esquerdo";
		return "";
	}
	


	public static Finger valueFromImport(String importValue) {
		if(importValue.equals("Indicador direito")){
			return Finger.RIGHT_INDEX;
		}else if(importValue.equals("Polegar direito")){
			return Finger.RIGHT_THUMB;
		}else if(importValue.equals("Medio direito")){
			return Finger.RIGHT_MIDDLE;
		}else if(importValue.equals("Anelar direito")){
			return Finger.RIGHT_RING;
		}else if(importValue.equals("Minimo direito")){
			return Finger.RIGHT_LITTLE;
		}else if(importValue.equals("Indicador esquerdo")){
			return Finger.LEFT_INDEX;
		}else if(importValue.equals("Polegar esquerdo")){
			return Finger.LEFT_THUMB;
		}else if(importValue.equals("Medio esquerdo")){
			return Finger.LEFT_MIDDLE;
		}else if(importValue.equals("Anelar esquerdo")){
			return Finger.LEFT_RING;
		}else if(importValue.equals("Minimo esquerdo")){
			return Finger.LEFT_LITTLE;
		}
		else
			return null;
	}




	public Integer getPosition() {
		return position;
	}




	public void setPosition(Integer position) {
		this.position = position;
	}

	
}
