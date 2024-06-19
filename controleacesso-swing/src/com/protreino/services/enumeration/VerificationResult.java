package com.protreino.services.enumeration;

import com.protreino.services.utils.Utils;

public enum VerificationResult {

	ALLOWED,
	AUTHORIZED,
	NOT_ALLOWED,
	NOT_ALLOWED_FACE_REQUIRED,
	NOT_FOUND,
	ERROR,
	NOT_ALLOWED_TODAY,
	NOT_ALLOWED_NOW,
	TOLERANCE_PERIOD,
	ALLOWED_ONLY_ONCE,
	NOT_ALLOWED_ORIGEM,
	NOT_ALLOWED_SENSOR,
	NOT_ALLOWED_BOX;
	
	public String getMessage() {
		if(this.equals(ALLOWED))
			return Utils.getPreference("messageAllowed");
		if(this.equals(AUTHORIZED))
			return Utils.getPreference("messageAccessAuthorized");
		if(this.equals(NOT_ALLOWED))
			return Utils.getPreference("messageNotAllowed");
		if(this.equals(NOT_FOUND))
			return Utils.getPreference("messageNotFound");
		if(this.equals(ERROR))
			return Utils.getPreference("messageError");
		if(this.equals(NOT_ALLOWED_TODAY))
			return Utils.getPreference("messageNotAllowedToday");
		if(this.equals(NOT_ALLOWED_NOW))
			return Utils.getPreference("messageNotAllowedNow");
		if(this.equals(TOLERANCE_PERIOD))
			return Utils.getPreference("messageTolerancePeriod");
		if(this.equals(ALLOWED_ONLY_ONCE))
			return Utils.getPreference("messageAllowedOnlyOnce");
		if(this.equals(NOT_ALLOWED_ORIGEM))
			return Utils.getPreference("messageNotAllowedOrigem");
		if(this.equals(NOT_ALLOWED_SENSOR))
			return Utils.getPreference("messageNotAllowedSensor");
		if(this.equals(NOT_ALLOWED_FACE_REQUIRED))
			return Utils.getPreference("messageNotAllowedFaceRequired");
		if(this.equals(NOT_ALLOWED_BOX))
			return Utils.getPreference("messageNotAllowedBox");
		
		return "";
	}
	
}
