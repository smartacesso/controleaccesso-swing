package com.protreino.services.enumeration;

public enum NotificationType {

	GOOD,
	BAD,
	BIRTHDAY;
	
	public String getIconName(){
		if (NotificationType.GOOD.equals(this))
			return "good_notification_icon.png";
		if (NotificationType.BAD.equals(this))
			return "bad_notification_icon.png";
		if (NotificationType.BIRTHDAY.equals(this))
			return "birthday_notification_icon.png";
		return "";
	}
	
	public String getSmallIconName(){
		if (NotificationType.GOOD.equals(this))
			return "good_notification_stick.png";
		if (NotificationType.BAD.equals(this))
			return "bad_notification_stick.png";
		if (NotificationType.BIRTHDAY.equals(this))
			return "birthday_notification_stick.png";
		return "";
	}
	
}
