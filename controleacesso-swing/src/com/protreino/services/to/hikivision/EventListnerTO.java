package com.protreino.services.to.hikivision;

import java.util.Date;

public class EventListnerTO {

	private String ipAddress;
	private String ipv6Address;
	private Integer portNo;
	private String protocol;
	private String macAddress;
	private Integer channelID;
	private Integer activePostCount;
	private Date dateTime;
	private String eventType;
	private String eventState;
	private String eventDescription;
	private AccessControllerEvent AccessControllerEvent;
	
	public class AccessControllerEvent {
		private String deviceName;
		private Integer majorEventType;
		private Integer subEventType;
		private String cardNo;
		private Integer cardType;
		private String name;
		private Integer cardReaderKind;
		private Integer cardReaderNo;
		private Integer verifyNo;
		private String employeeNoString;
		private Integer serialNo;
		private String userType;
		private String currentVerifyMode;
		private Integer frontSerialNo;
		private String attendanceStatus;
		private String label;
		private Integer statusValue;
		private String mask;
		private String helmet;
		private Integer picturesNumber;
		private Boolean purePwdVerifyEnable;
		
		public String getDeviceName() {
			return deviceName;
		}
		public void setDeviceName(String deviceName) {
			this.deviceName = deviceName;
		}
		public Integer getMajorEventType() {
			return majorEventType;
		}
		public void setMajorEventType(Integer majorEventType) {
			this.majorEventType = majorEventType;
		}
		public Integer getSubEventType() {
			return subEventType;
		}
		public void setSubEventType(Integer subEventType) {
			this.subEventType = subEventType;
		}
		public String getCardNo() {
			return cardNo;
		}
		public void setCardNo(String cardNo) {
			this.cardNo = cardNo;
		}
		public Integer getCardType() {
			return cardType;
		}
		public void setCardType(Integer cardType) {
			this.cardType = cardType;
		}
		public String getName() {
			return name;
		}
		public void setName(String name) {
			this.name = name;
		}
		public Integer getCardReaderKind() {
			return cardReaderKind;
		}
		public void setCardReaderKind(Integer cardReaderKind) {
			this.cardReaderKind = cardReaderKind;
		}
		public Integer getCardReaderNo() {
			return cardReaderNo;
		}
		public void setCardReaderNo(Integer cardReaderNo) {
			this.cardReaderNo = cardReaderNo;
		}
		public Integer getVerifyNo() {
			return verifyNo;
		}
		public void setVerifyNo(Integer verifyNo) {
			this.verifyNo = verifyNo;
		}
		public String getEmployeeNoString() {
			return employeeNoString;
		}
		public void setEmployeeNoString(String employeeNoString) {
			this.employeeNoString = employeeNoString;
		}
		public Integer getSerialNo() {
			return serialNo;
		}
		public void setSerialNo(Integer serialNo) {
			this.serialNo = serialNo;
		}
		public String getUserType() {
			return userType;
		}
		public void setUserType(String userType) {
			this.userType = userType;
		}
		public String getCurrentVerifyMode() {
			return currentVerifyMode;
		}
		public void setCurrentVerifyMode(String currentVerifyMode) {
			this.currentVerifyMode = currentVerifyMode;
		}
		public Integer getFrontSerialNo() {
			return frontSerialNo;
		}
		public void setFrontSerialNo(Integer frontSerialNo) {
			this.frontSerialNo = frontSerialNo;
		}
		public String getAttendanceStatus() {
			return attendanceStatus;
		}
		public void setAttendanceStatus(String attendanceStatus) {
			this.attendanceStatus = attendanceStatus;
		}
		public String getLabel() {
			return label;
		}
		public void setLabel(String label) {
			this.label = label;
		}
		public Integer getStatusValue() {
			return statusValue;
		}
		public void setStatusValue(Integer statusValue) {
			this.statusValue = statusValue;
		}
		public String getMask() {
			return mask;
		}
		public void setMask(String mask) {
			this.mask = mask;
		}
		public String getHelmet() {
			return helmet;
		}
		public void setHelmet(String helmet) {
			this.helmet = helmet;
		}
		public Integer getPicturesNumber() {
			return picturesNumber;
		}
		public void setPicturesNumber(Integer picturesNumber) {
			this.picturesNumber = picturesNumber;
		}
		public Boolean getPurePwdVerifyEnable() {
			return purePwdVerifyEnable;
		}
		public void setPurePwdVerifyEnable(Boolean purePwdVerifyEnable) {
			this.purePwdVerifyEnable = purePwdVerifyEnable;
		}
	}

	public String getIpAddress() {
		return ipAddress;
	}

	public void setIpAddress(String ipAddress) {
		this.ipAddress = ipAddress;
	}

	public String getIpv6Address() {
		return ipv6Address;
	}

	public void setIpv6Address(String ipv6Address) {
		this.ipv6Address = ipv6Address;
	}

	public Integer getPortNo() {
		return portNo;
	}

	public void setPortNo(Integer portNo) {
		this.portNo = portNo;
	}

	public String getProtocol() {
		return protocol;
	}

	public void setProtocol(String protocol) {
		this.protocol = protocol;
	}

	public String getMacAddress() {
		return macAddress;
	}

	public void setMacAddress(String macAddress) {
		this.macAddress = macAddress;
	}

	public Integer getChannelID() {
		return channelID;
	}

	public void setChannelID(Integer channelID) {
		this.channelID = channelID;
	}

	public Integer getActivePostCount() {
		return activePostCount;
	}

	public void setActivePostCount(Integer activePostCount) {
		this.activePostCount = activePostCount;
	}

	public String getEventType() {
		return eventType;
	}

	public void setEventType(String eventType) {
		this.eventType = eventType;
	}

	public String getEventState() {
		return eventState;
	}

	public void setEventState(String eventState) {
		this.eventState = eventState;
	}

	public String getEventDescription() {
		return eventDescription;
	}

	public void setEventDescription(String eventDescription) {
		this.eventDescription = eventDescription;
	}

	public AccessControllerEvent getAccessControllerEvent() {
		return AccessControllerEvent;
	}

	public void setAccessControllerEvent(AccessControllerEvent AccessControllerEvent) {
		this.AccessControllerEvent = AccessControllerEvent;
	}

	public Date getDateTime() {
		return dateTime;
	}

	public void setDateTime(Date dateTime) {
		this.dateTime = dateTime;
	}
}
