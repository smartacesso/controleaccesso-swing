package com.protreino.services.to;

import java.io.Serializable;

import com.protreino.services.devices.Device;
import com.protreino.services.enumeration.Manufacturer;

public class DeviceTO implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private String name;
	private String location;
	private String identifier;
	private Manufacturer manufacturer;
	private boolean isConnected;
	
	public DeviceTO() {
		
	}
	public DeviceTO(Device device) {
		this.name = device.getName();
		this.location = device.getLocation();
		this.identifier = device.getIdentifier();
		this.manufacturer = device.getManufacturer();
		this.isConnected = device.isConnected();
		
		
	}
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public String getLocation() {
		return location;
	}
	public void setLocation(String location) {
		this.location = location;
	}
	public String getIdentifier() {
		return identifier;
	}
	public void setIdentifier(String identifier) {
		this.identifier = identifier;
	}
	public Manufacturer getManufacturer() {
		return manufacturer;
	}
	public void setManufacturer(Manufacturer manufacturer) {
		this.manufacturer = manufacturer;
	}
	public boolean isConnected() {
		return isConnected;
	}
	public void setConnected(boolean isConnected) {
		this.isConnected = isConnected;
	}
	
}
