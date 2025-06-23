package com.protreino.services.repository;

import java.util.HashMap;
import java.util.Objects;

import com.protreino.services.devices.Device;
import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.main.Main;

public class DeviceRepository {

	public boolean isDeviceAlreadyExists(String identifier) {
		HashMap<String, Object> args = new HashMap<>();
		args.put("IDENTIFIER", identifier);
		
		DeviceEntity device = (DeviceEntity) HibernateAccessDataFacade.getUniqueResultWithParams(DeviceEntity.class,
				"DeviceEntity.findByIdentifier", args);
		
		return Objects.nonNull(device);
	}
	
	public static Device getDeviceByFullIdentifier(final String identifier) {
		if(Objects.isNull(Main.devicesList) || Main.devicesList.isEmpty() || Objects.isNull(identifier)) {
			return null;
		}
		
		for(Device device : Main.devicesList) {
			if(device.getName().endsWith(identifier)) {
				return device;
			}
		}
		
		return null;
	}
	
	public static Device getDeviceByName(final String deviceName) {
		if(Objects.isNull(Main.devicesList) || Main.devicesList.isEmpty() || Objects.isNull(deviceName)) {
			return null;
		}
		
		for(Device device : Main.devicesList) {
			if(deviceName.equals(device.getName())) {
				return device;
			}
		}
		
		return null;
	}
	
	public Device getDeviceByIdentifier(String identifier) {
		if (Objects.isNull(Main.devicesList) || Main.devicesList.isEmpty() || Objects.isNull(identifier)) {
			return null;
		}

		for (Device device : Main.devicesList) {
			if (identifier.equals(device.getIdentifier())) {
				return device;
			}
		}

		return null;
	}
	
	
	public static Device getDeviceByIdentifierNomeAlterado(String identifier) {
	    if (Objects.isNull(Main.devicesList) || Main.devicesList.isEmpty() || Objects.isNull(identifier)) {
	        return null;
	    }

	    String code = identifier.replaceAll("\\D+", ""); // extrai número

	    for (Device device : Main.devicesList) {
	        if (device.getIdentifier() != null && device.getIdentifier().contains(code + ";")) {
	            return device;
	        }
	    }

	    return null;
	}

	
}
