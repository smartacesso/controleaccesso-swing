package com.protreino.services.usecase;

import java.util.Objects;

import com.protreino.services.devices.TopDataDevice;
import com.protreino.services.main.Main;

public class SyncTemplatesInTopDataDevices {

	public void execute() {
		if(Objects.isNull(Main.devicesList) || Main.devicesList.isEmpty()) {
			System.out.println("Sem devices para enviar templates");
			return;
		}
		
		Main.devicesList.forEach(device -> {
			if(device instanceof TopDataDevice) {
				final TopDataDevice topDataDevice = (TopDataDevice) device;
				topDataDevice.atualizaDigitaisLFD(false, false, null);
			}
		});
	}
}
