package com.protreino.services.utils;

import java.util.Vector;

import javax.swing.SwingWorker;

import com.digitalpersona.onetouch.DPFPGlobal;
import com.digitalpersona.onetouch.readers.DPFPReaderDescription;
import com.digitalpersona.onetouch.readers.DPFPReadersCollection;
import com.ftdichip.ftd2xx.Service;
import com.nitgen.SDK.BSP.NBioBSPJNI;
import com.protreino.services.devices.ComputerIdDevice;
import com.protreino.services.devices.Device;
import com.protreino.services.devices.LcDevice;
import com.protreino.services.devices.NitgenDevice;
import com.protreino.services.enumeration.Manufacturer;

public class ScanDevices extends SwingWorker<Vector<Device>, Void> {
	
	private Manufacturer manufacturer;
	
	public ScanDevices(Manufacturer manufacturer){
		this.manufacturer = manufacturer;
	}
	
	@Override
	protected Vector<Device> doInBackground() {
		
		Vector<Device> dispositivosEncontrados = new Vector<Device>();
		
		try {
			if (Manufacturer.COMPUTER_ID.equals(manufacturer)) {
				DPFPReadersCollection readersCollectionComputerID = null;
				readersCollectionComputerID = DPFPGlobal.getReadersFactory().getReaders();
				if (readersCollectionComputerID.size() > 0){
					for (DPFPReaderDescription dpfpReaderDescription : readersCollectionComputerID) {
						ComputerIdDevice device = new ComputerIdDevice(dpfpReaderDescription.getSerialNumber() + ";" +  
								dpfpReaderDescription.getProductName());
						dispositivosEncontrados.add(device);
					}
				}
			}
			
//			if (Manufacturer.PROVEU.equals(manufacturer)) {
//				String stringEquipamentos = new ProveuUtil().getInterfaceJna().localizarEquipamentos();
//				if (stringEquipamentos != null && !stringEquipamentos.isEmpty()){
//					String[] equipamentos = stringEquipamentos.split("\r\n");
//					for (String equipamento : equipamentos){
//						if (!equipamento.isEmpty()){
//							String[] campos = equipamento.split(";");
//							String ip = campos[0];
//							String porta = campos[1];
//							ProveuDevice device = new ProveuDevice(ip + ";" + porta);
//							dispositivosEncontrados.add(device);
//						}
//					}
//				}
//			}
			
			if (Manufacturer.NITGEN.equals(manufacturer)) {
				NBioBSPJNI bsp = new NBioBSPJNI();
				NBioBSPJNI.DEVICE_ENUM_INFO deviceEnumInfo = bsp.new DEVICE_ENUM_INFO();
				bsp.EnumerateDevice(deviceEnumInfo);
				if (deviceEnumInfo.DeviceCount > 0) {
					for (int i = 0; i < deviceEnumInfo.DeviceCount; i++) {
						NitgenDevice device = new NitgenDevice(deviceEnumInfo.DeviceInfo[i].Description + ";" +
								Short.toString(deviceEnumInfo.DeviceInfo[i].NameID) + ";" + 
								Short.toString(deviceEnumInfo.DeviceInfo[i].Instance));
						dispositivosEncontrados.add(device);
					}
				}
			}
			
			if(Manufacturer.LC_DEVICE.equals(manufacturer)) {
				//TODO como buscar os dispositivos que sao LD_DEVICE
				com.ftdichip.ftd2xx.Device[] item = Service.listDevices();
				System.out.println(item.length);
				if(item != null && item.length > 0) {
					for (com.ftdichip.ftd2xx.Device device : item) {
						LcDevice d = new LcDevice(device.getDeviceDescriptor().toString());
						dispositivosEncontrados.add(d);
					}
				}
			}
			
		} catch (Exception e) {
			e.printStackTrace();
		}
		return dispositivosEncontrados;
	}
}
