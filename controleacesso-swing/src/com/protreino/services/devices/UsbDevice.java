package com.protreino.services.devices;

import java.util.Set;

import javax.swing.SwingWorker;

import com.ftdichip.ftd2xx.FTD2xxException;
import com.ftdichip.ftd2xx.Service;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.enumeration.DeviceStatus;
import com.protreino.services.enumeration.Manufacturer;
import com.protreino.services.enumeration.NotificationType;
import com.protreino.services.main.Main;
import com.protreino.services.utils.Utils;

public class UsbDevice extends Device {
	
	private PlacaUSB placaUSB;
	
	public UsbDevice(DeviceEntity deviceEntity){
		this();
		this.deviceEntity = deviceEntity;
		this.name = deviceEntity.getName();
		this.athleteScreenConfig = deviceEntity.getAthleteScreenConfig();
	}
	
	public UsbDevice(){
		this.manufacturer = Manufacturer.USB;
		this.name = "USB";
	}
	
	@Override
	public void connect(String... args) throws Exception {
		if (Service.listDevices().length > 0) {
			placaUSB = new PlacaUSB(0);
			setStatus(DeviceStatus.CONNECTED);
			
			watchDogEnabled = true;
			watchDog = new SwingWorker<Void, Void>(){
				@Override
				protected Void doInBackground() throws Exception {
					while (watchDogEnabled) {
						try {
							if (!busy) {
								if (Service.listDevices().length > 0)
									setStatus(DeviceStatus.CONNECTED);
								else
									setStatus(DeviceStatus.DISCONNECTED);
							}
						} catch (Exception e) {
							e.printStackTrace();
							setStatus(DeviceStatus.DISCONNECTED);
		                
						} finally {
							Utils.sleep(5000);
						}
					}
					return null;
				}
			};
			watchDog.execute();
			
		} else {
			setStatus(DeviceStatus.DISCONNECTED);
			throw new Exception ("Sem catracas USB");
		}
	}
	
	@Override
	public void disconnect(String... args) throws Exception {
		super.disconnect();
		if (placaUSB != null)
			placaUSB.fechar();
		placaUSB = null;
		setStatus(DeviceStatus.DISCONNECTED);
	}
	
	@Override
	public void createDefaultConfiguration() {
	}
	
	@Override
	public void sendConfiguration() throws Exception {
	}
	
	@Override
	public void allowAccess() {
		try {
			System.out.println("\nENVIANDO SINAIS...");
			
			Thread thread = new Thread(new Runnable() {
				@Override
				public void run() {
					placaUSB.liberaEntrada();
				}
			});
			thread.setDaemon(true);
			thread.start();
			
		} catch (Throwable e) {
			e.printStackTrace();
		}
		
	}
	
	@Override
	public void denyAccess() {
	}
	
	@Override
	public void processSampleForEnrollment(Object obj) {
	}
	
	@Override
	public void processAccessRequest(Object obj) {
	}
	
	@Override
	public Set<Integer> getRegisteredUserList() throws Exception {
		return null;
	}
	
	@Override
	public String cadastrateUser(PedestrianAccessEntity athleteAccessEntity) {
		return null;
	}
	
	@Override
	public String removeUser(PedestrianAccessEntity athleteAccessEntity) {
		return null;
	}
	
	public boolean exist(){
		return placaUSB != null;
	}
	
	
	
	private class PlacaUSB {

		private com.ftdichip.ftd2xx.Device device;
		int loc = 0;
		
		public PlacaUSB(int loc) throws FTD2xxException {
			this.loc = loc;
			device = Service.listDevices()[loc];
			device.open();
			device.setBitBangMode(0x30,0x20);
			device.close();
		}
		
		public void liberaEntrada() {
			device = null;
			busy = true;
			try {
				boolean comandoEnviado;
				int cont, data, numero, tmpval;
				
				System.out.print("Listando os devices...  ");
				comandoEnviado = false;
				cont = 0;
				while (!comandoEnviado && cont <= 3) {
					try {
						cont++;
						System.out.print(cont + "  ");
						device = Service.listDevices()[loc];
						comandoEnviado = true;
					
					} catch (Exception e){
						Utils.sleep(200);
					}
				}
				if (!comandoEnviado) {
					Main.mainScreen.addEvento("Não foi possível encontrar o dispositivo USB");
					return;
				}
				
				System.out.print("\nAbrindo o device...  ");
				comandoEnviado = false;
				cont = 0;
				while (!comandoEnviado && cont <= 3) {
					try {
						cont++;
						System.out.print(cont + "  ");
						device.open();
						comandoEnviado = true;
					
					} catch (Exception e){
						Utils.sleep(200);
					}
				}
				if (!comandoEnviado) {
					Main.mainScreen.addEvento("Não foi possível comunicar com o dispositivo USB");
					return;
				}
				
				// abre porta 1
				System.out.print("\nAbrindo porta 1...  ");
				comandoEnviado = false;
				cont = 0;
				while (!comandoEnviado && cont <= 3) {
					try {
						cont++;
						System.out.print(cont + "  ");
						data = device.getBitBangMode();
						numero = 1;
						tmpval = ((numero | data) & 0xF);
						tmpval = tmpval | 0xF0;
						device.setBitBangMode(tmpval, 0x20);
						comandoEnviado = true;
					
					} catch (Exception e){
						Utils.sleep(200);
					}
				}
				if (!comandoEnviado) {
					Main.mainScreen.addEvento("Não foi possível abrir a porta 1");
					return;
				}
				
				// abre porta 2
				System.out.print("\nAbrindo porta 2...  ");
				comandoEnviado = false;
				cont = 0;
				while (!comandoEnviado && cont <= 3) {
					try {
						cont++;
						System.out.print(cont + "  ");
						data = device.getBitBangMode();
						numero = 2;
						tmpval = ((numero | data) & 0xF);
						tmpval = tmpval | 0xF0;
						device.setBitBangMode(tmpval, 0x20);
						comandoEnviado = true;
					
					} catch (Exception e){
						Utils.sleep(200);
					}
				}
				if (!comandoEnviado) {
					Main.mainScreen.addEvento("Não foi possível abrir a porta 2");
					return;
				}
				
				// aguarda
				System.out.println("\nAguardando...");
				Utils.sleep(1000);
				
				// fecha porta 1
				System.out.print("Fechando porta 1...  ");
				comandoEnviado = false;
				cont = 0;
				while (!comandoEnviado && cont <= 3) {
					try {
						cont++;
						System.out.print(cont + "  ");
						data = device.getBitBangMode();
						numero = 1;
						numero = 0xFF - numero; // complemento
						tmpval = ((numero & data) & 0xF);
						tmpval = tmpval | 0xF0;
						device.setBitBangMode(tmpval, 0x20);
						comandoEnviado = true;
					
					} catch (Exception e){
						Utils.sleep(200);
					}
				}
				if (!comandoEnviado) {
					Main.mainScreen.addEvento("Não foi possível fechar a porta 2");
					return;
				}
				
				// fecha porta 2
				System.out.print("\nFechando porta 2...  ");
				comandoEnviado = false;
				cont = 0;
				while (!comandoEnviado && cont <= 3) {
					try {
						cont++;
						System.out.print(cont + "  ");
						data = device.getBitBangMode();
						numero = 2;
						numero = 0xFF - numero; // complemento
						tmpval = ((numero & data) & 0xF);
						tmpval = tmpval | 0xF0;
						device.setBitBangMode(tmpval, 0x20);
						comandoEnviado = true;
					
					} catch (Exception e){
						Utils.sleep(200);
					}
				}
				if (!comandoEnviado) {
					Main.mainScreen.addEvento("Não foi possível fechar a porta 2");
					return;
				}
				
				
				System.out.print("\nFechando o device...  ");
				comandoEnviado = false;
				cont = 0;
				while (!comandoEnviado && cont <= 3) {
					try {
						cont++;
						System.out.print(cont + "  ");
						device.close();
						comandoEnviado = true;
					
					} catch (Exception e){
						Utils.sleep(200);
					}
				}
				if (!comandoEnviado) {
					Main.mainScreen.addEvento("Não foi possível fechar o dispositivo USB");
					return;
				}
				
				System.out.println("\nSINAIS ENVIADOS...");
				
			} catch (Exception e) {
				e.printStackTrace();
				Utils.createNotification("Erro ao enviar para a USB. " + e.getMessage(), NotificationType.BAD);
			
			} finally {
				busy = false;
			}
		}
		
		public int ligaSaida(int numero){
			device = null;
			try{
				device = Service.listDevices()[loc];
				device.open();
				
				int data = device.getBitBangMode();
				if (numero>4) 
					return -1;
				if (numero==3)
					numero  =4;
				else if( numero==4)
					numero=8;

				int tmpval = 0;
				tmpval = ((numero | data) & 0xF);

				tmpval = tmpval | 0xF0;
				device.setBitBangMode(tmpval, 0x20);
				fechar();
				return 0;
			
			} catch (Exception e) {
				e.printStackTrace();
				fechar();
				return -1;
			}
		}

		public int desligaSaida(int numero){
			device = null;
			try{
				device = Service.listDevices()[loc];
				device.open();
				
				int data = device.getBitBangMode();
				if (numero>4) return -1;

				if (numero==3) 
					numero  = 4;
				else if( numero==4)
					numero=8;
				
				numero = 0xFF - numero;// complemento

				int tmpval = 0;
				tmpval = ((numero & data) & 0xF);

				tmpval = tmpval | 0xF0;
				device.setBitBangMode(tmpval, 0x20);
				fechar();
				return 0;
			
			} catch (Exception e) {
				e.printStackTrace();
				fechar();
				return -1;
			}
		}

		public int leEntradas(){
			device = null;
			try{
				device = Service.listDevices()[loc];
				device.open();
				device.setLatencyTimer(16);
				int bit =  device.getBitBangMode();
				int valor = (bit & 0xf) / 4;
				System.out.println("bit" + Integer.toBinaryString(bit));
				fechar();
				return valor;
			
			} catch (Exception e) {
				e.printStackTrace();
				fechar();
				return -1;
			}
		}
		
		public void fechar() {
			try {
				device.close();
			} catch (Exception e) {}
		}
	}
	
}
