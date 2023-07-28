package com.protreino.services.to;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.io.OutputStream;
import gnu.io.CommPortIdentifier;  
import gnu.io.SerialPort;  
import gnu.io.SerialPortEvent; 
import gnu.io.SerialPortEventListener; 
import java.util.Enumeration;

import javax.swing.JOptionPane;

import com.protreino.services.main.Main;
import com.protreino.services.utils.Utils;

public class SerialCommunication implements SerialPortEventListener {
	
	public SerialPort serialPort;

	private BufferedReader input;
	private OutputStream output;
	private static final int TIME_OUT = 2000;
	private static final int DATA_RATE = 9600;

	@SuppressWarnings("rawtypes")
	public String initialize() {
		
		serialPort = null;
		String portName = null;
		
		// Verifica se a dll rxtxSerial.dll estão na pasta SysWOW64 (64bits) ou System32 (32bits)
		File dll = new File(System.getenv("WINDIR") + "\\" + (Utils.isWindows64bits() ? "SysWOW64" : "System32") +"\\rxtxSerial.dll");
		if (!dll.exists()) {
			JOptionPane.showMessageDialog(Main.mainScreen, "É necessário copiar 'rxtxSerial.dll' para a pasta '" + 
					(Utils.isWindows64bits() ? "SysWOW64" : "System32") + "'", "Biblioteca necessária", JOptionPane.PLAIN_MESSAGE);
			return null;
		}
		
		CommPortIdentifier portId = null;
		Enumeration portEnum = CommPortIdentifier.getPortIdentifiers();
		
		if (portEnum.hasMoreElements()) {
			CommPortIdentifier currPortId = (CommPortIdentifier) portEnum.nextElement();
			portId = currPortId;
			portName = currPortId.getName();
		}
		if (portId == null) {
			System.out.println("Could not find COM port.");
			return null;
		}

		try {
			serialPort = (SerialPort) portId.open(this.getClass().getName(), TIME_OUT);

			serialPort.setSerialPortParams(DATA_RATE, SerialPort.DATABITS_8, SerialPort.STOPBITS_1,
					SerialPort.PARITY_NONE);

			input = new BufferedReader(new InputStreamReader(serialPort.getInputStream()));
			output = serialPort.getOutputStream();

			serialPort.addEventListener(this);
			serialPort.notifyOnDataAvailable(true);
			
		} 
		catch (Exception e) {
			System.err.println(e.toString());
			portName = null;
		}
		return portName;
	}
	
	public synchronized void write(byte[] array) {
		try {
			output.write(array, 0, array.length);
			output.flush();
		}
		catch (Exception e){
			e.printStackTrace();
		}
	}

	/**
	 * This should be called when you stop using the port. This will prevent
	 * port locking on platforms like Linux.
	 */
	public synchronized void close() {
		if (serialPort != null) {
			serialPort.removeEventListener();
			serialPort.close();
		}
	}

	/**
	 * Handle an event on the serial port. Read the data and print it.
	 */
	public synchronized void serialEvent(SerialPortEvent oEvent) {
	    if (oEvent.getEventType() == SerialPortEvent.DATA_AVAILABLE) {
	        try {
	            String inputLine=input.readLine();
	            System.out.println("PORTA SERIAL <-- " + inputLine);
	        } catch (Exception e) {
	            System.err.println(e.toString());
	        }
	    }
	    // Ignore all the other eventTypes, but you should consider the other ones.
	}
	
}
