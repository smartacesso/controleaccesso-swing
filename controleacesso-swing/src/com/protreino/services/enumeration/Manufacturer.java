package com.protreino.services.enumeration;

import java.util.ArrayList;
import java.util.List;

import com.protreino.services.devices.AlmitecDevice;
import com.protreino.services.devices.ComputerIdDevice;
import com.protreino.services.devices.ControlIDUHFDevice;
import com.protreino.services.devices.ControlIdDevice;
import com.protreino.services.devices.Device;
import com.protreino.services.devices.FacialDevice;
import com.protreino.services.devices.LcDevice;
import com.protreino.services.devices.NitgenDevice;
import com.protreino.services.devices.ServerDevice;
import com.protreino.services.devices.TopDataAcessoDevice;
import com.protreino.services.devices.TopDataDevice;
import com.protreino.services.devices.TopDataExpedidoraDevice;
import com.protreino.services.devices.UsbDevice;
import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.to.FieldTO;
import com.protreino.services.utils.Utils;

public enum Manufacturer {
	
	NENHUM,
	COMPUTER_ID,
	PROVEU,
	TOP_DATA,
	TOP_DATA_ACESSO,
	TOP_DATA_EXPEDIDORA,
	HENRY_8X,
	HENRY_7X,
	TECNIBRA,
	CONTROL_ID,
	CONTROL_ID_UHF,
	RWTECH,
	COMM,
	USB,
	NITGEN,
	TOLETUS,
	SYSTEMTEC,
	SERVER,
	LC_DEVICE,
	FACIAL,
	ALMITEC;
	
	public String toString(){
		if (this.equals(NENHUM))
			return "<Selecione>";
		if (this.equals(COMPUTER_ID))
			return "Computer ID";
		if (this.equals(PROVEU))
			return "Proveu";
		if (this.equals(TOP_DATA))
			return "TopData Catraca";
		if (this.equals(TOP_DATA_ACESSO))
			return "TopData Acesso";
		if (this.equals(TOP_DATA_EXPEDIDORA))
			return "TopData Expedidora";
		if (this.equals(HENRY_8X))
			return "Henry 8X";
		if (this.equals(HENRY_7X))
			return "Henry 7X";
		if (this.equals(TECNIBRA))
			return "Tecnibra";
		if (this.equals(CONTROL_ID))
			return "Control ID";
		if (this.equals(CONTROL_ID_UHF))
			return "Control ID Antena UHF";
		if (this.equals(RWTECH))
			return "RWTech";
		if (this.equals(COMM))
			return "COMM";
		if (this.equals(USB))
			return "USB";
		if (this.equals(NITGEN))
			return "Nitgen";
		if (this.equals(TOLETUS))
			return "Toletus";
		if (this.equals(SYSTEMTEC))
			return "Systemtec";
		if (this.equals(SERVER))
			return "Servidor";
		if (this.equals(FACIAL))
			return "Reconhecimento facial";
		if(this.equals(LC_DEVICE))
			return "Leitor LC";
		if(this.equals(ALMITEC))
			return "Catraca Almitec";
		return "";
	}
	
	public static Manufacturer valueFromImport(String string) {
		if ("Computer ID".equals(string))
			return Manufacturer.COMPUTER_ID;
		if ("Proveu".equals(string))
			return Manufacturer.PROVEU;
		if ("TopData Catraca".equals(string))
			return Manufacturer.TOP_DATA;
		if ("TopData Acesso".equals(string))
			return Manufacturer.TOP_DATA_ACESSO;
		if ("TopData Expedidora".equals(string))
			return Manufacturer.TOP_DATA_EXPEDIDORA;
		if ("Henry 8X".equals(string))
			return Manufacturer.HENRY_8X;
		if ("Henry 7X".equals(string))
			return Manufacturer.HENRY_7X;
		if ("Tecnibra".equals(string))
			return Manufacturer.TECNIBRA;
		if ("Control ID".equals(string))
			return Manufacturer.CONTROL_ID;
		if ("Control ID Antena UHF".equals(string))
			return Manufacturer.CONTROL_ID_UHF;
		if ("RWTech".equals(string))
			return Manufacturer.RWTECH;
		if ("COMM".equals(string))
			return Manufacturer.COMM;
		if ("USB".equals(string))
			return Manufacturer.USB;
		if ("Nitgen".equals(string))
			return Manufacturer.NITGEN;
		if ("Toletus".equals(string))
			return Manufacturer.TOLETUS;
		if ("Systemtec".equals(string))
			return Manufacturer.SYSTEMTEC;
		if ("Servidor".equals(string))
			return Manufacturer.SERVER;
		if ("Reconhecimento facial".equals(string))
			return Manufacturer.FACIAL;
		if("Leitor LC".equals(string))
			return Manufacturer.LC_DEVICE;
		if("Catraca Almitec".equals(string))
			return Manufacturer.ALMITEC;
		return null;
	}
	
	public Boolean useLogin(){
		if (this.equals(CONTROL_ID) || this.equals(CONTROL_ID_UHF))
			return true;
		else
			return false;
	}
	
	public Boolean usePassword(){
		// A senha da Proveu Ã© definida nas configuracoes da catraca
		if (this.equals(CONTROL_ID) || this.equals(CONTROL_ID_UHF))
			return true;
		else
			return false;
	}
	
	public DeviceType getType(){
		if (this.equals(COMPUTER_ID)
				|| this.equals(NITGEN)
					|| this.equals(LC_DEVICE))
			return DeviceType.BIOMETRIC_READER;
		else if (this.equals(FACIAL))
			return DeviceType.CAMERA;
		else if(this.equals(SERVER))
			return DeviceType.SERVER;
		else
			return DeviceType.TICKET_GATE;
	}
	
	public String getIconName(){
		if (this.equals(COMPUTER_ID))
			return "leitor_computer_id.png";
		if (this.equals(PROVEU))
			return "catraca_proveu.png";
		if (this.equals(TOP_DATA))
			return "catraca_topdata.png";
		if (this.equals(TOP_DATA_ACESSO))
			return "acesso_topdata_inner1.gif";
		if (this.equals(TOP_DATA_EXPEDIDORA))
			return "acesso_topdata_expedidora.png";
		if (this.equals(HENRY_8X))
			return "catraca_henry.png";
		if (this.equals(HENRY_7X))
			return "catraca_henry.png";
		if (this.equals(TECNIBRA))
			return "catraca_tecnibra.png";
		if (this.equals(CONTROL_ID))
			return "catraca_control_id.png";
		if (this.equals(CONTROL_ID_UHF))
			return "antena_control_id_uhf.png";
		if (this.equals(RWTECH))
			return "catraca_rwtech.png";
		if (this.equals(NITGEN))
			return "leitor_nitgen.png";
		if (this.equals(TOLETUS))
			return "catraca_toletus.png";
		if (this.equals(SYSTEMTEC))
			return "systemtec.png";
		if (this.equals(SERVER))
			return "server.png";
		if (this.equals(FACIAL))
			return "facial.png";
		if(this.equals(LC_DEVICE))
			return "leitor_lc.png";
		if(this.equals(ALMITEC))
			return "catraca_almitec.png";
		return "";
	}
	
	public Device recoverDevice(DeviceEntity deviceEntity){
		
		//TODO : converter lista de string de catracas vinculadas
		//em lista de objetos em cada dispositivo
		
		if (this.equals(COMPUTER_ID))
			return new ComputerIdDevice(deviceEntity);
		//if (this.equals(PROVEU))
		//	return new ProveuDevice(deviceEntity);
		if (this.equals(TOP_DATA))
			return new TopDataDevice(deviceEntity);
		if (this.equals(TOP_DATA_ACESSO))
			return new TopDataAcessoDevice(deviceEntity);
		if (this.equals(TOP_DATA_EXPEDIDORA))
			return new TopDataExpedidoraDevice(deviceEntity);
		//if (this.equals(HENRY_8X))
		//	return new Henry8XDevice(deviceEntity);
		//if (this.equals(HENRY_7X))
		//if (this.equals(TECNIBRA))
		//	return new TecnibraDevice(deviceEntity);
		if (this.equals(CONTROL_ID))
			return new ControlIdDevice(deviceEntity);
		if (this.equals(CONTROL_ID_UHF))
			return new ControlIDUHFDevice(deviceEntity);
		//if (this.equals(RWTECH))
		//	return new RWTechDevice(deviceEntity);
		//if (this.equals(COMM))
		//	return new CommDevice(deviceEntity);
		if (this.equals(USB))
			return new UsbDevice(deviceEntity);
		if (this.equals(NITGEN))
			return new NitgenDevice(deviceEntity);
		//if (this.equals(TOLETUS))
		//	return new ToletusDevice(deviceEntity);
		//if (this.equals(SYSTEMTEC))
		//	return new SystemtecDevice(deviceEntity);
		if (this.equals(SERVER))
			return new ServerDevice(deviceEntity);
		if (this.equals(FACIAL))
			return new FacialDevice(deviceEntity);
		if(this.equals(LC_DEVICE))
			return new LcDevice(deviceEntity);
		if(this.equals(ALMITEC))
			return new AlmitecDevice(deviceEntity);
		return null;
	}
	
	public Device getNewDevice(String identifier){
		if (this.equals(COMPUTER_ID))
			return new ComputerIdDevice(identifier);
		//if (this.equals(PROVEU))
		//	return new ProveuDevice(identifier);
		if (this.equals(TOP_DATA))
			return new TopDataDevice(identifier);
		if (this.equals(TOP_DATA_ACESSO))
			return new TopDataAcessoDevice(identifier);
		if (this.equals(TOP_DATA_EXPEDIDORA))
			return new TopDataExpedidoraDevice(identifier);
		//if (this.equals(HENRY_8X))
		//	return new Henry8XDevice(identifier);
		//if (this.equals(HENRY_7X))
		//	return new Henry7XDevice(identifier);
		//if (this.equals(TECNIBRA))
		//	return new TecnibraDevice(identifier);
		if (this.equals(CONTROL_ID))
			return new ControlIdDevice(identifier);
		if (this.equals(CONTROL_ID_UHF))
			return new ControlIDUHFDevice(identifier);
		//if (this.equals(RWTECH))
		//	return new RWTechDevice(identifier);
		//if (this.equals(COMM))
		//	return new CommDevice();
		if (this.equals(USB))
			return new UsbDevice();
		if (this.equals(NITGEN))
			return new NitgenDevice(identifier);
		//if (this.equals(TOLETUS))
		//	return new ToletusDevice(identifier);
		//if (this.equals(SYSTEMTEC))
		//	return new SystemtecDevice(identifier);
		if (this.equals(SERVER))
			return new ServerDevice(identifier);
		if (this.equals(FACIAL))
			return new FacialDevice(identifier);
		if(this.equals(LC_DEVICE))
			return new LcDevice(identifier);
		if(this.equals(ALMITEC))
			return new AlmitecDevice(identifier);
		return null;
	}
	
	public List<FieldTO> getFields(){
		
		List<FieldTO> fields = new ArrayList<FieldTO>();
		if (this.equals(COMPUTER_ID)
				|| this.equals(PROVEU)
				|| this.equals(COMM)
				|| this.equals(USB)
				|| this.equals(NITGEN)) {
			return null;
		
		} else if (this.equals(TOP_DATA) || this.equals(TOP_DATA_ACESSO) || this.equals(TOP_DATA_EXPEDIDORA)){
			fields.add(new FieldTO("Número do inner", FieldType.TEXT, "1"));
			fields.add(new FieldTO("Número da porta", FieldType.TEXT, "3570"));
			if(this.equals(TOP_DATA_EXPEDIDORA)) {
				String[] opcoes = {"ENTRADA", "SAIDA"};
				fields.add(new FieldTO("Selecione o tipo", FieldType.COMBOBOX, null, opcoes));
			}
		
		} else if (this.equals(HENRY_8X) || this.equals(HENRY_7X)){
			fields.add(new FieldTO("Número IP do dispositivo", FieldType.TEXT, "192.168.0.200"));
			fields.add(new FieldTO("Número da porta", FieldType.TEXT, "3000"));
		
		} else if (this.equals(TECNIBRA)){
			fields.add(new FieldTO("Número IP do dispositivo", FieldType.TEXT, "192.168.10.145"));
			fields.add(new FieldTO("Número da porta", FieldType.TEXT, "2051"));
			fields.add(new FieldTO("Número do terminal", FieldType.TEXT, "01"));
		
		} else if (this.equals(CONTROL_ID) || this.equals(CONTROL_ID_UHF)){
			fields.add(new FieldTO("Número IP do dispositivo", FieldType.TEXT, "192.168.100.200"));
			fields.add(new FieldTO("Ip deste computador", FieldType.COMBOBOX, null, Utils.getAllLocalIps()));
			fields.add(new FieldTO("Número da porta deste computador", FieldType.TEXT, "2050"));
		
		} else if (this.equals(RWTECH)){
			fields.add(new FieldTO("Número IP do dispositivo", FieldType.TEXT, "192.168.0.20"));
			fields.add(new FieldTO("Número da porta", FieldType.TEXT, "1001"));
		
		} else if (this.equals(TOLETUS)){
			fields.add(new FieldTO("Número IP do dispositivo", FieldType.TEXT, "192.168.0.125"));
			fields.add(new FieldTO("Número da porta", FieldType.TEXT, "1001"));
		
		} else if (this.equals(SYSTEMTEC)){
			fields.add(new FieldTO("Número IP do dispositivo", FieldType.TEXT, "192.168.0.125"));
			fields.add(new FieldTO("Número da porta", FieldType.TEXT, "1001"));
		
		} else if (this.equals(SERVER)){
			fields.add(new FieldTO("Número IP do servidor", FieldType.TEXT, "192.168.0.100"));
			fields.add(new FieldTO("Porta do servidor", FieldType.TEXT, "2020"));
		
		} else if (this.equals(FACIAL)) {
			String[] opcoes = {"USB", "IP"};
			fields.add(new FieldTO("Selecione o tipo de camÃªra", FieldType.COMBOBOX, null, opcoes));
		
		} else if(this.equals(LC_DEVICE)) {
			fields.add(new FieldTO("Número do leitor", FieldType.TEXT, "1"));
		}else if(this.equals(ALMITEC)) {
			fields.add(new FieldTO("Numero IP do servidor", FieldType.TEXT, "192.168.0.190"));
			fields.add(new FieldTO("Porta do servidor", FieldType.TEXT, "2000"));
			fields.add(new FieldTO("Porta do servidor", FieldType.TEXT, "2001"));
			fields.add(new FieldTO("Porta do servidor", FieldType.TEXT, "2000"));
		}
		return fields;
	}
	
	public List<FieldTO> getCameraFacialFields(String tipoCamera) {
		List<FieldTO> fields = new ArrayList<FieldTO>();
		
		if("USB".equals(tipoCamera)) {
			fields.add(new FieldTO("Selecione uma cÃ¢mera", FieldType.COMBOBOX, null, Utils.getAvailableCameras()));
			
		} else if("IP".equals(tipoCamera)) {
			fields.add(new FieldTO("URL da camÃªra", FieldType.TEXT, "http://192.168.2.148:4747/"));
			fields.add(new FieldTO("Usuário (se necessario)", FieldType.TEXT, ""));
			fields.add(new FieldTO("Senha (se necessÃ¡rio)", FieldType.TEXT, ""));
		}
		
		return fields;
	}
	
	/**
	 * Indica se Ã© possível obter uma lista com os usuários cadastrados na catraca
	 * @return
	 */
	public Boolean giveListRegisteredUsers(){
		/*if (this.equals(CONTROL_ID)) // muito lento para ser feito toda hora
			return true;*/
		return false;
		
		// CONTROL_ID fornece rapido
		// TOPDATA fornece mas Ã© lento
		// HENRY 8X fornece e nao testei para saber se Ã© rapido
	}
	
	
	/**
	 * Indica se o cadastro de usuários Ã© iniciado diretamente pela catraca usando o id do usuário
	 * @return
	 */
	public Boolean isRegistrationProcessStartedOnDevice(){
		if (this.equals(CONTROL_ID) || this.equals(CONTROL_ID_UHF))
			return false;
		if (this.equals(RWTECH))
			return false;
		if (this.equals(SYSTEMTEC))
			return false;
		// if (this.equals(TOP_DATA)) informacao Ã© definida por configuracao especifica do dispositivo
		return true;
	}
	
	public int getSamplesCount(){
		if (this.equals(COMPUTER_ID))
			return 4;
		if (this.equals(NITGEN))
			return 2;
		if (this.equals(LC_DEVICE))
			return 3;
		return 0;
	}
}
