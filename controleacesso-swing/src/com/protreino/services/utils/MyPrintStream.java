package com.protreino.services.utils;

import java.io.FileOutputStream;
import java.io.PrintStream;
import java.text.SimpleDateFormat;

public class MyPrintStream extends PrintStream {
	
	//private FileOutputStream fileOutputStream;
	//private PrintStream originalOut;
	
	/*public MyPrintStream(FileOutputStream fileOutputStream, PrintStream originalOut){
		super(fileOutputStream);
		this.fileOutputStream = fileOutputStream;
		this.originalOut = originalOut;
	}*/
	
	private SimpleDateFormat sdf;
	
	public MyPrintStream(FileOutputStream fileOutputStream){
		super(fileOutputStream);
		sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss:sss");
	}
	
	@Override
	public void print(String string){
		super.print(string);
	}
	
	@Override
	public void println(String string){
		print(string + "\r\n");
	}
	
}
