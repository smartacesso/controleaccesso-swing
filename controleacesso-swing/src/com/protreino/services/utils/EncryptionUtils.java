package com.protreino.services.utils;

import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.UUID;

/**
 * 
 * Criptografa e descriptografa Strings.
 * Poder ser utilizado para qualquer propósito
 * 
 * @author Gustavo Diniz
 *
 */
public final class EncryptionUtils {
	
	/**
	 * Criptografa String Usando o Método SHA-256
	 * 
	 * @param value
	 * @return
	 * @throws NoSuchAlgorithmException
	 * @throws UnsupportedEncodingException
	 */
	public static String encrypt(String value) throws NoSuchAlgorithmException, UnsupportedEncodingException{
		
		MessageDigest algorithm = MessageDigest.getInstance("SHA-256");
		byte messageDigest[] = algorithm.digest(value.getBytes("UTF-8"));
		 
		StringBuilder hexString = new StringBuilder();
		for (byte b : messageDigest) {
		  hexString.append(String.format("%02X", 0xFF & b));
		}
		
		return hexString.toString();
	}
	
	/**
	 * Retorna senha aleatória com 8 dígitos
	 * de 128 bits
	 * 
	 * @return
	 */
	public static String getRandomPassword(){
		
		UUID uuid = UUID.randomUUID();  
		String myRandom = uuid.toString();  
		return myRandom.substring(0,8);  
		
	}
	
	/**
	 * Retorna senha aleatória com 8 dígitos
	 * de 128 bits
	 * 
	 * @return
	 */
	public static String getRandomString(int qtd){
		
		UUID uuid = UUID.randomUUID();  
		String myRandom = uuid.toString();  
		return myRandom.substring(0,qtd);  
		
	}
	
	/**
	 * Testa encriptação
	 * 
	 * @param args
	 */
	public static void main(String [] args){
		
		try{
		
			System.out.println("Criptografa 'sescts18': " + encrypt("sescts18"));
			System.out.println("Criptografa '123456': " + encrypt("123456"));
			
			System.out.println("Senha aleatória: " + getRandomPassword());
			System.out.println("Senha aleatória: " + getRandomPassword());
			System.out.println("Senha aleatória: " + getRandomPassword());
			
		}catch (Exception e) {
			e.printStackTrace();
		}
		
	}

}
