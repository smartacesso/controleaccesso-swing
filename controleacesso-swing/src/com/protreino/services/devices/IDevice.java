package com.protreino.services.devices;

import java.io.Serializable;
import java.util.Set;

import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.enumeration.DeviceMode;

public interface IDevice extends Serializable {
	
	/**
	 * Conectar no disposivo
	 * @param args
	 * @throws Exception
	 */
	public void connect(String... args) throws Exception;
	
	/**
	 * Desconectar o dispositivo
	 * @throws Exception
	 */
	public void disconnect(String... args) throws Exception;
	
	/**
	 * Cria configuração
	 */
	public void createDefaultConfiguration();
	
	/**
	 * Envia configuração
	 * @throws Exception
	 */
	public void sendConfiguration() throws Exception;
	
	/**
	 * Libera acesso
	 */
	public void allowAccess();
	
	/**
	 * Acesso não permitido
	 */
	public void denyAccess();
	
	/**
	 * Processa digital para salvar
	 * @param obj
	 */
	public void processSampleForEnrollment(Object obj);
	
	/**
	 * Processa digital para liberar
	 * @param obj
	 */
	public void processAccessRequest(Object obj);
	
	/**
	 * Altera modo entre salvar / liberar com digital
	 * @param mode
	 */
	public void switchMode(DeviceMode mode);
	
	/**
	 * Retorna um set com os ids dos usuários cadastrados ou retorna nulo caso nao seja possivel obter essa lista
	 * @return
	 * @throws Exception
	 */
	public Set<Integer> getRegisteredUserList() throws Exception;
	
	/**
	 * Retorna uma string vazia em caso de sucesso ou uma mensagem em caso de erro
	 * @param athleteAccessEntity
	 * @return
	 */
	public String cadastrateUser(PedestrianAccessEntity athleteAccessEntity);
	
	/**
	 * Retorna uma string vazia em caso de sucesso ou uma mensagem em caso de erro
	 * @param athleteAccessEntity
	 * @return
	 */
	public String removeUser(PedestrianAccessEntity athleteAccessEntity);
	
	public boolean isConnected();
	
	public boolean isBusy();
	
	public String getIdentifier();
	
	public byte[] getSample();
	
	public byte[] getTemplate();
	
}
