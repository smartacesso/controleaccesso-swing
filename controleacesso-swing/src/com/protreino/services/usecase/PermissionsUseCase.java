package com.protreino.services.usecase;

import static com.protreino.services.constants.Permissions.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import javax.swing.JOptionPane;

import com.protreino.services.entity.UserEntity;
import com.protreino.services.enumeration.PerfilAcesso;;

public class PermissionsUseCase {

	public boolean hasAccessPreferencesButton(final UserEntity userEntity) {
		return Objects.nonNull(userEntity) 
				&& userEntity.hasPermission(PREFERENCES_BUTTON);
	}
	
	public boolean hasAccessHikivisionSyncDevicesButton(final UserEntity userEntity) {
		return Objects.nonNull(userEntity) 
				&& userEntity.hasPermission(SYNC_DEVICES_HIKIVISION_BUTTON);
	}
	
	public boolean hasCadastrarPedestre(final UserEntity userEntity) {
		return Objects.nonNull(userEntity) 
				&& userEntity.hasPermission(CADASTRAR_PEDESTRE);
	}
	
	public boolean hasCadastrarVisitante(final UserEntity userEntity) {
		return Objects.nonNull(userEntity) 
				&& userEntity.hasPermission(CADASTRAR_VISITANTE);
	}
	
	public boolean hasAdicionarDecvice(final UserEntity userEntity) {
		return Objects.nonNull(userEntity) 
				&& userEntity.hasPermission(ADICIONAR_DEVICE);
	}
	
	public boolean hasRemoverDecvice(final UserEntity userEntity) {
		return Objects.nonNull(userEntity) 
				&& userEntity.hasPermission(REMOVER_DEVICE);
	}
	
	public boolean hasLiberarAcesso(final UserEntity userEntity) {
		return Objects.nonNull(userEntity) 
				&& userEntity.hasPermission(LIBERAR_ACESSO);
	}
	
	public static int exibeDialogoSemPermissao() {
		Object[] options = { "OK" };
		int result = JOptionPane.showOptionDialog(null, "Você não tem permissão para executar essa ação",
				"Sem permissão", 0, JOptionPane.PLAIN_MESSAGE, null, options, null);
		return result;
	}
	
	public static List<String> getDefaultPermissions(final PerfilAcesso perfilAcesso) {
		if(perfilAcesso == PerfilAcesso.OPERADOR) {
			return Arrays.asList(CADASTRAR_PEDESTRE, CADASTRAR_VISITANTE);
		}
		
		if(perfilAcesso == PerfilAcesso.GERENTE) {
			return Arrays.asList(CADASTRAR_PEDESTRE, CADASTRAR_VISITANTE, ADICIONAR_DEVICE, REMOVER_DEVICE, LIBERAR_ACESSO);
		}
		
		if(perfilAcesso == PerfilAcesso.ADMINISTRADOR) {
			return Arrays.asList(SYNC_DEVICES_HIKIVISION_BUTTON, PREFERENCES_BUTTON, CADASTRAR_PEDESTRE, CADASTRAR_VISITANTE, ADICIONAR_DEVICE,
					REMOVER_DEVICE, LIBERAR_ACESSO);
		}
		
		return new ArrayList<>();
	}
	
}
