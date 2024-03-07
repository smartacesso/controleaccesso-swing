package com.protreino.services.repository;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.protreino.services.entity.PermissaoEntity;
import com.protreino.services.entity.UserEntity;
import com.protreino.services.enumeration.PerfilAcesso;
import com.protreino.services.enumeration.Permissions;

public class PermissaoRepository {

	public List<PermissaoEntity> findAllByUserEntity(final UserEntity userEntity) {
		if(Objects.isNull(userEntity) || Objects.isNull(userEntity.getPerfilAcesso())) {
			return new ArrayList<>();
		}
		
		if(PerfilAcesso.ADMINISTRADOR == userEntity.getPerfilAcesso()) {
			List<PermissaoEntity> permissaoEntities = new ArrayList<>();
			permissaoEntities.add(new PermissaoEntity(1L, Permissions.PREFERENCES_BUTTON));
			permissaoEntities.add(new PermissaoEntity(2L, Permissions.SYNC_DEVICES_HIKIVISION_BUTTON));
			
			return permissaoEntities;
		}
		
		if(PerfilAcesso.GERENTE == userEntity.getPerfilAcesso()) {
			List<PermissaoEntity> permissaoEntities = new ArrayList<>();
			permissaoEntities.add(new PermissaoEntity(2L, Permissions.SYNC_DEVICES_HIKIVISION_BUTTON));
			
			return permissaoEntities;
		}
		
		if(PerfilAcesso.OPERADOR == userEntity.getPerfilAcesso()) {
			List<PermissaoEntity> permissaoEntities = new ArrayList<>();
			
			return permissaoEntities;
		}
		
		return new ArrayList<>();
	}
}
