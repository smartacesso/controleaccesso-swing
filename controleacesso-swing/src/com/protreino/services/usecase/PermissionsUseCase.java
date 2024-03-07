package com.protreino.services.usecase;

import java.util.List;
import java.util.Objects;

import com.protreino.services.entity.PermissaoEntity;
import com.protreino.services.entity.UserEntity;
import com.protreino.services.enumeration.Permissions;
import com.protreino.services.repository.PermissaoRepository;

public class PermissionsUseCase {

	private final PermissaoRepository permissaoRepository;

	public PermissionsUseCase() {
		this(new PermissaoRepository());
	}

	public PermissionsUseCase(final PermissaoRepository permissaoRepository) {
		this.permissaoRepository = permissaoRepository;
	}
	
	public boolean hasAccessPreferencesButton(final UserEntity userEntity) {
		return hasAccess(Permissions.PREFERENCES_BUTTON, userEntity);
	}
	
	public boolean hasAccessHikivisionSyncDevicesButton(final UserEntity userEntity) {
		return hasAccess(Permissions.SYNC_DEVICES_HIKIVISION_BUTTON, userEntity);
	}

	public boolean hasAccess(final Permissions permission, final UserEntity userEntity) {
		if(Objects.isNull(userEntity)) {
			return false;
		}

		List<PermissaoEntity> permissions = permissaoRepository.findAllByUserEntity(userEntity);

		if (Objects.isNull(permissions) || permissions.isEmpty()) {
			return false;
		}

		return permissions.stream()
				.filter(p -> p.getPermission() == permission)
				.findFirst()
				.isPresent();
	}
}
