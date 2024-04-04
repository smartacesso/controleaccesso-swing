package com.protreino.services.usecase;

import java.util.Objects;

import com.protreino.services.entity.EmpresaEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.main.Main;
import com.protreino.services.repository.EmpresaRepository;
import com.protreino.services.utils.SMSUtils;
import com.protreino.services.utils.Utils;

public class EnviaSmsDeRegistroUseCase {
	
	private final EmpresaRepository empresaRepository = new EmpresaRepository();

	public void execute(PedestrianAccessEntity pedestre) {
		if (!Main.loggedUser.temChaveIntegracaoComtele()) {
			return;
		}

		if (!Boolean.TRUE.equals(pedestre.getEnviaSmsAoPassarNaCatraca()) || Objects.isNull(pedestre.getIdEmpresa())) {
			return;
		}

		EmpresaEntity empresa = empresaRepository.buscaEmpresaById(pedestre.getIdEmpresa());

		if (Objects.isNull(empresa) || Objects.isNull(empresa.getTelefone()) || empresa.getTelefone().isEmpty()) {
			return;
		}

		try {
			SMSUtils sms = new SMSUtils(Main.loggedUser.getChaveIntegracaoComtele());
			String message = pedestre.getName() + " " + Utils.getPreference("messageSMSAfterPassInDevice");

			String numeroTelefone = formataNumeroTelefone(empresa.getCelular());

			sms.enviaSMS(numeroTelefone, message);

		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	private String formataNumeroTelefone(final String telefone) {
		return telefone.replace(" ", "").replace("-", "").replace("(", "").replace(")", "");
	}
	
}
