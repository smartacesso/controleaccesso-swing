package com.protreino.services.usecase;

import java.util.Date;

import com.protreino.services.constants.Origens;
import com.protreino.services.enumeration.VerificationResult;

public class TestePedestreRegraUseCase {

	private final ProcessAccessRequestUseCase processAccessRequestUseCase = new ProcessAccessRequestUseCase();
	
	public VerificationResult execute(final String cardNumber, final Date date) {
		final Object[] result = processAccessRequestUseCase.processAccessRequest(cardNumber, Origens.ORIGEM_TESTE_PEDESTRE_REGRA, date);
		
		return (VerificationResult) result[0];
	}
}
