package com.protreino.services.usecase;

import java.net.SocketException;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import com.protreino.services.entity.HorarioEntity;
import com.protreino.services.entity.RegraEntity;
import com.protreino.services.enumeration.DiaSemana;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.repository.RegraRepository;
import com.protreino.services.to.hikivision.DiaHIkivision;
import com.protreino.services.to.hikivision.PlanoHorarioHikivision;
import com.protreino.services.to.hikivision.TimeSegment;

public class SincronismoHorariosHikivision {
	public final RegraRepository regraRepository = new RegraRepository();
	private HikivisionUseCases hikivisionUseCases;
	
	public void execute() {
		List<RegraEntity> regrasComHorario = regraRepository.buscaRegrasComHorario();
		if(Objects.isNull(regrasComHorario) || regrasComHorario.isEmpty()) {
			System.out.println("Sem regras para sincronizar");			
			return;
		}
		
		regrasComHorario.forEach(regra -> {
			PlanoHorarioHikivision planoHorarios = montaPlanoHorario(regra.getHorarios());
			Integer idPlanoHorario = getIdPlanoHorario(regra,regrasComHorario);
			
			//chamar hibernateUseCases para cadastrara horarios
			hikivisionUseCases.sincronizarHorarioHIkivision(idPlanoHorario, planoHorarios);
			
			Integer idTemplate = getIdTemplate(regra,regrasComHorario);
			//chamar hibernateUsecases para cadastrar Templates
			
			if(Objects.isNull(regra.getIdPlano()) || Objects.isNull(regra.getIdTemplate())) {
				regra.setIdPlano(idPlanoHorario);
				regra.setIdTemplate(idTemplate);
				HibernateAccessDataFacade.save(RegraEntity.class, regra);
			}
		});
	}
	
	private PlanoHorarioHikivision montaPlanoHorario(List<HorarioEntity> horarios) {
	    List<DiaHIkivision> dias = horarios.stream()
	            .flatMap(horario -> horario.getDiasSemana().chars() // Converte string para chars
	                .mapToObj(c -> DiaSemana.intForDia(Character.getNumericValue(c))) // Mapeia para Enum
	                .map(dia -> new DiaHIkivision(dia.name(), 1, true, new TimeSegment(horario.getHorarioInicio(), horario.getHorarioFim()))) // Cria DiaHIkivision
	            )
	            .collect(Collectors.toList());

	        return new PlanoHorarioHikivision(true, dias);
	}
	
	private Integer getIdPlanoHorario(RegraEntity regraAtual, List<RegraEntity> regras ) {
		if(Objects.nonNull(regraAtual.getIdPlano())) {
			return Integer.valueOf(regraAtual.getIdPlano());
		}
		return null;
		

	}
	
	
	private Integer getIdTemplate(RegraEntity regraAtual, List<RegraEntity> regras ) {
		if(Objects.nonNull(regraAtual.getIdTemplate())) {
			return Integer.valueOf(regraAtual.getIdTemplate());
		}
		return null;
	}
	
	
	public static void main(String[] args) throws InterruptedException, SocketException {
		SincronismoHorariosHikivision sincronismoHorariosHikivision = new SincronismoHorariosHikivision();
		sincronismoHorariosHikivision.execute();

	}
	
}
