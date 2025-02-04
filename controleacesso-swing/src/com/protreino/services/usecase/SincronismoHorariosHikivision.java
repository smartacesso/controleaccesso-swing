package com.protreino.services.usecase;

import java.net.SocketException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;
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
	private HikivisionUseCases hikivisionUseCases = new HikivisionUseCases();
	
	public void execute() {
		List<RegraEntity> regrasComHorario = regraRepository.buscaRegrasComHorario();
		if(Objects.isNull(regrasComHorario) || regrasComHorario.isEmpty()) {
			System.out.println("Sem regras para sincronizar");			
			return;
		}
		System.out.println("Quantidade de regras : " + regrasComHorario.size());
		regrasComHorario.forEach(regra -> {
			PlanoHorarioHikivision planoHorarios = montaPlanoHorario(regra.getHorarios());
			
			Integer idPlanoHorario = getIdPlanoHorario(regra,regrasComHorario);
			//chamar hibernateUseCases para cadastrara horarios
			hikivisionUseCases.sincronizarHorarioHIkivision(idPlanoHorario, planoHorarios);
			
			Integer idTemplate = getIdTemplate(regra,regrasComHorario);		
			//chamar hibernateUsecases para cadastrar Templates
			hikivisionUseCases.sincronizarTemplateHIkivision(idTemplate,idPlanoHorario, regra.getNome());
			
			if(Objects.isNull(regra.getIdPlano()) || Objects.isNull(regra.getIdTemplate())) {
				regra.setIdPlano(idPlanoHorario);
				regra.setIdTemplate(idTemplate);
				HibernateAccessDataFacade.save(RegraEntity.class, regra);
			}
		});
	}
	
	private PlanoHorarioHikivision montaPlanoHorario(List<HorarioEntity> horarios) {
        Map<String, AtomicInteger> idCounterByDay = new HashMap<>();
        List<DiaHIkivision> dias = new ArrayList<>();

        for (HorarioEntity horario : horarios) {
            String horarioInicio = formataHora(horario.getHorarioInicio());
            String horarioFim = formataHora(horario.getHorarioFim());

            for (char c : horario.getDiasSemana().toCharArray()) {
                String diaSemana = DiaSemana.intForDia(Character.getNumericValue(c)).name();

                // Obtém o contador de ID para o dia da semana
                idCounterByDay.putIfAbsent(diaSemana, new AtomicInteger(1));
                int id = idCounterByDay.get(diaSemana).getAndIncrement();

                dias.add(new DiaHIkivision(
                    diaSemana,
                    id,
                    true,
                    new TimeSegment(horarioInicio, horarioFim)
                ));
            }
        }

        return new PlanoHorarioHikivision(true, dias);
    }
	
	private Integer getIdPlanoHorario(RegraEntity regraAtual, List<RegraEntity> regras ) {
		if(Objects.nonNull(regraAtual.getIdPlano())) {
			return Integer.valueOf(regraAtual.getIdPlano());
		}
		
		return regras.stream()
		.map(RegraEntity::getIdPlano)
		.filter(Objects::nonNull)
		.max(Comparator.naturalOrder())
		.map(id -> id+1)
		.orElse(1);

	}
	
	
	private Integer getIdTemplate(RegraEntity regraAtual, List<RegraEntity> regras ) {
		if(Objects.nonNull(regraAtual.getIdTemplate())) {
			return Integer.valueOf(regraAtual.getIdTemplate());
		}
		return regras.stream()
		.map(RegraEntity::getIdTemplate)
		.filter(Objects::nonNull)
		.max(Comparator.naturalOrder())
		.map(id -> id+1)
		.orElse(1);

	}
	
	
	public static void main(String[] args) throws InterruptedException, SocketException {
		SincronismoHorariosHikivision sincronismoHorariosHikivision = new SincronismoHorariosHikivision();
		sincronismoHorariosHikivision.execute();

	}
	
	private String formataHora(Date data) {
	    Calendar cal = Calendar.getInstance();
	    cal.setTime(data);
	    cal.add(Calendar.HOUR_OF_DAY, -3); // Remove 3 horas
	    SimpleDateFormat sdf = new SimpleDateFormat("HH:mm:ss");
	    return sdf.format(cal.getTime());
	}

}
