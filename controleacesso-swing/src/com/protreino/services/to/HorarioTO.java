package com.protreino.services.to;

import java.util.Calendar;
import java.util.Date;

import com.protreino.services.entity.HorarioEntity;
import com.protreino.services.entity.PedestreRegraEntity;

public class HorarioTO {

	private Long id;
	private String nome;
	private String status;
	private String diasSemana;
	private Date horarioInicio;
	private Date horarioFim;
	
	private Long qtdeDeCreditos;
	private Long idPedestreRegra;
	
	public HorarioEntity toEntity(final PedestreRegraEntity pedestreRegra) {
		final HorarioEntity horarioEntity = new HorarioEntity();
		horarioEntity.setId(id);
		horarioEntity.setNome(nome);
		horarioEntity.setStatus(status);
		horarioEntity.setDiasSemana(diasSemana);
		horarioEntity.setHorarioInicio(getHorarioInicio());
		horarioEntity.setHorarioFim(getHorarioFim());
		horarioEntity.setQtdeDeCreditos(qtdeDeCreditos);
		horarioEntity.setPedestreRegra(pedestreRegra);
		
		return horarioEntity;
	}
	
	public Long getId() {
		return id;
	}
	public void setId(Long id) {
		this.id = id;
	}
	public String getNome() {
		return nome;
	}
	public void setNome(String nome) {
		this.nome = nome;
	}
	public String getStatus() {
		return status;
	}
	public void setStatus(String status) {
		this.status = status;
	}
	public String getDiasSemana() {
		return diasSemana;
	}
	public void setDiasSemana(String diasSemana) {
		this.diasSemana = diasSemana;
	}
	public Date getHorarioInicio() {
		
	    Calendar calendar = Calendar.getInstance();
	    calendar.setTime(horarioInicio);
	    // Ajuste a hora, por exemplo, subtraindo 3 horas
	    calendar.add(Calendar.HOUR_OF_DAY, -3);
	    return calendar.getTime();
		
	}
	public void setHorarioInicio(Date horarioInicio) {
		this.horarioInicio = horarioInicio;
	}
	
	public Date getHorarioFim() {
	    Calendar calendar = Calendar.getInstance();
	    calendar.setTime(horarioFim);
	    // Ajuste a hora, por exemplo, subtraindo 3 horas
	    calendar.add(Calendar.HOUR_OF_DAY, -3);
	    return calendar.getTime();
	}
	
	public void setHorarioFim(Date horarioFim) {
		this.horarioFim = horarioFim;
	}
	public Long getQtdeDeCreditos() {
		return qtdeDeCreditos;
	}
	public void setQtdeDeCreditos(Long qtdeDeCreditos) {
		this.qtdeDeCreditos = qtdeDeCreditos;
	}
	public Long getIdPedestreRegra() {
		return idPedestreRegra;
	}
	public void setIdPedestreRegra(Long idPedestreRegra) {
		this.idPedestreRegra = idPedestreRegra;
	}
	
}
