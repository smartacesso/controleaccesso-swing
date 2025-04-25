package com.protreino.services.to;

import java.util.ArrayList;
import java.util.Date;

import com.protreino.services.entity.CargoEntity;
import com.protreino.services.entity.CentroCustoEntity;
import com.protreino.services.entity.DepartamentoEntity;

public class EmpresaTO {
	
	private Long id;
	private String nome;
	private String cnpj;
	private String email;
	private String telefone;
	private String celular;
	private String status;
	
	private Boolean removed;
	private Date dataRemovido;
	
	private Boolean autoAtendimentoLiberado;

	private ArrayList<CargoEntity> cargos;
	private ArrayList<CentroCustoEntity> centros;
	private ArrayList<DepartamentoEntity> departamentos;
	
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
	public String getCnpj() {
		return cnpj;
	}
	public void setCnpj(String cnpj) {
		this.cnpj = cnpj;
	}
	public String getEmail() {
		return email;
	}
	public void setEmail(String email) {
		this.email = email;
	}
	public String getTelefone() {
		return telefone;
	}
	public void setTelefone(String telefone) {
		this.telefone = telefone;
	}
	public String getCelular() {
		return celular;
	}
	public void setCelular(String celular) {
		this.celular = celular;
	}
	public ArrayList<CargoEntity> getCargos() {
		return cargos;
	}
	public void setCargos(ArrayList<CargoEntity> cargos) {
		this.cargos = cargos;
	}
	public ArrayList<CentroCustoEntity> getCentros() {
		return centros;
	}
	public void setCentros(ArrayList<CentroCustoEntity> centros) {
		this.centros = centros;
	}
	public ArrayList<DepartamentoEntity> getDepartamentos() {
		return departamentos;
	}
	public void setDepartamentos(ArrayList<DepartamentoEntity> departamentos) {
		this.departamentos = departamentos;
	}
	public Boolean getRemoved() {
		return removed;
	}
	public void setRemoved(Boolean removed) {
		this.removed = removed;
	}
	public Date getDataRemovido() {
		return dataRemovido;
	}
	public void setDataRemovido(Date dataRemovido) {
		this.dataRemovido = dataRemovido;
	}
	public String getStatus() {
		return status;
	}
	public void setStatus(String status) {
		this.status = status;
	}
	public Boolean getAutoAtendimentoLiberado() {
		return autoAtendimentoLiberado;
	}
	public void setAutoAtendimentoLiberado(Boolean autoAtendimentoLiberado) {
		this.autoAtendimentoLiberado = autoAtendimentoLiberado;
	}
}
