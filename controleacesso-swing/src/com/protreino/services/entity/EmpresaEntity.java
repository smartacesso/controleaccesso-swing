package com.protreino.services.entity;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.hibernate.annotations.Fetch;
import org.hibernate.annotations.FetchMode;
import org.hibernate.annotations.Type;

import com.protreino.services.to.EmpresaTO;

@Entity
@Table(name="TB_EMPRESA")
@NamedQueries({
	@NamedQuery(name  = "EmpresaEntity.findAll", 
				query = "select obj "
				      + "from EmpresaEntity obj "
					  + "order by obj.id asc"),
	@NamedQuery(name  = "EmpresaEntity.findById", 
				query = "select obj from EmpresaEntity obj "
					  + "where obj.id = :ID order by obj.id asc"),
	@NamedQuery(name  = "EmpresaEntity.findAllActive",
				query = "select obj from EmpresaEntity obj "
					  + "where obj.status = 'ATIVO' "
					  + "and (obj.removed = false or obj.removed is null) "
					  + "order by obj.id asc")
})
@SuppressWarnings("serial")
public class EmpresaEntity extends BaseEntity implements ObjectWithId {
	
	@Id
	@Column(name="ID_EMPRESA", nullable=false, length=4)
	private Long id;
	
	@Column(name="NOME", nullable=true, length=255)
	private String nome;
	
	@Column(name="CNPJ", nullable=true, length=50)
	private String cnpj;
	
	@Column(name="EMAIL", nullable=true, length=100)
	private String email;
	
	@Column(name="TELEFONE", nullable=true, length=20)
	private String telefone;
	
	@Column(name="CELULAR", nullable=true, length=20)
	private String celular;
	
	@Column(name="ID_CLIENT", nullable=false, length=10)
	private String idClient;

	@Column(name="STATUS", nullable=true, length=20)
	private String status;
	
	@Type(type="org.hibernate.type.NumericBooleanType")
	@Column(name="REMOVED", nullable=true, length=30)
	private Boolean removed;
	
	@Type(type="org.hibernate.type.NumericBooleanType")
	@Column(name="AUTO_ATENDIMENTO", nullable=true, length=30)
	private Boolean autoAtendimentoLiberado;
	
	
	@Temporal( TemporalType.TIMESTAMP)
	@Column(name="DATA_REMOVIDO", nullable=true, length=11)
	private Date dataRemovido = null;
	
	@OneToMany(cascade=CascadeType.ALL, fetch=FetchType.EAGER, 
			   orphanRemoval=true, targetEntity=DepartamentoEntity.class,
			   mappedBy="empresa")
	@Fetch(FetchMode.SUBSELECT)
	private List<DepartamentoEntity> departamentos;
	
	@OneToMany(cascade=CascadeType.ALL, fetch=FetchType.EAGER, 
			   orphanRemoval=true, targetEntity=CentroCustoEntity.class,
			   mappedBy="empresa")
	@Fetch(FetchMode.SUBSELECT)
	private List<CentroCustoEntity> centros;
	
	@OneToMany(cascade=CascadeType.ALL, fetch=FetchType.EAGER, 
			   orphanRemoval=true, targetEntity=CargoEntity.class,
			   mappedBy="empresa")
	@Fetch(FetchMode.SUBSELECT)
	private List<CargoEntity> cargos;
	
	public EmpresaEntity() {}

	public EmpresaEntity(EmpresaTO empresa) {
		this.id = empresa.getId();
		this.nome = empresa.getNome();
		this.cnpj = empresa.getCnpj();
		this.email = empresa.getEmail();
		this.telefone = empresa.getTelefone();
		this.celular = empresa.getCelular();
		this.status = empresa.getStatus();
		this.removed = empresa.getRemoved();
		this.dataRemovido = empresa.getDataRemovido();
		this.autoAtendimentoLiberado = empresa.getAutoAtendimentoLiberado();
		
		if(empresa.getDepartamentos() != null) {
			this.departamentos = new ArrayList<>();
			this.departamentos.addAll(empresa.getDepartamentos());
			
			for(DepartamentoEntity d : this.departamentos)
				d.setEmpresa(this);
		}
		
		if(empresa.getCentros() != null) {
			this.centros = new ArrayList<>();
			this.centros.addAll(empresa.getCentros());
			
			for(CentroCustoEntity c : this.centros)
				c.setEmpresa(this);
		}
		
		if(empresa.getCargos() != null) {
			this.cargos = new ArrayList<>();
			this.cargos.addAll(empresa.getCargos());
			
			for(CargoEntity c : this.cargos)
				c.setEmpresa(this);
		}
	}
	
	public void update(EmpresaEntity empresa) {
		this.setNome(empresa.getNome() != null ? empresa.getNome() : null);
		this.setCnpj(empresa.getCnpj() != null ? empresa.getCnpj() : null);
		this.setEmail(empresa.getEmail() != null ? empresa.getEmail() : null);
		this.setCelular(empresa.getCelular() != null ? empresa.getCelular() : null);
		this.setStatus(empresa.getStatus() != null ? empresa.getStatus() : null);
		this.setRemoved(empresa.getRemoved() != null ? empresa.getRemoved() : null);
		this.setDataRemovido(empresa.getDataRemovido() != null ? empresa.getDataRemovido() : null);
		
		this.setAutoAtendimentoLiberado(empresa.getAutoAtendimentoLiberado() != null ? empresa.getAutoAtendimentoLiberado() : null);
		
		if(empresa.getDepartamentos() != null && !empresa.getDepartamentos().isEmpty()) {
			if(this.getDepartamentos() == null)
				this.setDepartamentos(new ArrayList<>());
			
			atualizaDepartamentos(empresa.getDepartamentos());
		}
		
		if(empresa.getCargos() != null && !empresa.getCargos().isEmpty()) {
			if(this.getCargos() == null)
				this.setCargos(new ArrayList<>());
			
			atualizaCargos(empresa.getCargos());
		}
		
		if(empresa.getCentros() != null && !empresa.getCentros().isEmpty()) {
			if(this.getCentros() == null)
				this.setCentros(new ArrayList<>());
			
			atualizaCentros(empresa.getCentros());
		}

		this.setDataAlteracao(new Date());
	}
	
	private void atualizaCentros(List<CentroCustoEntity> novosCentros) {
		for(CentroCustoEntity newCentro : novosCentros) {
			boolean centroExiste = false;
			
			for(CentroCustoEntity oldCentro : this.getCentros()) {
				if(!oldCentro.getId().equals(newCentro.getId()))
					continue;

				oldCentro.setNome(newCentro.getNome());
				oldCentro.setStatus(newCentro.getStatus());
				oldCentro.setRemoved(newCentro.getRemoved());
				oldCentro.setDataRemovido(newCentro.getDataRemovido());
				
				centroExiste = true;
				break;
			}
			
			if(!centroExiste) {
				newCentro.setEmpresa(this);
				this.getCentros().add(newCentro);
			}
		}
	}
	
	private void atualizaCargos(List<CargoEntity> novosCargos) {
		for(CargoEntity newCargo : novosCargos) {
			boolean cargoExistente = false;
			
			for(CargoEntity oldCargo : this.getCargos()) {
				if(!oldCargo.getId().equals(newCargo.getId()))
					continue;
				
				oldCargo.setNome(newCargo.getNome());
				oldCargo.setStatus(newCargo.getStatus());
				oldCargo.setRemoved(newCargo.getRemoved());
				oldCargo.setDataRemovido(newCargo.getDataRemovido());
				
				cargoExistente = true;
				break;
			}
			
			if(!cargoExistente) {
				newCargo.setEmpresa(this);
				this.getCargos().add(newCargo);
			}
		}
	}

	private void atualizaDepartamentos(List<DepartamentoEntity> novosDepartamentos) {
		for(DepartamentoEntity newDepartamento : novosDepartamentos) {
			boolean departamentoExiste = false;
			
			for(DepartamentoEntity oldDepartamento : this.getDepartamentos()) {
				if(!oldDepartamento.getId().equals(newDepartamento.getId()))
					continue;

				oldDepartamento.setNome(newDepartamento.getNome());
				oldDepartamento.setStatus(newDepartamento.getStatus());
				oldDepartamento.setRemoved(newDepartamento.getRemoved());
				oldDepartamento.setDataRemovido(newDepartamento.getDataRemovido());

				departamentoExiste = true;
				break;
			}
			
			if(!departamentoExiste) {
				newDepartamento.setEmpresa(this);
				this.getDepartamentos().add(newDepartamento);
			}
		}
	}
	
	public Boolean autoAtendimentoLiberado() {
		if(this.getAutoAtendimentoLiberado() != null) {
			return this.getAutoAtendimentoLiberado();
		}
		return false;
	}

	public String getNome() {
		return nome;
	}
	public void setNome(String nome) {
		this.nome = nome;
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
	public List<DepartamentoEntity> getDepartamentos() {
		return departamentos;
	}
	public void setDepartamentos(List<DepartamentoEntity> departamentos) {
		this.departamentos = departamentos;
	}
	public List<CentroCustoEntity> getCentros() {
		return centros;
	}
	public void setCentros(List<CentroCustoEntity> centros) {
		this.centros = centros;
	}
	public List<CargoEntity> getCargos() {
		return cargos;
	}
	public void setCargos(List<CargoEntity> cargos) {
		this.cargos = cargos;
	}
	public Long getId() {
		return id;
	}
	public void setId(Long id) {
		this.id = id;
	}
	public String getCnpj() {
		return cnpj;
	}
	public void setCnpj(String cnpj) {
		this.cnpj = cnpj;
	}
	public String getIdClient() {
		return idClient;
	}
	public void setIdClient(String idClient) {
		this.idClient = idClient;
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
