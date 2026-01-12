package com.protreino.services.to.hikivision;

import java.util.List;
import com.protreino.services.to.PedestreRegraTO;

public class WebSocketPedestrianAccessTO {
	private Long id;
	private String name;
	private String cardNumber;
	private String tipo;
	private String status;
	private String cpf;
	private String rg;
	private Boolean sempreLiberado;
	private String fotoBase64;

	private Long idLocal;
	private String uuidLocal;
	private Long idEmpresa;
	private List<PedestreRegraTO> pedestreRegras;

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getCardNumber() {
		return cardNumber;
	}

	public void setCardNumber(String cardNumber) {
		this.cardNumber = cardNumber;
	}

	public String getTipo() {
		return tipo;
	}

	public void setTipo(String tipo) {
		this.tipo = tipo;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public String getCpf() {
		return cpf;
	}

	public void setCpf(String cpf) {
		this.cpf = cpf;
	}

	public String getRg() {
		return rg;
	}

	public void setRg(String rg) {
		this.rg = rg;
	}

	public Boolean getSempreLiberado() {
		return sempreLiberado;
	}

	public void setSempreLiberado(Boolean sempreLiberado) {
		this.sempreLiberado = sempreLiberado;
	}

	public String getFotoBase64() {
		return fotoBase64;
	}

	public void setFotoBase64(String fotoBase64) {
		this.fotoBase64 = fotoBase64;
	}

	public Long getIdLocal() {
		return idLocal;
	}

	public void setIdLocal(Long idLocal) {
		this.idLocal = idLocal;
	}

	public Long getIdEmpresa() {
		return idEmpresa;
	}

	public void setIdEmpresa(Long idEmpresa) {
		this.idEmpresa = idEmpresa;
	}

	public List<PedestreRegraTO> getPedestreRegras() {
		return pedestreRegras;
	}

	public void setPedestreRegras(List<PedestreRegraTO> pedestreRegras) {
		this.pedestreRegras = pedestreRegras;
	}

	public String getUuidLocal() {
		return uuidLocal;
	}

	public void setUuidLocal(String uuidLocal) {
		this.uuidLocal = uuidLocal;
	}

}
