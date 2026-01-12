package com.protreino.services.to;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.protreino.services.entity.LocalEntity;


public class LocalTo {

    private Long id;
    private String uuid;
    private String nome;
    private String idClient;
    private List<String> hikivisionDeviceNames;  // mesmo nome do JSON
    private Boolean removed;
    private Date dataRemovido;

    public LocalEntity toLocalEntity() {
        LocalEntity local = new LocalEntity();
        local.setUuid(uuid);
        local.setNome(this.nome);
        local.setHikivisionDeviceNames(this.hikivisionDeviceNames);  // direto
        local.setIdClient(idClient);
        local.setRemoved(removed);
        local.setDataRemovido(dataRemovido);
        return local;
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

	public String getIdClient() {
		return idClient;
	}

	public void setIdClient(String idClient) {
		this.idClient = idClient;
	}

	public List<String> getHikivisionDeviceNames() {
		return hikivisionDeviceNames;
	}

	public void setHikivisionDeviceNames(List<String> hikivisionDeviceNames) {
		this.hikivisionDeviceNames = hikivisionDeviceNames;
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

	public String getUuid() {
		return uuid;
	}

	public void setUuid(String uuid) {
		this.uuid = uuid;
	}
    
}
