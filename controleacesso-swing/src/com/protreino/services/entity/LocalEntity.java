package com.protreino.services.entity;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;


@Entity
@Table(name = "TB_LOCAL")
@NamedQueries({
    @NamedQuery(name = "LocalEntity.findAll", query = "SELECT obj FROM LocalEntity obj ORDER BY obj.id ASC"),
    @NamedQuery(name = "LocalEntity.findById", query = "SELECT obj FROM LocalEntity obj WHERE obj.id = :ID ORDER BY obj.id ASC"),
    @NamedQuery(name = "LocalEntity.findByName", query = "SELECT obj FROM LocalEntity obj WHERE obj.nome = :NOME ORDER BY obj.id ASC")
})
public class LocalEntity extends BaseEntity implements ObjectWithId, Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "ID_LOCAL", nullable = false)
    private Long id;

    @Column(name = "NOME", nullable = true, length = 255)
    private String nome;
    
    @ElementCollection(targetClass = String.class, fetch = FetchType.EAGER)
    @CollectionTable(name = "tb_hikivision_devices_name", joinColumns = @JoinColumn(name = "ID_LOCAL"))
    @Column(name = "DEVICE_NAME", nullable = false)
    private List<String> hikivisionDeviceNames = new ArrayList<>();

    @Override
    public Long getId() {
        return this.id;
    }

    @Override
    public void setId(Long id) {
        this.id = id;
    }

    @Override
    public String toString() {
        return "LocalEntity [id=" + id + ", nome=" + nome + "]";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof LocalEntity)) return false;
        LocalEntity that = (LocalEntity) o;
        return id != null && id.equals(that.id);
    }

    @Override
    public int hashCode() {
        return getClass().hashCode();
    }

	public String getNome() {
		return nome;
	}

	public void setNome(String nome) {
		this.nome = nome;
	}

	public List<String> getHikivisionDeviceNames() {
		return hikivisionDeviceNames;
	}

	public void setHikivisionDeviceNames(List<String> hikivisionDeviceNames) {
		this.hikivisionDeviceNames = hikivisionDeviceNames;
	}
    
}

