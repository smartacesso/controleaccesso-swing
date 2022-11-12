package com.protreino.services.entity;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.Lob;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.hibernate.annotations.Type;

import com.protreino.services.enumeration.Manufacturer;

@SuppressWarnings("serial")
@Entity
@Table(name="TB_TEMPLATES")
@NamedQueries({
	@NamedQuery(name  = "TemplateEntity.findAll", query = "select obj from TemplateEntity obj"),
	@NamedQuery(name  = "TemplateEntity.findAllNaoRemovido", 
				query = "select obj "
					  + "from TemplateEntity obj "
					  + "where (obj.pedestrianAccess.removido is null "
					  + "	   or obj.pedestrianAccess.removido = false) "
					  + "order by obj.id asc"),
	@NamedQuery(name  = "TemplateEntity.findById", query = "select obj from TemplateEntity obj "
					  + " join fetch obj.pedestrianAccess a"
					  + " where obj.id = :ID"),
	@NamedQuery(name  = "TemplateEntity.findAllNaoLocal",
				query = "select obj from TemplateEntity obj "
					  + "where obj.local = false "
					  + "order by obj.id asc"),
	@NamedQuery(name  = "TemplateEntity.findByIdToRemove", 
				query = "select obj from TemplateEntity obj "
					  + " where obj.id = :ID"),
	@NamedQuery(name  = "TemplateEntity.findAllComplete", 
				query = "select obj from TemplateEntity obj "
					  + " join fetch obj.pedestrianAccess a "),
	@NamedQuery(name  = "TemplateEntity.findAlteradosComplete", 
				query = "select obj from TemplateEntity obj "
					  + " join fetch obj.pedestrianAccess a "
					  + "where obj.dataAlteracao >= :ULTIMA_SINC "),
	@NamedQuery(name  = "TemplateEntity.findAllNaoLocalComplete", 
				query = "select obj from TemplateEntity obj "
					  + " join fetch obj.pedestrianAccess a "
					  + "where obj.local = false "),
	@NamedQuery(name  = "TemplateEntity.findByIdUser", 
				query = "select obj from TemplateEntity obj "
					  + " join fetch obj.pedestrianAccess a"
					  + " where a.id = :ID_USER")
})
public class TemplateEntity extends BaseEntity implements ObjectWithId {
	
	@Id
	@GeneratedValue(strategy=GenerationType.IDENTITY)
	@Column(name="ID_TEMPLATE", nullable=false, length=11)
	private Long id;
	
	@ManyToOne(cascade={}, fetch=FetchType.LAZY)
	@JoinColumn(name="PEDESTRIAN_ACCESS", nullable=true)
	private PedestrianAccessEntity pedestrianAccess;
	
	@Column(name="PEDESTRIAN_ACCESS", nullable=true, length=40, insertable = false, updatable = false)
	private Long idPedestreianAccess;

	@Enumerated(EnumType.STRING)
	@Column(name="MANUFACTURER", nullable=true, length=100)
	private Manufacturer manufacturer;
	
	@Lob
	@Column(name="TEMPLATE", nullable=true, length=11)
	private byte[] template;
	
	@Lob
	@Column(name="sample", nullable=true, length=11)
	private byte[] sample;
	
	@Column(name="TEMPLATE_STRING", nullable=true, length=10000)
	private String templateString;
	
	@Type(type = "org.hibernate.type.NumericBooleanType")
	@Column(name="LOCAL", nullable=true, length=30)
	private Boolean local = false; // indica que é uma biometria local e que seria excluida na proxima sincronizacao para evitar duplicados
	
	@Transient
	private Long idPedestrianAccess;
	
	public TemplateEntity(){
	}

	public TemplateEntity(byte[] template) {
		this.template = template;
	}
	
	public TemplateEntity(PedestrianAccessEntity pedestrianAccess, byte[] template) {
		this.template = template;
		this.pedestrianAccess = pedestrianAccess;
	}
	
	public TemplateEntity(PedestrianAccessEntity pedestrianAccess, byte[] template, Date dataCriacao) {
		this.template = template;
		this.pedestrianAccess = pedestrianAccess;
		this.setDataCriacao(dataCriacao);
	}
	
	public TemplateEntity(Manufacturer manufacturer, byte[] template) {
		super();
		this.manufacturer = manufacturer;
		this.template = template;
	}
	
	public TemplateEntity(TemplateEntity other){
		this.idPedestrianAccess = other.getPedestrianAccess().getId();
		this.manufacturer = other.getManufacturer();
		this.template = other.getTemplate();
		this.templateString = other.getTemplateString();
		this.local = other.getLocal();
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}


	public byte[] getTemplate() {
		return template;
	}

	public void setTemplate(byte[] template) {
		this.template = template;
	}

	public Manufacturer getManufacturer() {
		return manufacturer;
	}

	public void setManufacturer(Manufacturer manufacturer) {
		this.manufacturer = manufacturer;
	}

	public String getTemplateString() {
		return templateString;
	}

	public void setTemplateString(String templateString) {
		this.templateString = templateString;
	}

	public Boolean getLocal() {
		return local;
	}

	public void setLocal(Boolean local) {
		this.local = local;
	}

	public PedestrianAccessEntity getPedestrianAccess() {
		return pedestrianAccess;
	}

	public void setPedestrianAccess(PedestrianAccessEntity pedestrianAccess) {
		this.pedestrianAccess = pedestrianAccess;
	}

	public Long getIdPedestrianAccess() {
		return idPedestrianAccess;
	}

	public void setIdPedestrianAccess(Long idPedestrianAccess) {
		this.idPedestrianAccess = idPedestrianAccess;
	}

	public byte[] getSample() {
		return sample;
	}

	public void setSample(byte[] sample) {
		this.sample = sample;
	}

	public Long getIdPedestreianAccess() {
		return idPedestreianAccess;
	}

	public void setIdPedestreianAccess(Long idPedestreianAccess) {
		this.idPedestreianAccess = idPedestreianAccess;
	}
	
}
