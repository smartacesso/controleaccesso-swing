package com.protreino.services.entity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Lob;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

import com.protreino.services.enumeration.Finger;

@Entity
@Table(name="TB_BIOMETRY")
@NamedQueries({
	@NamedQuery(name  = "BiometricEntity.findAll", 
				query = "select obj from BiometricEntity obj"),
	@NamedQuery(name  = "BiometricEntity.findById",
				query = "select obj from BiometricEntity obj "
					  + "where obj.id = :ID"),
	@NamedQuery(name  = "BiometricEntity.findByIdUser", 
				query = "select obj from BiometricEntity obj"
					  + " where obj.user = :ID_USER")
})
@SuppressWarnings("serial")
public class BiometricEntity extends BaseEntity implements ObjectWithId {
	
	@Id
	@GeneratedValue(strategy=GenerationType.IDENTITY)
	@Column(name="ID_BIOMETRY", nullable=false, length=4)
	private Long id;
	
	@Column(name="ID_USER", nullable=false, length=4)
	private Long user;
	
	@Column(name="USERNAME", nullable=false, length=200)
	private String userName;
	
	@Column(name="FINGER", nullable=false, length=100)
	private Finger finger;
	
	@Lob
	@Column(name="TEMPLATE", nullable=false, length=11)
	private byte[] template;
	
	@Lob
	@Column(name="SAMPLE", nullable=true, length=11)
	private byte[] sample;
	
	public BiometricEntity() {
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public Long getUser() {
		return user;
	}

	public void setUser(Long user) {
		this.user = user;
	}
	
	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}

	public Finger getFinger() {
		return finger;
	}

	public void setFinger(Finger finger) {
		this.finger = finger;
	}

	public byte[] getTemplate() {
		return template;
	}

	public void setTemplate(byte[] template) {
		this.template = template;
	}

	public byte[] getSample() {
		return sample;
	}

	public void setSample(byte[] sample) {
		this.sample = sample;
	}
	
}
