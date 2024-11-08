package com.protreino.services.entity;

import java.io.Serializable;
import java.io.UnsupportedEncodingException;
import java.security.NoSuchAlgorithmException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.Lob;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;

import org.apache.commons.codec.binary.Base64;
import org.hibernate.annotations.Fetch;
import org.hibernate.annotations.FetchMode;
import org.hibernate.annotations.Type;

import com.protreino.services.devices.Device;
import com.protreino.services.devices.TopDataDevice;
import com.protreino.services.enumeration.TipoRegra;
import com.protreino.services.main.Main;
import com.protreino.services.repository.DeviceRepository;
import com.protreino.services.repository.HibernateAccessDataFacade;
import com.protreino.services.to.DocumentoTo;
import com.protreino.services.to.PedestreRegraTO;
import com.protreino.services.to.PedestrianAccessTO;
import com.protreino.services.utils.EncryptionUtils;

@SuppressWarnings("serial")
@Entity
@Table(name="TB_PEDESTRIAN_ACCESS")
@NamedQueries({
	@NamedQuery(name = "PedestrianAccessEntity.findAll", query = "select obj from PedestrianAccessEntity obj"),
	@NamedQuery(name = "PedestrianAccessEntity.findAllPedestre", query = "select obj from PedestrianAccessEntity obj "
					  + "where obj.tipo = 'PEDESTRE' and obj.status = 'ATIVO'"),
	@NamedQuery(name = "PedestrianAccessEntity.findAllPedestrian", query = "select obj from PedestrianAccessEntity obj "
					  + "order by obj.id desc"),
	@NamedQuery(name  = "PedestrianAccessEntity.findAllAlterados", 
				query = "select obj "
					  + "from PedestrianAccessEntity obj "
					  + "where obj.dataAlteracao >= :ULTIMA_SINC "),
	@NamedQuery(name  = "PedestrianAccessEntity.findAllPedestreAlterados", 
				query = "select obj "
					  + "from PedestrianAccessEntity obj "
					  + "where obj.tipo = 'PEDESTRE' and obj.dataAlteracao >= :ULTIMA_SINC "),
	@NamedQuery(name = "PedestrianAccessEntity.findAllOrdered", query = "select obj from PedestrianAccessEntity obj order by obj.name"),
	@NamedQuery(name = "PedestrianAccessEntity.findById", query = "select obj from PedestrianAccessEntity obj where obj.id = :ID "),
	@NamedQuery(name  = "PedestrianAccessEntity.findByIdNaoRemovido", 
				query = "select obj from PedestrianAccessEntity obj "
					  + "where obj.id = :ID "
					  + "	   and (obj.removido is null or obj.removido = false) "),
	@NamedQuery(name  = "PedestrianAccessEntity.findByCardNumber", 
				query = "select obj from PedestrianAccessEntity obj "
					  + " where obj.cardNumber != null "
					  + "and obj.cardNumber != '' "
					  + "and cast(obj.cardNumber as long) = :CARD_NUMBER "
					  + "order by obj.id asc"),
	@NamedQuery(name = "PedestrianAccessEntity.findByMatricula", 
				query = "select obj from PedestrianAccessEntity obj "
					  + "where obj.matricula != null "
					  + "and obj.matricula != '' "
					  + "and cast(obj.matricula as long) = :MATRICULA "
					  + "order by obj.id asc"),
	@NamedQuery(name = "PedestrianAccessEntity.findByOnlyCPF", 
				query = "select obj from PedestrianAccessEntity obj "
					  + " where obj.cpf = :CPF "
					  + "	    and (obj.removido is null or obj.removido = false) "
					  + "order by obj.id asc"),
	@NamedQuery(name = "PedestrianAccessEntity.findByOnlyRG", 
				query = "select obj from PedestrianAccessEntity obj "
					  + "left join fetch obj.pedestreRegra p "
					  + " where obj.rg = :RG "
					  + "	    and (obj.removido is null or obj.removido = false) "
					  + "order by obj.id asc"),
	@NamedQuery(name = "PedestrianAccessEntity.findDesatualizadosRWTech", 
				query = "select new com.protreino.services.entity.PedestrianAccessEntity(obj.id) "
					  + " from PedestrianAccessEntity obj "
					  + " where obj.desatualizadoNaCatracaRWTech = true"
					  + "	   or obj.cadastradoNaCatracaRWTech is null "
					  + "	   or obj.cadastradoNaCatracaRWTech = false"),
	@NamedQuery(name = "PedestrianAccessEntity.findAllOnlyId", query = "select new com.protreino.services.entity.PedestrianAccessEntity(obj.id) "
			+ " from PedestrianAccessEntity obj"),
	@NamedQuery(name = "PedestrianAccessEntity.findAllOnlyIdName", query = "select new com.protreino.services.entity.PedestrianAccessEntity(obj.id, obj.name) "
			+ " from PedestrianAccessEntity obj"),
	@NamedQuery(name = "PedestrianAccessEntity.findWithoutPhoto", query = "select obj from PedestrianAccessEntity obj "
			+ " where obj.foto is null"),
	@NamedQuery(name = "PedestrianAccessEntity.findAllWithPhoto", query = "select obj from PedestrianAccessEntity obj "
			+ " where obj.foto is not null"),
	@NamedQuery(name = "PedestrianAccessEntity.findAllCadastradosOuEditadosDesktop",
				query = "select obj from PedestrianAccessEntity obj "
					  + "where (obj.cadastradoNoDesktop = true or obj.editadoNoDesktop = true) "
					  + "order by obj.id asc"),
	@NamedQuery(name = "PedestrianAccessEntity.findAllNaoRemovidosOrdered",
				query = "select obj from PedestrianAccessEntity obj "
					  + "where (obj.removido is null or obj.removido = false)"
					  + "order by obj.name"),
	@NamedQuery(name  = "PedestrianAccessEntity.countNaoRemovidosOrderedToAccessList",
				query = "select count(*) "
					  + "from PedestrianAccessEntity obj "
					  + "where (obj.removido is null or obj.removido = false) "
					  + "	   and (obj.invisivel is null or obj.invisivel = false) "),
	@NamedQuery(name  = "PedestrianAccessEntity.findAllNaoRemovidosOrderedToAccessList",
				query = "select new com.protreino.services.entity.PedestrianAccessEntity(obj.id, obj.cardNumber, obj.name, "
					  + "obj.tipo, obj.status, obj.quantidadeCreditos, obj.validadeCreditos, obj.dataInicioPeriodo, obj.dataFimPeriodo) "
					  + "from PedestrianAccessEntity obj "
					  + "where (obj.removido is null or obj.removido = false) and (obj.invisivel is null or obj.invisivel = false) "
					  + "order by obj.name asc"),
	@NamedQuery(name  = "PedestrianAccessEntity.findAllNaoRemovidosOrderedToAccessListPedestre",
				query = "select new com.protreino.services.entity.PedestrianAccessEntity(obj.id, obj.cardNumber, obj.name, "
					  + "obj.tipo, obj.status, obj.quantidadeCreditos, obj.validadeCreditos, obj.dataInicioPeriodo, obj.dataFimPeriodo) "
					  + "from PedestrianAccessEntity obj "
					  + "where (obj.removido is null or obj.removido = false) and (obj.invisivel is null or obj.invisivel = false) "
					  + " and obj.tipo = 'PEDESTRE' "
					  + "order by obj.name asc"),
	@NamedQuery(name  = "PedestrianAccessEntity.findAllNaoRemovidosOrderedToRegisterUser",
				query = "select new com.protreino.services.entity.PedestrianAccessEntity(obj.id, obj.name, obj.status, "
					  + "count(temp.id), obj.cadastradoNaCatracaRWTech, obj.cardNumber, obj.cadastradoNoDesktop, obj.luxandIdentifier) "
					  + "from PedestrianAccessEntity obj "
					  + "left join obj.templates temp "
					  + "where (obj.removido is null or obj.removido = false) and (obj.invisivel is null or obj.invisivel = false) "
					  + "group by obj.id, obj.name, obj.status, obj.cadastradoNaCatracaRWTech, obj.cardNumber, obj.cadastradoNoDesktop, obj.luxandIdentifier "
					  + "order by obj.name"),
	@NamedQuery(name  = "PedestrianAccessEntity.countNaoRemovidosOrderedToRegisterUser",
				query = "select count(*) "
					  + "from PedestrianAccessEntity obj "
					  + "where (obj.removido is null or obj.removido = false) and (obj.invisivel is null or obj.invisivel = false) "),
	
	@NamedQuery(name  = "PedestrianAccessEntity.countNaoRemovidosOrderedToRegisterUserPedestre",
				query = "select count(*) "
					  + "from PedestrianAccessEntity obj "
					  + "where obj.tipo = 'PEDESTRE'and (obj.removido is null or obj.removido = false) and (obj.invisivel is null or obj.invisivel = false) "),
	@NamedQuery(name  = "PedestrianAccessEntity.findAllNaoRemovidosOrderedToRegisterUserPedestre",
				query = "select new com.protreino.services.entity.PedestrianAccessEntity(obj.id, obj.name, obj.tipo, obj.status, "
					  + "count(temp.id), obj.cadastradoNaCatracaRWTech, obj.cardNumber, obj.cadastradoNoDesktop, obj.luxandIdentifier) "
					  + "from PedestrianAccessEntity obj "
					  + "left join obj.templates temp "
					  + "where obj.tipo = 'PEDESTRE' and (obj.removido is null or obj.removido = false) and (obj.invisivel is null or obj.invisivel = false) "
					  + "group by obj.id, obj.name, obj.tipo, obj.status ,obj.cadastradoNaCatracaRWTech, obj.cardNumber, obj.cadastradoNoDesktop, obj.luxandIdentifier "
					  + "order by obj.name"),
	@NamedQuery(name = "PedestrianAccessEntity.findAllComFotoParaUpload",
				query = "select obj from PedestrianAccessEntity obj "
					  + "where obj.latestPhotosTaken > :LAST_SYNC_PHOTO "
					  + "and (obj.removido is null or obj.removido = false) "
					  + "order by obj.id asc"),
	@NamedQuery(name = "PedestrianAccessEntity.findAllActivesOnlyIdAndLastPhotosTaken",
				query = "select new com.protreino.services.entity.PedestrianAccessEntity("
					  + "		obj.id, obj.latestPhotosTaken, obj.fotosForamExcluidas) "
					  + "from PedestrianAccessEntity obj "
					  + "where obj.status = 'ATIVO' "
					  + "and (obj.removido is null or obj.removido = false) "
					  + "order by obj.id asc"),
	@NamedQuery(name = "PedestrianAccessEntity.findAllComFotosExcluidasOnlyIdAndDate",
				query = "select new com.protreino.services.entity.PedestrianAccessEntity(obj.id, obj.datePhotosExcluded) "
					  + "from PedestrianAccessEntity obj "
					  + "where obj.fotosForamExcluidas = true "
					  + "order by obj.id asc"),
	@NamedQuery(name = "PedestrianAccessEntity.findByIdTemp",
				query = "select new com.protreino.services.entity.PedestrianAccessEntity(obj.id) "
					  + "from PedestrianAccessEntity obj "
					  + "where obj.idTemp = :ID_TEMP "
					  + "order by obj.id asc"),
	@NamedQuery(name = "PedestrianAccessEntity.findByIdTemp2",
				query = "select obj "
					  + "from PedestrianAccessEntity obj "
					  + "where obj.idTemp = :ID "),
	@NamedQuery(name  = "PedestrianAccessEntity.findAllWithoutDigital",
				query = "select new com.protreino.services.entity.PedestrianAccessEntity(obj.id) "
					  + "from PedestrianAccessEntity obj "
					  + "where not exists (select t "
					  + "				   from TemplateEntity t "
					  + "				   where t.pedestrianAccess.id = obj.id ) "),
	@NamedQuery(name = "PedestrianAccessEntity.findByQRCode",
				query = "select obj from PedestrianAccessEntity obj "
					  + "where obj.qrCodeParaAcesso = :QR_CODE "
					  + "and (obj.removido is null or obj.removido = false) "
					  + "order by obj.id asc"),
	@NamedQuery(name  = "PedestrianAccessEntity.findByCPF",
				query = "select obj from PedestrianAccessEntity obj "
					  + "where obj.cpf = :CPF "
					  + "and obj.id != :ID_PEDESTRE "
					  + "and (obj.removido is null or obj.removido = false) "
					  + "order by obj.id asc"),
	@NamedQuery(name  = "PedestrianAccessEntity.findByRG",
				query = "select obj from PedestrianAccessEntity obj "
					  + "where obj.rg = :RG "
					  + "and obj.id != :ID_PEDESTRE "
					  + "and (obj.removido is null or obj.removido = false) "
					  + "order by obj.id asc"),
	@NamedQuery(name  = "PedestrianAccessEntity.findByCartaoAcesso",
				query = "select obj from PedestrianAccessEntity obj "
					  + "where obj.cardNumber = :CARD_NUMBER "
					  + "and obj.id != :ID_PEDESTRE "
					  + "and (obj.removido is null or obj.removido = false) "
					  + "order by obj.id asc"),
	@NamedQuery(name  = "PedestrianAccessEntity.findByRemovedCardNumber",
				query = "select obj from PedestrianAccessEntity obj "
						+ "where obj.cardNumber = :CARD_NUMBER "
						+ "and obj.id != :ID_PEDESTRE "
						+ "and obj.removido = true "
						+ "order by obj.id asc"),
	@NamedQuery(name  = "PedestrianAccessEntity.findByCartaoAcessoWithoutID",
	query = "select obj from PedestrianAccessEntity obj "
		  + "where obj.cardNumber = :CARD_NUMBER "
		  + "and (obj.removido is null or obj.removido = false) "
		  + "order by obj.id asc"),
	@NamedQuery(name  = "PedestrianAccessEntity.findByMatricula2",
				query = "select obj from PedestrianAccessEntity obj "
					  + "where obj.matricula = :MATRICULA "
					  + "and obj.id != :ID_PEDESTRE "
					  + "and (obj.removido is null or obj.removido = false) "
					  + "order by obj.id asc"),
	@NamedQuery(name  = "PedestrianAccessEntity.findBylastID",
				query = "select obj from PedestrianAccessEntity obj "
					  + "where obj.removido is null and "
					  + "obj.removido = false "
					  + "order by obj.id desc"),
	@NamedQuery(name = "PedestrianAccessEntity.findAllWithPhotoByLastSync",
				query = "select new com.protreino.services.entity.PedestrianAccessEntity(obj.id, obj.foto, obj.cardNumber, obj.name, obj.removido)  " +
						"from PedestrianAccessEntity obj " +
						"where obj.foto != null " +
						"and obj.dataAlteracao > :LAST_SYNC_HIKIVISION " +
						"order by obj.id asc"),
	@NamedQuery(name = "PedestrianAccessEntity.countAllWithHikiVisionImageOnRegistred",
				query = "select count(obj) from PedestrianAccessEntity obj " +
						"where obj.dataCadastroFotoNaHikivision != null " +
						"and obj.tipo = 'PEDESTRE' " + 
						"and obj.cardNumber != null "),
	@NamedQuery(name = "PedestrianAccessEntity.findAllWithHikiVisionImageOnRegistred",
				query = "select new com.protreino.services.entity.PedestrianAccessEntity(obj.id, obj.foto, obj.cardNumber, obj.name, obj.removido) " +
						"from PedestrianAccessEntity obj " +
						"where obj.dataCadastroFotoNaHikivision != null " +
						"and obj.cardNumber != null " + 
						"and obj.tipo = 'PEDESTRE' " + 						
						"order by obj.id asc"),
	@NamedQuery(name = "PedestrianAccessEntity.countAllWithHikiVisionImageOnRegistredBeteenDate",
				query = "select count(obj) " +
						"from PedestrianAccessEntity obj " +
						"where obj.dataCadastroFotoNaHikivision != null " +
						"and obj.tipo = 'PEDESTRE' " + 
						"and obj.dataCadastroFotoNaHikivision between :INIT_DATE and :END_DATE " +
						"and obj.cardNumber != null "),
	@NamedQuery(name = "PedestrianAccessEntity.findAllWithHikiVisionImageOnRegistredBeteenDate",
				query = "select new com.protreino.services.entity.PedestrianAccessEntity(obj.id, obj.foto, obj.cardNumber, obj.name, obj.removido) " +
						"from PedestrianAccessEntity obj " +
						"where obj.dataCadastroFotoNaHikivision != null " +
						"and obj.tipo = 'PEDESTRE' " + 
						"and obj.dataCadastroFotoNaHikivision between :INIT_DATE and :END_DATE " +
						"and obj.cardNumber != null " +
						"order by obj.id asc"),
	@NamedQuery(name = "PedestrianAccessEntity.findAllWhitLastAccessHikivision",
				query = "select new com.protreino.services.entity.PedestrianAccessEntity(obj.id, obj.cardNumber, obj.name) " +
						"from PedestrianAccessEntity obj " +
						"where lastAccessHikiVision != null " +
						"and obj.cardNumber != null " + 
						"order by obj.id asc"),
	@NamedQuery(name = "PedestrianAccessEntity.countAllVisitantesWhitDataCadastroFotoNaHikivision",
				query = "select count(obj) from PedestrianAccessEntity obj " +
						"where obj.dataCadastroFotoNaHikivision != null " +
						"and obj.tipo = 'VISITANTE' " + 
						"and obj.cardNumber != null "),
	@NamedQuery(name = "PedestrianAccessEntity.findAllVisitantesWhitDataCadastroFotoNaHikivision",
				query = "select obj from PedestrianAccessEntity obj " +
						"where obj.dataCadastroFotoNaHikivision != null " +
						"and obj.tipo = 'VISITANTE' " + 
						"and obj.cardNumber != null ")
})
public class PedestrianAccessEntity extends BaseEntity implements ObjectWithId, Serializable {
	
	@Id
	@Column(name="ID_PEDESTRIAN_ACCESS", nullable=false, length=4)
	private Long id;
	
	@Column(name="ID_PEDESTRIAN_ACCESS_TEMP", nullable=true)
	private Long idTemp;
	
	@Column(name="LUXAND_IDENTIFIER", nullable=true)
	private String luxandIdentifier;
	
	@Column(name="NAME", nullable=false, length=100)
	private String name;
	
	@Column(name="CARD_NUMBER", nullable=true, length=30)
	private String cardNumber;
	
	@Column(name="STATUS", nullable=false, length=30)
	private String status;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="BIRTHDAY", nullable=true, length=30)
	private Date dataNascimento;
	
	@Column(name="TYPE", nullable=true, length=30)
	private String tipo;
	
	@Column(name="REGRA", nullable=true, length=30)
	private Long idRegra;
	
	@Column(name="QTD_CREDITPS", nullable=true, length=30)
	private Long quantidadeCreditos;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="VALIDADE_CREDITOS", nullable=true, length=30)
	private Date validadeCreditos;
	
	@Temporal(TemporalType.DATE)
	@Column(name = "DATA_INICIO_PERIODO", nullable = true, length = 30)
	private Date dataInicioPeriodo;
	
	@Temporal(TemporalType.DATE)
	@Column(name = "DATA_FIM_PERIODO", nullable = true, length = 30)
	private Date dataFimPeriodo;
	
	@Column(name="TIPO_TURNO", nullable=true, length=30)
	private String tipoTurno;
	
	@Temporal( TemporalType.TIMESTAMP)
	@Column(name="INICIO_TURNO", nullable=true, length=30)
	private Date inicioTurno;
	
	@Type(type = "org.hibernate.type.NumericBooleanType")
	@Column(name="REMOVIDO", nullable=true, length=11)
	private Boolean removido;
	
	@Column(name="EMAIL", nullable=true, length=40)
	private String email;

	@Column(name="CPF", nullable=true, length=15)
	private String cpf;
	
	@Column(name="GENERO", nullable=true, length=15)
	private String genero;
	
	@Column(name="RG", nullable=true, length=20)
	private String rg;
	
	@Column(name="TELEFONE", nullable=true, length=20)
	private String telefone;
	
	@Column(name="CELULAR", nullable=true, length=20)
	private String celular;
	
	@Column(name="RESPONSAVEL", nullable=true, length=1000)
	private String responsavel;
	
	@Column(name="OBSERVACOES", nullable=true, length=300)
	private String observacoes;

	@Column(name="MATRICULA", nullable=true, length=100)
	private String matricula;
	
	@Type(type = "org.hibernate.type.NumericBooleanType")
	@Column(name="SEMPRE_LIBERADO", nullable=true, length=11)
	private Boolean sempreLiberado;
	
	@Type(type = "org.hibernate.type.NumericBooleanType")
	@Column(name="HABILITAR_TECLADO", nullable=true, length=11)
	private Boolean habilitarTeclado;
	
	@Type(type = "org.hibernate.type.NumericBooleanType")
	@Column(name="ENVIAR_SMS_AO_PASSAR_NA_CATRACA", nullable=true, length=11)
	private Boolean enviaSmsAoPassarNaCatraca;
	
	@Column(name="QR_CODE_PARA_ACESSO", nullable=true, length=100)
	private String qrCodeParaAcesso;
	
	@Type(type = "org.hibernate.type.NumericBooleanType")
	@Column(name="CADASTRO_FACIAL_OBRIGATORIO", nullable=true, length=11)
	private Boolean cadastroFacialObrigatorio;
	
	@Column(name="CEP", nullable=true, length=20)
	private String cep;
	
	@Column(name="LOGRADOURO", nullable=true, length=255)
	private String logradouro;
	
	@Column(name="NUMERO", nullable=true, length=10)
	private String numero;
	
	@Column(name="COMPLEMENTO", nullable=true, length=255)
	private String complemento;

	@Column(name="BAIRRO", nullable=true, length=100)
	private String bairro;
	
	@Column(name="CIDADE", nullable=true, length=255)
	private String cidade;
	
	@Column(name="ESTADO", nullable=true, length=100)
	private String estado;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="LATEST_PHOTOS_TAKEN", nullable=true, length=30)
	private Date latestPhotosTaken;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="DATE_PHOTOS_EXCLUDED", nullable=true, length=30)
	private Date datePhotosExcluded;
	
	@Type(type = "org.hibernate.type.NumericBooleanType")
	@Column(name="FOTOS_FORAM_EXCLUIDAS", nullable=true, length=11)
	private Boolean fotosForamExcluidas;
	
	@Column(name="ID_EMPRESA", nullable=true, length=15)
	private Long idEmpresa;
	
	@Column(name="ID_DEPARTAMENTO", nullable=true, length=15)
	private Long idDepartamento;
	
	@Column(name="ID_CENTRO_CUSTO", nullable=true, length=15)
	private Long idCentroCusto;
	
	@Column(name="ID_CARGO", nullable=true, length=15)
	private Long idCargo;
	
	@Column(name="LOGIN", nullable=true, length=100)
	private String login;
	
	@Column(name="SENHA", nullable=true, length=255)
	private String senha;
	
	@Transient
	private String senhaLivre;
	
	@Column(name="TIPO_ACESSO", nullable=true, length=255)
	private String tipoAcesso;
	
	@Column(name="TIPO_QRCODE", nullable=true, length=100)
	private String tipoQRCode;
	
	@OneToMany(cascade=CascadeType.ALL, orphanRemoval=true, fetch=FetchType.EAGER,
			 targetEntity=TemplateEntity.class, mappedBy="pedestrianAccess")
	@Fetch(FetchMode.SUBSELECT)
	public List<TemplateEntity> templates;
	
	@OneToMany(cascade=CascadeType.ALL, orphanRemoval=true, fetch=FetchType.EAGER,
			 targetEntity=AllowedTimeEntity.class, mappedBy="pedestrianAccess")
	@Fetch(FetchMode.SUBSELECT)
	private List<AllowedTimeEntity> horariosPermitidos; 
	
	@OneToMany(cascade=CascadeType.ALL, orphanRemoval=true, fetch=FetchType.EAGER,
			 targetEntity=PedestrianEquipamentEntity.class, mappedBy="pedestrianAccess")
	@Fetch(FetchMode.SUBSELECT)
	private List<PedestrianEquipamentEntity> equipamentos;
	
	@OneToMany(cascade=CascadeType.ALL, orphanRemoval=true, fetch=FetchType.EAGER,
			 targetEntity=PedestrianMessagesEntity.class, mappedBy="pedestrianAccess")
	@Fetch(FetchMode.SUBSELECT)
	private List<PedestrianMessagesEntity> mensagens; 
	
	@OneToMany(cascade=CascadeType.ALL, orphanRemoval=true, fetch=FetchType.EAGER,
			 targetEntity=DocumentoEntity.class, mappedBy="pedestrianAccess")
	@Fetch(FetchMode.SUBSELECT)
	private List<DocumentoEntity> documentos; 
	
	@OneToMany(cascade=CascadeType.ALL, orphanRemoval=true, fetch=FetchType.EAGER,
			 targetEntity=PedestreRegraEntity.class, mappedBy="pedestrianAccess")
	@Fetch(FetchMode.SUBSELECT)
	private List<PedestreRegraEntity> pedestreRegra; 
	
	@Type(type = "org.hibernate.type.NumericBooleanType")
	@Column(name="REGISTERED_RWTECH", nullable=true, length=30)
	private Boolean cadastradoNaCatracaRWTech = false;
	
	@Type(type = "org.hibernate.type.NumericBooleanType")
	@Column(name="OUT_OF_DATE_RWTECH", nullable=true, length=30)
	private Boolean desatualizadoNaCatracaRWTech = false;
	
	@Lob
	@Column(name="PHOTO", nullable=true, length=11)
	private byte[] foto;
	
	@Column(name="LAST_ACCESS_TOLETUS", nullable=true, length=100)
	private String ultimoAcessoToletus;
	
	@Type(type="org.hibernate.type.NumericBooleanType")
	@Column(name="CADASTRADO_NO_DESKTOP", nullable=true, length=30)
	private Boolean cadastradoNoDesktop = false;

	@Type(type="org.hibernate.type.NumericBooleanType")
	@Column(name="EDITADO_NO_DESKTOP", nullable=true, length=30)
	private Boolean editadoNoDesktop = false;
	
	@Column(name="QUANTIDADE_ACESSO_ANTES_SINC", nullable=true, length=15)
	private Integer qtdAcessoAntesSinc;
	
	@Column(name="ID_USUARIO", nullable=true, length=15)
	private Long idUsuario;
	
	@Type(type = "org.hibernate.type.NumericBooleanType")
	@Column(name="INVISIVEL", nullable=true, length=11)
	private Boolean invisivel = false;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="DATA_CADASTRO_FOTO_HIKIVISION", nullable=true, length=30)
	private Date dataCadastroFotoNaHikivision;
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name="ULTIMO_ACESSO_HIKIVISION", nullable=true, length=30)
	private Date lastAccessHikiVision;
		
	@Transient
	private Integer origemCatraca;
	
	@Transient
	private List<LogPedestrianAccessEntity> listaAcessosTransient;
	
	@Transient
	private List<BiometricEntity> listaBiometriasTransient;
	
	@Transient
	private Boolean cadastradoNaCatraca = false; // Campo preenchido quando tenho retorno da catraca com uma lista de usuarios cadastrados

	@Transient
	private Long tamanhoListaTemplates;
	
	@Transient
	private Boolean novasDigitais = false;
	
	@Transient
	private String nomeUuarioQueCriou;

	public PedestrianAccessEntity() {
	}

	public PedestrianAccessEntity(Long id) {
		this.id = id;
	}

	public PedestrianAccessEntity(byte[] foto, String cardNumber, String name, Boolean removido) {
		this.foto = foto;
		this.cardNumber = cardNumber;
		this.name = name;
		this.removido = removido;
	}
	
	public PedestrianAccessEntity(Long id, byte[] foto, String cardNumber, String name, Boolean removido) {
		this.id = id;
		this.foto = foto;
		this.cardNumber = cardNumber;
		this.name = name;
		this.removido = removido;
	}

	public PedestrianAccessEntity(Long id, String name) {
		this.id = id;
		this.name = name;
	}
	
	public PedestrianAccessEntity(Long id, String cardNumber, String name) {
		this.id = id;
		this.name = name;
		this.cardNumber = cardNumber;
	}
	
	
	public PedestrianAccessEntity(Long id, Date datePhotosExcluded) {
		this.id = id;
		this.datePhotosExcluded = datePhotosExcluded;
	}
	
	public PedestrianAccessEntity(Long id, Date latestPhotosTaken, Boolean fotosForamExcluidas) {
		this.id = id;
		this.latestPhotosTaken = latestPhotosTaken;
		this.fotosForamExcluidas = fotosForamExcluidas;
	}
	
	public PedestrianAccessEntity(Long id, String name, String status, Long tamanhoListaTemplates, Boolean cadastradoNaCatracaRWTech,
			String cardNumber, Boolean cadastradoNoDesktop) {
		this.id = id;
		this.name = name;
		this.status = status;
		this.tamanhoListaTemplates = tamanhoListaTemplates;
		this.cadastradoNaCatracaRWTech = cadastradoNaCatracaRWTech;
		this.cardNumber = cardNumber;
		this.cadastradoNoDesktop = cadastradoNoDesktop;
	}
	
	
	public PedestrianAccessEntity(Long id, String name, String status, Integer tamanhoListaTemplates, Boolean cadastradoNaCatracaRWTech,
			String cardNumber, Boolean cadastradoNoDesktop) {
		this.id = id;
		this.name = name;
		this.status = status;
		this.tamanhoListaTemplates = Long.valueOf(tamanhoListaTemplates);
		this.cadastradoNaCatracaRWTech = cadastradoNaCatracaRWTech;
		this.cardNumber = cardNumber;
		this.cadastradoNoDesktop = cadastradoNoDesktop;
	}
	//nesse
	public PedestrianAccessEntity(Long id, String name, String status, Long tamanhoListaTemplates, Boolean cadastradoNaCatracaRWTech,
			String cardNumber, Boolean cadastradoNoDesktop, String luxandIdentifier) {
		this.id = id;
		this.name = name;
		this.status = status;
		this.tamanhoListaTemplates = Long.valueOf(tamanhoListaTemplates);
		this.cadastradoNaCatracaRWTech = cadastradoNaCatracaRWTech;
		this.cardNumber = cardNumber;
		this.cadastradoNoDesktop = cadastradoNoDesktop;
		this.luxandIdentifier = luxandIdentifier;
	}
	
	public PedestrianAccessEntity(Long id, String name, String status, Integer tamanhoListaTemplates, Boolean cadastradoNaCatracaRWTech,
			String cardNumber, Boolean cadastradoNoDesktop, String luxandIdentifier) {
		this.id = id;
		this.name = name;
		this.status = status;
		this.tamanhoListaTemplates = Long.valueOf(tamanhoListaTemplates);
		this.cadastradoNaCatracaRWTech = cadastradoNaCatracaRWTech;
		this.cardNumber = cardNumber;
		this.cadastradoNoDesktop = cadastradoNoDesktop;
		this.luxandIdentifier = luxandIdentifier;
	}
	
	public PedestrianAccessEntity(Long id, String name, String tipo ,String status, Long tamanhoListaTemplates, Boolean cadastradoNaCatracaRWTech,
			String cardNumber, Boolean cadastradoNoDesktop, String luxandIdentifier) {
		this.id = id;
		this.name = name;
		this.tipo = tipo;
		this.status = status;
		this.tamanhoListaTemplates = Long.valueOf(tamanhoListaTemplates);
		this.cadastradoNaCatracaRWTech = cadastradoNaCatracaRWTech;
		this.cardNumber = cardNumber;
		this.cadastradoNoDesktop = cadastradoNoDesktop;
		this.luxandIdentifier = luxandIdentifier;
	}
	
	public PedestrianAccessEntity(Long id, String cardNumber, String name, String tipo, String status,
			Long quantidadeCreditos, Date validadeCreditos, Date dataInicioPeriodo, Date dataFimPeriodo) {
		this.id = id;
		this.cardNumber = cardNumber;
		this.name = name;
		this.tipo = tipo;
		this.status = status;
		this.quantidadeCreditos = quantidadeCreditos;
		this.validadeCreditos = validadeCreditos;
		this.dataInicioPeriodo = dataInicioPeriodo;
		this.dataFimPeriodo = dataFimPeriodo;
	}
	
	public PedestrianAccessEntity(Long id, String cardNumber, String name, String tipo, String status,
			Long quantidadeCreditos, Date validadeCreditos, Date dataInicioPeriodo, Date dataFimPeriodo, 
			Long idUsuario) {
		this.id = id;
		this.cardNumber = cardNumber;
		this.name = name;
		this.tipo = tipo;
		this.status = status;
		this.quantidadeCreditos = quantidadeCreditos;
		this.validadeCreditos = validadeCreditos;
		this.dataInicioPeriodo = dataInicioPeriodo;
		this.dataFimPeriodo = dataFimPeriodo;
		this.idUsuario = idUsuario;
	}
	
	public PedestrianAccessEntity(PedestrianAccessTO athleteAccessTO){
		//Dados basicos
		this.id = athleteAccessTO.getId();
		this.idTemp = athleteAccessTO.getIdTemp() != null ? athleteAccessTO.getIdTemp() : null;
		this.name = athleteAccessTO.getName().toUpperCase();
		this.dataNascimento = athleteAccessTO.getDataNascimento();
		this.email = athleteAccessTO.getEmail();
		this.cpf = athleteAccessTO.getCpf();
		this.genero = athleteAccessTO.getGenero();
		this.rg = athleteAccessTO.getRg();
		this.telefone = athleteAccessTO.getTelefone();
		this.celular = athleteAccessTO.getCelular();
		this.observacoes = athleteAccessTO.getObservacoes();
		this.responsavel = athleteAccessTO.getResponsavel();
		
		//Dados Empresa
		this.idEmpresa = athleteAccessTO.getIdEmpresa();
		this.idCargo = athleteAccessTO.getIdCargo();
		this.idCentroCusto = athleteAccessTO.getIdCentroCusto();
		this.idDepartamento = athleteAccessTO.getIdDepartamento();
		
		//Dados endereco
		this.cep = athleteAccessTO.getCep();
		this.logradouro = athleteAccessTO.getLogradouro();
		this.numero = athleteAccessTO.getNumero();
		this.complemento = athleteAccessTO.getComplemento();
		this.bairro = athleteAccessTO.getBairro();
		this.cidade = athleteAccessTO.getCidade();
		this.estado = athleteAccessTO.getEstado();

		//Aba lateral
		this.tipo = athleteAccessTO.getTipo();
		this.status = athleteAccessTO.getStatus();
		this.matricula = athleteAccessTO.getMatricula();
		this.cardNumber = athleteAccessTO.getCardNumber();
		this.sempreLiberado = athleteAccessTO.getSempreLiberado();
		this.habilitarTeclado = athleteAccessTO.getHabilitarTeclado();
		this.cadastroFacialObrigatorio = athleteAccessTO.getCadastroFacialObrigatorio();
		this.enviaSmsAoPassarNaCatraca = athleteAccessTO.getEnviaSmsAoPassarNaCatraca();

		//Outros dados
		this.removido = athleteAccessTO.getRemovido();
		this.luxandIdentifier = athleteAccessTO.getLuxandIdentifier();
		this.qrCodeParaAcesso = athleteAccessTO.getQrCodeParaAcesso();
		this.idRegra = athleteAccessTO.getIdRegra();
		this.quantidadeCreditos = athleteAccessTO.getQuantidadeCreditos();
		this.validadeCreditos = athleteAccessTO.getValidadeCreditos();
		this.tipoTurno = athleteAccessTO.getTipoTurno();
		this.inicioTurno = athleteAccessTO.getInicioTurno();
		this.dataInicioPeriodo = athleteAccessTO.getDataInicioPeriodo();
		this.dataFimPeriodo = athleteAccessTO.getDataFimPeriodo();
		this.qtdAcessoAntesSinc = athleteAccessTO.getQtdAcessoAntesSinc();
		this.idUsuario = athleteAccessTO.getIdUsuario();
		this.dataCadastroFotoNaHikivision = athleteAccessTO.getDataCadastroFotoNaHikivision();
		
		//Dados de acesso
		this.login = athleteAccessTO.getLogin();
		this.senha = athleteAccessTO.getSenha();
		this.tipoAcesso = athleteAccessTO.getTipoAcesso();
		this.tipoQRCode = athleteAccessTO.getTipoQRCode();
		
		if (athleteAccessTO.getHorariosPermitidos() != null && !athleteAccessTO.getHorariosPermitidos().isEmpty()) {
			this.horariosPermitidos = new ArrayList<AllowedTimeEntity>();
			for (AllowedTimeEntity horario : athleteAccessTO.getHorariosPermitidos())
				horariosPermitidos.add(new AllowedTimeEntity(this,horario.getInicio(), horario.getFim(), horario.getDiasPermitidos()));
		}
		if (athleteAccessTO.getTemplates() != null && !athleteAccessTO.getTemplates().isEmpty()){
			this.templates = new ArrayList<TemplateEntity>();
			for (String s : athleteAccessTO.getTemplates())
				templates.add(new TemplateEntity(this, Base64.decodeBase64(s)));
			novasDigitais = true;
		}

		if (athleteAccessTO.getEquipamentos() != null && !athleteAccessTO.getEquipamentos().isEmpty()){
			this.equipamentos = new ArrayList<PedestrianEquipamentEntity>();
			for (PedestrianEquipamentEntity eq : athleteAccessTO.getEquipamentos())
				equipamentos.add(new PedestrianEquipamentEntity(this, eq));
		}
		if (athleteAccessTO.getMensagens() != null && !athleteAccessTO.getMensagens().isEmpty()){
			this.mensagens = new ArrayList<PedestrianMessagesEntity>();
			for (PedestrianMessagesEntity m : athleteAccessTO.getMensagens())
				mensagens.add(new PedestrianMessagesEntity(this, m));
		}
		if (athleteAccessTO.getDocumentos() != null && !athleteAccessTO.getDocumentos().isEmpty()) {
			this.documentos = new ArrayList<DocumentoEntity>();
			for(DocumentoTo doc : athleteAccessTO.getDocumentos())
				documentos.add(new DocumentoEntity(this, doc));
		}
		if(athleteAccessTO.getPedestreRegras() != null && !athleteAccessTO.getPedestreRegras().isEmpty()) {
			this.pedestreRegra = new ArrayList<>();
			for(PedestreRegraTO pr : athleteAccessTO.getPedestreRegras()) {
				RegraEntity regra = (RegraEntity) HibernateAccessDataFacade.getSingleResultById(RegraEntity.class, pr.getIdRegra());
				pedestreRegra.add(new PedestreRegraEntity(this, pr, regra));
			}
		}
		
	}

	public String toString(){
		Calendar c = Calendar.getInstance();
		if(validadeCreditos != null)
			c.setTime(validadeCreditos);
		
		StringBuilder stringBuilder = new StringBuilder();
		stringBuilder
			//Dados basicos
			.append(id).append(";")
			.append(name.toUpperCase()).append(";")
			.append(dataNascimento != null ? dataNascimento.getTime() : "").append(";")
			.append(email != null ? email : "").append(";")
			.append(cpf != null ? cpf : "").append(";")
			.append(genero != null ? genero : "").append(";")
			.append(rg != null ? rg : "").append(";")
			.append(telefone != null ? telefone  : "").append(";")
			.append(celular != null ? celular : "").append(";")
			.append(responsavel != null ? responsavel : "").append(";")
			.append(observacoes != null ? observacoes : "").append(";")

			//Dados empresa
			.append(idEmpresa != null ? idEmpresa : "").append(";")
			.append(idCargo != null ? idCargo : "").append(";")
			.append(idCentroCusto != null ? idCentroCusto : "").append(";")
			.append(idDepartamento != null ? idDepartamento : "").append(";")
			
			//aba lateral
			.append(tipo).append(";")
			.append(status).append(";")
			.append(matricula != null ? matricula : "").append(";")
			.append(cardNumber).append(";")
			.append(habilitarTeclado).append(";")
			.append(sempreLiberado).append(";")
			.append(enviaSmsAoPassarNaCatraca != null ? enviaSmsAoPassarNaCatraca : "").append(";")
			.append(cadastroFacialObrigatorio).append(";")
			
			//endereco
			.append(cep != null ? cep : "").append(";")
			.append(logradouro != null ? logradouro : "").append(";")
			.append(numero != null ? numero : "").append(";")
			.append(complemento != null ? complemento : "").append(";")
			.append(bairro != null ? bairro : "").append(";")
			.append(cidade != null ? cidade : "").append(";")
			.append(estado != null ? estado : "").append(";")
			
			//outros dados
			.append(luxandIdentifier == null || luxandIdentifier.isEmpty() ? "null" : luxandIdentifier).append(";")
			.append(horariosPermitidos == null || horariosPermitidos.isEmpty() ? "null" : horariosPermitidos).append(";")
			.append(removido).append(";")
			.append(qrCodeParaAcesso != null ? qrCodeParaAcesso : "").append(";")
			.append(quantidadeCreditos != null ? quantidadeCreditos : "").append(";")
			.append(validadeCreditos != null ? c.get(Calendar.DAY_OF_YEAR) : "").append(";")
			.append(tipoTurno != null ? tipoTurno : "").append(";")
			.append(inicioTurno != null ? inicioTurno.getTime() : "").append(";")
			.append(dataInicioPeriodo != null ? dataInicioPeriodo.getTime() : "").append(";")
			.append(dataFimPeriodo != null ? dataFimPeriodo.getTime() : "").append(";")
			.append(qtdAcessoAntesSinc != null ? qtdAcessoAntesSinc : "").append(";")
			.append(idUsuario != null ? idUsuario : "").append(";")
			.append(login != null ? login : "").append(";")
			.append(senha != null ? senha : "").append(";")
			.append(tipoAcesso != null ? tipoAcesso : "").append(";")
			.append(tipoQRCode != null ? tipoQRCode : "").append(";")
			.append(dataCadastroFotoNaHikivision != null ? dataCadastroFotoNaHikivision.getTime() : "");
			
		
		if(this.mensagens != null) {
			for(PedestrianMessagesEntity m : this.mensagens) {
				stringBuilder.append(m.getId()).append(";");
			}
		}
		
		if(this.templates != null) {
			for(TemplateEntity t : this.templates) {
				stringBuilder.append(Base64.encodeBase64String(t.getTemplate())).append(";");
			}
		}
		
		if(this.equipamentos != null) {
			for(PedestrianEquipamentEntity e : this.equipamentos) {
				stringBuilder.append(e.getId()).append(";");
			}
		}
		
		if(this.documentos != null) {
			for(DocumentoEntity doc : this.documentos) {
				stringBuilder.append(doc.getId()).append(";");
			}
		}
		
		if(this.pedestreRegra != null) {
			for(PedestreRegraEntity pr : this.pedestreRegra) {
				stringBuilder.append(pr.getId()).append(";");
			}
		}
		
		if(this.horariosPermitidos != null) {
			for(AllowedTimeEntity a : this.horariosPermitidos) {
				if(a.getInicio() != null) {
					stringBuilder.append(a.getInicio()).append(";");
				}
			}
		}
		
		return stringBuilder.toString();
	}
	
	public void update(PedestrianAccessTO athleteAccessTO){
		//Dados basicos
		this.idTemp = athleteAccessTO.getIdTemp() != null ? athleteAccessTO.getIdTemp() : null;
		this.name = athleteAccessTO.getName().toUpperCase();
		this.dataNascimento = athleteAccessTO.getDataNascimento();
		this.email = athleteAccessTO.getEmail();
		this.cpf = athleteAccessTO.getCpf();
		this.genero = athleteAccessTO.getGenero();
		this.rg = athleteAccessTO.getRg();
		this.telefone = athleteAccessTO.getTelefone();
		this.celular = athleteAccessTO.getCelular();
		this.observacoes = athleteAccessTO.getObservacoes();
		this.responsavel = athleteAccessTO.getResponsavel();

		//Dados empresa
		this.idEmpresa = athleteAccessTO.getIdEmpresa();
		this.idCargo = athleteAccessTO.getIdCargo();
		this.idCentroCusto = athleteAccessTO.getIdCentroCusto();
		this.idDepartamento = athleteAccessTO.getIdDepartamento();
		
		//Dados endereco
		this.cep = athleteAccessTO.getCep();
		this.logradouro = athleteAccessTO.getLogradouro();
		this.numero = athleteAccessTO.getNumero();
		this.complemento = athleteAccessTO.getComplemento();
		this.bairro = athleteAccessTO.getBairro();
		this.cidade = athleteAccessTO.getCidade();
		this.estado = athleteAccessTO.getEstado();
		
		//Aba lateral
		this.tipo = athleteAccessTO.getTipo();
		this.status = athleteAccessTO.getStatus();
		this.matricula = athleteAccessTO.getMatricula();
		this.cardNumber = athleteAccessTO.getCardNumber();
		this.sempreLiberado = athleteAccessTO.getSempreLiberado();
		this.habilitarTeclado = athleteAccessTO.getHabilitarTeclado();
		this.cadastroFacialObrigatorio = athleteAccessTO.getCadastroFacialObrigatorio();
		this.enviaSmsAoPassarNaCatraca = athleteAccessTO.getEnviaSmsAoPassarNaCatraca();
		
		//Outros dados
		this.removido = athleteAccessTO.getRemovido();
		this.luxandIdentifier = athleteAccessTO.getLuxandIdentifier();
		this.qrCodeParaAcesso = athleteAccessTO.getQrCodeParaAcesso();
		this.quantidadeCreditos = athleteAccessTO.getQuantidadeCreditos();
		this.idRegra = athleteAccessTO.getIdRegra();
		this.validadeCreditos = athleteAccessTO.getValidadeCreditos();
		this.tipoTurno = athleteAccessTO.getTipoTurno();
		this.inicioTurno = athleteAccessTO.getInicioTurno();
		this.dataInicioPeriodo = athleteAccessTO.getDataInicioPeriodo();
		this.dataFimPeriodo = athleteAccessTO.getDataFimPeriodo();
		this.qtdAcessoAntesSinc = athleteAccessTO.getQtdAcessoAntesSinc();
		this.idUsuario = athleteAccessTO.getIdUsuario();
		this.dataCadastroFotoNaHikivision = athleteAccessTO.getDataCadastroFotoNaHikivision();
		
		//Dados de acesso
		this.login = athleteAccessTO.getLogin();
		this.senha = athleteAccessTO.getSenha();
		this.tipoAcesso = athleteAccessTO.getTipoAcesso();
		this.tipoQRCode = athleteAccessTO.getTipoQRCode();
	    
		if (athleteAccessTO.getHorariosPermitidos() != null
				&& !athleteAccessTO.getHorariosPermitidos().isEmpty()) {
			if (horariosPermitidos != null && !horariosPermitidos.isEmpty()) {
				horariosPermitidos.clear();
			} else {
				horariosPermitidos = new ArrayList<AllowedTimeEntity>();
			}

			for (AllowedTimeEntity horario : athleteAccessTO.getHorariosPermitidos()){
				horariosPermitidos.add(new AllowedTimeEntity(this, horario.getInicio(), horario.getFim(), horario.getDiasPermitidos()));
			}
		} else {
			if (horariosPermitidos != null) {
				horariosPermitidos.clear();
			}
		}
		
		if (athleteAccessTO.getTemplates() != null
				&& !athleteAccessTO.getTemplates().isEmpty()) {
			//verifica antes se alterou templates
			boolean alterar = false;
			if(templates != null && !templates.isEmpty()) {
				if(templates.size() != athleteAccessTO.getTemplates().size()) {
					//tamanhos diferentes, ja altera
					alterar = true;
					if(Main.desenvolvimento)
						System.out.println("Digitais diferentes");
				} else {
					//verifica se lista de digitais existes 
					//é igual a lista de digitais recebidas
					List<String> templatesExistentes = new ArrayList<String>();
					for (TemplateEntity t : templates) {
						String existente = Base64.encodeBase64String(t.getTemplate());
						templatesExistentes.add(existente.replaceAll("(?:\\n|\\r)", ""));
					}
					
					if(!templatesExistentes.equals(athleteAccessTO.getTemplates())) {
						alterar = true;
						if(Main.desenvolvimento)
							System.out.println("Digitais diferentes");
					}else {
						if(Main.desenvolvimento)
							System.out.println("Digitais iguais");
					}
				}
			} else {
				alterar = true;
			}
			
			if(alterar) {
				if (templates != null && !templates.isEmpty()) {
					templates.clear();
				
				} else {
					templates = new ArrayList<TemplateEntity>();
				}
				
				for (String s : athleteAccessTO.getTemplates()) {
					templates.add(new TemplateEntity(this, Base64.decodeBase64(s), athleteAccessTO.getDataAlteracao()));
				}
				
				novasDigitais = true;
			}
		} else {
			if (templates != null) {
				templates.clear();
			}
		}
		
		//equipamentos
		if (athleteAccessTO.getEquipamentos() != null && !athleteAccessTO.getEquipamentos().isEmpty()) {
			if (equipamentos == null || equipamentos.isEmpty()) {
				equipamentos = new ArrayList<>();
				
				for(PedestrianEquipamentEntity newEquip : athleteAccessTO.getEquipamentos()) {
					equipamentos.add(new PedestrianEquipamentEntity(this, newEquip));
				}
			
			} else {
				List<PedestrianEquipamentEntity> equipamentosAux = new ArrayList<>();
				for (PedestrianEquipamentEntity newEquip : athleteAccessTO.getEquipamentos()) {
					boolean equipExistente = false;
					
					for(PedestrianEquipamentEntity equipamentoExistente : equipamentos) {
						if(equipamentoExistente.getId().equals(newEquip.getId())) {
							equipamentosAux.add(equipamentoExistente);
							equipExistente = true;
							break;
						}
					}
					
					if(!equipExistente) {
						equipamentosAux.add(new PedestrianEquipamentEntity(this, newEquip));
					}
				}
				equipamentos = equipamentosAux;
			}
		
		} else {
			if (equipamentos != null)
				equipamentos.clear();
		}
		
		//mensagens
		if (athleteAccessTO.getMensagens() != null && !athleteAccessTO.getMensagens().isEmpty()) {
			if (mensagens == null || mensagens.isEmpty()) {
				mensagens = new ArrayList<>();
				
				for(PedestrianMessagesEntity newMensagem : athleteAccessTO.getMensagens())
					mensagens.add(new PedestrianMessagesEntity(this, newMensagem));
				
			} else {
				List<PedestrianMessagesEntity> mensagensAux = new ArrayList<>();
				for(PedestrianMessagesEntity newMensagem : athleteAccessTO.getMensagens()) {
					boolean msgExistente = false;
					
					for(PedestrianMessagesEntity mensagemExistente : mensagens) {
						if(mensagemExistente.getId().equals(newMensagem.getId())) {
							mensagemExistente.setQuantidade(newMensagem.getQuantidade());
							
							mensagensAux.add(mensagemExistente);
							msgExistente = true;
							break;
						}
					}
					
					if(!msgExistente) {
						mensagensAux.add(new PedestrianMessagesEntity(this, newMensagem));
					}
				}
				
				mensagens = mensagensAux;
			}
		
		} else {
			if (mensagens != null) {
				mensagens.clear();
			}
				
		}
		
		//documentos
		if(athleteAccessTO.getDocumentos() != null && !athleteAccessTO.getDocumentos().isEmpty()) {
			if(documentos == null || documentos.isEmpty()) {
				documentos = new ArrayList<>();
				
				for(DocumentoTo newDoc : athleteAccessTO.getDocumentos()) {
					documentos.add(new DocumentoEntity(this, newDoc));
				}
			
			} else {
				List<DocumentoEntity> documentoAux = new ArrayList<>();
				for(DocumentoTo newDoc : athleteAccessTO.getDocumentos()) {
					boolean docJaExiste = false;
					
					for(DocumentoEntity documetoExistente : documentos) {
						if(documetoExistente.getId().equals(newDoc.getId())) {
							
							documentoAux.add(documetoExistente);
							docJaExiste = true;
							break;
						}
					}
					
					if(!docJaExiste) {
						documentoAux.add(new DocumentoEntity(this, newDoc));
					}
				}
				documentos = documentoAux;
			}
			
		} else {
			if (documentos != null) {
				documentos.clear();
			}
		}
		
		//pedestreRegras
		if(athleteAccessTO.getPedestreRegras() != null && !athleteAccessTO.getPedestreRegras().isEmpty()) {
			if(pedestreRegra == null || pedestreRegra.isEmpty()) {
				pedestreRegra = new ArrayList<PedestreRegraEntity>();
				
				for(PedestreRegraTO newPr : athleteAccessTO.getPedestreRegras()) {
					RegraEntity regra = (RegraEntity) HibernateAccessDataFacade.getSingleResultById(RegraEntity.class, newPr.getIdRegra());
					pedestreRegra.add(new PedestreRegraEntity(this, newPr, regra));
				}
			
			} else {
				List<PedestreRegraEntity> naoEcontrados = new ArrayList<>();
				
				for(PedestreRegraEntity pedestreRegraExistente : this.pedestreRegra) {
					Optional<PedestreRegraTO> first = athleteAccessTO.getPedestreRegras().stream()
						.filter(pedestreRegraTo -> pedestreRegraTo.getId().equals(pedestreRegraExistente.getId()))
						.findFirst();
					
					if(!first.isPresent()) {
						naoEcontrados.add(pedestreRegraExistente);
					}
				}

				this.pedestreRegra.removeAll(naoEcontrados);
				
				for(PedestreRegraTO newPr : athleteAccessTO.getPedestreRegras()) {
					boolean pRJaExiste = false;
					
					for(PedestreRegraEntity pedestreRegraExistente : this.pedestreRegra) {
						if(pedestreRegraExistente.getId().equals(newPr.getId())) {
							pedestreRegraExistente.setQtdeDeCreditos(newPr.getQtdeDeCreditos());
							
							pRJaExiste = true;
							break;
						}
					}
					
					if(!pRJaExiste) {
						RegraEntity regra = (RegraEntity) HibernateAccessDataFacade.getSingleResultById(RegraEntity.class, newPr.getIdRegra());
						this.pedestreRegra.add(new PedestreRegraEntity(this, newPr, regra));
					}
				}
			}
		
		} else {
			if(pedestreRegra != null) {
				pedestreRegra.clear();
			}
		}
		
		setDataAlteracao(new Date());
	}
	
	public boolean hasOnlyRestrictedEquipaments() {
		if(Objects.isNull(equipamentos) || equipamentos.isEmpty() || Objects.isNull(Main.devicesList)) {
			return false;
		}
		
		for(PedestrianEquipamentEntity equipamento : equipamentos) {
			Device device = DeviceRepository.getDeviceByName(equipamento.getNomeEquipamento());
			
			if(Objects.nonNull(device) && device instanceof TopDataDevice && !((TopDataDevice) device).isDeviceRestrito()) {
				return false;
			}
		}
		
		return true;
	}
	
	public boolean temCreditos() {
		return Objects.nonNull(quantidadeCreditos) && quantidadeCreditos > 0;
	}
	
	public boolean temCreditosValidos() {
		return temCreditosValidos(null);
	}
	
	public boolean temCreditosValidos(Date data) {
		if(Objects.isNull(data)) {
			data = new Date();
		}
		
		return Objects.nonNull(quantidadeCreditos) 
				&& quantidadeCreditos > 0
				&& (Objects.isNull(validadeCreditos) || getValidadeCreditos().getTime() >= new Date().getTime());
	}
	
	public boolean temRegraDeAcessoPorPeriodoValido() {
		if (Objects.isNull(pedestreRegra) || pedestreRegra.isEmpty()) {
			return false;
		}
		
		for (PedestreRegraEntity pedestreRegra : pedestreRegra) {
			if (pedestreRegra.getRemovidoNoDesktop()) {
				continue;
			}

			if (pedestreRegra.isPeriodoValido()) {
				return true;
			}
		}

		return false;
	}
	
	public Optional<PedestreRegraEntity> getRegraAtiva() {
		if (Objects.isNull(pedestreRegra)) {
			return Optional.empty();
		}

		for (PedestreRegraEntity pedestreRegra : pedestreRegra) {
			if (pedestreRegra.temCreditos()
					&& pedestreRegra.isNaoRemovidoNoDesktop()) {
				return Optional.of(pedestreRegra);
			}
		}

		return Optional.empty();
	}
	
	public Optional<PedestreRegraEntity> getRegraAtivaPedestre() {
		if (Objects.isNull(pedestreRegra)) {
			return Optional.empty();
		}

		for (PedestreRegraEntity pedestreRegra : pedestreRegra) {
			if (pedestreRegra.isNaoRemovidoNoDesktop()) {
				return Optional.of(pedestreRegra);
			}
		}

		return Optional.empty();
	}
	
	
	public boolean temTipoEscala3x3() {
		if (Objects.isNull(pedestreRegra)) {
			return false;
		}

		final Optional<PedestreRegraEntity> regraAtiva = getRegraAtivaPedestre();
		
		return regraAtiva.isPresent() 
				? TipoRegra.ACESSO_ESCALA_3_3.equals(regraAtiva.get().getRegra().getTipo())
				: false;
	}
	
	
	public void apagarCartao() {
		if(Objects.nonNull(dataCadastroFotoNaHikivision)) {
			return;
		}
		
		setCardNumber(null); 
	}
	
	public void decrementaCreditos() {
		if (temRegraDeAcessoPorPeriodoValido()) {
			return;
		}
		
		if(temCreditos()) {
			setQuantidadeCreditos(getQuantidadeCreditos() - 1);
		}
		
		Optional<PedestreRegraEntity> regraAtiva = getRegraAtiva();
		
		if(regraAtiva.isPresent() && regraAtiva.get().temCreditos()) {
			regraAtiva.get().decrementaCreditos();
		}
	}
	
	public boolean isUltimoCredito() {
		return Long.valueOf(1).equals(quantidadeCreditos);
	}
	
	public boolean isVisitante() {
		return "VISITANTE".equals(tipo);
	}
	
	public boolean isPedestre() {
		return "PEDESTRE".equals(tipo);
	}
	
	public void decrementaQRCodeUso() {
		if (Objects.isNull(qrCodeParaAcesso) || !qrCodeParaAcesso.startsWith("U_")) {
			return;
		}
		
		String[] parts = qrCodeParaAcesso.split("_");
		Long usos = Long.valueOf(parts[parts.length - 1]);
		usos++;

		setQrCodeParaAcesso("U_" + EncryptionUtils.getRandomString(4) + "_" + usos);
		setEditadoNoDesktop(true);
		setDataAlteracao(new Date());
	}
	
	public boolean temMensagens() {
		return Objects.nonNull(mensagens) && mensagens.isEmpty();
	}
	
	public void decrementaMensagens() {
		for (PedestrianMessagesEntity message : mensagens) {
			if (Objects.nonNull(message.getQuantidade()) 
					&& message.getQuantidade() > 0) {
				message.setQuantidade(message.getQuantidade() - 1);
			}
		}
	}
	
	public boolean temTipoTurno() {
		return Objects.nonNull(tipoTurno) && !tipoTurno.isEmpty();
	}
	
	
	public boolean isInativo() {
		return "INATIVO".equals(status);
	}
	
	public boolean isAtivo() {
		return "ATIVO".equals(status);
	}
	
	public boolean isRemovido() {
		return Boolean.TRUE.equals(removido);
	}
	
	public boolean isQrCodeUsoDinamico() {
		return "DINAMICO_USO".equals(tipoQRCode);
	}
	
	public boolean isBirthday(Date data) {
		if (Objects.nonNull(dataNascimento)) {
			SimpleDateFormat sdf = new SimpleDateFormat("dd/MM");
			if (sdf.format(Objects.nonNull(data) ? data : new Date()).equals(sdf.format(dataNascimento))) {
				return true;
			}
		}
		return false;
	}
	
	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getLuxandIdentifier() {
		return luxandIdentifier;
	}

	public void setLuxandIdentifier(String luxandIdentifier) {
		this.luxandIdentifier = luxandIdentifier;
		this.setEditadoNoDesktop(true);
	}

	public String getName() {
		return name;
	}
	
	public String getFirstName(){
		if (name.indexOf(" ") != -1)
			return name.substring(0, name.indexOf(" "));
		else
			return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public String getCardNumber() {
		return cardNumber;
	}

	public void setCardNumber(String cardNumber) {
		this.cardNumber = cardNumber;
	}

	public List<TemplateEntity> getTemplates() {
		return templates;
	}

	public void setTemplates(List<TemplateEntity> templates) {
		this.templates = templates;
	}

	public String getTipo() {
		return tipo;
	}

	public void setTipo(String tipo) {
		this.tipo = tipo;
	}

	public Boolean getCadastradoNaCatracaRWTech() {
		return cadastradoNaCatracaRWTech;
	}

	public void setCadastradoNaCatracaRWTech(Boolean cadastradoNaCatracaRWTech) {
		this.cadastradoNaCatracaRWTech = cadastradoNaCatracaRWTech;
	}

	public Boolean getDesatualizadoNaCatracaRWTech() {
		return desatualizadoNaCatracaRWTech;
	}

	public void setDesatualizadoNaCatracaRWTech(Boolean desatualizadoNaCatracaRWTech) {
		this.desatualizadoNaCatracaRWTech = desatualizadoNaCatracaRWTech;
	}

	public List<AllowedTimeEntity> getHorariosPermitidos() {
		return horariosPermitidos;
	}

	public void setHorariosPermitidos(List<AllowedTimeEntity> horariosPermitidos) {
		this.horariosPermitidos = horariosPermitidos;
	}

	public Boolean getCadastradoNaCatraca() {
		return cadastradoNaCatraca;
	}

	public void setCadastradoNaCatraca(Boolean cadastradoNaCatraca) {
		this.cadastradoNaCatraca = cadastradoNaCatraca;
	}

	public Date getDataNascimento() {
		return dataNascimento;
	}

	public void setDataNascimento(Date dataNascimento) {
		this.dataNascimento = dataNascimento;
	}

	public byte[] getFoto() {
		return foto;
	}

	public void setFoto(byte[] foto) {
		this.foto = foto;
	}

	public String getUltimoAcessoToletus() {
		return ultimoAcessoToletus;
	}

	public void setUltimoAcessoToletus(String ultimoAcessoToletus) {
		this.ultimoAcessoToletus = ultimoAcessoToletus;
	}

	public Long getIdRegra() {
		return idRegra;
	}

	public void setIdRegra(Long idRegra) {
		this.idRegra = idRegra;
	}

	public Long getQuantidadeCreditos() {
		return quantidadeCreditos;
	}

	public void setQuantidadeCreditos(Long quantidadeCreditos) {
		this.quantidadeCreditos = quantidadeCreditos;
	}

	public Date getValidadeCreditos() {
		return validadeCreditos;
	}

	public void setValidadeCreditos(Date validadeCreditos) {
		this.validadeCreditos = validadeCreditos;
	}

	public String getTipoTurno() {
		return tipoTurno;
	}

	public void setTipoTurno(String tipoTurno) {
		this.tipoTurno = tipoTurno;
	}

	public Date getInicioTurno() {
		return inicioTurno;
	}

	public void setInicioTurno(Date inicioTurno) {
		this.inicioTurno = inicioTurno;
	}

	public List<PedestrianEquipamentEntity> getEquipamentos() {
		return equipamentos;
	}

	public void setEquipamentos(List<PedestrianEquipamentEntity> equipamentos) {
		this.equipamentos = equipamentos;
	}

	public List<PedestrianMessagesEntity> getMensagens() {
		return mensagens;
	}

	public void setMensagens(List<PedestrianMessagesEntity> mensagens) {
		this.mensagens = mensagens;
	}

	public Date getDataInicioPeriodo() {
		return dataInicioPeriodo;
	}

	public void setDataInicioPeriodo(Date dataInicioPeriodo) {
		this.dataInicioPeriodo = dataInicioPeriodo;
	}

	public Date getDataFimPeriodo() {
		return dataFimPeriodo;
	}

	public void setDataFimPeriodo(Date dataFimPeriodo) {
		this.dataFimPeriodo = dataFimPeriodo;
	}

	public Integer getOrigemCatraca() {
		return origemCatraca;
	}

	public void setOrigemCatraca(Integer origemCatraca) {
		this.origemCatraca = origemCatraca;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getCpf() {
		return cpf;
	}

	public void setCpf(String cpf) {
		this.cpf = cpf;
	}

	public String getGenero() {
		return genero;
	}

	public void setGenero(String genero) {
		this.genero = genero;
	}

	public String getRg() {
		return rg;
	}

	public void setRg(String rg) {
		this.rg = rg;
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

	public String getResponsavel() {
		return responsavel;
	}

	public void setResponsavel(String responsavel) {
		this.responsavel = responsavel;
	}

	public String getObservacoes() {
		return observacoes;
	}

	public void setObservacoes(String observacoes) {
		this.observacoes = observacoes;
	}

	public String getCep() {
		return cep;
	}

	public void setCep(String cep) {
		this.cep = cep;
	}

	public String getLogradouro() {
		return logradouro;
	}

	public void setLogradouro(String logradouro) {
		this.logradouro = logradouro;
	}

	public String getNumero() {
		return numero;
	}

	public void setNumero(String numero) {
		this.numero = numero;
	}

	public String getComplemento() {
		return complemento;
	}

	public void setComplemento(String complemento) {
		this.complemento = complemento;
	}

	public String getBairro() {
		return bairro;
	}

	public void setBairro(String bairro) {
		this.bairro = bairro;
	}

	public String getCidade() {
		return cidade;
	}

	public void setCidade(String cidade) {
		this.cidade = cidade;
	}

	public String getEstado() {
		return estado;
	}

	public void setEstado(String estado) {
		this.estado = estado;
	}

	public Boolean getCadastradoNoDesktop() {
		return cadastradoNoDesktop;
	}

	public void setCadastradoNoDesktop(Boolean cadastradoNoDesktop) {
		this.cadastradoNoDesktop = cadastradoNoDesktop;
	}

	public Boolean getEditadoNoDesktop() {
		return editadoNoDesktop;
	}

	public void setEditadoNoDesktop(Boolean editadoNoDesktop) {
		this.editadoNoDesktop = editadoNoDesktop;
	}

	public List<LogPedestrianAccessEntity> getListaAcessosTransient() {
		return listaAcessosTransient;
	}

	public void setListaAcessosTransient(List<LogPedestrianAccessEntity> listaAcessosTransient) {
		this.listaAcessosTransient = listaAcessosTransient;
	}

	public List<BiometricEntity> getListaBiometriasTransient() {
		return listaBiometriasTransient;
	}

	public void setListaBiometriasTransient(List<BiometricEntity> listaBiometriasTransient) {
		this.listaBiometriasTransient = listaBiometriasTransient;
	}

	public Boolean getRemovido() {
		return removido;
	}

	public void setRemovido(Boolean removido) {
		this.removido = removido;
	}

	public String getMatricula() {
		return matricula;
	}

	public void setMatricula(String matricula) {
		this.matricula = matricula;
	}

	public Boolean getSempreLiberado() {
		return sempreLiberado;
	}

	public void setSempreLiberado(Boolean sempreLiberado) {
		this.sempreLiberado = sempreLiberado;
	}

	public Boolean getHabilitarTeclado() {
		return habilitarTeclado;
	}

	public void setHabilitarTeclado(Boolean habilitarTeclado) {
		this.habilitarTeclado = habilitarTeclado;
	}

	public Date getLatestPhotosTaken() {
		return latestPhotosTaken;
	}

	public void setLatestPhotosTaken(Date latestPhotosTaken) {
		this.latestPhotosTaken = latestPhotosTaken;
	}

	public Boolean getFotosForamExcluidas() {
		return fotosForamExcluidas;
	}

	public void setFotosForamExcluidas(Boolean fotosForamExcluidas) {
		this.fotosForamExcluidas = fotosForamExcluidas;
	}

	public Date getDatePhotosExcluded() {
		return datePhotosExcluded;
	}

	public void setDatePhotosExcluded(Date datePhotosExcluded) {
		this.datePhotosExcluded = datePhotosExcluded;
	}

	public Long getIdTemp() {
		return idTemp;
	}

	public void setIdTemp(Long idTemp) {
		this.idTemp = idTemp;
	}

	public String getQrCodeParaAcesso() {
		return qrCodeParaAcesso;
	}

	public void setQrCodeParaAcesso(String qrCodeParaAcesso) {
		this.qrCodeParaAcesso = qrCodeParaAcesso;
	}

	public Boolean getCadastroFacialObrigatorio() {
		return cadastroFacialObrigatorio;
	}

	public void setCadastroFacialObrigatorio(Boolean cadastroFacialObrigatorio) {
		this.cadastroFacialObrigatorio = cadastroFacialObrigatorio;
	}

	public Long getIdEmpresa() {
		return idEmpresa;
	}

	public void setIdEmpresa(Long idEmpresa) {
		this.idEmpresa = idEmpresa;
	}

	public Long getIdDepartamento() {
		return idDepartamento;
	}

	public void setIdDepartamento(Long idDepartamento) {
		this.idDepartamento = idDepartamento;
	}

	public Long getIdCentroCusto() {
		return idCentroCusto;
	}

	public void setIdCentroCusto(Long idCentroCusto) {
		this.idCentroCusto = idCentroCusto;
	}

	public Long getIdCargo() {
		return idCargo;
	}

	public void setIdCargo(Long idCargo) {
		this.idCargo = idCargo;
	}

	public Boolean getEnviaSmsAoPassarNaCatraca() {
		return enviaSmsAoPassarNaCatraca;
	}

	public void setEnviaSmsAoPassarNaCatraca(Boolean enviaSmsAoPassarNaCatraca) {
		this.enviaSmsAoPassarNaCatraca = enviaSmsAoPassarNaCatraca;
	}

	public List<DocumentoEntity> getDocumentos() {
		return documentos;
	}

	public void setDocumentos(List<DocumentoEntity> documentos) {
		this.documentos = documentos;
	}

	public List<PedestreRegraEntity> getPedestreRegra() {
		return pedestreRegra;
	}

	public void setPedestreRegra(List<PedestreRegraEntity> pedestreRegra) {
		this.pedestreRegra = pedestreRegra;
	}

	public Integer getQtdAcessoAntesSinc() {
		return qtdAcessoAntesSinc;
	}

	public void setQtdAcessoAntesSinc(Integer qtdAcessoAntesSinc) {
		this.qtdAcessoAntesSinc = qtdAcessoAntesSinc;
	}

	public Long getTamanhoListaTemplates() {
		return tamanhoListaTemplates;
	}

	public void setTamanhoListaTemplates(Long tamanhoListaTemplates) {
		this.tamanhoListaTemplates = tamanhoListaTemplates;
	}

	public Long getIdUsuario() {
		return idUsuario;
	}

	public void setIdUsuario(Long idUsuario) {
		this.idUsuario = idUsuario;
	}

	public Boolean getInvisivel() {
		if(invisivel == null)
			return false;
		return invisivel;
	}

	public void setInvisivel(Boolean invisivel) {
		this.invisivel = invisivel;
	}

	public String getLogin() {
		return login;
	}

	public void setLogin(String login) {
		this.login = login;
	}

	public String getSenha() {
		return senha;
	}

	public void setSenha(String senha) {
		this.senha = senha;
	}

	public String getSenhaLivre() {
		return senhaLivre;
	}

	public void setSenhaLivre(String senhaLivre) {
		
		if(senhaLivre != null && !"".equals(senhaLivre)) {
			try {
				senha = EncryptionUtils.encrypt(senhaLivre);
			} catch (NoSuchAlgorithmException | UnsupportedEncodingException e) {
				e.printStackTrace();
			}
		}else {
			senha = null;
		}
		
		this.senhaLivre = senhaLivre;
	}

	public String getTipoAcesso() {
		return tipoAcesso;
	}

	public void setTipoAcesso(String tipoAcesso) {
		this.tipoAcesso = tipoAcesso;
	}

	public String getTipoQRCode() {
		return tipoQRCode;
	}

	public void setTipoQRCode(String tipoQRCode) {
		this.tipoQRCode = tipoQRCode;
	}
	
	public Boolean getNovasDigitais() {
		return novasDigitais;
	}

	public void setNovasDigitais(Boolean novasDigitais) {
		this.novasDigitais = novasDigitais;
	}

	public String getNomeUuarioQueCriou() {
		return nomeUuarioQueCriou;
	}

	public void setNomeUuarioQueCriou(String nomeUuarioQueCriou) {
		this.nomeUuarioQueCriou = nomeUuarioQueCriou;
	}

	public Date getDataCadastroFotoNaHikivision() {
		return dataCadastroFotoNaHikivision;
	}

	public void setDataCadastroFotoNaHikivision(Date dataCadastroFotoNaHikivision) {
		this.dataCadastroFotoNaHikivision = dataCadastroFotoNaHikivision;
	}

	public Date getLastAccessHikiVision() {
		return lastAccessHikiVision;
	}

	public void setLastAccessHikiVision(Date lastAccessHikiVision) {
		this.lastAccessHikiVision = lastAccessHikiVision;
	}
	
}
