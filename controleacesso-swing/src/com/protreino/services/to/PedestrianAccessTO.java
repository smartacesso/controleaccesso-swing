package com.protreino.services.to;

import java.util.Calendar;
import java.util.Date;
import java.util.List;

import com.protreino.services.entity.AllowedTimeEntity;
import com.protreino.services.entity.BaseEntity;
import com.protreino.services.entity.PedestrianEquipamentEntity;
import com.protreino.services.entity.PedestrianMessagesEntity;

@SuppressWarnings("serial")
public class PedestrianAccessTO extends BaseEntity{
	
	private Long id;
	private Long idTemp;
	private Long idUsuario;
	private String luxandIdentifier;
	private String name;
	private String cardNumber;
	private String status;
	private Date dataNascimento;
	//private Date dataCriacao;
	private String tipo;
	private String email;
	private String cpf;
	private String genero;
	private String rg;
	private String telefone;
	private String celular;
	private String responsavel;
	private String observacoes;
	private String matricula;
	private Boolean removido;
	private Boolean sempreLiberado;
	private Boolean habilitarTeclado;
	private String qrCodeParaAcesso;
	private Boolean cadastroFacialObrigatorio;
	
	private String cep;
	private String logradouro;
	private String numero;
	private String complemento;
	private String bairro;
	private String cidade;
	private String estado;
	
	private List<String> templates;
	
	private Long idRegra;
	private Long quantidadeCreditos;
	private Date validadeCreditos;
	private Date dataInicioPeriodo;
	private Date dataFimPeriodo;
	private String tipoTurno;
	private Date inicioTurno;
	
	private Boolean enviaSmsAoPassarNaCatraca;
	
	private Long idLocal;
	private Long idEmpresa;
	private Long idCargo;
	private Long idCentroCusto;
	private Long idDepartamento;
	
	private List<AllowedTimeEntity> horariosPermitidos;

	private List<PedestrianEquipamentEntity> equipamentos;
	private List<PedestrianMessagesEntity> mensagens;
	private List<DocumentoTo> documentos;
	private List<PedestreRegraTO> pedestreRegras;
	
	private String fotoBase64;
	
	private Integer qtdAcessoAntesSinc;
	
	private String login;
	private String senha;
	private String tipoAcesso;
	private String tipoQRCode;
	private Date dataCadastroFotoNaHikivision;

	public PedestrianAccessTO() {
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
			for(PedestrianMessagesEntity m : this.mensagens)
				stringBuilder.append(m.getId()).append(";");
		}
		
		if(this.templates != null) {
			for(String t : this.templates)
				stringBuilder.append(t).append(";");
		}
		
		if(this.equipamentos != null) {
			for(PedestrianEquipamentEntity e : this.equipamentos)
				stringBuilder.append(e.getId()).append(";");
		}
		
		if(this.documentos != null) {
			for(DocumentoTo doc : this.documentos)
				stringBuilder.append(doc.getId()).append(";");
		}
		
		if(this.pedestreRegras != null) {
			for(PedestreRegraTO pr : this.pedestreRegras)
				stringBuilder.append(pr.getId()).append(";");
		}
		
		if(this.horariosPermitidos != null) {
			for(AllowedTimeEntity a : this.horariosPermitidos) {
				if(a.getInicio() != null)
					stringBuilder.append(a.getInicio()).append(";");
			}
		}
		
		return stringBuilder.toString();
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

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public Date getDataNascimento() {
		return dataNascimento;
	}

	public void setDataNascimento(Date dataNascimento) {
		this.dataNascimento = dataNascimento;
	}

//	public Date getDataCriacao() {
//		return dataCriacao;
//	}
//
//	public void setDataCriacao(Date dataCriacao) {
//		this.dataCriacao = dataCriacao;
//	}

	public String getTipo() {
		return tipo;
	}

	public void setTipo(String tipo) {
		this.tipo = tipo;
	}

	public List<String> getTemplates() {
		return templates;
	}

	public void setTemplates(List<String> templates) {
		this.templates = templates;
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
	    if (inicioTurno == null) {
	        return inicioTurno; // Retorna null se inicioTurno for nulo
	    }

	    Calendar calendar = Calendar.getInstance();
	    calendar.setTime(inicioTurno);
	    // Ajuste a hora, por exemplo, subtraindo 3 horas
	    calendar.add(Calendar.HOUR_OF_DAY, -3);
	    return calendar.getTime();
		
	}

	public void setInicioTurno(Date inicioTurno) {
		this.inicioTurno = inicioTurno;
	}

	public List<AllowedTimeEntity> getHorariosPermitidos() {
		return horariosPermitidos;
	}

	public void setHorariosPermitidos(List<AllowedTimeEntity> horariosPermitidos) {
		this.horariosPermitidos = horariosPermitidos;
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

	public String getFotoBase64() {
		return fotoBase64;
	}
	public void setFotoBase64(String fotoBase64) {
		this.fotoBase64 = fotoBase64;
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
	public String getMatricula() {
		return matricula;
	}
	public void setMatricula(String matricula) {
		this.matricula = matricula;
	}
	public Boolean getRemovido() {
		return removido;
	}
	public void setRemovido(Boolean removido) {
		this.removido = removido;
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

	public Long getIdCargo() {
		return idCargo;
	}
	public void setIdCargo(Long idCargo) {
		this.idCargo = idCargo;
	}

	public Long getIdCentroCusto() {
		return idCentroCusto;
	}
	public void setIdCentroCusto(Long idCentroCusto) {
		this.idCentroCusto = idCentroCusto;
	}

	public Long getIdDepartamento() {
		return idDepartamento;
	}
	public void setIdDepartamento(Long idDepartamento) {
		this.idDepartamento = idDepartamento;
	}

	public Boolean getEnviaSmsAoPassarNaCatraca() {
		return enviaSmsAoPassarNaCatraca;
	}

	public void setEnviaSmsAoPassarNaCatraca(Boolean enviaSmsAoPassarNaCatraca) {
		this.enviaSmsAoPassarNaCatraca = enviaSmsAoPassarNaCatraca;
	}

	public List<DocumentoTo> getDocumentos() {
		return documentos;
	}

	public void setDocumentos(List<DocumentoTo> documentos) {
		this.documentos = documentos;
	}

	public List<PedestreRegraTO> getPedestreRegras() {
		return pedestreRegras;
	}

	public void setPedestreRegras(List<PedestreRegraTO> pedestreRegras) {
		this.pedestreRegras = pedestreRegras;
	}

	public Integer getQtdAcessoAntesSinc() {
		return qtdAcessoAntesSinc;
	}

	public void setQtdAcessoAntesSinc(Integer qtdAcessoAntesSinc) {
		this.qtdAcessoAntesSinc = qtdAcessoAntesSinc;
	}

	public Long getIdUsuario() {
		return idUsuario;
	}

	public void setIdUsuario(Long idUsuario) {
		this.idUsuario = idUsuario;
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

	public Date getDataCadastroFotoNaHikivision() {
		return dataCadastroFotoNaHikivision;
	}

	public void setDataCadastroFotoNaHikivision(Date dataCadastroFotoNaHikivision) {
		this.dataCadastroFotoNaHikivision = dataCadastroFotoNaHikivision;
	}

	public Long getIdLocal() {
		return idLocal;
	}

	public void setIdLocal(Long idLocal) {
		this.idLocal = idLocal;
	}
	
}