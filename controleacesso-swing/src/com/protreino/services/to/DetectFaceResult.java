package com.protreino.services.to;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class DetectFaceResult {
	
	private int resultCode;
	private String resultDescription;
	private Face face;
	
	public DetectFaceResult() {
		this.resultCode = -1;
		this.resultDescription = errors.getOrDefault(resultCode, String.valueOf(resultCode));
	}
	
	public int getResultCode() {
		return resultCode;
	}
	
	public void setResultCode(int result) {
		this.resultCode = result;
		this.resultDescription = errors.getOrDefault(resultCode, String.valueOf(resultCode));
	}
	
	public String getResultDescription() {
		return resultDescription;
	}

	public void setResultDescription(String resultDescription) {
		this.resultDescription = resultDescription;
	}

	public Face getFace() {
		return face;
	}

	public void setFace(Face face) {
		this.face = face;
	}
	
	@SuppressWarnings("serial")
	Map<Integer, String> errors = new HashMap<Integer, String>() {{
		put(0, "OK"); // FSDKE_OK
		put(-1, "Falha"); // FSDKE_FAILED
		put(-2, "SDK naoo ativada"); // FSDKE_NOT_ACTIVATED
		put(-3, "Falta de meméria"); // FSDKE_OUT_OF_MEMORY
		put(-4, "Argumentos inválidos"); // FSDKE_INVALID_ARGUMENT
		put(-5, "Erro de entrada de dados"); // FSDKE_IO_ERROR
		put(-6, "Imagem muito pequena"); // FSDKE_IMAGE_TOO_SMALL
		put(-7, "Rosto nao encontrado"); // FSDKE_FACE_NOT_FOUND
		put(-8, "Tamanho de buffer insuficiente"); // FSDKE_INSUFFICIENT_BUFFER_SIZE
		put(-9, "Extensão de arquivo nao suportado"); // FSDKE_UNSUPPORTED_IMAGE_EXTENSION
		put(-10, "Não foi possivel abrir o arquivo"); // FSDKE_CANNOT_OPEN_FILE
		put(-11, "Não foi possivel criar o arquivo"); // FSDKE_CANNOT_CREATE_FILE
		put(-12, "Formato de arquivo inválido"); // FSDKE_BAD_FILE_FORMAT
		put(-13, "Arquivo nao encontrado"); // FSDKE_FILE_NOT_FOUND
		put(-14, "Conexão fechada"); // FSDKE_CONNECTION_CLOSED
		put(-15, "Conexão falhou"); // FSDKE_CONNECTION_FAILED
		put(-16, "Inicialização IP falhou"); // FSDKE_IP_INIT_FAILED
		put(-17, "Necessita ativação do servidor"); // FSDKE_NEED_SERVER_ACTIVATION
		put(-18, "ID nao encontrado"); // FSDKE_ID_NOT_FOUND
		put(-19, "Atributo nao detectado"); // FSDKE_ATTRIBUTE_NOT_DETECTED
		put(-20, "Limite de memória do tracker insuficiente"); // FSDKE_INSUFFICIENT_TRACKER_MEMORY_LIMIT
		put(-21, "Atributo desconhecido"); // FSDKE_UNKNOWN_ATTRIBUTE
		put(-22, "Versao de arquivo nao suportada"); // FSDKE_UNSUPPORTED_FILE_VERSION
		put(-23, "Erro de sintaxe"); // FSDKE_SYNTAX_ERROR
		put(-24, "Parêmetro nao encontrado"); // FSDKE_PARAMETER_NOT_FOUND
		put(-25, "Template inválido"); // FSDKE_INVALID_TEMPLATE
		put(-26, "Versao do template nao suportada"); // FSDKE_UNSUPPORTED_TEMPLATE_VERSION
		put(-27, "índice da câmera nao existe"); // FSDKE_CAMERA_INDEX_DOES_NOT_EXIST
		put(-28, "Plataforma nao licensiada"); // FSDKE_PLATFORM_NOT_LICENSED
		put(-29, "Muitos rostos na imagem"); // FSDKE_TOO_MANY_FACES_ON_IMAGE
		put(-30, "Exceção no servidor"); // FSDKE_SERVER_EXCEPTION
		put(-31, "Usuario ja registrado"); // FSDKE_USER_ALREADY_REGISTERED
		put(-32, "Usuario registrado com outro nome"); // FSDKE_USER_REGISTERED_WITH_OTHER_NAME
		put(-33, "Reconhecimento falhou"); // FSDKE_RECOGNITION_FAILED
		put(-34, "Erro ao converter imagem para buffer"); // FSDKE_IMAGE_TO_BUFFER_ERROR
		put(-35, "Usuario nao encontrado"); // FSDKE_USER_NOT_FOUND
		put(-36, "Usuario nao está logado no servidor"); // FSDKE_USER_NOT_LOGGED_IN
		put(-37, "Exceção local"); // FSDKE_LOCAL_EXCEPTION
	}};

}
