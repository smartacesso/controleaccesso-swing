package com.protreino.services.repository;

import java.io.File;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.ConnectException;
import java.net.Socket;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.codec.binary.Base64;

import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.entity.ObjectWithId;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.entity.TopdataFacialErrorEntity;
import com.protreino.services.entity.UserEntity;
import com.protreino.services.enumeration.TcpMessageType;
import com.protreino.services.main.Main;
import com.protreino.services.to.DeviceTO;
import com.protreino.services.to.TcpMessageTO;
import com.protreino.services.utils.Utils;

public class HibernateServerAccessData {

	public static boolean executando = false;
	public static boolean executandoPing = false;
	
	public static Socket clientSocket;
	public static ObjectOutputStream outToServer;
	public static Socket clientSocketFaceRecognizer;
	public static ObjectOutputStream outToServerFaceRecogizer;
	
	public static void openConnection() throws ConnectException {
		try {
			System.out.println("Abrindo conexao");
			clientSocket = new Socket(Main.getServidor().getIp(), Main.getServidor().getPort());
			outToServer = new ObjectOutputStream(clientSocket.getOutputStream());

			//clientSocketFaceRecognizer = new Socket(Main.getServidor().getIp(), Main.getServidor().getPort());
			//outToServerFaceRecogizer = new ObjectOutputStream(clientSocketFaceRecognizer.getOutputStream());

		} catch (ConnectException ce) {
			ce.printStackTrace();
			throw new ConnectException();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public static void closeConnetion() {
		try {
			System.out.println("Fechando conexao");
			if (outToServer != null) {
				outToServer.close();
			}
			if (clientSocket != null) {
				clientSocket.close();
			}
			if (clientSocketFaceRecognizer != null) {
				clientSocketFaceRecognizer.close();
			}
			if (outToServerFaceRecogizer != null) {
				outToServerFaceRecogizer.close();
			}

		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	private static synchronized void verificaExecucaoDePing() {
		while (executandoPing) {
			Utils.sleep(500);
		}
	}
	
	public static synchronized <T> Object getSingleResultById(Class<T> entityClass, Long id) {
		verificaExecucaoDePing();
		try {
			String classe = entityClass.getCanonicalName();

			TcpMessageTO req = new TcpMessageTO(TcpMessageType.GET_SINGLE_RESULT_BY_ID);
			req.getParans().put("entityClass", classe);
			req.getParans().put("id", id);

			executando = true;
			outToServer.writeObject(req);
			outToServer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			return resp.getParans().get("object");

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}

		return null;
	}
	
	public static synchronized <T> Object getAllPedestresById(Long id) {
		verificaExecucaoDePing();
		try {
			TcpMessageTO req = new TcpMessageTO(TcpMessageType.GET_ALL_PEDESTRIAN_BY_ID);
			req.getParans().put("id", id);

			executando = true;
			outToServer.writeObject(req);
			outToServer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			return resp.getParans().get("object");

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}
		
		return null;
	}
	
	public static synchronized int getResultListCount(Class<?> entityClass, String namedQuery, HashMap<String, Object> args) {
		verificaExecucaoDePing();
		try {
			String classe = entityClass.getCanonicalName();

			TcpMessageTO req = new TcpMessageTO(TcpMessageType.GET_RESULT_LIST_COUNT);
			req.getParans().put("entityClass", classe);
			req.getParans().put("namedQuery", namedQuery);
			req.getParans().put("args", args);

			executando = true;

			outToServer.writeObject(req);
			outToServer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			return (int) resp.getParans().get("count");

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}
		
		return 0;
	}
	
	@SuppressWarnings("unchecked")
	public static synchronized List<LogPedestrianAccessEntity> buscaLogsDeAcessoPaginados(String namedQuery,
			HashMap<String, Object> args, Integer inicio, Integer quantidade) {
		verificaExecucaoDePing();
		try {

			TcpMessageTO req = new TcpMessageTO(TcpMessageType.BUSCA_LOGS_DE_ACESSO_PAGINADOS);
			req.getParans().put("namedQuery", namedQuery);
			req.getParans().put("args", args);
			req.getParans().put("inicio", inicio);
			req.getParans().put("quantidade", quantidade);

			executando = true;
			outToServer.writeObject(req);
			outToServer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			return (List<LogPedestrianAccessEntity>) resp.getParans().get("list");

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}
		
		return null;
	}
	
	public static synchronized <T> List<?> getResultList(Class<T> entityClass, String namedQuery) {
		verificaExecucaoDePing();
		try {
			String classe = entityClass.getCanonicalName();

			TcpMessageTO req = new TcpMessageTO(TcpMessageType.GET_RESULT_LIST);
			req.getParans().put("entityClass", classe);
			req.getParans().put("namedQuery", namedQuery);

			executando = true;

			outToServer.writeObject(req);
			outToServer.flush();
			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			return (List<?>) resp.getParans().get("list");

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}
		
		return null;
	}
	
	public static synchronized <T> List<?> buscaListaDevicesDoServidor(Class<T> entityClass, String namedQuery) {
		verificaExecucaoDePing();
		try {
			String classe = entityClass.getCanonicalName();

			TcpMessageTO req = new TcpMessageTO(TcpMessageType.GET_ALL_DEVICES_FROM_SERVER);
			req.getParans().put("entityClass", classe);
			req.getParans().put("namedQuery", namedQuery);

			executando = true;

			outToServer.writeObject(req);
			outToServer.flush();
			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			return (List<?>) resp.getParans().get("list");

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}
		
		return null;
	}
	
	public static synchronized <T> List<?> getResultListLimited(Class<T> entityClass, String namedQuery, Long quantidade) {
		verificaExecucaoDePing();
		try {
			String classe = entityClass.getCanonicalName();

			TcpMessageTO req = new TcpMessageTO(TcpMessageType.GET_RESULT_LIST_LIMITED);
			req.getParans().put("entityClass", classe);
			req.getParans().put("namedQuery", namedQuery);
			req.getParans().put("quantidade", quantidade);

			executando = true;

			outToServer.writeObject(req);
			outToServer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			return (List<?>) resp.getParans().get("list");

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}
		
		return null;
	}
	
	public static synchronized <T> List<?> getResultListWithParams(Class<T> entityClass, String namedQuery,
			HashMap<String, Object> args, Integer inicio, Integer quantidade) {
		verificaExecucaoDePing();
		try {
			String classe = entityClass.getCanonicalName();

			TcpMessageTO req = new TcpMessageTO(TcpMessageType.GET_RESULT_LIST_WITH_PARAMS);
			req.getParans().put("entityClass", classe);
			req.getParans().put("namedQuery", namedQuery);
			req.getParans().put("args", args);
			req.getParans().put("inicio", inicio);
			req.getParans().put("quantidade", quantidade);

			executando = true;
			outToServer.writeObject(req);
			outToServer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			return (List<?>) resp.getParans().get("list");

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}
		
		return null;
	}
	
	public static synchronized int getResultListWithParamsCount(Class<?> entityClass, String namedQuery, HashMap<String, Object> args) {
		verificaExecucaoDePing();
		try {
			String classe = entityClass.getCanonicalName();

			TcpMessageTO req = new TcpMessageTO(TcpMessageType.GET_RESULT_LIST_WITH_PARAMS_COUNT);
			req.getParans().put("entityClass", classe);
			req.getParans().put("namedQuery", namedQuery);
			req.getParans().put("args", args);

			executando = true;
			outToServer.writeObject(req);
			outToServer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			return (Integer) resp.getParans().get("count");

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}
		
		return 0;
	}
	
	public static synchronized <T> Object getUniqueResultWithParams(Class<T> entityClass, String namedQuery, HashMap<String, Object> args) {
		verificaExecucaoDePing();
		try {
			String classe = entityClass.getCanonicalName();

			TcpMessageTO req = new TcpMessageTO(TcpMessageType.GET_UNIQUE_RESULT_WITH_PARAMS);
			req.getParans().put("entityClass", classe);
			req.getParans().put("namedQuery", namedQuery);
			req.getParans().put("args", args);

			executando = true;
			outToServer.writeObject(req);
			outToServer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			return resp.getParans().get("object");

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}
		
		return null;
	}
	
	public static synchronized <T> List<?> getResultListWithDynamicParams(Class<T> entityClass, String construtor,
			String join, String groupBy, String orderColumn, HashMap<String, Object> args, Integer inicio, Integer quantidade) {
		verificaExecucaoDePing();
		try {
			String classe = entityClass.getCanonicalName();

			TcpMessageTO req = new TcpMessageTO(TcpMessageType.GET_RESULT_LIST_WITH_DYNAMIC_PARAMS);
			req.getParans().put("entityClass", classe);
			req.getParans().put("construtor", construtor);
			req.getParans().put("join", join);
			req.getParans().put("groupBy", groupBy);
			req.getParans().put("orderColumn", orderColumn);
			req.getParans().put("args", args);
			req.getParans().put("inicio", inicio);
			req.getParans().put("quantidade", quantidade);

			executando = true;
			outToServer.writeObject(req);
			outToServer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			return (List<?>) resp.getParans().get("list");

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}
		
		return null;
	}
	
	public static synchronized int getResultListWithDynamicParamsCount(Class<?> entityClass, String construtor,
			String join, String groupBy, HashMap<String, Object> args) {
		verificaExecucaoDePing();
		try {
			String classe = entityClass.getCanonicalName();

			TcpMessageTO req = new TcpMessageTO(TcpMessageType.GET_RESULT_LIST_WITH_DYNAMIC_PARAMS_COUNT);
			req.getParans().put("entityClass", classe);
			req.getParans().put("construtor", construtor);
			req.getParans().put("join", join);
			req.getParans().put("groupBy", groupBy);
			req.getParans().put("args", args);

			executando = true;
			outToServer.writeObject(req);
			outToServer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());

			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			return (Integer) resp.getParans().get("count");

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}
		
		return 0;
	}
	
	public static synchronized <T> Object getSingleResult(Class<T> entityClass, String namedQuery) {
		verificaExecucaoDePing();
		try {
			String classe = entityClass.getCanonicalName();

			TcpMessageTO req = new TcpMessageTO(TcpMessageType.GET_SINGLE_RESULT);
			req.getParans().put("entityClass", classe);
			req.getParans().put("namedQuery", namedQuery);

			executando = true;
			outToServer.writeObject(req);
			outToServer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			return resp.getParans().get("object");

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}
		
		return null;
	}
	
	public static synchronized long countAcessosPedestre(Long idPedestre) {
		verificaExecucaoDePing();
		try {
			TcpMessageTO req = new TcpMessageTO(TcpMessageType.COUNT_ACESSOS_PEDESTRE);
			req.getParans().put("idPedestre", idPedestre);

			executando = true;
			outToServer.writeObject(req);
			outToServer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			return (long) resp.getParans().get("qtdeAcessos");

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}
		
		return 0l;
	}
	
	public static synchronized <T> Object getSingleResultByCardNumber(Class<T> entityClass, Long cardNumber) {
		verificaExecucaoDePing();
		try {
			String classe = entityClass.getCanonicalName();

			TcpMessageTO req = new TcpMessageTO(TcpMessageType.GET_SINGLE_RESULT_BY_CARD_NUMBER);
			req.getParans().put("entityClass", classe);
			req.getParans().put("cardNumber", cardNumber);

			executando = true;
			outToServer.writeObject(req);
			outToServer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			return resp.getParans().get("object");

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}
		
		return null;
	}
	
	public static synchronized <T> Object getSingleResultByRG(Class<T> entityClass, String rg) {
		verificaExecucaoDePing();
		try {
			String classe = entityClass.getCanonicalName();

			TcpMessageTO req = new TcpMessageTO(TcpMessageType.GET_SINGLE_RESULT_BY_RG);
			req.getParans().put("entityClass", classe);
			req.getParans().put("rg", rg);

			executando = true;
			outToServer.writeObject(req);
			outToServer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			return resp.getParans().get("object");

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}
		
		return null;
	}
	
	public static synchronized <T> Object getSingleResultByCPF(Class<T> entityClass, String cpf) {
		verificaExecucaoDePing();
		try {
			String classe = entityClass.getCanonicalName();
			
			TcpMessageTO req = new TcpMessageTO(TcpMessageType.GET_SINGLE_RESULT_BY_CPF);
			req.getParans().put("entityClass", classe);
			req.getParans().put("cpf", cpf);
			
			executando = true;
			outToServer.writeObject(req);
			outToServer.flush();
			
			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();
			
			return resp.getParans().get("object");
			
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}
		
		return null;
	}
	
	public static synchronized Object[] save(Class<?> classeEntidade, ObjectWithId object) {
		Object[] retorno = null;

		verificaExecucaoDePing();
		try {
			String classe = classeEntidade.getCanonicalName();
			Object objeto = object;

			TcpMessageTO req = new TcpMessageTO(TcpMessageType.SAVE);
			req.getParans().put("classeEntidade", classe);
			req.getParans().put("object", objeto);

			executando = true;
			outToServer.writeObject(req);
			outToServer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			retorno = (Object[]) resp.getParans().get("object");

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}

		return retorno;
	}
	
	public static synchronized Object[] update(Class<?> classeEntidade, ObjectWithId object) {
		Object[] retorno = null;

		verificaExecucaoDePing();
		try {
			String classe = classeEntidade.getCanonicalName();
			Object objeto = object;

			TcpMessageTO req = new TcpMessageTO(TcpMessageType.UPDATE);
			req.getParans().put("classeEntidade", classe);
			req.getParans().put("object", objeto);

			executando = true;
			outToServer.writeObject(req);
			outToServer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			retorno = (Object[]) resp.getParans().get("object");

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}

		return retorno;
	}
	
	public static synchronized void remove(Object object) {
		verificaExecucaoDePing();
		try {
			TcpMessageTO req = new TcpMessageTO(TcpMessageType.REMOVE);
			req.getParans().put("object", object);

			executando = true;
			outToServer.writeObject(req);
			outToServer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			reader.readObject();

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}
	}
	
	public static synchronized UserEntity buscaUsuarioPeloLogin(String loginName, String password) {
		verificaExecucaoDePing();
		try {
			TcpMessageTO req = new TcpMessageTO(TcpMessageType.BUSCA_USUARIO_PELO_LOGIN);
			req.getParans().put("loginName", loginName);
			req.getParans().put("password", password);

			executando = true;
			outToServer.writeObject(req);
			outToServer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			return (UserEntity) resp.getParans().get("user");

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}
		
		return null;
	}
	
	public static synchronized <T> Object getSingleResultByRegistration(Class<T> entityClass, Long matricula) {
		verificaExecucaoDePing();

		try {
			TcpMessageTO req = new TcpMessageTO(TcpMessageType.GET_SINGLE_RESULT_BY_REGISTRATION);
			String classe = entityClass.getCanonicalName();
			req.getParans().put("entityClass", classe);
			req.getParans().put("cardNumber", matricula);

			executando = true;
			outToServer.writeObject(req);
			outToServer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();
			return resp.getParans().get("object");

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}
		
		return null;
	}
	
	public static synchronized void uploadFotosParaServidorDeReconhecimento(String caminhoDasFotos, Long idUsuario) {
		if (!Main.temServidor() || Main.getServidor().isNotConnected()) {
			return;
		}
		
		try {
			TcpMessageTO req = new TcpMessageTO(TcpMessageType.UPLOAD_FOTOS_PARA_SERVIDOR_DE_RECONHECIMENTO);
			byte[] zipFileByteArray = converteFotosParaArrayDeBytes(caminhoDasFotos);

			if (zipFileByteArray != null) {
				req.getParans().put("zipFileByteArray", Base64.encodeBase64String(zipFileByteArray));
				req.getParans().put("idUsuario", idUsuario);

				outToServerFaceRecogizer.writeObject(req);
				outToServerFaceRecogizer.flush();

				ObjectInputStream reader = new ObjectInputStream(clientSocketFaceRecognizer.getInputStream());
				reader.readObject();

				apagarFotosLocais(caminhoDasFotos);
			}

		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public static synchronized void apagarPastaDeFotos(Long idUser) {
		try {
			TcpMessageTO req = new TcpMessageTO(TcpMessageType.APAGAR_PASTA_DE_FOTOS);
			req.getParans().put("idUser", idUser);

			outToServerFaceRecogizer.writeObject(req);
			outToServerFaceRecogizer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocketFaceRecognizer.getInputStream());
			reader.readObject();

		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public static synchronized void registraNovasFotosPedestre(Long idUsuario) {
		try {
			TcpMessageTO req = new TcpMessageTO(TcpMessageType.REGISTRA_NOVAS_FOTOS_PEDESTRE);
			req.getParans().put("idUsuario", idUsuario);

			outToServerFaceRecogizer.writeObject(req);
			outToServerFaceRecogizer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocketFaceRecognizer.getInputStream());
			reader.readObject();

		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public static synchronized void registraExclusaoFotosPedestre(Long idUsuario) {
		try {
			TcpMessageTO req = new TcpMessageTO(TcpMessageType.REGISTRA_EXCLUSAO_FOTOS_PEDESTRE);
			req.getParans().put("idUsuario", idUsuario);

			outToServerFaceRecogizer.writeObject(req);
			outToServerFaceRecogizer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocketFaceRecognizer.getInputStream());
			reader.readObject();

		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public static synchronized Boolean isPastaDeFotosExistente(Integer idUser) {
		try {
			TcpMessageTO req = new TcpMessageTO(TcpMessageType.IS_PASTA_DE_FOTOS_EXISTENTE);
			req.getParans().put("idUser", idUser);

			outToServerFaceRecogizer.writeObject(req);
			outToServerFaceRecogizer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocketFaceRecognizer.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			return (Boolean) resp.getParans().get("resp");

		} catch (Exception e) {
			e.printStackTrace();
		}
		
		return false;
	}
	
	public static synchronized void enviaInicioVerificandoAcesso() {
		try {
			TcpMessageTO req = new TcpMessageTO(TcpMessageType.ACCESS_VALIDATING_INI);

			outToServerFaceRecogizer.writeObject(req);
			outToServerFaceRecogizer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocketFaceRecognizer.getInputStream());
			reader.readObject();

		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public static synchronized void enviaFimVerificandoAcesso() {
		try {
			TcpMessageTO req = new TcpMessageTO(TcpMessageType.ACCESS_VALIDATING_FIM);

			outToServerFaceRecogizer.writeObject(req);
			outToServerFaceRecogizer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocketFaceRecognizer.getInputStream());
			reader.readObject();

		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	@SuppressWarnings("unchecked")
	public static synchronized List<DeviceTO> getListDeviceFromServer() {
		List<DeviceTO> result = null;
		
		verificaExecucaoDePing();
		try {
			TcpMessageTO req = new TcpMessageTO(TcpMessageType.GET_DEVICES_FROM_SERVER);
			executando = true;
			outToServer.writeObject(req);
			outToServer.flush();
			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			result = (List<DeviceTO>) resp.getParans().get("list");
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}
		
		return result;
	}
	
	public static synchronized void liberarAcessoNoServidor(String indentifier, String sentido) {
		verificaExecucaoDePing();

		try {
			TcpMessageTO req = new TcpMessageTO(TcpMessageType.LIBERAR_ACESSO_DEVICE_NO_SERVIDOR);
			req.getParans().put("indentifier", indentifier);
			req.getParans().put("sentido", sentido);

			executando = true;
			outToServer.writeObject(req);
			outToServer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			reader.readObject();

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}

	}
	
	public static synchronized <T> Object getSingleResultByIdTemp(Class<PedestrianAccessEntity> entityClass, Long id) {
		verificaExecucaoDePing();
		try {
			String classe = entityClass.getCanonicalName();

			TcpMessageTO req = new TcpMessageTO(TcpMessageType.GET_SINGLE_RESULT_BY_ID);
			req.getParans().put("entityClass", classe);
			req.getParans().put("id", id);

			executando = true;
			outToServer.writeObject(req);
			outToServer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			return resp.getParans().get("object");

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}
		
		return null;
	}
	
	public static void EnviaComandoCadastroFotoTopDataServidor(String card, String nome, byte[] foto) {
	    verificaExecucaoDePing();

	    if (card == null || card.isEmpty()) {
	        throw new IllegalArgumentException("O número do cartão não pode ser nulo ou vazio.");
	    }
	    if (nome == null || nome.isEmpty()) {
	        throw new IllegalArgumentException("O nome não pode ser nulo ou vazio.");
	    }
	    if (foto == null || foto.length == 0) {
	        throw new IllegalArgumentException("A foto não pode ser nula ou vazia.");
	    }


	    try {
	        TcpMessageTO req = new TcpMessageTO(TcpMessageType.SEND_COMMAND_TO_DEVICES);
	        req.getParans().put("cardNumber", card);
	        req.getParans().put("name", nome);
	        req.getParans().put("foto", foto);

	        if (outToServer == null) {
	            throw new IllegalStateException("Conexão com o servidor não está inicializada.");
	        }

	        outToServer.writeObject(req);
	        outToServer.flush();
	        
	        // Ler a resposta do servidor para evitar inconsistências no fluxo
	        ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
	        TcpMessageTO resp = (TcpMessageTO) reader.readObject();

	        System.out.println("Resposta do servidor: " + resp.getParans().get("status"));
	        
	    } catch (Exception e) {
	        e.printStackTrace();
	        throw new RuntimeException("Erro ao enviar comando para o servidor", e);
	    } finally {
	        executando = false;
	    }
	}
	
	public static List<TopdataFacialErrorEntity> BuscaErrosTopDataServidor() {
	    verificaExecucaoDePing();

	    try {
	        TcpMessageTO req = new TcpMessageTO(TcpMessageType.GET_ERROS_TOP_DATA_LIST);

	        if (outToServer == null) {
	            throw new IllegalStateException("Conexão com o servidor não está inicializada.");
	        }

	        outToServer.writeObject(req);
	        outToServer.flush();
	        
	        // Ler a resposta do servidor para evitar inconsistências no fluxo
	        ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			return (List<TopdataFacialErrorEntity>) resp.getParans().get("list");
	        
	    } catch (Exception e) {
	        e.printStackTrace();
	        throw new RuntimeException("Erro ao enviar comando para o servidor", e);
	    } finally {
	        executando = false;
	    }
	}
	
	public static Integer CountBuscaErrosTopDataServidor() {
	    verificaExecucaoDePing();

	    try {
	        TcpMessageTO req = new TcpMessageTO(TcpMessageType.GET_ERROS_TOP_DATA_LIST_COUNT);

	        if (outToServer == null) {
	            throw new IllegalStateException("Conexão com o servidor não está inicializada.");
	        }

	        outToServer.writeObject(req);
	        outToServer.flush();
	        
	        // Ler a resposta do servidor para evitar inconsistências no fluxo
			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			return (Integer) resp.getParans().get("count");
	        
	    } catch (Exception e) {
	        e.printStackTrace();
	        throw new RuntimeException("Erro ao enviar comando para o servidor", e);
	    } finally {
	        executando = false;
	    }
	}
	
	public static void resetStatusAllCards() {
		verificaExecucaoDePing();
		try {
			TcpMessageTO req = new TcpMessageTO(TcpMessageType.RESET_STATUS_ALL_CARDS);

			executando = true;
			outToServer.writeObject(req);
			outToServer.flush();

			ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
			TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			resp.getParans().get("object");

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			executando = false;
		}
	}
	
	private static byte[] converteFotosParaArrayDeBytes(String caminhoDasFotos) {
		String zipFilePath = caminhoDasFotos + ".zip";
		byte[] zipFileByteArray = null;

		try {
			Main.zip(caminhoDasFotos, zipFilePath);
			zipFileByteArray = Files.readAllBytes(Paths.get(zipFilePath));
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			Main.apagarArquivo(zipFilePath);
		}

		return zipFileByteArray;
	}
	
	private static void apagarFotosLocais(String caminhoDasFotos) {
		File pastaFotos = new File(caminhoDasFotos);
		try {
			deleteDirectoryRecursion(pastaFotos.toPath());
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private static void deleteDirectoryRecursion(Path path) throws IOException {
		if (Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS)) {
			try (DirectoryStream<Path> entries = Files.newDirectoryStream(path)) {
				for (Path entry : entries) {
					deleteDirectoryRecursion(entry);
				}
			}
		}
		if (Files.exists(path)) {
			Files.delete(path);
		}
	}
	
}
