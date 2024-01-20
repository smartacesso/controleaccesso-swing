package com.protreino.services.utils;

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
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import javax.persistence.TypedQuery;

import org.apache.commons.codec.binary.Base64;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.boot.MetadataSources;
import org.hibernate.cfg.Configuration;
import org.hibernate.query.Query;
import org.hibernate.service.ServiceRegistry;
import org.hibernate.tool.hbm2ddl.SchemaExport;
import org.hibernate.tool.hbm2ddl.SchemaExport.Action;
import org.hibernate.tool.schema.TargetType;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.protreino.services.constants.Origens;
import com.protreino.services.constants.Tipo;
import com.protreino.services.devices.ControlIdDevice;
import com.protreino.services.devices.Device;
import com.protreino.services.devices.FacialDevice;
import com.protreino.services.devices.TopDataAcessoDevice;
import com.protreino.services.devices.TopDataDevice;
import com.protreino.services.entity.BiometricEntity;
import com.protreino.services.entity.ConfigurationGroupEntity;
import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.entity.ObjectWithId;
import com.protreino.services.entity.PedestreRegraEntity;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.entity.PedestrianEquipamentEntity;
import com.protreino.services.entity.PreferenceEntity;
import com.protreino.services.entity.TemplateEntity;
//import com.protreino.services.entity.URLEntity;
import com.protreino.services.entity.UserEntity;
import com.protreino.services.enumeration.BroadcastMessageType;
import com.protreino.services.enumeration.NotificationType;
import com.protreino.services.enumeration.TcpMessageType;
import com.protreino.services.enumeration.TipoEscala;
import com.protreino.services.enumeration.VerificationResult;
import com.protreino.services.exceptions.QrcodeVencidoException;
import com.protreino.services.main.Main;
import com.protreino.services.to.AttachedTO;
import com.protreino.services.to.BroadcastMessageTO;
import com.protreino.services.to.DeviceTO;
import com.protreino.services.to.TcpMessageTO;
import com.protreino.services.to.hikivision.HikivisionDeviceTO;
import com.protreino.services.usecase.HikivisionUseCases;
import com.topdata.easyInner.enumeradores.Enumeradores;

public class HibernateUtil {

	private static SessionFactory sessionFactory;
	private static boolean aniversariante = false;
	private static PedestrianAccessEntity matchedAthleteAccess;
	public static Socket clientSocket;
	public static ObjectOutputStream outToServer;
	public static Socket clientSocketFaceRecognizer;
	public static ObjectOutputStream outToServerFaceRecogizer;

	public static boolean executando = false;
	public static boolean executandoPing = false;

	public static SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss:sss");

	static {
		try {
			sessionFactory = new Configuration().configure("hibernate.cfg.xml").buildSessionFactory();

		} catch (Throwable ex) {
			System.err.println("Initial SessionFactory creation failed." + ex);
			throw new ExceptionInInitializerError(ex);
		}
	}

	public static void openConnection() throws ConnectException {
		try {
			System.out.println("Abrindo conexao");
			clientSocket = new Socket(Main.servidor.getIp(), Main.servidor.getPort());
			outToServer = new ObjectOutputStream(clientSocket.getOutputStream());

			clientSocketFaceRecognizer = new Socket(Main.servidor.getIp(), Main.servidor.getPort());
			outToServerFaceRecogizer = new ObjectOutputStream(clientSocketFaceRecognizer.getOutputStream());

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
			if (outToServer != null)
				outToServer.close();
			if (clientSocket != null)
				clientSocket.close();
			if (clientSocketFaceRecognizer != null)
				clientSocketFaceRecognizer.close();
			if (outToServerFaceRecogizer != null)
				outToServerFaceRecogizer.close();

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public static SessionFactory getSessionFactory() {
		if (sessionFactory != null && !sessionFactory.isOpen())
			sessionFactory = new Configuration().configure("hibernate.cfg.xml").buildSessionFactory();
		return sessionFactory;
	}

	public static void shutdown() {
		if (getSessionFactory().isOpen()) {
			if (getSessionFactory().getCurrentSession().getTransaction().isActive())
				getSessionFactory().getCurrentSession().getTransaction().rollback();
			if (getSessionFactory().getCurrentSession().isOpen())
				getSessionFactory().getCurrentSession().close();
			getSessionFactory().close();
		}
	}

	private static synchronized void verificaExecucaoDePing() {
		while (executandoPing) {
			Utils.sleep(500);
		}
	}

	public static synchronized <T> Object getSingleResultById(Class<T> entityClass, Long id) {
		Object result = null;

		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return null;

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

				result = resp.getParans().get("object");

			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				executando = false;
			}

		} else {
			Session session = getSessionFactory().getCurrentSession();
			if (session.getTransaction() == null || !session.getTransaction().isActive())
				session.beginTransaction();

			try {

				Query<?> query = session.createNamedQuery(entityClass.getSimpleName()
						+ (entityClass.equals(PedestrianAccessEntity.class) ? ".findByIdNaoRemovido" : ".findById"),
						entityClass);
				query.setParameter("ID", id);
				List<?> resultList = (List<?>) query.getResultList();
				if (resultList.isEmpty())
					result = null;
				else
					result = resultList.get(0);
				session.getTransaction().commit();

			} catch (Exception e) {
				result = null;
				session.getTransaction().rollback();
				e.printStackTrace();

			} finally {
				session.close();
			}
		}

		return result;
	}

	public static synchronized <T> Object getAllPedestresById(Long id) {
		Object result = null;

		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return null;

			verificaExecucaoDePing();
			try {
				TcpMessageTO req = new TcpMessageTO(TcpMessageType.GET_ALL_PEDESTRIAN_BY_ID);
				req.getParans().put("id", id);

				executando = true;
				outToServer.writeObject(req);
				outToServer.flush();

				ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
				TcpMessageTO resp = (TcpMessageTO) reader.readObject();

				result = resp.getParans().get("object");

			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				executando = false;
			}

		} else {
			Session session = getSessionFactory().getCurrentSession();
			if (session.getTransaction() == null || !session.getTransaction().isActive())
				session.beginTransaction();

			try {

				Query<?> query = session.createNamedQuery(PedestrianAccessEntity.class.getSimpleName() + ".findById",
						PedestrianAccessEntity.class);
				query.setParameter("ID", id);
				List<?> resultList = (List<?>) query.getResultList();
				if (resultList.isEmpty())
					result = null;
				else
					result = resultList.get(0);
				session.getTransaction().commit();

			} catch (Exception e) {
				result = null;
				session.getTransaction().rollback();
				e.printStackTrace();

			} finally {
				session.close();
			}
		}

		return result;
	}

	public static synchronized Integer getResultListCount(Class entityClass, String namedQuery) {
		return getResultListCount(entityClass, namedQuery, null);
	}

	public static synchronized List<LogPedestrianAccessEntity> buscaLogsDeAcessoPaginados(String namedQuery,
			HashMap<String, Object> args, Integer inicio, Integer quantidade) {
		List<LogPedestrianAccessEntity> resultList = null;

		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return null;

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

				resultList = (List<LogPedestrianAccessEntity>) resp.getParans().get("list");

			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				executando = false;
			}

		} else {
			Session session = getSessionFactory().getCurrentSession();
			if (session.getTransaction() == null || !session.getTransaction().isActive())
				session.beginTransaction();

			try {
				Query<LogPedestrianAccessEntity> query = session.createNamedQuery(namedQuery,
						LogPedestrianAccessEntity.class);

				if (args != null) {
					for (Map.Entry<String, Object> entry : args.entrySet()) {
						query.setParameter(entry.getKey(), entry.getValue());
					}
				}

				if (inicio != null) {
					query.setFirstResult(inicio);
				}

				if (quantidade != null) {
					query.setMaxResults(quantidade);
				}

				resultList = query.getResultList();
				session.getTransaction().commit();

			} catch (Exception e) {
				resultList = null;
				session.getTransaction().rollback();
				e.printStackTrace();

			} finally {
				session.close();
			}
		}
		return resultList;
	
	}

	@SuppressWarnings("unchecked")
	public static synchronized Integer getResultListCount(Class entityClass, String namedQuery,
			HashMap<String, Object> args) {
		Integer count = 0;

		if (Main.servidor != null && !DeviceEntity.class.equals(entityClass)) {
			if (!Main.servidor.isConnected())
				return 0;

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

				count = (Integer) resp.getParans().get("count");

			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				executando = false;
			}

		} else {
			Session session = getSessionFactory().getCurrentSession();
			if (session.getTransaction() == null || !session.getTransaction().isActive())
				session.beginTransaction();

			try {
				Query<?> query = session.createNamedQuery(namedQuery);

				if (args != null && !args.isEmpty()) {
					args.forEach(query::setParameter);
				}

				Long qtd = (Long) query.getSingleResult();
				if (qtd != null) {
					count = qtd.intValue();
				}

				session.getTransaction().commit();

			} catch (Exception e) {
				count = 0;
				session.getTransaction().rollback();
				e.printStackTrace();

			} finally {
				session.close();
			}
		}
		return count;
	}

	public static synchronized <T> List<?> getResultList(Class<T> entityClass, String namedQuery) {
		List<?> resultList = null;

		if (Main.servidor != null && !DeviceEntity.class.equals(entityClass)) {
			if (!Main.servidor.isConnected()) {
				return null;
			}

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

				resultList = (List<?>) resp.getParans().get("list");

			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				executando = false;
			}

		} else {
			Session session = getSessionFactory().getCurrentSession();
			if (session.getTransaction() == null || !session.getTransaction().isActive())
				session.beginTransaction();

			try {
				Query<?> query = session.createNamedQuery(namedQuery, entityClass);
				resultList = (List<?>) query.getResultList();
				session.getTransaction().commit();

			} catch (Exception e) {
				resultList = null;
				session.getTransaction().rollback();
				e.printStackTrace();

			} finally {
				session.close();
			}
		}
		return resultList;
	}

	public static synchronized <T> List<?> buscaListaDevicesDoServidor(Class<T> entityClass, String namedQuery) {
		List<?> resultList = null;

		if (Main.servidor != null) {
			if (!Main.servidor.isConnected()) {
				return null;
			}

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

				resultList = (List<?>) resp.getParans().get("list");

			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				executando = false;
			}

		} else {
			Session session = getSessionFactory().getCurrentSession();
			if (session.getTransaction() == null || !session.getTransaction().isActive())
				session.beginTransaction();

			try {
				Query<?> query = session.createNamedQuery(namedQuery, entityClass);
				resultList = (List<?>) query.getResultList();
				session.getTransaction().commit();

			} catch (Exception e) {
				resultList = null;
				session.getTransaction().rollback();
				e.printStackTrace();

			} finally {
				session.close();
			}
		}
		return resultList;
	}

	public static synchronized <T> List<?> getResultListLimited(Class<T> entityClass, String namedQuery,
			Long quantidade) {
		List<?> resultList = null;

		if (Main.servidor != null && !DeviceEntity.class.equals(entityClass)) {
			if (!Main.servidor.isConnected())
				return null;

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

				resultList = (List<?>) resp.getParans().get("list");

			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				executando = false;
			}

		} else {
			Session session = getSessionFactory().getCurrentSession();
			if (session.getTransaction() == null || !session.getTransaction().isActive())
				session.beginTransaction();

			try {
				Query<?> query = session.createNamedQuery(namedQuery, entityClass);
				resultList = (List<?>) query.setMaxResults(quantidade.intValue()).getResultList();
				session.getTransaction().commit();

			} catch (Exception e) {
				resultList = null;
				session.getTransaction().rollback();
				e.printStackTrace();

			} finally {
				session.close();
			}
		}
		return resultList;
	}

	public static synchronized <T> List<?> getResultListWithParams(Class<T> entityClass, String namedQuery,
			HashMap<String, Object> args) {
		return getResultListWithParams(entityClass, namedQuery, args, null, null);
	}

	public static synchronized <T> List<?> getResultListWithParams(Class<T> entityClass, String namedQuery,
			HashMap<String, Object> args, Integer inicio, Integer quantidade) {
		List<?> resultList = null;

		if (Main.servidor != null && !entityClass.equals(PreferenceEntity.class)) {
			if (!Main.servidor.isConnected())
				return null;

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

				resultList = (List<?>) resp.getParans().get("list");

			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				executando = false;
			}

		} else {
			Session session = getSessionFactory().getCurrentSession();
			if (session.getTransaction() == null || !session.getTransaction().isActive())
				session.beginTransaction();

			try {
				Query<?> query = session.createNamedQuery(namedQuery, entityClass);

				if (args != null) {
					for (Map.Entry<String, Object> entry : args.entrySet()) {
						query.setParameter(entry.getKey(), entry.getValue());
					}
				}

				if (inicio != null) {
					query.setFirstResult(inicio);
				}

				if (quantidade != null) {
					query.setMaxResults(quantidade);
				}

				resultList = (List<?>) query.getResultList();
				session.getTransaction().commit();

			} catch (Exception e) {
				resultList = null;
				session.getTransaction().rollback();
				e.printStackTrace();

			} finally {
				session.close();
			}
		}
		return resultList;
	}

	public static synchronized Integer getResultListWithParamsCount(Class entityClass, String namedQuery,
			HashMap<String, Object> args) {
		Integer count = 0;

		if (Main.servidor != null && !entityClass.equals(PreferenceEntity.class)) {
			if (!Main.servidor.isConnected())
				return count;

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

				count = (Integer) resp.getParans().get("count");

			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				executando = false;
			}

		} else {
			Session session = getSessionFactory().getCurrentSession();
			if (session.getTransaction() == null || !session.getTransaction().isActive())
				session.beginTransaction();

			try {
				Query<?> query = session.createNamedQuery(namedQuery);
				if(Objects.nonNull(args)) {
					for (Map.Entry<String, Object> entry : args.entrySet()) {
						query.setParameter(entry.getKey(), entry.getValue());
					}
				}

				Long qtd = (Long) query.getSingleResult();
				if (qtd != null) {
					count = qtd.intValue();
				}

				session.getTransaction().commit();

			} catch (Exception e) {
				session.getTransaction().rollback();
				e.printStackTrace();

			} finally {
				session.close();
			}
		}
		return count;
	}

	public static synchronized <T> Object getUniqueResultWithParams(Class<T> entityClass, String namedQuery,
			HashMap<String, Object> args) {
		Object obj = null;

		if (Main.servidor != null && !entityClass.equals(PreferenceEntity.class)) {
			if (!Main.servidor.isConnected())
				return null;

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

				obj = resp.getParans().get("object");

			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				executando = false;
			}

		} else {
			try {
				List<?> resultList = getResultListWithParams(entityClass, namedQuery, args);
				if (resultList != null && !resultList.isEmpty()) {
					obj = resultList.get(0);					
				}

			} catch (Exception e) {
				obj = null;
				e.printStackTrace();
			}
		}

		return obj;
	}

	public static synchronized <T> List<?> getResultListWithDynamicParams(Class<T> entityClass, String orderColumn,
			HashMap<String, Object> args) {
		return getResultListWithDynamicParams(entityClass, null, null, null, orderColumn, args);
	}

	public static synchronized <T> List<?> getResultListWithDynamicParams(Class<T> entityClass, String orderColumn,
			HashMap<String, Object> args, Integer inicio, Integer quantidade) {
		return getResultListWithDynamicParams(entityClass, null, null, null, orderColumn, args, inicio, quantidade);
	}

	public static synchronized <T> List<?> getResultListWithDynamicParams(Class<T> entityClass, String construtor,
			String join, String groupBy, String orderColumn, HashMap<String, Object> args) {
		return getResultListWithDynamicParams(entityClass, null, null, null, orderColumn, args, null, null);
	}

	public static synchronized <T> List<?> getResultListWithDynamicParams(Class<T> entityClass, String construtor,
			String join, String groupBy, String orderColumn, HashMap<String, Object> args, Integer inicio,
			Integer quantidade) {
		List<?> resultList = null;

		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return null;
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

				resultList = (List<?>) resp.getParans().get("list");

			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				executando = false;
			}

		} else {
			Session session = getSessionFactory().getCurrentSession();
			if (session.getTransaction() == null || !session.getTransaction().isActive())
				session.beginTransaction();
			try {
				String sql = "";
				if (construtor != null) {
					sql = "select new " + construtor + " from " + entityClass.getSimpleName() + " obj ";

				} else {
					sql = "select obj from " + entityClass.getSimpleName() + " obj ";
				}

				if (join != null)
					sql += " " + join + " ";

				if (args.entrySet() != null && !args.entrySet().isEmpty()) {
					sql += "where";
				}

				String clause = "";
				for (Map.Entry<String, Object> entry : args.entrySet()) {
					String paramName = entry.getKey();
					Object paramValue = entry.getValue();
					if (paramValue != null) {
						if (paramValue instanceof Boolean || paramValue instanceof Long) {
							if ("removido".equals(paramName))
								sql += clause + " (obj." + paramName + " = :" + paramName + " or obj." + paramName
										+ " is null)";
							else
								sql += clause + " obj." + paramName + " = :" + paramName;

						} else {
							sql += clause + " obj." + paramName + " like :" + paramName;
						}
						clause = " and";
					}
				}

				if (groupBy != null)
					sql += " " + groupBy + " ";

				if (orderColumn != null)
					sql += " order by obj." + orderColumn + " asc";

				Query<?> query = session.createQuery(sql);

				for (Map.Entry<String, Object> entry : args.entrySet()) {
					if (entry.getValue() instanceof Boolean || entry.getValue() instanceof Long) {
						query.setParameter(entry.getKey(), entry.getValue());
					} else {
						query.setParameter(entry.getKey(), "%" + String.valueOf(entry.getValue()).toUpperCase() + "%");
					}
				}

				if (inicio != null)
					query.setFirstResult(inicio);
				if (quantidade != null)
					query.setMaxResults(quantidade);

				resultList = (List<?>) query.getResultList();
				session.getTransaction().commit();
			} catch (Exception e) {
				resultList = null;
				session.getTransaction().rollback();
				e.printStackTrace();

			} finally {
				session.close();
			}
		}

		return resultList;
	}

	public static synchronized Integer getResultListWithDynamicParamsCount(Class entityClass, String construtor,
			String join, String groupBy, HashMap<String, Object> args) {
		Integer count = 0;

		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return null;
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

				count = (Integer) resp.getParans().get("count");

			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				executando = false;
			}

		} else {
			Session session = getSessionFactory().getCurrentSession();
			if (session.getTransaction() == null || !session.getTransaction().isActive())
				session.beginTransaction();
			try {
				String sql = "";
				if (construtor != null) {
					sql = "select new " + construtor + " from " + entityClass.getSimpleName() + " obj ";

				} else {
					sql = "select count(obj) from " + entityClass.getSimpleName() + " obj ";
				}

				if (join != null)
					sql += " " + join + " ";

				if (args.entrySet() != null && !args.entrySet().isEmpty()) {
					sql += "where";
				}

				String clause = "";
				for (Map.Entry<String, Object> entry : args.entrySet()) {
					String paramName = entry.getKey();
					Object paramValue = entry.getValue();
					if (paramValue != null) {
						if (paramValue instanceof Boolean || paramValue instanceof Long) {
							if ("removido".equals(paramName))
								sql += clause + " (obj." + paramName + " = :" + paramName + " or obj." + paramName
										+ " is null)";
							else
								sql += clause + " obj." + paramName + " = :" + paramName;

						} else {
							sql += clause + " obj." + paramName + " like :" + paramName;
						}
						clause = " and";
					}
				}

				if (groupBy != null)
					sql += " " + groupBy + " ";

				Query<?> query = session.createQuery(sql);

				for (Map.Entry<String, Object> entry : args.entrySet()) {
					if (entry.getValue() instanceof Boolean || entry.getValue() instanceof Long) {
						query.setParameter(entry.getKey(), entry.getValue());
					} else {
						query.setParameter(entry.getKey(), "%" + String.valueOf(entry.getValue()).toUpperCase() + "%");
					}
				}

				List<?> result = query.getResultList();

				if (result != null) {
					count = result.size();
				}

				session.getTransaction().commit();
			} catch (Exception e) {
				session.getTransaction().rollback();
				e.printStackTrace();

			} finally {
				session.close();
			}
		}

		return count;
	}

	// Esse metodo deve sempre buscar na maquina.
	public static UserEntity getLoggedUser(String namedQuery) {
		UserEntity user = null;

		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive())
			session.beginTransaction();
		try {
			Query<UserEntity> query = session.createNamedQuery(namedQuery, UserEntity.class);
			List<UserEntity> resultList = query.getResultList();
			if (resultList.isEmpty())
				user = null;
			else
				user = resultList.get(0);
			session.getTransaction().commit();

		} catch (Exception e) {
			user = null;
			session.getTransaction().rollback();
			e.printStackTrace();
		}

		return user;
	}

	public static synchronized <T> Object getSingleResult(Class<T> entityClass, String namedQuery) {
		Object result = null;

		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return null;

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

				result = resp.getParans().get("object");

			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				executando = false;
			}

		} else {
			Session session = getSessionFactory().getCurrentSession();
			if (session.getTransaction() == null || !session.getTransaction().isActive())
				session.beginTransaction();
			try {
				Query<?> query = session.createNamedQuery(namedQuery, entityClass);
				List<?> resultList = (List<?>) query.getResultList();
				if (resultList.isEmpty())
					result = null;
				else
					result = resultList.get(0);
				session.getTransaction().commit();

			} catch (Exception e) {
				result = null;
				session.getTransaction().rollback();
				e.printStackTrace();

			} finally {
				session.close();
			}
		}

		return result;
	}

	/**
	 * @param codigo
	 * @return Object[] { resultadoVerificacao, userName, matchedAthleteAccess }
	 */
	public static synchronized Object[] processAccessRequest(String codigo, String location, boolean createNotification,
			boolean ignoraRegras) {
		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return null;

			Object[] object = null;

			try {
				TcpMessageTO req = new TcpMessageTO(TcpMessageType.PROCESS_ACCESS_REQUEST);
				req.getParans().put("codigo", codigo);
				req.getParans().put("location", location);
				req.getParans().put("ignoraRegras", ignoraRegras);

				req.getParans().put("createNotification", createNotification);

				outToServerFaceRecogizer.writeObject(req);
				outToServerFaceRecogizer.flush();

				ObjectInputStream reader = new ObjectInputStream(clientSocketFaceRecognizer.getInputStream());
				TcpMessageTO resp = (TcpMessageTO) reader.readObject();

				object = (Object[]) resp.getParans().get("object");

			} catch (Exception e) {
				e.printStackTrace();
			}

			return object;

		} else {
			return processAccessRequest(codigo, location, null, null, false, createNotification, null, false,
					ignoraRegras);
		}
	}

	/**
	 * @param codigo
	 * @return Object[] { resultadoVerificacao, userName, matchedAthleteAccess }
	 */
	public static synchronized Object[] processAccessRequest(String codigo, String equipamento, Integer origem,
			String location, boolean usaUrna, boolean createNotification, Date data, Boolean digitaisCatraca,
			Boolean ignoraRegras) {

		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return null;

			Object[] object = null;

			try {
				TcpMessageTO req = new TcpMessageTO(TcpMessageType.PROCESS_ACCESS_REQUEST_2);
				req.getParans().put("codigo", codigo);
				req.getParans().put("equipamento", equipamento);
				req.getParans().put("origem", origem);
				req.getParans().put("location", location);
				req.getParans().put("usaUrna", usaUrna);
				req.getParans().put("createNotification", createNotification);
				req.getParans().put("data", data);
				req.getParans().put("digitaisCatraca", digitaisCatraca);
				req.getParans().put("ignoraRegras", ignoraRegras);

				outToServerFaceRecogizer.writeObject(req);
				outToServerFaceRecogizer.flush();

				ObjectInputStream reader = new ObjectInputStream(clientSocketFaceRecognizer.getInputStream());
				TcpMessageTO resp = (TcpMessageTO) reader.readObject();

				object = (Object[]) resp.getParans().get("object");

			} catch (Exception e) {
				e.printStackTrace();
			}

			return object;

		} else {
			return processAccessRequest(codigo, location, null, equipamento, origem, usaUrna, createNotification, data,
					digitaisCatraca, ignoraRegras);
		}
	}

	/**
	 * @param codigo
	 * @return Object[] { resultadoVerificacao, userName, matchedAthleteAccess }
	 */
	public static synchronized Object[] processAccessRequest(String codigo, String equipamento, Integer origem,
			String location, boolean usaUrna, boolean createNotification, boolean ignoraRegras) {

		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return null;

			Object[] object = null;

			try {
				TcpMessageTO req = new TcpMessageTO(TcpMessageType.PROCESS_ACCESS_REQUEST_2);
				req.getParans().put("codigo", codigo);
				req.getParans().put("equipamento", equipamento);
				req.getParans().put("origem", origem);
				req.getParans().put("location", location);
				req.getParans().put("usaUrna", usaUrna);
				req.getParans().put("createNotification", createNotification);
				req.getParans().put("ignoraRegras", ignoraRegras);

				outToServerFaceRecogizer.writeObject(req);
				outToServerFaceRecogizer.flush();

				ObjectInputStream reader = new ObjectInputStream(clientSocketFaceRecognizer.getInputStream());
				TcpMessageTO resp = (TcpMessageTO) reader.readObject();

				object = (Object[]) resp.getParans().get("object");

			} catch (Exception e) {
				e.printStackTrace();
			}

			return object;

		} else {
			return processAccessRequest(codigo, location, null, equipamento, origem, usaUrna, createNotification, null,
					false, ignoraRegras);
		}
	}

	/**
	 * @param codigo
	 * @return Object[] { resultadoVerificacao, userName, matchedAthleteAccess }
	 */
	@SuppressWarnings("unchecked")
	private static Object[] processAccessRequest(String codigo, String location, String direction, String equipament,
			Integer origem, Boolean usaUrna, boolean createNotification, Date data, Boolean digitaisCatraca,
			Boolean ignoraRegras) {

		aniversariante = false;
		VerificationResult resultadoVerificacao = VerificationResult.NOT_FOUND;
		String userName = "";
		String motivo = "";
		byte[] foto = null;
		matchedAthleteAccess = null;

		try {

			/*
			 * Processamento de dados que vieram da catraca para encontrar pedestre
			 */
			if (Utils.isNullOrEmpty(codigo)) {
				return new Object[] { resultadoVerificacao, userName, matchedAthleteAccess };
			}

			// processa dados do qrcode
			if (codigo.contains("_")) {
				try {
					matchedAthleteAccess = trataPedestreQRCode(codigo);
				} catch (QrcodeVencidoException e) {
					if (createNotification)
						Utils.createNotification("QRCode do usuário expirado.", NotificationType.BAD, foto);
					return new Object[] { VerificationResult.NOT_FOUND, userName, matchedAthleteAccess };
				}

			} else {
				Long codigoUsuario = Long.valueOf(codigo.trim());

				if (Utils.isNullOrZero(codigoUsuario)) {
					return new Object[] { resultadoVerificacao, userName, matchedAthleteAccess };					
				}

				if (origem != null && origem.equals(Enumeradores.VIA_TECLADO)) {
					// tenta encontrar pela matricula
					matchedAthleteAccess = (PedestrianAccessEntity) HibernateUtil
							.getSingleResultByRegistration(PedestrianAccessEntity.class, codigoUsuario);

				} else if (origem != null && (origem == Origens.ORIGEM_LEITOR_1 || origem == Origens.ORIGEM_LEITOR_2)) {
					matchedAthleteAccess = (PedestrianAccessEntity) HibernateUtil
							.getSingleResultByCardNumber(PedestrianAccessEntity.class, codigoUsuario);
				} else {
					// tenta procurar pelo ID
					matchedAthleteAccess = (PedestrianAccessEntity) HibernateUtil
							.getSingleResultById(PedestrianAccessEntity.class, codigoUsuario);
				}

				if (matchedAthleteAccess == null) {
					// Se nao encontrou por ID, entao procura pelo numero do cartao
					matchedAthleteAccess = (PedestrianAccessEntity) HibernateUtil
							.getSingleResultByCardNumber(PedestrianAccessEntity.class, codigoUsuario);
				}

				if (matchedAthleteAccess == null && Boolean.TRUE.equals(digitaisCatraca)
						&& origem == Origens.ORIGEM_LEITOR_1) {
					// para digitais na catraca cadastradas com ID
					matchedAthleteAccess = (PedestrianAccessEntity) HibernateUtil
							.getSingleResultById(PedestrianAccessEntity.class, codigoUsuario);
				}

			}

			if (matchedAthleteAccess == null) {
				resultadoVerificacao = VerificationResult.NOT_FOUND;
				if (createNotification) {
					Utils.createNotification("Usuário de Código " + codigo + " não encontrado.", NotificationType.BAD, foto);					
				}

				return new Object[] { resultadoVerificacao, userName, matchedAthleteAccess };
			}

			userName = matchedAthleteAccess.getFirstName().toUpperCase();
			foto = matchedAthleteAccess.getFoto();
			Boolean acessoRestrito = Boolean.valueOf(Utils.getPreference("restrictAccess"));
			Boolean permitidoHoje = true;
			Boolean permitidoSensor = true;
			Boolean permitido = true;
			Boolean permitidoRetornar = true;

			// achou o pedestre, pula regras e envia acesso liberado
			// verifica somente se no for pra pular
			if (!Boolean.TRUE.equals(ignoraRegras)) {
				if (isObrigatorioPassagemViaLeitorFacial(matchedAthleteAccess, origem)) {
					if (createNotification) {
						Utils.createNotification(userName + " deve cadastrar sua face.", NotificationType.BAD, foto);
					}
					return new Object[] { VerificationResult.NOT_ALLOWED_FACE_REQUIRED, userName,
							matchedAthleteAccess };
				}

				if (isPedestreNaoPossuiRegras(matchedAthleteAccess)) {
					if (createNotification) {
						Utils.createNotification(userName + " não possui regras.", NotificationType.BAD, foto);
					}
					return new Object[] { VerificationResult.NOT_ALLOWED, userName, matchedAthleteAccess };
				}
			}

			matchedAthleteAccess.setOrigemCatraca(origem);

			if (matchedAthleteAccess.getStatus().equals("INATIVO")) {
				System.out.println("chegou no pedestre ");

				Utils.createNotification(" Acesso Negado, usuário: " + userName + "Inativo", NotificationType.BAD,
						foto);
				motivo = "Usuário inativo.";
				return new Object[] { VerificationResult.NOT_ALLOWED, userName, matchedAthleteAccess };
			}
			

			if (origem == null || (Boolean.TRUE.equals(matchedAthleteAccess.getSempreLiberado())
					|| Boolean.TRUE.equals(ignoraRegras)) && origem != Origens.ORIGEM_LEITOR_2) {

				criaLogDeAcessoSempreLiberado(ignoraRegras, origem, matchedAthleteAccess, location, direction, data,
						codigo, createNotification, equipament, foto, userName);

				return new Object[] { VerificationResult.ALLOWED, userName, matchedAthleteAccess };
			}

			if (matchedAthleteAccess.getTipo().equals("PEDESTRE") && origem == Origens.ORIGEM_LEITOR_2) {
				permitidoSensor = false;
			}
			
			if (!validaAcessoEquipamento(equipament, matchedAthleteAccess.getEquipamentos())) {
				System.out.println("o que Ã© equipament" + equipament);
				System.out.println("o que Ã© pessoa equipamento" + matchedAthleteAccess.getEquipamentos());
				if (createNotification)
					Utils.createNotification(userName + " não permitido nesse equipamento.", NotificationType.BAD,
							foto);
				return new Object[] { VerificationResult.NOT_ALLOWED_ORIGEM, userName, matchedAthleteAccess };
			}

			LogPedestrianAccessEntity ultimoAcesso = buscaUltimoAcesso(matchedAthleteAccess.getId(),
					matchedAthleteAccess.getQtdAcessoAntesSinc());

			if (Integer.valueOf(Enumeradores.VIA_TECLADO).equals(origem)
					&& Boolean.FALSE.equals(matchedAthleteAccess.getHabilitarTeclado())) {
				permitido = false;

			} else if (origem != null && equipament != null
					&& !verificaUltimaPassagemEmCatracaVinculada(equipament, matchedAthleteAccess.getId())) {
				permitido = false;

			} else if (ultimoAcesso != null && Tipo.SAIDA.equals(ultimoAcesso.getDirection())
					&& !isPedestrePermitidoRetornar(matchedAthleteAccess)) {
				permitidoRetornar = true;

			} else if ("VISITANTE".equals(matchedAthleteAccess.getTipo())) {
				if (!Integer.valueOf(Origens.ORIGEM_LEITOR_2).equals(origem)) {
					if (matchedAthleteAccess.getQuantidadeCreditos() != null
							|| isPermitidoPedestreRegra(matchedAthleteAccess)) {
						permitido = matchedAthleteAccess.getQuantidadeCreditos() > 0
								&& (matchedAthleteAccess.getValidadeCreditos() == null || matchedAthleteAccess
										.getValidadeCreditos().getTime() >= new Date().getTime());

						// fazer um for validando se existe regra livre ou com quantidade veazia, sÃ³
						// assim libero

						if ((matchedAthleteAccess.getQuantidadeCreditos().equals(1l)
								|| (matchedAthleteAccess.getPedestreRegra().get(0).getQtdeTotalDeCreditos() != null
										&& matchedAthleteAccess.getPedestreRegra().get(0).getQtdeTotalDeCreditos().equals(1L)))
								&& !Integer.valueOf(Origens.ORIGEM_BIOMETRIA).equals(origem) && usaUrna) {
							permitidoSensor = isPermitidoNoSensor(ultimoAcesso, origem, matchedAthleteAccess);
						}

						if (isPermitidoPedestreRegra(matchedAthleteAccess)) {
							permitido = true;
						}

					} else if (Utils.pedestreTemRegraDeAcessoPorPeriodoValido(matchedAthleteAccess)) {
						permitido = true;

					} else {
						if (Integer.valueOf(FacialDevice.ORIGEM_FACIAL).equals(origem)
								&& matchedAthleteAccess.getCardNumber() == null) {
							permitido = false;
						
						} else if (usaUrna) {
							permitidoSensor = isPermitidoNoSensor(ultimoAcesso, origem, matchedAthleteAccess);
						}
					}

				} else {
					if (usaUrna) {
						permitidoSensor = isPermitidoNoSensor(ultimoAcesso, origem, matchedAthleteAccess);
					}
				}

			} else {

				/*
				 * regras de pedestres
				 */

				if (matchedAthleteAccess.getQuantidadeCreditos() != null) {

					/*
					 * verificar creditos
					 */

					// verifica se tem crÃ©ditos para passar

					permitido = matchedAthleteAccess.getQuantidadeCreditos() > 0
							&& !isPermitidoPedestreRegra(matchedAthleteAccess)
							&& matchedAthleteAccess.getCardNumber() != null
							&& (matchedAthleteAccess.getValidadeCreditos() == null || matchedAthleteAccess
									.getValidadeCreditos().getTime() >= (data != null ? data : new Date()).getTime());

				} else if (matchedAthleteAccess.getTipoTurno() != null
						&& !"".equals(matchedAthleteAccess.getTipoTurno())) {

					/*
					 * verificar turno/escala
					 */

					TipoEscala tipo = TipoEscala.valueOf(matchedAthleteAccess.getTipoTurno());
					int tipoAdicao = TipoEscala.ESCALA_12_36.equals(tipo) || TipoEscala.ESCALA_24_04.equals(tipo)
							? Calendar.HOUR
							: Calendar.DATE;
					Calendar dataAcesso = Calendar.getInstance();
					String[] escala = tipo.name().replace("ESCALA_", "").split("_");

					// data de partida
					Date dataInicial = calculaDataInicialEscala(matchedAthleteAccess, escala, tipoAdicao);
					System.out
							.println("Data calculada: " + new SimpleDateFormat("dd/MM/yyyy HH:mm").format(dataInicial));

					Calendar periodoPermitidoIni = Calendar.getInstance();
					periodoPermitidoIni.setTime(dataInicial);

					Calendar periodoPermitidoFim = Calendar.getInstance();
					periodoPermitidoFim.setTime(dataInicial);
					periodoPermitidoFim.add(tipoAdicao, Integer.parseInt(escala[0]));

					if (dataAcesso.after(periodoPermitidoIni) && dataAcesso.before(periodoPermitidoFim)) {
						// pode acessar
						permitido = true;
					} else {
						// não pode acessar
						permitido = false;
					}

				} else if (acessoRestrito) {
					// verifica se hÃ¡ algum log de acesso para este aluno hoje
					HashMap<String, Object> args = new HashMap<String, Object>();
					args.put("ID_ATLETA", matchedAthleteAccess.getId());
					List<LogPedestrianAccessEntity> list = (List<LogPedestrianAccessEntity>) HibernateUtil
							.getResultListWithParams(LogPedestrianAccessEntity.class,
									"LogPedestrianAccessEntity.findTodayByAthlete", args);

					Integer limiteAcessos = Integer.valueOf(Utils.getPreference("restrictAccessDays"));

					if (list != null && list.size() >= limiteAcessos) {
						permitidoHoje = false;
					}
				}

			}

			if (!permitidoSensor) {
				// para lÃ³gica de urna
				if (origem != Origens.ORIGEM_LEITOR_2)
					resultadoVerificacao = VerificationResult.NOT_ALLOWED_SENSOR;
				else
					resultadoVerificacao = VerificationResult.NOT_ALLOWED_BOX;

				if (createNotification) {
					if (origem != Origens.ORIGEM_LEITOR_2) {
						Utils.createNotification(userName + " deve depositar cartão na urna.", NotificationType.BAD,
								foto);
						motivo = "Deve depositar cartão na urna.";

					} else {
						Utils.createNotification(userName + " não deve depositar na urna", NotificationType.BAD, foto);
						motivo = "Não deve depositar cartão na urna.";
					}
				}

			} else if (!permitido) {
				// para lÃ³gica de crÃ©ditos finalizados
				resultadoVerificacao = VerificationResult.NOT_ALLOWED;
				if (createNotification) {
					Utils.createNotification(userName + " não permitido.", NotificationType.BAD, foto);
					motivo = "Não permitido.";
				}

			} else if (!permitidoRetornar) {
				resultadoVerificacao = VerificationResult.NOT_ALLOWED_NOW;
				if (createNotification) {
					Utils.createNotification(userName + " não pode retornar agora.", NotificationType.BAD, foto);
					motivo = "Não pode retornar agora.";
				}

			} else if (permitidoHoje) {

				// para lÃ³gicas do pedestre:
				// - horÃ¡rio
				// - periodo
				// - escala
				LogPedestrianAccessEntity logAccess = new LogPedestrianAccessEntity(Main.loggedUser.getId(),
						matchedAthleteAccess.getId(), matchedAthleteAccess.getStatus(), location, motivo, direction,
						equipament);
				if (data != null) {
					logAccess.setAccessDate(data);
				}

				if ("ATIVO".equals(matchedAthleteAccess.getStatus())) {
					resultadoVerificacao = validaDiasHorarios(createNotification, userName, matchedAthleteAccess,
							logAccess, VerificationResult.ALLOWED, foto, origem, data);

					// indefine acesso
//					if ("ATIVO".equals(logAccess.getStatus()))
//						logAccess.setStatus("INDEFINIDO");

				} else {
					resultadoVerificacao = VerificationResult.NOT_ALLOWED;
					logAccess.setStatus("INATIVO");
					if (createNotification) {
						Utils.createNotification(userName + " não permitido.", NotificationType.BAD, foto);
						motivo = "Não permitido.";
					}
				}

				logAccess.setReason(motivo);

				if (deveGravarCartaoRecebidoNoLog(origem))
					logAccess.setCartaoAcessoRecebido(codigo);

				if (Main.broadcastServer != null)
					Main.broadcastServer
							.sendMessage(new BroadcastMessageTO(BroadcastMessageType.LOG_ACCESS, logAccess));

				if ((origem != null && !origem.equals(Origens.ORIGEM_LIBERADO_SISTEMA))
						|| !VerificationResult.ALLOWED.equals(resultadoVerificacao))
					HibernateUtil.save(LogPedestrianAccessEntity.class, logAccess);

			} else {
				resultadoVerificacao = VerificationResult.ALLOWED_ONLY_ONCE;
				if (createNotification)
					Utils.createNotification(userName + " já registrado hoje.", NotificationType.BAD, foto);
			}

		} catch (Exception e) {
			e.printStackTrace();
			resultadoVerificacao = VerificationResult.ERROR;
			if (createNotification)
				Utils.createNotification("Falha ao processar requisiÃ§Ã£o de acesso. " + e.getMessage(),
						NotificationType.BAD, foto);
		}

		return new Object[] { resultadoVerificacao, userName, matchedAthleteAccess };
	}

	private static void criaLogDeAcessoSempreLiberado(Boolean ignoraRegras, Integer origem,
			PedestrianAccessEntity matchedAthleteAccess2, String location, String direction, Date data, String codigo,
			boolean createNotification, String equipament, byte[] foto, String userName) {

		String motivo = ignoraRegras ? "Regras ignoradas" : "Sempre liberado";

		if (Origens.ORIGEM_LIBERADO_SISTEMA.equals(origem)) {
			return;
		}

		LogPedestrianAccessEntity logAccess = new LogPedestrianAccessEntity(Main.loggedUser.getId(),
				matchedAthleteAccess.getId(), matchedAthleteAccess.getStatus(), location, motivo, direction,
				equipament);
		logAccess.setStatus("INDEFINIDO");
		if (data != null) {
			logAccess.setAccessDate(data);
		}

		if (deveGravarCartaoRecebidoNoLog(origem)) {
			logAccess.setCartaoAcessoRecebido(codigo);
		}

		if (createNotification) {
			Utils.createNotification(userName + " liberado.", NotificationType.GOOD, foto);
		}

		if (Main.broadcastServer != null) {
			Main.broadcastServer.sendMessage(new BroadcastMessageTO(BroadcastMessageType.LOG_ACCESS, logAccess));
		}

		HibernateUtil.save(LogPedestrianAccessEntity.class, logAccess);
	}

	private static boolean isPermitidoPedestreRegra(PedestrianAccessEntity pedestre) {
		if (pedestre.getPedestreRegra() == null) {
			return false;
		}

		for (PedestreRegraEntity pedestreRegra : pedestre.getPedestreRegra()) {
			if (pedestreRegra.getQtdeDeCreditos() != null && pedestreRegra.getQtdeDeCreditos() > 0
					&& (pedestreRegra.getRemovidoNoDesktop() == null
							|| Boolean.FALSE.equals(pedestreRegra.getRemovidoNoDesktop()))) {
				return true;
			}

		}

		return false;
	}

	private static boolean isPedestreNaoPossuiRegras(PedestrianAccessEntity pedestre) {
		return Boolean.FALSE.equals(pedestre.getSempreLiberado())
				&& (pedestre.getPedestreRegra() == null || pedestre.getPedestreRegra().isEmpty());
	}

	private static boolean isObrigatorioPassagemViaLeitorFacial(PedestrianAccessEntity pedestre, Integer origem) {
		return Boolean.TRUE.equals(pedestre.getCadastroFacialObrigatorio())
				&& !Integer.valueOf(FacialDevice.ORIGEM_FACIAL).equals(origem);
	}

	private static PedestrianAccessEntity trataPedestreQRCode(String codigo) throws ParseException {

		PedestrianAccessEntity pedestre = buscaPedestrePeloQrCode(codigo);
		// se tiver _ processa como QRCode
		String qrCode = "";
		String tempo = null;
		if (codigo.startsWith("T_")) {
			// retira tempo do qrcode gerado
			String[] parts = codigo.split("_");
			tempo = parts[parts.length - 1];
			qrCode = parts[0] + "_" + parts[1];
		} else {
			// valida cdigo inteiro
			qrCode = codigo;
		}

		pedestre = buscaPedestrePeloQrCode(qrCode);

		// verifica se QRCode valido ainda
		if (pedestre != null && tempo != null) {

			SimpleDateFormat sdf = new SimpleDateFormat("ddMMyyyyHHmmss");
			Date tempoQrCode = sdf.parse(new SimpleDateFormat("ddMMyyyy").format(new Date()) + tempo);

			Long agora = new Date().getTime();
			if (tempoQrCode.getTime() <= agora) {
				throw new QrcodeVencidoException();
			}
		}

		return pedestre;
	}

	private static Date calculaDataInicialEscala(PedestrianAccessEntity pedestre, String[] escala, int tipoAdicao) {

		// recupera Ãºltima entrada da pessoa
		HashMap<String, Object> args = new HashMap<String, Object>();
		args.put("ID_PEDESTRE", matchedAthleteAccess.getId());
		LogPedestrianAccessEntity ultimoAcesso = (LogPedestrianAccessEntity) HibernateUtil.getUniqueResultWithParams(
				LogPedestrianAccessEntity.class, "LogPedestrianAccessEntity.findByPedestreEntrada", args);

		Calendar c = Calendar.getInstance();
		c.setTime(ultimoAcesso != null ? ultimoAcesso.getAccessDate() : pedestre.getDataInicioPeriodo());

		Calendar h = Calendar.getInstance();
		h.setTime(pedestre.getInicioTurno());

		c.set(Calendar.HOUR_OF_DAY, h.get(Calendar.HOUR_OF_DAY));
		c.set(Calendar.MINUTE, h.get(Calendar.MINUTE));
		c.set(Calendar.SECOND, 0);

		if (ultimoAcesso != null) {
			Calendar ajuste = Calendar.getInstance();
			ajuste.setTime(c.getTime());
			ajuste.add(tipoAdicao, Integer.parseInt(escala[0]));

			Calendar agora = Calendar.getInstance();
			if (agora.after(ajuste)) {
				// adiciona o tempo de folga
				c.add(tipoAdicao, Integer.parseInt(escala[1]));
			} else {
				c.add(tipoAdicao, Integer.parseInt(escala[0]) * -1);
			}
		}

		return c.getTime();
	}

	@SuppressWarnings("unchecked")
	private static PedestrianAccessEntity buscaPedestrePeloQrCode(String codigoUsuario) {
		HashMap<String, Object> args = new HashMap<>();
		args.put("QR_CODE", codigoUsuario.trim());
		List<PedestrianAccessEntity> pedestres = (List<PedestrianAccessEntity>) HibernateUtil
				.getResultListWithParams(PedestrianAccessEntity.class, "PedestrianAccessEntity.findByQRCode", args);

		if (pedestres != null && !pedestres.isEmpty())
			return pedestres.get(0);

		return null;
	}

	@SuppressWarnings("unchecked")
	public static List<PedestrianAccessEntity> buscaPedestresAtivosComCartao() {

		PedestrianAccessEntity acesso = null;
		List<PedestrianAccessEntity> acessos = null;
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive())
			session.beginTransaction();
		try {
			TypedQuery<PedestrianAccessEntity> query = session
					.createQuery(
							"select obj from PedestrianAccessEntity obj " + "where obj.cardNumber is not null "
									+ "and obj.cardNumber <> '' "
									+ "and (obj.removido is null or obj.removido = false) " + "order by obj.id desc",
							PedestrianAccessEntity.class);

			acessos = query.getResultList();
			session.getTransaction().commit();
			if (acessos != null)
				return acessos;

		} catch (Exception e) {
			session.getTransaction().rollback();
			e.printStackTrace();

		} finally {
			session.close();
		}

		return null;

	}

	@SuppressWarnings("unchecked")
	public static List<PedestrianAccessEntity> buscaPedestresAtivosComBiometria() {

		List<PedestrianAccessEntity> acessos = null;
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive())
			session.beginTransaction();
		try {
			TypedQuery<PedestrianAccessEntity> query = session.createQuery(
					"select obj from PedestrianAccessEntity obj "
							+ "where obj.removido is null or obj.removido = false " + "order by obj.id asc",
					PedestrianAccessEntity.class);

			acessos = query.getResultList();
			session.getTransaction().commit();
			if (acessos != null)
				return acessos;

		} catch (Exception e) {
			session.getTransaction().rollback();
			e.printStackTrace();

		} finally {
			session.close();
		}

		return null;

	}

	private static Boolean isPermitidoNoSensor(LogPedestrianAccessEntity ultimoAcesso, Integer origem,
			PedestrianAccessEntity pedestre) {
		Boolean permitidoSensor = true;
		System.out.println(pedestre.getCardNumber());

		if (pedestre.getQrCodeParaAcesso() != null && pedestre.getQrCodeParaAcesso().contains("_")

				&& Utils.pedestreTemRegraDeAcessoPorPeriodoValido(pedestre)) {
			return true;

		}
//		Se origem diferentes das que não sÃ£o permitidas como exemplo, biometria.

//			verificar o máximo possível para ver o que não pode estar contido na mesma informaÃ§Ã£o
		if (origem == Origens.ORIGEM_LEITOR_1 && ultimoAcesso != null
				&& Tipo.ENTRADA.equals(ultimoAcesso.getDirection())) {
			return false;
		}
		if (origem == Origens.ORIGEM_LEITOR_2
				&& (ultimoAcesso == null || Tipo.SAIDA.equals(ultimoAcesso.getDirection()))) {
			return false;
		}

		return permitidoSensor;
	}

	public static synchronized LogPedestrianAccessEntity buscaUltimoAcesso(Long idPedestre,
			Integer qtdAcessosAntesSinc) {
		HashMap<String, Object> args = new HashMap<String, Object>();
		args.put("ID_PEDESTRE", idPedestre);

		String query = null;
		if (Main.loggedUser != null && Main.loggedUser.getDateNewAccess() != null) {
			query = "LogPedestrianAccessEntity.findByLastAccessbyIdPedestrianAndDate";
			args.put("DATE", Main.loggedUser.getDateNewAccess());
		} else {
			query = "LogPedestrianAccessEntity.findByLastAccessbyIdPedestrian";
		}

		LogPedestrianAccessEntity lastAccess = (LogPedestrianAccessEntity) HibernateUtil
				.getUniqueResultWithParams(LogPedestrianAccessEntity.class, query, args);

//		buscar pelo dia de "hoje" e confefir se tem datas, já dar entrada
		if (qtdAcessosAntesSinc != null && qtdAcessosAntesSinc > 0 && lastAccess == null) {

			lastAccess = new LogPedestrianAccessEntity();
			if (qtdAcessosAntesSinc % 2 == 0) {
				lastAccess.setDirection(Tipo.SAIDA);
			} else {
				lastAccess.setDirection(Tipo.ENTRADA);
			}
		}
		return lastAccess;
	}

	public static synchronized Long countAcessosPedestre(Long idPedestre) {
		Long qtdeAcessos = 0L;

		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return null;

			verificaExecucaoDePing();
			try {
				TcpMessageTO req = new TcpMessageTO(TcpMessageType.COUNT_ACESSOS_PEDESTRE);
				req.getParans().put("idPedestre", idPedestre);

				executando = true;
				outToServer.writeObject(req);
				outToServer.flush();

				ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
				TcpMessageTO resp = (TcpMessageTO) reader.readObject();

				qtdeAcessos = (Long) resp.getParans().get("qtdeAcessos");

			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				executando = false;
			}

		} else {
			UserEntity loggedUser = HibernateUtil.getLoggedUser("UserEntity.findAll");

			Session session = getSessionFactory().getCurrentSession();
			if (session.getTransaction() == null || !session.getTransaction().isActive())
				session.beginTransaction();

			try {
				TypedQuery<Long> query = session.createQuery("select count(distinct obj.id) "
						+ "from LogPedestrianAccessEntity obj " + "where obj.idPedestrian = :ID_PEDESTRE "
						+ "		 and obj.accessDate >= :DATA " + "		 and obj.status = 'ATIVO' ", Long.class);

				query.setParameter("ID_PEDESTRE", idPedestre);
				query.setParameter("DATA", loggedUser.getDateNewAccess());
				qtdeAcessos = query.getSingleResult();
				session.getTransaction().commit();

			} catch (Exception e) {
				session.getTransaction().rollback();
				e.printStackTrace();

			} finally {
				session.close();
			}
		}

		if (qtdeAcessos == null)
			return 0l;

		return qtdeAcessos;
	}

	private static Date buscaUltimoAcessoAtivoPedestre(Long idPedestre) {
		LogPedestrianAccessEntity acesso = null;

		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive())
			session.beginTransaction();
		try {
			TypedQuery<LogPedestrianAccessEntity> query = session.createQuery(
					"select obj from LogPedestrianAccessEntity obj " + "where obj.idPedestrian = :ID_PEDESTRE "
							+ "and obj.status = 'ATIVO' and obj.direction = 'ENTRADA' " + "order by obj.id desc",
					LogPedestrianAccessEntity.class);

			query.setMaxResults(1);
			query.setParameter("ID_PEDESTRE", idPedestre);
			List<LogPedestrianAccessEntity> acessos = query.getResultList();
			if (acessos != null && !acessos.isEmpty())
				acesso = acessos.get(0);

			session.getTransaction().commit();

		} catch (Exception e) {
			session.getTransaction().rollback();
			e.printStackTrace();

		} finally {
			session.close();
		}

		if (acesso != null)
			return acesso.getAccessDate();

		return null;
	}

	private static boolean isPedestrePermitidoRetornar(PedestrianAccessEntity matchedAthleteAccess) {
		boolean permitido;

		Date ultimoAcesso = buscaUltimoAcessoAtivoPedestre(matchedAthleteAccess.getId());

		if (ultimoAcesso == null) {
			permitido = true;

		} else {
			permitido = Utils.isPodeEntrarNovamente(ultimoAcesso);
		}

		return permitido;
	}

	private static boolean validaAcessoEquipamento(String equipament, List<PedestrianEquipamentEntity> equipamentos) {
		if (equipament == null)
			return true;

		// não tem bloqueo por equipamento
		if (equipamentos == null || equipamentos.isEmpty())
			return true;

		boolean pode = false;
		for (PedestrianEquipamentEntity e : equipamentos) {
			String idEquipament = equipament.replace("Inner ", "").replace("Inner Acesso ", "").replace("Control ", "");

			if (e.getIdEquipamento().equals(idEquipament) || idEquipament.contains(e.getIdEquipamento())) {
				pode = true;
				break;
			}
		}
		return pode;
	}

	private static boolean verificaUltimaPassagemEmCatracaVinculada(String equipamento, Long idPedestre) {
		Device deviceAtual = null;
		boolean permitido = true;

		for (Device device : Main.devicesList) {
			String idDevice = device.getIdentifier().split(";")[0];
			String idEquipamento = equipamento.replace("Inner ", "").replace("Inner Acesso ", "").replace("Control ",
					"");

			if (idDevice != null && equipamento != null && idDevice.equals(idEquipamento)) {
				deviceAtual = device;
				break;
			}
		}

		if (deviceAtual != null && deviceAtual.getAttachedDevices() != null
				&& !deviceAtual.getAttachedDevices().isEmpty()) {
			for (AttachedTO attachedTO : deviceAtual.getAttachedDevices()) {
				String idDevice = attachedTO.getIdDevice().split(";")[0];

				if (deviceAtual instanceof TopDataDevice)
					idDevice = "Inner " + idDevice;
				else if (deviceAtual instanceof TopDataAcessoDevice)
					idDevice = "Inner Acesso " + idDevice;
				else if (deviceAtual instanceof ControlIdDevice)
					idDevice = "Control " + idDevice;

				permitido = verificaSePossuiEntradaNoEquipamento(idDevice, idPedestre);

				if (permitido)
					return true;
			}

			permitido = false;
		}

		return permitido;
	}

	private static boolean verificaSePossuiEntradaNoEquipamento(String idDevice, Long idPedestre) {
		List<LogPedestrianAccessEntity> acesso = null;

		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive())
			session.beginTransaction();
		try {
			TypedQuery<LogPedestrianAccessEntity> query = session.createQuery(
					"select obj " + "from LogPedestrianAccessEntity obj " + "where obj.idPedestrian = :ID_PEDESTRE "
							+ "and obj.status = 'ATIVO' " + "and obj.direction = 'ENTRADA' "
							+ "and obj.equipament = :ID_DEVICE " + "and (select count(saida.id) "
							+ "		from LogPedestrianAccessEntity saida "
							+ "		where obj.idPedestrian = saida.idPedestrian "
							+ "			and saida.direction = 'SAIDA' " + "			and saida.equipament = :ID_DEVICE "
							+ "			and saida.status = 'ATIVO' "
							+ "			and saida.accessDate > obj.accessDate) = 0 " + "order by obj.id desc",
					LogPedestrianAccessEntity.class);

			query.setMaxResults(1);
			query.setParameter("ID_PEDESTRE", idPedestre);
			query.setParameter("ID_DEVICE", idDevice);
			acesso = query.getResultList();
			session.getTransaction().commit();

		} catch (Exception e) {
			session.getTransaction().rollback();
			e.printStackTrace();

		} finally {
			session.close();
		}

		return acesso != null && !acesso.isEmpty();
	}

	private static VerificationResult validaDiasHorarios(boolean createNotification, String userName,
			PedestrianAccessEntity matchedAthleteAccess, LogPedestrianAccessEntity logAccess,
			VerificationResult validado, byte[] foto, Integer origem, Date data) {

		VerificationResult resultadoVerificacao;

		// verifica se hÃ¡ um dia liberado para acesso
//		if (Utils.isDiaLivre(matchedAthleteAccess.getDiasLivres())) {
//			resultadoVerificacao = validado;
//			logAccess.setStatus("ATIVO");
//			aniversariante = Utils.isBirthday(matchedAthleteAccess);
//			if (createNotification) {
//				Utils.createNotification(userName + " permitido" 
//						+ (VerificationResult.TOLERANCE_PERIOD.equals(validado) ? " pela tolerÃ¢ncia." : "." ), 
//						aniversariante ? NotificationType.BIRTHDAY : NotificationType.GOOD, foto);
//			}
//		}
//		else {

		// verifica se hÃ¡ um dia permitido
		if (Utils.isDiaPermitido(matchedAthleteAccess, data)) {

			// verifica se estÃ¡ num horario permitido
			if (Utils.isDentroDoHorario(matchedAthleteAccess, data)) {
				resultadoVerificacao = validado;
				logAccess.setStatus("ATIVO");
				aniversariante = Utils.isBirthday(matchedAthleteAccess, data);

				if (createNotification && Origens.ORIGEM_LIBERADO_SISTEMA.equals(origem))
					Utils.createNotification(
							userName + " permitido"
									+ (VerificationResult.TOLERANCE_PERIOD.equals(validado) ? " pela tolerÃ¢ncia."
											: "."),
							aniversariante ? NotificationType.BIRTHDAY : NotificationType.GOOD, foto);
			} else {
				resultadoVerificacao = VerificationResult.NOT_ALLOWED_NOW;
				logAccess.setStatus("INATIVO");
				if (createNotification)
					Utils.createNotification(userName + " fora do horÃ¡rio.", NotificationType.BAD, foto);
			}

		} else {
			resultadoVerificacao = VerificationResult.NOT_ALLOWED_TODAY;
			logAccess.setStatus("INATIVO");
			if (createNotification)
				Utils.createNotification(userName + " não permitido hoje.", NotificationType.BAD, foto);
		}
		// }

		return resultadoVerificacao;
	}

	public static synchronized <T> Object getSingleResultByCardNumber(Class<T> entityClass, Long cardNumber) {
		Object result = null;

		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return null;

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

				result = resp.getParans().get("object");

			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				executando = false;
			}

		} else {
			Session session = getSessionFactory().getCurrentSession();
			if (session.getTransaction() == null || !session.getTransaction().isActive()) {
				session.beginTransaction();
			}

			try {
				Query<?> query = session.createNamedQuery(entityClass.getSimpleName() + ".findByCardNumber",
						entityClass);
				query.setParameter("CARD_NUMBER", cardNumber);
				List<?> resultList = (List<?>) query.getResultList();
				if (resultList.isEmpty()) {
					result = null;
				} else {
					result = resultList.get(0);
				}

				session.getTransaction().commit();
			} catch (Exception e) {
				result = null;
				session.getTransaction().rollback();
				e.printStackTrace();

			} finally {
				session.close();
			}
		}

		return result;
	}

	public static synchronized <T> Object getSingleResultByRG(Class<T> entityClass, String rg) {
		Object result = null;

		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return null;

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

				result = resp.getParans().get("object");

			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				executando = false;
			}

		} else {
			Session session = getSessionFactory().getCurrentSession();
			if (session.getTransaction() == null || !session.getTransaction().isActive())
				session.beginTransaction();
			try {
				Query<?> query = session.createNamedQuery(entityClass.getSimpleName() + ".findByOnlyRG", entityClass);
				query.setParameter("RG", rg);
				List<?> resultList = (List<?>) query.getResultList();
				if (resultList.isEmpty())
					result = null;
				else
					result = resultList.get(0);
				session.getTransaction().commit();
			} catch (Exception e) {
				result = null;
				session.getTransaction().rollback();
				e.printStackTrace();

			} finally {
				session.close();
			}
		}

		return result;
	}

	public static synchronized <T> Object getSingleResultByCPF(Class<T> entityClass, String cpf) {
		Object result = null;

		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return null;

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

				result = resp.getParans().get("object");

			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				executando = false;
			}

		} else {
			Session session = getSessionFactory().getCurrentSession();
			if (session.getTransaction() == null || !session.getTransaction().isActive())
				session.beginTransaction();
			try {
				Query<?> query = session.createNamedQuery(entityClass.getSimpleName() + ".findByOnlyCPF", entityClass);
				query.setParameter("CPF", cpf);
				List<?> resultList = (List<?>) query.getResultList();
				if (resultList.isEmpty())
					result = null;
				else
					result = resultList.get(0);
				session.getTransaction().commit();
			} catch (Exception e) {
				result = null;
				session.getTransaction().rollback();
				e.printStackTrace();

			} finally {
				session.close();
			}
		}

		return result;
	}

	// Esse metodo deve sempre buscar na maquina.
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static Object[] saveUser(Class classeEntidade, ObjectWithId object) {
		Object retorno = object;
		String mensagem = "";
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive())
			session.beginTransaction();
		try {
			session.flush();
			session.clear();
			session.saveOrUpdate(object);
			retorno = session.load(classeEntidade, object.getId());
			session.getTransaction().commit();

		} catch (Exception e) {
			e.printStackTrace();
			session.getTransaction().rollback();
			mensagem = e.getMessage();

		} finally {
			session.close();
		}

		return new Object[] { retorno, mensagem };
	}

	// Esse metodo deve sempre buscar na maquina.
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static Object[] updateUser(Class classeEntidade, ObjectWithId object) {
		Object retorno = object;
		String mensagem = "";
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive())
			session.beginTransaction();
		try {
			session.update(object);
			retorno = session.load(classeEntidade, object.getId());
			session.getTransaction().commit();

		} catch (Exception e) {
			session.getTransaction().rollback();
			e.printStackTrace();
			mensagem = e.getMessage();

		} finally {
			session.close();
		}

		return new Object[] { retorno, mensagem };
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static synchronized Object[] save(Class classeEntidade, ObjectWithId object) {
		if (Main.servidor != null && !(object instanceof DeviceEntity) && !(object instanceof PreferenceEntity)) {
			if (!Main.servidor.isConnected())
				return null;

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

		} else {
			Object retorno = object;
			String mensagem = "";
			Session session = getSessionFactory().getCurrentSession();
			if (session.getTransaction() == null || !session.getTransaction().isActive())
				session.beginTransaction();
			try {
				session.saveOrUpdate(object);
				retorno = session.load(classeEntidade, object.getId());
				session.getTransaction().commit();

			} catch (Exception e) {
				e.printStackTrace();
				session.getTransaction().rollback();
				mensagem = e.getMessage();

			} finally {
				session.close();
			}

			return new Object[] { retorno, mensagem };
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static synchronized Object[] update(Class classeEntidade, ObjectWithId object) {
		if (Main.servidor != null && !(object instanceof DeviceEntity) && !(object instanceof PreferenceEntity)) {
			if (!Main.servidor.isConnected())
				return null;

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

		} else {
			Object retorno = object;
			String mensagem = "";
			Session session = getSessionFactory().getCurrentSession();
			if (session.getTransaction() == null || !session.getTransaction().isActive())
				session.beginTransaction();

			try {
				session.update(object);
				retorno = session.load(classeEntidade, object.getId());
				session.getTransaction().commit();

			} catch (Exception e) {
				session.getTransaction().rollback();
				e.printStackTrace();
				mensagem = e.getMessage();

			} finally {
				session.close();
			}

			return new Object[] { retorno, mensagem };
		}
	}

	public static synchronized void remove(Object object) {
		if (Main.servidor != null && !(object instanceof DeviceEntity)
				&& !(object instanceof ConfigurationGroupEntity)) {
			if (!Main.servidor.isConnected())
				return;

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

		} else {

			Session session = getSessionFactory().getCurrentSession();
			if (session.getTransaction() == null || !session.getTransaction().isActive())
				session.beginTransaction();
			try {
				session.delete(object);
				session.getTransaction().commit();

			} catch (Exception e) {
				session.getTransaction().rollback();
				e.printStackTrace();

			} finally {
				session.close();
			}
		}
	}

	public static Boolean cleanUserSession() {
		Boolean retorno = true;

		try {

			Configuration config = new Configuration().configure("hibernate.cfg.xml");
			ServiceRegistry serviceRegistry = config.getStandardServiceRegistryBuilder().build();
			MetadataSources metadata = new MetadataSources(serviceRegistry);

			EnumSet<TargetType> enumSet = EnumSet.of(TargetType.DATABASE);
			SchemaExport schemaExport = new SchemaExport();
			schemaExport.execute(enumSet, Action.BOTH, metadata.buildMetadata());

			sessionFactory = new Configuration().buildSessionFactory(serviceRegistry);

		} catch (Throwable ex) {
			retorno = false;
			System.err.println("Initial SessionFactory creation failed." + ex);
			throw new ExceptionInInitializerError(ex);
		}

		return retorno;
	}

	@SuppressWarnings("unchecked")
	public static synchronized void removeTemplates(Long idAthleteAccess) {
		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return;

			verificaExecucaoDePing();
			try {
				TcpMessageTO req = new TcpMessageTO(TcpMessageType.REMOVE_TEMPLATES);
				req.getParans().put("idAthleteAccess", idAthleteAccess);

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

		} else {
			try {
				HashMap<String, Object> args = new HashMap<String, Object>();
				args.put("ID_USER", idAthleteAccess);

				// apaga qualquer coleta de biometria recente localmente
				List<BiometricEntity> biometrias = (List<BiometricEntity>) getResultListWithParams(
						BiometricEntity.class, "BiometricEntity.findByIdUser", args);
				if (biometrias != null && !biometrias.isEmpty()) {
					for (BiometricEntity biometria : biometrias) {
						remove(biometria);
					}
				}

				// apaga os templates localmente
				List<TemplateEntity> templates = (List<TemplateEntity>) getResultListWithParams(TemplateEntity.class,
						"TemplateEntity.findByIdUser", args);
				if (templates != null && !templates.isEmpty()) {
					for (TemplateEntity template : templates) {
						remove(template);
					}
				}
			} catch (Exception e) {
				e.printStackTrace();
				Main.mainScreen.addEvento("Falha ao remover templates: " + e.getMessage());
			}
		}
	}

	@SuppressWarnings("unchecked")
	public static PedestrianAccessEntity validadeUsedID(PedestrianAccessEntity confereID) {
		try {

			List<PedestrianAccessEntity> pedestrianList = (List<PedestrianAccessEntity>) getResultList(
					PedestrianAccessEntity.class, "PedestrianAccessEntity.findAllPedestrian");
			int shiftTemporyPedestrian = 0;
			if (pedestrianList != null) {
				System.out.println(pedestrianList.get(shiftTemporyPedestrian).getId());
				System.out.println(pedestrianList.get(shiftTemporyPedestrian).getIdTemp());
				while (pedestrianList.get(shiftTemporyPedestrian).getId()
						.equals(pedestrianList.get(shiftTemporyPedestrian).getIdTemp()))
					shiftTemporyPedestrian += 1;
				return pedestrianList.get(shiftTemporyPedestrian);
			}

		} catch (Exception e) {
			e.printStackTrace();
			Main.mainScreen.addEvento("usuário já cadastrado: " + " " + confereID.getName() + e.getMessage());
		}
		return null;
	}

	public static void removeTemplatesFromServer(Long idPedestrianAccess) {
		try {
			// envia requisicao para apagar do servidor
			HttpConnection con = new HttpConnection(
					Main.urlApplication + "/restful-services/access/deleteBiometry?idPedestrian=" + idPedestrianAccess);
			Integer responseCode = con.getResponseCode();
			if (responseCode != 200) {
				throw new Exception(con.getErrorString());
			}

		} catch (Exception e) {
			e.printStackTrace();
			Main.mainScreen.addEvento("Falha ao remover templates do servidor: " + e.getMessage());
		}
	}

	public static synchronized boolean isAniversariante() {
		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return false;

			Boolean retorno = false;

			verificaExecucaoDePing();
			try {
				TcpMessageTO req = new TcpMessageTO(TcpMessageType.IS_ANIVERSARIANTE);

				executando = true;
				outToServer.writeObject(req);
				outToServer.flush();

				ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
				TcpMessageTO resp = (TcpMessageTO) reader.readObject();

				retorno = (Boolean) resp.getParans().get("isAniversariante");

			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				executando = false;
			}

			return retorno;
		} else {
			return aniversariante;
		}
	}

	public static synchronized PedestrianAccessEntity getMatchedAthleteAccess() {
		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return null;

			PedestrianAccessEntity pedestre = null;

			verificaExecucaoDePing();
			try {
				TcpMessageTO req = new TcpMessageTO(TcpMessageType.GET_MATCHED_ATHLETE_ACCESS);

				executando = true;
				outToServer.writeObject(req);
				outToServer.flush();

				ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
				TcpMessageTO resp = (TcpMessageTO) reader.readObject();

				pedestre = (PedestrianAccessEntity) resp.getParans().get("pedestre");

			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				executando = false;
			}

			return pedestre;

		} else {
			return matchedAthleteAccess;
		}
	}

	public static synchronized UserEntity buscaUsuarioPeloLogin(String loginName, String password) {
		UserEntity usuario = null;

		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return null;

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

				usuario = (UserEntity) resp.getParans().get("user");

			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				executando = false;
			}

		} else {
			Session session = getSessionFactory().getCurrentSession();
			if (session.getTransaction() == null || !session.getTransaction().isActive())
				session.beginTransaction();

			Query<UserEntity> query = session.createNamedQuery("UserEntity.findByLoginName", UserEntity.class);
			query.setParameter("LOGIN_NAME", loginName);
			query.setParameter("PASSWORD", password);

			List<UserEntity> resultList = query.getResultList();
			if (resultList != null && !resultList.isEmpty()) {
				usuario = resultList.get(0);
			}
		}

		return usuario;
	}

//	public static synchronized URLEntity salvarURLDeConfiguracao(String urlConfiguration) {
//		URLEntity url = new URLEntity(urlConfiguration);
//		Session session = getSessionFactory().getCurrentSession();
//		if(session.getTransaction() == null 
//				|| !session.getTransaction().isActive())
//    		session.beginTransaction();
//		try {
//			session.saveOrUpdate(url);
//			session.getTransaction().commit();
//			
//		} catch (Exception e) {
//			url = null;
//			session.getTransaction().rollback();
//			e.printStackTrace();
//		} finally {
//			session.close();
//		}
//		
//		return url;
//	}

//	public static synchronized URLEntity buscarURConfiguracao(String urlConfiguration) {
//		URLEntity url = null;
//		
//		if(Main.servidor != null) {
//			if(!Main.servidor.isConnected())
//				return null;
//			
//			verificaExecucaoDePing();
//			try {
//				TcpMessageTO req = new TcpMessageTO(TcpMessageType.BUSCA_URL_DE_CONFIGURACAO);
//				req.getParans().put("url", urlConfiguration);
//				
//				executando = true;
//				outToServer.writeObject(req);
//				outToServer.flush();
//
//				ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
//				TcpMessageTO resp = (TcpMessageTO) reader.readObject();
//				
//				url =  (URLEntity) resp.getParans().get("url");
//				
//			} catch (Exception e) {e.printStackTrace();}
//			finally {
//				executando = false;
//			}
//		
//		} else {
//			Session session = getSessionFactory().getCurrentSession();
//			if(session.getTransaction() == null 
//					|| !session.getTransaction().isActive())
//	    		session.beginTransaction();
//			
//			Query<URLEntity> query = session.createNamedQuery("URLEntity.findByUrl", URLEntity.class);
//			query.setParameter("url", urlConfiguration);
//			
//			List<URLEntity> resultList = query.getResultList();
//			if (resultList != null && !resultList.isEmpty()) {
//				url = resultList.get(0);
//			}
//		}
//
//		return url;
//	}

	public static synchronized <T> Object getSingleResultByRegistration(Class<T> entityClass, Long cardNumber) {
		Object result = null;

		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return null;

			verificaExecucaoDePing();

			try {
				TcpMessageTO req = new TcpMessageTO(TcpMessageType.GET_SINGLE_RESULT_BY_REGISTRATION);
				String classe = entityClass.getCanonicalName();
				req.getParans().put("entityClass", classe);
				req.getParans().put("cardNumber", cardNumber);

				executando = true;
				outToServer.writeObject(req);
				outToServer.flush();

				ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
				TcpMessageTO resp = (TcpMessageTO) reader.readObject();
				result = resp.getParans().get("object");

			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				executando = false;
			}

		} else {
			Session session = getSessionFactory().getCurrentSession();
			if (session.getTransaction() == null || !session.getTransaction().isActive())
				session.beginTransaction();
			try {
				Query<?> query = session.createNamedQuery(entityClass.getSimpleName() + ".findByMatricula",
						entityClass);
				query.setParameter("MATRICULA", cardNumber);
				List<?> resultList = (List<?>) query.getResultList();

				if (resultList != null && !resultList.isEmpty())
					result = resultList.get(0);

				session.getTransaction().commit();

			} catch (Exception e) {
				result = null;
				session.getTransaction().rollback();
				e.printStackTrace();

			} finally {
				session.close();
			}
		}
		return result;
	}

	public static synchronized void uploadFotosParaServidorDeReconhecimento(String caminhoDasFotos, Long idUsuario) {
		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return;

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

	public static synchronized void apagarPastaDeFotos(Long idUser) {
		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return;

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

		} else {
			System.out.println("Apagando fotos do usuario " + idUser);
			String caminhoDasFotos = Utils.getAppDataFolder() + "/reconhecimento_facial/fotos/" + idUser;
			apagarFotosLocais(caminhoDasFotos);

			if (idUser != null) {
				registraExclusaoFotosPedestre(idUser);
			}
		}
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
		if (Files.exists(path))
			Files.delete(path);
	}

	public static synchronized void registraNovasFotosPedestre(Long idUsuario) {
		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return;

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

		} else {
			PedestrianAccessEntity pedestre = (PedestrianAccessEntity) getSingleResultById(PedestrianAccessEntity.class,
					idUsuario);
			pedestre.setLatestPhotosTaken(new Date());
			pedestre.setFotosForamExcluidas(false);
			pedestre.setDatePhotosExcluded(null);

			HibernateUtil.save(PedestrianAccessEntity.class, pedestre);
		}
	}

	public static synchronized void registraExclusaoFotosPedestre(Long idUsuario) {
		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return;

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

		} else {
			PedestrianAccessEntity pedestre = (PedestrianAccessEntity) getSingleResultById(PedestrianAccessEntity.class,
					idUsuario);
			pedestre.setLatestPhotosTaken(null);
			pedestre.setFotosForamExcluidas(true);
			pedestre.setDatePhotosExcluded(new Date());

			HibernateUtil.save(PedestrianAccessEntity.class, pedestre);
		}
	}

	public static synchronized Boolean isPastaDeFotosExistente(Integer idUser) {
		Boolean existePastaComFotos = false;

		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return null;

			try {
				TcpMessageTO req = new TcpMessageTO(TcpMessageType.IS_PASTA_DE_FOTOS_EXISTENTE);
				req.getParans().put("idUser", idUser);

				outToServerFaceRecogizer.writeObject(req);
				outToServerFaceRecogizer.flush();

				ObjectInputStream reader = new ObjectInputStream(clientSocketFaceRecognizer.getInputStream());
				TcpMessageTO resp = (TcpMessageTO) reader.readObject();

				existePastaComFotos = (Boolean) resp.getParans().get("resp");

			} catch (Exception e) {
				e.printStackTrace();
			}
		} else {
			File pastaFotos = new File(Utils.getAppDataFolder() + "/reconhecimento_facial/fotos/" + idUser);
			existePastaComFotos = pastaFotos.exists();
		}

		return existePastaComFotos;
	}

	public static synchronized void enviaInicioVerificandoAcesso() {
		if (Main.servidor != null) {
//			consertar o if
			if (!Main.servidor.isConnected())
				return;

			try {
				TcpMessageTO req = new TcpMessageTO(TcpMessageType.ACCESS_VALIDATING_INI);

				outToServerFaceRecogizer.writeObject(req);
				outToServerFaceRecogizer.flush();

				ObjectInputStream reader = new ObjectInputStream(clientSocketFaceRecognizer.getInputStream());
				TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	public static synchronized void enviaFimVerificandoAcesso() {
		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return;

			try {
				TcpMessageTO req = new TcpMessageTO(TcpMessageType.ACCESS_VALIDATING_FIM);

				outToServerFaceRecogizer.writeObject(req);
				outToServerFaceRecogizer.flush();

				ObjectInputStream reader = new ObjectInputStream(clientSocketFaceRecognizer.getInputStream());
				TcpMessageTO resp = (TcpMessageTO) reader.readObject();

			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	@SuppressWarnings("unchecked")
	public static synchronized List<DeviceTO> getListDeviceFromServer() {
		List<DeviceTO> result = null;
		if (Main.servidor == null || !Main.servidor.isConnected()) {
			return null;
		}
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
		if (Main.servidor == null || !Main.servidor.isConnected()) {
			return;
		}

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
		Object result = null;

		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return null;

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

				result = resp.getParans().get("object");

			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				executando = false;
			}

		} else {
			Session session = getSessionFactory().getCurrentSession();
			if (session.getTransaction() == null || !session.getTransaction().isActive())
				session.beginTransaction();

			try {
				Query<?> query = session.createNamedQuery(entityClass.getSimpleName() + ".findByIdTemp2", entityClass);
				query.setParameter("ID", id);
				List<?> resultList = (List<?>) query.getResultList();
				if (resultList.isEmpty())
					result = null;
				else
					result = resultList.get(0);
				session.getTransaction().commit();

			} catch (Exception e) {
				result = null;
				session.getTransaction().rollback();
				e.printStackTrace();

			} finally {
				session.close();
			}
		}

		return result;
	}

	private static Boolean deveGravarCartaoRecebidoNoLog(Integer origem) {
		return origem != null && origem != Origens.ORIGEM_LIBERADO_SISTEMA && origem != 18;
	}

	@SuppressWarnings("rawtypes")
	public static void apagaDadosCartao() {

		System.out.println("chegou no apadaDados ");
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive())
			session.beginTransaction();

		try {
//			antes o count
			Query q = session.createQuery("update PedestrianAccessEntity p "
					+ "set p.cardNumber = null, qtdAcessoAntesSinc = 0, p.quantidadeCreditos = 0, p.editadoNoDesktop = true "
					+ "	where  p.tipo = 'VISITANTE' " + " and p.cardNumber != null " + " and p.cardNumber != '' "
					+ "and p.dataCadastroFotoNaHikivision = null "
					+ " and p.qrCodeParaAcesso is null ");
			q.executeUpdate();
			session.getTransaction().commit();
//			depois do commit a nova query
		} catch (Exception e) {
			session.getTransaction().rollback();
			e.printStackTrace();

		} finally {
			session.close();
		}

	}

	public static void apagaDadosDeUltimoSentido() {
		System.out.println(" chegou no apaga logs do do pedestre ");
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive())
			session.beginTransaction();

		try {
//					antes o count
			Query p = session.createQuery(
					"update LogPedestrianAccessEntity l " + "set l.direction = null " + "where l.status = 'ATIVO' ");

			p.executeUpdate();
			session.getTransaction().commit();
//					depois do commit a nova query
		} catch (Exception e) {
			session.getTransaction().rollback();
			e.printStackTrace();

		} finally {
			session.close();
		}

	}

	@SuppressWarnings("rawtypes")
	public static void apagaDadosDeGiro(Date data) {
//		Parou na apagaDadosDeGIRO O QUE Ã‰ DATA?

		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive())
			session.beginTransaction();

		// TODO : voltar a validaÃ§Ã£o da data de cadastro, porÃ©m para maior que
		// a data calculada a baixo

		Calendar c = Calendar.getInstance();
		c.setTime(data);
		c.add(Calendar.DATE, -1);

		try {
			Query q = session.createQuery("update PedestrianAccessEntity p "
					+ "set p.qtdAcessoAntesSinc = 0, p.editadoNoDesktop = true " + "where p.qtdAcessoAntesSinc > 0 "
					+ " and p.cardNumber != null " + " and p.cardNumber != '' " + "	and p.dataCriacao >= :DATA ");
			q.setParameter("DATA", data);
			q.executeUpdate();
			session.getTransaction().commit();
		} catch (Exception e) {
			session.getTransaction().rollback();
			e.printStackTrace();

		} finally {
			session.close();
		}

	}

	public static void apagaQuantidadeAcessosAsinc() {
		System.out.println("Entrou no apagaQuantidadeAcessosAsinc");

		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive())
			session.beginTransaction();

		try {
			Query q = session.createQuery("update PedestrianAccessEntity p " + "set p.qtdAcessoAntesSinc = 0 "
					+ "where p.qtdAcessoAntesSinc > 0 ");
			q.executeUpdate();
			session.getTransaction().commit();
		} catch (Exception e) {
			session.getTransaction().rollback();
			e.printStackTrace();

		} finally {
			session.close();
		}

	}

	@SuppressWarnings("rawtypes")
	public static void resetStatusAllCards() {

		if (Main.servidor != null) {
			if (!Main.servidor.isConnected())
				return;

			verificaExecucaoDePing();
			try {

				TcpMessageTO req = new TcpMessageTO(TcpMessageType.RESET_STATUS_ALL_CARDS);

				executando = true;
				outToServer.writeObject(req);
				outToServer.flush();

				ObjectInputStream reader = new ObjectInputStream(clientSocket.getInputStream());
				TcpMessageTO resp = (TcpMessageTO) reader.readObject();

				Object result = resp.getParans().get("object");

			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				executando = false;
			}

		} else {

			Session session = getSessionFactory().getCurrentSession();
			if (session.getTransaction() == null || !session.getTransaction().isActive())
				session.beginTransaction();

			try {
				Query q = session.createQuery(
						"update CartaoComandaEntity c " + "set c.status = 'AGUARDANDO', c.dataAlteracao = :DATA ");
				q.setParameter("DATA", new Date());
				q.executeUpdate();
				session.getTransaction().commit();
			} catch (Exception e) {
				session.getTransaction().rollback();
				e.printStackTrace();

			} finally {
				session.close();
			}

		}

	}

	@SuppressWarnings("unchecked")
	public static synchronized void sendLogs(Integer qtdeTotalLogos, String namedQuery, HashMap<String, Object> args,
			boolean marcaLogsComoEnviados) {
		if (qtdeTotalLogos <= 0) {
			return;
		}

		int pageSize = 50;
		int offset = 0;

		do {
			List<LogPedestrianAccessEntity> logList = (List<LogPedestrianAccessEntity>) getResultListWithParams(
					LogPedestrianAccessEntity.class, namedQuery, args, (offset * pageSize), pageSize);

			if (logList == null) {
				continue;
			}

			System.out
					.println(sdf.format(new Date()) + "  LOG DE ACESSO: " + logList.size() + " registros para enviar");
			JsonArray requestArray = new JsonArray();
			for (LogPedestrianAccessEntity log : logList) {
				JsonObject requestObj = new JsonObject();
				requestObj.addProperty("idLoggedUser", log.getIdLoggedUser().toString());
				requestObj.addProperty("idPedestrian",
						log.getIdPedestrian() == null ? "" : log.getIdPedestrian().toString());
				requestObj.addProperty("accessDate", log.getAccessDate().getTime() + "");
				requestObj.addProperty("status", log.getStatus());
				requestObj.addProperty("location", log.getLocation());
				requestObj.addProperty("reason", log.getReason());
				requestObj.addProperty("direction", log.getDirection() == null ? Tipo.ENTRADA : log.getDirection());
				requestObj.addProperty("equipament", log.getEquipament() == null ? "--" : log.getEquipament());
				requestObj.addProperty("bloquearSaida",
						log.getBloquearSaida() != null ? log.getBloquearSaida() : false);
				requestObj.addProperty("cartaoAcessoRecebido",
						log.getCartaoAcessoRecebido() != null ? log.getCartaoAcessoRecebido() : "");
				requestArray.add(requestObj);
			}

			try {
				HttpConnection con = new HttpConnection(Main.urlApplication + "/restful-services/access/registerlog");
				int responseCode = con.sendResponse(requestArray.toString());
				if (responseCode != 200) {
					System.out.println(sdf.format(new Date()) + "  ERRO AO ENVIAR LOG DE ACESSO: Response Code: "
							+ responseCode + "  Error String: " + con.getErrorString());
					setFailAtSync(logList, true);
				}

				if (marcaLogsComoEnviados) {
					setFailAtSync(logList, false);
				}

			} catch (Throwable e) {
				System.out.println(e.getMessage());
				setFailAtSync(logList, true);
			}

			offset++;

		} while (offset * pageSize <= qtdeTotalLogos);
	}

	private static synchronized void setFailAtSync(List<LogPedestrianAccessEntity> logList, boolean status) {
		if (logList == null || logList.isEmpty()) {
			return;
		}

		logList.forEach(log -> {
			log.setFailAtSync(status);

			HibernateUtil.save(LogPedestrianAccessEntity.class, log);
		});
	}

	public static synchronized Device getDeviceByIdentifier(String identifier) {
		if (Main.devicesList == null || Main.devicesList.isEmpty()) {
			return null;
		}

		for (Device device : Main.devicesList) {
			if (identifier.equals(device.getIdentifier())) {
				return device;
			}
		}

		return null;
	}

}