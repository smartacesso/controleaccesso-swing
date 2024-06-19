package com.protreino.services.repository;

import java.io.File;
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import javax.persistence.TypedQuery;

import org.hibernate.CacheMode;
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
import com.protreino.services.constants.Tipo;
import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.entity.ObjectWithId;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.entity.UserEntity;
import com.protreino.services.main.Main;
import com.protreino.services.utils.HttpConnection;
import com.protreino.services.utils.Utils;

public class HibernateLocalAccessData {

	private static SessionFactory sessionFactory;

	public static SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss:sss");

	static {
		try {
			sessionFactory = new Configuration().configure("hibernate.cfg.xml").buildSessionFactory();

		} catch (Throwable ex) {
			System.err.println("Initial SessionFactory creation failed." + ex);
			throw new ExceptionInInitializerError(ex);
		}
	}

	public static SessionFactory getSessionFactory() {
		if (sessionFactory != null && !sessionFactory.isOpen()) {
			sessionFactory = new Configuration().configure("hibernate.cfg.xml").buildSessionFactory();
		}
		return sessionFactory;
	}

	public static void shutdown() {
		if (getSessionFactory().isOpen()) {
			if (getSessionFactory().getCurrentSession().getTransaction().isActive()) {
				getSessionFactory().getCurrentSession().getTransaction().rollback();
			}
			if (getSessionFactory().getCurrentSession().isOpen()) {
				getSessionFactory().getCurrentSession().close();
			}
			getSessionFactory().close();
		}
	}

	public static synchronized <T> Object getSingleResultById(Class<T> entityClass, Long id) {
		final Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
			session.setCacheMode(CacheMode.IGNORE);
		}
		
		Object result = null;
		try {
			Query<?> query = session.createNamedQuery(entityClass.getSimpleName()
					+ (entityClass.equals(PedestrianAccessEntity.class) ? ".findByIdNaoRemovido" : ".findById"), entityClass);
			query.setParameter("ID", id);
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
		
		return result;
	}

	public static synchronized <T> Object getAllPedestresById(Long id) {
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

		Object result = null;
		try {
			Query<?> query = session.createNamedQuery(PedestrianAccessEntity.class.getSimpleName() + ".findById",
					PedestrianAccessEntity.class);
			query.setParameter("ID", id);
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

		return result;
	}

	public static synchronized List<LogPedestrianAccessEntity> buscaLogsDeAcessoPaginados(String namedQuery,
			HashMap<String, Object> args, Integer inicio, Integer quantidade) {
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

		List<LogPedestrianAccessEntity> resultList = null;
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

		return resultList;
	}
	
	public static synchronized Integer getResultListCount(Class<?> entityClass, String namedQuery, HashMap<String, Object> args) {
		Integer count = 0;

		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

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
		
		return count;
	}

	public static synchronized <T> List<?> getResultList(Class<T> entityClass, String namedQuery) {
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

		List<?> resultList = null;
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
		
		return resultList;
	}

	public static synchronized <T> List<?> buscaListaDevicesDoServidor(Class<T> entityClass, String namedQuery) {
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

		List<?> resultList = null;
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

		return resultList;
	}
	

	public static synchronized <T> List<?> getResultListLimited(Class<T> entityClass, String namedQuery, Long quantidade) {
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

		List<?> resultList = null;
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
		
		return resultList;
	}

	public static synchronized <T> List<?> getResultListWithParams(Class<T> entityClass, String namedQuery,
			HashMap<String, Object> args, Integer inicio, Integer quantidade) {
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

		List<?> resultList = null;
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
			
		return resultList;
	}

	public static synchronized Integer getResultListWithParamsCount(Class<?> entityClass, String namedQuery, HashMap<String, Object> args) {

		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

		int count = 0;
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

		return count;
	}

	public static synchronized <T> Object getUniqueResultWithParams(Class<T> entityClass, String namedQuery, HashMap<String, Object> args) {
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

		try {
			Query<?> query = session.createNamedQuery(namedQuery, entityClass);

			if (args != null) {
				for (Map.Entry<String, Object> entry : args.entrySet()) {
					query.setParameter(entry.getKey(), entry.getValue());
				}
			}

			query.setMaxResults(1);

			List<?> resultList = (List<?>) query.getResultList();
			if(Objects.nonNull(resultList) && !resultList.isEmpty()) {
				return resultList.get(0);
			}
			
			session.getTransaction().commit();

		} catch (Exception e) {
			session.getTransaction().rollback();
			e.printStackTrace();

		} finally {
			session.close();
		}
		
		return null;
	}


	public static synchronized <T> List<?> getResultListWithDynamicParams(Class<T> entityClass, String construtor,
			String join, String groupBy, String orderColumn, HashMap<String, Object> args, Integer inicio, Integer quantidade) {
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

		List<?> resultList = null;
		try {
			String sql = "";
			if (construtor != null) {
				sql = "select new " + construtor + " from " + entityClass.getSimpleName() + " obj ";

			} else {
				sql = "select obj from " + entityClass.getSimpleName() + " obj ";
			}

			if (join != null) {
				sql += " " + join + " ";
			}

			if (args.entrySet() != null && !args.entrySet().isEmpty()) {
				sql += "where";
			}

			String clause = "";
			for (Map.Entry<String, Object> entry : args.entrySet()) {
				String paramName = entry.getKey();
				Object paramValue = entry.getValue();
				if (paramValue != null) {
					if (paramValue instanceof Boolean || paramValue instanceof Long) {
						if ("removido".equals(paramName)) {
							sql += clause + " (obj." + paramName + " = :" + paramName + " or obj." + paramName + " is null)";
						
						} else {
							sql += clause + " obj." + paramName + " = :" + paramName;
						}

					} else {
						sql += clause + " obj." + paramName + " like :" + paramName;
					}

					clause = " and";
				}
			}

			if (groupBy != null) {
				sql += " " + groupBy + " ";
			}

			if (orderColumn != null) {
				sql += " order by obj." + orderColumn + " asc";
			}

			Query<?> query = session.createQuery(sql);

			for (Map.Entry<String, Object> entry : args.entrySet()) {
				if (entry.getValue() instanceof Boolean || entry.getValue() instanceof Long) {
					query.setParameter(entry.getKey(), entry.getValue());
				} else {
					query.setParameter(entry.getKey(), "%" + String.valueOf(entry.getValue()).toUpperCase() + "%");
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

		return resultList;
	}

	public static synchronized Integer getResultListWithDynamicParamsCount(Class<?> entityClass, String construtor,
			String join, String groupBy, HashMap<String, Object> args) {
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

		int count = 0;

		try {
			String sql = "";
			if (construtor != null) {
				sql = "select new " + construtor + " from " + entityClass.getSimpleName() + " obj ";

			} else {
				sql = "select count(obj) from " + entityClass.getSimpleName() + " obj ";
			}

			if (join != null) {
				sql += " " + join + " ";
			}

			if (args.entrySet() != null && !args.entrySet().isEmpty()) {
				sql += "where";
			}

			String clause = "";
			for (Map.Entry<String, Object> entry : args.entrySet()) {
				String paramName = entry.getKey();
				Object paramValue = entry.getValue();
				if (paramValue != null) {
					if (paramValue instanceof Boolean || paramValue instanceof Long) {
						if ("removido".equals(paramName)) {
							sql += clause + " (obj." + paramName + " = :" + paramName + " or obj." + paramName + " is null)";

						} else {
							sql += clause + " obj." + paramName + " = :" + paramName;
						}

					} else {
						sql += clause + " obj." + paramName + " like :" + paramName;
					}
					
					clause = " and";
				}
			}

			if (groupBy != null) {
				sql += " " + groupBy + " ";
			}

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

		return count;
	}

	// Esse metodo deve sempre buscar na maquina.
	public static UserEntity getLoggedUser(String namedQuery) {
		UserEntity user = null;

		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

		try {
			Query<UserEntity> query = session.createNamedQuery(namedQuery, UserEntity.class);
			List<UserEntity> resultList = query.getResultList();
			if (resultList.isEmpty()) {
				user = null;
			
			} else {
				user = resultList.get(0);
			}
			
			session.getTransaction().commit();

		} catch (Exception e) {
			user = null;
			session.getTransaction().rollback();
			e.printStackTrace();
		}

		return user;
	}

	public static synchronized <T> Object getSingleResult(Class<T> entityClass, String namedQuery) {
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

		Object result = null;

		try {
			Query<?> query = session.createNamedQuery(namedQuery, entityClass);
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

		return result;
	}

	public static synchronized List<PedestrianAccessEntity> buscaPedestresAtivosComCartao() {
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

		List<PedestrianAccessEntity> acessos = null;
		try {
			TypedQuery<PedestrianAccessEntity> query = session
					.createQuery(
							"select obj from PedestrianAccessEntity obj " + "where obj.cardNumber is not null "
									+ "and obj.cardNumber <> '' "
									+ "and (obj.removido is null or obj.removido = false) " + "order by obj.id desc",
							PedestrianAccessEntity.class);

			acessos = query.getResultList();
			session.getTransaction().commit();
			if (acessos != null) {
				return acessos;
			}

		} catch (Exception e) {
			session.getTransaction().rollback();
			e.printStackTrace();

		} finally {
			session.close();
		}

		return null;
	}

	public static synchronized List<PedestrianAccessEntity> buscaPedestresAtivosComBiometria() {
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

		List<PedestrianAccessEntity> acessos = null;
		try {
			TypedQuery<PedestrianAccessEntity> query = session.createQuery(
					"select obj from PedestrianAccessEntity obj "
							+ "where obj.removido is null or obj.removido = false " + "order by obj.id asc",
					PedestrianAccessEntity.class);

			acessos = query.getResultList();
			session.getTransaction().commit();
			if (acessos != null) {
				return acessos;
			}

		} catch (Exception e) {
			session.getTransaction().rollback();
			e.printStackTrace();

		} finally {
			session.close();
		}

		return null;
	}

	public static synchronized long countAcessosPedestre(Long idPedestre) {
		long qtdeAcessos = 0L;

		UserEntity loggedUser = HibernateAccessDataFacade.getLoggedUser("UserEntity.findAll");

		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

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

		return qtdeAcessos;
	}

	public static synchronized Date buscaUltimoAcessoAtivoPedestre(Long idPedestre) {
		LogPedestrianAccessEntity acesso = null;

		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

		try {
			TypedQuery<LogPedestrianAccessEntity> query = session.createQuery(
					  "select obj from LogPedestrianAccessEntity obj " 
					+ "where obj.idPedestrian = :ID_PEDESTRE "
					+ "and obj.status = 'ATIVO' and obj.direction = 'ENTRADA' " 
					+ "order by obj.id desc",
					LogPedestrianAccessEntity.class);

			query.setMaxResults(1);
			query.setParameter("ID_PEDESTRE", idPedestre);
			List<LogPedestrianAccessEntity> acessos = query.getResultList();
			if (acessos != null && !acessos.isEmpty()) {
				acesso = acessos.get(0);
			}

			session.getTransaction().commit();

		} catch (Exception e) {
			session.getTransaction().rollback();
			e.printStackTrace();

		} finally {
			session.close();
		}

		if (acesso != null) {
			return acesso.getAccessDate();
		}

		return null;
	}

	public static synchronized boolean verificaSePossuiEntradaNoEquipamento(String idDevice, Long idPedestre) {
		List<LogPedestrianAccessEntity> acesso = null;

		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}
		
		try {
			TypedQuery<LogPedestrianAccessEntity> query = session.createQuery(
					"select obj " + "from LogPedestrianAccessEntity obj " + "where obj.idPedestrian = :ID_PEDESTRE "
							+ "and obj.status = 'ATIVO' " + "and obj.direction = 'ENTRADA' "
							+ "and obj.equipament = :ID_DEVICE " + "and (select count(saida.id) "
							+ "		from LogPedestrianAccessEntity saida "
							+ "		where obj.idPedestrian = saida.idPedestrian "
							+ "			and saida.direction = 'SAIDA' " + "			and saida.equipament = :ID_DEVICE "
							+ "			and saida.status = 'ATIVO' "
							+ "			and saida.accessDate > obj.accessDate) = 0 " 
							+ "order by obj.id desc",
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

	public static synchronized <T> Object getSingleResultByCardNumber(Class<T> entityClass, Long cardNumber) {
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

		Object result = null;
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

		return result;
	}

	public static synchronized <T> Object getSingleResultByRG(Class<T> entityClass, String rg) {
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

		Object result = null;
		
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

		return result;
	}

	public static synchronized <T> Object getSingleResultByCPF(Class<T> entityClass, String cpf) {
		Object result = null;

		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

		try {
			Query<?> query = session.createNamedQuery(entityClass.getSimpleName() + ".findByOnlyCPF", entityClass);
			query.setParameter("CPF", cpf);
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

		return result;
	}

	// Esse metodo deve sempre buscar na maquina.
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static Object[] saveUser(Class classeEntidade, ObjectWithId object) {
		Object retorno = object;
		String mensagem = "";
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

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
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

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

	public static synchronized Object[] save(Class<?> classeEntidade, ObjectWithId object) {
		Session session = getSessionFactory().getCurrentSession();
	
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}
			
		Object retorno = object;
		String mensagem = "";

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

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static synchronized Object[] update(Class classeEntidade, ObjectWithId object) {
		Session session = getSessionFactory().getCurrentSession();
		
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

		Object retorno = object;
		String mensagem = "";

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

	public static synchronized void remove(Object object) {
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

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
	public static PedestrianAccessEntity validadeUsedID(PedestrianAccessEntity confereID) {
		try {
			List<PedestrianAccessEntity> pedestrianList = (List<PedestrianAccessEntity>) getResultList(
					PedestrianAccessEntity.class, "PedestrianAccessEntity.findAllPedestrian");
			int shiftTemporyPedestrian = 0;
			if (pedestrianList != null) {
				System.out.println(pedestrianList.get(shiftTemporyPedestrian).getId());
				System.out.println(pedestrianList.get(shiftTemporyPedestrian).getIdTemp());
				while (pedestrianList.get(shiftTemporyPedestrian).getId()
						.equals(pedestrianList.get(shiftTemporyPedestrian).getIdTemp())) {
					shiftTemporyPedestrian += 1;
				}

				return pedestrianList.get(shiftTemporyPedestrian);
			}

		} catch (Exception e) {
			e.printStackTrace();
			Main.mainScreen.addEvento("usuário já cadastrado: " + " " + confereID.getName() + e.getMessage());
		}
		return null;
	}

	public static synchronized UserEntity buscaUsuarioPeloLogin(String loginName, String password) {
		UserEntity usuario = null;

		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

		Query<UserEntity> query = session.createNamedQuery("UserEntity.findByLoginName", UserEntity.class);
		query.setParameter("LOGIN_NAME", loginName);
		query.setParameter("PASSWORD", password);

		List<UserEntity> resultList = query.getResultList();
		if (resultList != null && !resultList.isEmpty()) {
			usuario = resultList.get(0);
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
//		if(Main.temServidor()) {
//			if(Main.servidor.isNotConnected())
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

	public static synchronized <T> Object getSingleResultByRegistration(Class<T> entityClass, Long matricula) {
		Object result = null;

		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

		try {
			Query<?> query = session.createNamedQuery(entityClass.getSimpleName() + ".findByMatricula", entityClass);
			query.setParameter("MATRICULA", matricula);
			List<?> resultList = (List<?>) query.getResultList();

			if (resultList != null && !resultList.isEmpty()) {
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
		
		return result;
	}

	public static synchronized void apagarPastaDeFotos(Long idUser) {
		System.out.println("Apagando fotos do usuario " + idUser);
		String caminhoDasFotos = Utils.getAppDataFolder() + "/reconhecimento_facial/fotos/" + idUser;
		apagarFotosLocais(caminhoDasFotos);

		if (idUser != null) {
			registraExclusaoFotosPedestre(idUser);
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
		if (Files.exists(path)) {
			Files.delete(path);
		}
	}

	public static synchronized void registraNovasFotosPedestre(Long idUsuario) {
		PedestrianAccessEntity pedestre = (PedestrianAccessEntity) getSingleResultById(PedestrianAccessEntity.class, idUsuario);
		pedestre.setLatestPhotosTaken(new Date());
		pedestre.setFotosForamExcluidas(false);
		pedestre.setDatePhotosExcluded(null);

		HibernateAccessDataFacade.save(PedestrianAccessEntity.class, pedestre);
	}

	public static synchronized void registraExclusaoFotosPedestre(Long idUsuario) {
		PedestrianAccessEntity pedestre = (PedestrianAccessEntity) getSingleResultById(PedestrianAccessEntity.class, idUsuario);
		pedestre.setLatestPhotosTaken(null);
		pedestre.setFotosForamExcluidas(true);
		pedestre.setDatePhotosExcluded(new Date());

		save(PedestrianAccessEntity.class, pedestre);
	}

	public static synchronized Boolean isPastaDeFotosExistente(Integer idUser) {
		File pastaFotos = new File(Utils.getAppDataFolder() + "/reconhecimento_facial/fotos/" + idUser);
		return pastaFotos.exists();
	}

	public static synchronized <T> Object getSingleResultByIdTemp(Class<PedestrianAccessEntity> entityClass, Long id) {
		Object result = null;

		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

		try {
			Query<?> query = session.createNamedQuery(entityClass.getSimpleName() + ".findByIdTemp2", entityClass);
			query.setParameter("ID", id);
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

		return result;
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

	@SuppressWarnings("rawtypes")
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
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

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

	@SuppressWarnings("rawtypes")
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
		Session session = getSessionFactory().getCurrentSession();
		if (session.getTransaction() == null || !session.getTransaction().isActive()) {
			session.beginTransaction();
		}

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

			HibernateAccessDataFacade.save(LogPedestrianAccessEntity.class, log);
		});
	}
	
	/**
	 * CODIGO PARA LISTAR TODAS AS CONSTRAINTS DO BANCO DE DADOS
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static void listarConstraints() {
		Session session = getSessionFactory().getCurrentSession();
		session.beginTransaction();
		Query nativeQuery = session.createNativeQuery(" SELECT "
				+ "  'ALTER TABLE '||S.SCHEMANAME||'.'||T.TABLENAME||' DROP CONSTRAINT '||C.CONSTRAINTNAME||';'"
				+ " FROM " + "  SYS.SYSCONSTRAINTS C, " + "  SYS.SYSSCHEMAS S," + "  SYS.SYSTABLES T" + " WHERE"
				+ "  C.SCHEMAID = S.SCHEMAID" + " AND" + "  C.TABLEID = T.TABLEID" + " AND"
				+ "  S.SCHEMANAME = 'SMARTACESSO'" + " UNION"
				+ " SELECT 'DROP TABLE ' || schemaname ||'.' || tablename || ';'" + " FROM SYS.SYSTABLES"
				+ " INNER JOIN SYS.SYSSCHEMAS ON SYS.SYSTABLES.SCHEMAID = SYS.SYSSCHEMAS.SCHEMAID"
				+ " where schemaname='SMARTACESSO'");
		List<String> queries = nativeQuery.getResultList();

		for (String query : queries) {
			System.out.println(query);
		}

		session.getTransaction().commit();
		session.close();
	}

}