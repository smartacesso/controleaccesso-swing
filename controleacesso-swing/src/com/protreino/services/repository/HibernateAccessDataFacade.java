package com.protreino.services.repository;

import java.util.Date;
import java.util.HashMap;
import java.util.List;

import com.protreino.services.entity.BiometricEntity;
import com.protreino.services.entity.ConfigurationGroupEntity;
import com.protreino.services.entity.DeviceEntity;
import com.protreino.services.entity.LogPedestrianAccessEntity;
import com.protreino.services.entity.ObjectWithId;
import com.protreino.services.entity.PedestrianAccessEntity;
import com.protreino.services.entity.PreferenceEntity;
import com.protreino.services.entity.TemplateEntity;
import com.protreino.services.entity.UserEntity;
import com.protreino.services.main.Main;
import com.protreino.services.to.DeviceTO;

public class HibernateAccessDataFacade {

	public static synchronized <T> Object getSingleResultById(Class<T> entityClass, Long id) {
		if (Main.temServidor()) {
			if (Main.getServidor().isNotConnected()) {
				return null;
			}

			return HibernateServerAccessData.getSingleResultById(entityClass, id);

		} else {
			return HibernateLocalAccessData.getSingleResultById(entityClass, id);
		}
	}
	
	public static synchronized <T> Object getAllPedestresById(Long id) {
		if (Main.temServidor()) {
			if (Main.getServidor().isNotConnected()) {
				return null;
			}
			
			return HibernateServerAccessData.getAllPedestresById(id);
			
		} else {
			return HibernateLocalAccessData.getAllPedestresById(id);
		}
	}
	
	public static synchronized int getResultListCount(Class<?> entityClass, String namedQuery) {
		return getResultListCount(entityClass, namedQuery, null);
	}
	
	public static int getResultListCount(Class<?> entityClass, String namedQuery, HashMap<String, Object> args) {
		if (Main.temServidor() && !DeviceEntity.class.equals(entityClass)) {
			if (Main.getServidor().isNotConnected()) {
				return 0;
			}
		
			return HibernateServerAccessData.getResultListCount(entityClass, namedQuery, args);
			
		} else {
			return HibernateLocalAccessData.getResultListCount(entityClass, namedQuery, args);
		}
	}
	
	public static synchronized List<LogPedestrianAccessEntity> buscaLogsDeAcessoPaginados(String namedQuery, HashMap<String, Object> args, 
			Integer inicio, Integer quantidade) {
		if (Main.temServidor()) {
			if (Main.getServidor().isNotConnected()) {
				return null;
			}
		
			return HibernateServerAccessData.buscaLogsDeAcessoPaginados(namedQuery, args, inicio, quantidade);
			
		} else {
			return HibernateLocalAccessData.buscaLogsDeAcessoPaginados(namedQuery, args, inicio, quantidade);
		}
	}
	
	public static synchronized <T> List<?> getResultList(Class<T> entityClass, String namedQuery) {
		if (Main.temServidor() && !DeviceEntity.class.equals(entityClass)) {
			if (Main.getServidor().isNotConnected()) {
				return null;
			}
			
			return HibernateServerAccessData.getResultList(entityClass, namedQuery);
		
		} else {
			return HibernateLocalAccessData.getResultList(entityClass, namedQuery);
		}
	}
	
	public static synchronized <T> List<?> buscaListaDevicesDoServidor(Class<T> entityClass, String namedQuery) {
		if (Main.temServidor()) {
			if (Main.getServidor().isNotConnected()) {
				return null;
			}
		
			return HibernateServerAccessData.buscaListaDevicesDoServidor(entityClass, namedQuery);

		} else {
			return HibernateLocalAccessData.buscaListaDevicesDoServidor(entityClass, namedQuery);
		}
	}
	
	public static synchronized <T> List<?> getResultListLimited(Class<T> entityClass, String namedQuery, Long quantidade) {
		if (Main.temServidor() && !DeviceEntity.class.equals(entityClass)) {
			if (Main.getServidor().isNotConnected()) {
				return null;
			}
			
			return HibernateServerAccessData.getResultListLimited(entityClass, namedQuery, quantidade);

		} else {
			return HibernateLocalAccessData.getResultListLimited(entityClass, namedQuery, quantidade);
		}
	}
	
	public static synchronized <T> List<?> getResultListWithParams(Class<T> entityClass, String namedQuery,
			HashMap<String, Object> args) {
		return getResultListWithParams(entityClass, namedQuery, args, null, null);
	}

	public static synchronized <T> List<?> getResultListWithParams(Class<T> entityClass, String namedQuery,
			HashMap<String, Object> args, Integer inicio, Integer quantidade) {
		if (Main.temServidor() && !entityClass.equals(PreferenceEntity.class)) {
			if (Main.getServidor().isNotConnected()) {
				return null;
			}
		
			return HibernateServerAccessData.getResultListWithParams(entityClass, namedQuery, args, inicio, quantidade);
			
		} else {
			return HibernateLocalAccessData.getResultListWithParams(entityClass, namedQuery, args, inicio, quantidade);
		}
	}
	
	public static synchronized int getResultListWithParamsCount(Class<?> entityClass, String namedQuery, HashMap<String, Object> args) {
		if (Main.temServidor() && !entityClass.equals(PreferenceEntity.class)) {
			if (Main.getServidor().isNotConnected()) {
				return 0;
			}
			
			return HibernateServerAccessData.getResultListWithParamsCount(entityClass, namedQuery, args);
			
		} else {
			return HibernateLocalAccessData.getResultListWithParamsCount(entityClass, namedQuery, args);
		}
	}
	
	public static synchronized <T> Object getUniqueResultWithParams(Class<T> entityClass, String namedQuery, HashMap<String, Object> args) {
		if (Main.temServidor() && !entityClass.equals(PreferenceEntity.class)) {
			if (Main.getServidor().isNotConnected()) {
				return null;
			}
			
			return HibernateServerAccessData.getUniqueResultWithParams(entityClass, namedQuery, args);
			
		} else {
			return HibernateLocalAccessData.getUniqueResultWithParams(entityClass, namedQuery, args);
		}
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
			String join, String groupBy, String orderColumn, HashMap<String, Object> args, Integer inicio, Integer quantidade) {
		if (Main.temServidor()) {
			if (Main.getServidor().isNotConnected()) {
				return null;
			}
			
			return HibernateServerAccessData.getResultListWithDynamicParams(entityClass, construtor, join, groupBy, orderColumn, args, inicio, quantidade);
		
		} else {
			return HibernateLocalAccessData.getResultListWithDynamicParams(entityClass, construtor, join, groupBy, orderColumn, args, inicio, quantidade);
		}
	}
	
	public static synchronized int getResultListWithDynamicParamsCount(Class<?> entityClass, String construtor,
			String join, String groupBy, HashMap<String, Object> args) {
		if (Main.temServidor()) {
			if (Main.getServidor().isNotConnected()) {
				return 0;
			}

			return HibernateServerAccessData.getResultListWithDynamicParamsCount(entityClass, construtor, join, groupBy, args);
		
		} else {
			return HibernateLocalAccessData.getResultListWithDynamicParamsCount(entityClass, construtor, join, groupBy, args);
		}
	}
	
	public static synchronized <T> Object getSingleResult(Class<T> entityClass, String namedQuery) {
		if (Main.temServidor()) {
			if (Main.getServidor().isNotConnected()) {
				return null;
			}

			return HibernateServerAccessData.getSingleResult(entityClass, namedQuery);
			
		} else {
			return HibernateLocalAccessData.getSingleResult(entityClass, namedQuery);
		}
	}
	
	public static synchronized long countAcessosPedestre(Long idPedestre) {
		if (Main.temServidor()) {
			if (Main.getServidor().isNotConnected()) {
				return 0l;
			}
			
			return HibernateServerAccessData.countAcessosPedestre(idPedestre);

		} else {
			return HibernateLocalAccessData.countAcessosPedestre(idPedestre);
		}
	}
	
	public static synchronized <T> Object getSingleResultByCardNumber(Class<T> entityClass, Long cardNumber) {
		if (Main.temServidor()) {
			if (Main.getServidor().isNotConnected()) {
				return null;
			}
			
			return HibernateServerAccessData.getSingleResultByCardNumber(entityClass, cardNumber);
			
		} else {
			return HibernateLocalAccessData.getSingleResultByCardNumber(entityClass, cardNumber);
		}
	}
	
	public static synchronized <T> Object getSingleResultByRG(Class<T> entityClass, String rg) {
		if (Main.temServidor()) {
			if (Main.getServidor().isNotConnected()) {
				return null;
			}
			
			return HibernateServerAccessData.getSingleResultByRG(entityClass, rg);
			
		} else {
			return HibernateLocalAccessData.getSingleResultByRG(entityClass, rg);
		}
	}
	
	public static synchronized <T> Object getSingleResultByCPF(Class<T> entityClass, String cpf) {
		if (Main.temServidor()) {
			if (Main.getServidor().isNotConnected()) {
				return null;
			}
			
			return HibernateServerAccessData.getSingleResultByCPF(entityClass, cpf);
		
		} else {
			return HibernateLocalAccessData.getSingleResultByCPF(entityClass, cpf);
		}
	}
	
	public static synchronized Object[] save(Class<?> classeEntidade, ObjectWithId object) {
		if (Main.temServidor() && !(object instanceof DeviceEntity) && !(object instanceof PreferenceEntity)) {
			if (Main.getServidor().isNotConnected()) {
				return null;
			}

			return HibernateServerAccessData.save(classeEntidade, object);
		
		} else {
			return HibernateLocalAccessData.save(classeEntidade, object);
		}
	}
	
	public static synchronized Object[] update(Class<?> classeEntidade, ObjectWithId object) {
		if (Main.temServidor() && !(object instanceof DeviceEntity) && !(object instanceof PreferenceEntity)) {
			if (Main.getServidor().isNotConnected()) {
				return null;
			}
			
			return HibernateServerAccessData.update(classeEntidade, object);
			
		} else {
			return HibernateLocalAccessData.update(classeEntidade, object);
		}
	}
	
	public static synchronized void remove(Object object) {
		if (Main.temServidor() && !(object instanceof DeviceEntity)
				&& !(object instanceof ConfigurationGroupEntity)) {
			if (Main.getServidor().isNotConnected()) {
				return;
			}
			
			HibernateServerAccessData.remove(object);
			
		} else {
			HibernateLocalAccessData.remove(object);
		}
	}
	
	@SuppressWarnings("unchecked")
	public static synchronized void removeTemplates(Long idAthleteAccess) {
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
	
	public static synchronized UserEntity buscaUsuarioPeloLogin(String loginName, String password) {
		if (Main.temServidor()) {
			if (Main.getServidor().isNotConnected()) {
				return null;
			}
			
			return HibernateServerAccessData.buscaUsuarioPeloLogin(loginName, password);
			
		} else {
			return HibernateLocalAccessData.buscaUsuarioPeloLogin(loginName, password);
		}
	}
	
	public static synchronized <T> Object getSingleResultByRegistration(Class<T> entityClass, Long matricula) {
		if (Main.temServidor()) {
			if (Main.getServidor().isNotConnected()) {
				return null;
			}
			
			return HibernateServerAccessData.getSingleResultByRegistration(entityClass, matricula);
			
		} else {
			return HibernateLocalAccessData.getSingleResultByRegistration(entityClass, matricula);
		}
	}
	
	public static synchronized void apagarPastaDeFotos(Long idUser) {
		if (Main.temServidor()) {
			if (Main.getServidor().isNotConnected()) {
				return;
			}
			
			HibernateServerAccessData.apagarPastaDeFotos(idUser);
			
		} else {
			HibernateLocalAccessData.apagarPastaDeFotos(idUser);
		}
	}
	
	public static synchronized void registraNovasFotosPedestre(Long idUsuario) {
		if (Main.temServidor()) {
			if (Main.getServidor().isNotConnected()) {
				return;
			}
			
			HibernateServerAccessData.registraNovasFotosPedestre(idUsuario);
		
		} else {
			HibernateLocalAccessData.registraNovasFotosPedestre(idUsuario);
		}
	}
	
	public static synchronized void registraExclusaoFotosPedestre(Long idUsuario) {
		if (Main.temServidor()) {
			if (Main.getServidor().isNotConnected()) {
				return;
			}
			
			HibernateServerAccessData.registraExclusaoFotosPedestre(idUsuario);
		
		} else {
			HibernateLocalAccessData.registraExclusaoFotosPedestre(idUsuario);
		}
	}
	
	public static synchronized Boolean isPastaDeFotosExistente(Integer idUser) {
		if (Main.temServidor()) {
			if (Main.getServidor().isNotConnected()) {
				return null;
			}
			
			return HibernateServerAccessData.isPastaDeFotosExistente(idUser);

		} else {
			return HibernateLocalAccessData.isPastaDeFotosExistente(idUser);
		}
	}
	
	public static synchronized void enviaInicioVerificandoAcesso() {
		if (Main.temServidor()) {
			if (Main.getServidor().isNotConnected()) {
				return;
			}
		}
		
		HibernateServerAccessData.enviaInicioVerificandoAcesso();
	}
	
	public static synchronized void enviaFimVerificandoAcesso() {
		if (Main.temServidor()) {
			if (Main.getServidor().isNotConnected()) {
				return;
			}
			
			HibernateServerAccessData.enviaFimVerificandoAcesso();
		}
	}
	
	public static synchronized List<DeviceTO> getListDeviceFromServer() {
		if (!Main.temServidor() || Main.getServidor().isNotConnected()) {
			return null;
		}
		
		return HibernateServerAccessData.getListDeviceFromServer();
	}
	
	public static synchronized void liberarAcessoNoServidor(String indentifier, String sentido) {
		if (!Main.temServidor() || Main.getServidor().isNotConnected()) {
			return;
		}
		
		HibernateServerAccessData.liberarAcessoNoServidor(indentifier, sentido);
	}
	
	public static synchronized <T> Object getSingleResultByIdTemp(Class<PedestrianAccessEntity> entityClass, Long id) {
		if (Main.temServidor()) {
			if (Main.getServidor().isNotConnected()) {
				return null;
			}
			
			return HibernateServerAccessData.getSingleResultByIdTemp(entityClass, id);
		} else {
			return HibernateLocalAccessData.getSingleResultByIdTemp(entityClass, id);
		}
	}
	
	public static void resetStatusAllCards() {
		if (Main.temServidor()) {
			if (Main.getServidor().isNotConnected()) {
				return;
			}
			
			HibernateServerAccessData.resetStatusAllCards();

		} else {
			HibernateLocalAccessData.resetStatusAllCards();
		}
	}
	
	public static UserEntity getLoggedUser(String namedQuery) {
		return HibernateLocalAccessData.getLoggedUser(namedQuery);
	}
	
	public static Object[] updateUser(Class<?> classeEntidade, ObjectWithId object) {
		return HibernateLocalAccessData.updateUser(classeEntidade, object);
	}
	
	public static void shutdown() {
		HibernateLocalAccessData.shutdown();
	}
	
	public static Object[] saveUser(Class<?> classeEntidade, ObjectWithId object) {
		return HibernateLocalAccessData.saveUser(classeEntidade, object);
	}
	
	public static void apagaDadosDeGiro(Date data) {
		HibernateLocalAccessData.apagaDadosDeGiro(data);
	}
	
	public static void apagaDadosCartao() {
		HibernateLocalAccessData.apagaDadosCartao();
	}
	
	public static void apagaDadosDeUltimoSentido() {
		HibernateLocalAccessData.apagaDadosDeUltimoSentido();
	}
	
	public static void apagaQuantidadeAcessosAsinc() {
		HibernateLocalAccessData.apagaQuantidadeAcessosAsinc();
	}
	
	public static synchronized Date buscaUltimoAcessoAtivoPedestre(Long idPedestre) {
		return HibernateLocalAccessData.buscaUltimoAcessoAtivoPedestre(idPedestre);
	}
	
	public static synchronized boolean verificaSePossuiEntradaNoEquipamento(String idDevice, Long idPedestre) {
		return HibernateLocalAccessData.verificaSePossuiEntradaNoEquipamento(idDevice, idPedestre);
	}
	
	public static synchronized List<PedestrianAccessEntity> buscaPedestresAtivosComBiometria() {
		return HibernateLocalAccessData.buscaPedestresAtivosComBiometria();
	}
	
	public static synchronized List<PedestrianAccessEntity> buscaPedestresAtivosComCartao() {
		return HibernateLocalAccessData.buscaPedestresAtivosComCartao();
	}
	
}
