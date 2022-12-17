package com.protreino.services.constants;

import com.protreino.services.utils.Utils;

public abstract class Configurations {
	
	/**
	 * AO ALTERAR A VERSAO AQUI, CRIAR A VERSAO NOVA NO BANCO DE DADOS
	 */

	public static final String VERSION = "3.42";
	
	public static final String URL_APPLICATION = "http://www.smartacesso.com.br/sistema/";
	
	public static final int TIME_CHECK_NEW_REQUEST = 6000;
	
	public static final int TIME_CHECK_STATUS_CURRENT_REQUEST = 5000;
	
	public static final int TIME_LOG_ATHLETE_ACCESS = 1000 * 60 * 10;

	public static final int TIME_LOG_USER_ACCESS_LIST = 1000 * 60 * 10;
	
	public static final int TIME_DATA_SENT = 2000;
	
	public static final int TEMPO_CARENCIA = -5; // SEMPRE NEGATIVO em minutos
	
	public static final String DRIVER_DOWNLOAD_URL = "http://www.smartacesso.com.br/downloads/drivers";
	
	public static final String WIP_SITE = "http://checkip.amazonaws.com";
	
	public static final String IMAGE_FOLDER = "/com/protreino/services/resources/img/";
	
	public static final String LC_DATABASE_PATH = Utils.getAppDataFolder();
	
	public static final String LUXAND_KEY = "dte7wp+ICiX8FNYiviQNDLgkhuOYb7zd/mu/iBUFbw+BSqz8r7vea0wKQOHrxYe6pxoBOQDxGBNCBahyKvtFq+t3i/32RsOOfHsl/t/PQn4++zz8w4b1fzCd4XQ0kKOqALFSCpNfjb/NPiPCCoSlC1AR9ymK/dksdi5SD06EyH8=";



}
