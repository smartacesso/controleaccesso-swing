//******************************************************************************
//A Topdata Sistemas de Automação Ltda não se responsabiliza por qualquer
//tipo de dano que este software possa causar, este exemplo deve ser utilizado
//apenas para demonstrar a comunicação com os equipamentos da linha
//inner e não deve ser alterado, por este motivo ele não deve ser incluso em
//suas aplicações comerciais.
//
//Desenvolvido em Java.
//                                           Topdata Sistemas de Automação Ltda.
//******************************************************************************.
package com.topdata.easyInner.enumeradores;

/**
 * @author juliano.ezequiel
 *
 */
public class Enumeradores {

    //<editor-fold defaultstate="collapsed" desc="Enumeradores dos Estados Teclado">
    public enum EstadosTeclado {

        TECLADO_EM_BRANCO,
        AGUARDANDO_TECLADO
    }
//</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="Enumeradores dos Estados Inner">
    public enum EstadosInner {

        ESTADO_CONECTAR,
        ESTADO_ENVIAR_CFG_OFFLINE,
        ESTADO_COLETAR_BILHETES,
        ESTADO_ENVIAR_CFG_ONLINE,
        ESTADO_ENVIAR_DATA_HORA,
        ESTADO_ENVIAR_MSG_PADRAO,
        ESTADO_CONFIGURAR_ENTRADAS_ONLINE,
        ESTADO_POLLING,
        ESTADO_LIBERAR_CATRACA,
        ESTADO_ENVIAR_BIPCURTO,
        ESTADO_MONITORA_GIRO_CATRACA,
        ESTADO_PING_ONLINE,
        ESTADO_RECONECTAR,
        ESTADO_AGUARDA_TEMPO_MENSAGEM,
        ESTADO_DEFINICAO_TECLADO,
        ESTADO_AGUARDA_DEFINICAO_TECLADO,
        ESTADO_ENVIA_MSG_URNA,
        ESTADO_MONITORA_URNA,
        ESTADO_ACIONAR_RELE1,
        ESTADO_ACIONAR_RELE2,
        ESTADO_ENVIAR_MSG_OFFLINE_CATRACA,
        ESTADO_ENVIAR_CONFIGMUD_ONLINE_OFFLINE,
        ESTADO_ENVIAR_MSG_ACESSO_NEGADO,
        ESTADO_CAPTURA_HAMSTER,
        ESTADO_VERIFICA_USR_HAMSTER,
        ESTADO_ENVIA_USUARIO_ONLINE_INNER_ACESSO,
        ESTADO_ENVIA_USUARIO_ONLINE_INNER_NET,
        ESTADO_PREPARA_ENVIO_TEMPLATES_ONLINE,
        ESTADO_EXCLUI_TEMPLATE_ONLINE_INNERACESSO,
        ESTADO_EXCLUI_TEMPLATES_ONLINE_INNERNET,
        ESTADO_VERIFICA_TIPO_INNER_EXCLUSAO,
        ESTADO_VERIFICA_TIPO_INNER,
        ESTADO_TRATAR_RET_BIO_ACESSO,
        ESTADO_TRATAR_RET_BIO_NET,
        ESTADO_VALIDA_URNA_CHEIA,
        ESTADO_ENVIAR_MSG_URNA_CHEIA,
        ESTADO_VALIDAR_ACESSO,
        ESTADO_RECEBER_FIRWARE,
        ESTADO_RECEBER_MODELO_BIO,
        ESTADO_ENVIAR_LISTA_OFFLINE,
        ESTADO_ENVIAR_LISTA_SEMDIGITAL,
        ESTADO_RECEBER_QTD_BILHETES_OFF,
        ESTADO_ENVIAR_MSG_OFFLINE_COLETOR, 
        ESTADO_RECEBER_VERSAO_BIO,
        ESTADO_DESLIGAR_RELE    
    }
//</editor-fold>
    
    public enum TiposTempl
    {
        HFir,
        TextCode,
        Fir,
        Dados
    }

    //<editor-fold defaultstate="collapsed" desc="Tipos de Lista">
    public static final int SEM_LISTA = 0;
    public static final int LISTA_BRANCA = 1;
    public static final int LISTA_NEGRA = 0;
//</editor-fold>
    
    //<editor-fold defaultstate="collapsed" desc="Constantes Origem bilhete">   
    public static final int ORIGEM_VIA_TECLADO = 1;
    public static final int ORIGEM_VIA_LEITOR1 = 2;
    public static final int ORIGEM_VIA_LEITOR2 = 3;
    public static final int ORIGEM_SENSOR_DA_CATRACA_OBSOLETO = 4;
    public static final int ORIGEM_FIM_TEMPO_ACIONAMENTO = 5;
    public static final int ORIGEM_GIRO_DA_CATRACA_TOPDATA = 6;
    public static final int ORIGEM_URNA = 7;
    public static final int ORIGEM_EVENTO_SENSOR1 = 8;
    public static final int ORIGEM_EVENTO_SENSOR2 = 9;
    public static final int ORIGEM_EVENTO_SENSOR3 = 10;
    public static final int ORIGEM_SENSOR_BIOMETRICO = 12;
    public static final int ORIGEM_TECLA_FUNCAO = 65;
    public static final int ORIGEM_TECLA_ANULA = 42;
    public static final int ORIGEM_TECLA_ENTRADA = 66;
    public static final int ORIGEM_TECLA_SAIDA = 67;
    public static final int ORIGEM_TECLA_CONFIRMA = 35;
    public static final int ORIGEM_PRESENCA_DEDO = 37;
    public static final int ORIGEM_URNA_CHEIA  = 20;
    //</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="Constantes de Retorno">
    public static final int RET_COMANDO_OK = 0;
    public static final int RET_COMANDO_ERRO = 1;
    public static final int RET_PORTA_NAOABERTA = 2;
    public static final int RET_PORTA_JAABERTA = 3;
    public static final int RET_DLL_INNER2K_NAO_ENCONTRADA = 4;
    public static final int RET_DLL_INNERTCP_NAO_ENCONTRADA = 5;
    public static final int RET_DLL_INNERTCP2_NAO_ENCONTRADA = 6;
    public static final int RET_ERRO_GPF = 8;
    public static final int RET_TIPO_CONEXAO_INVALIDA = 9;
//</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="Constantes Habilita">
    public static final int DESABILITA = 0;
    public static final int HABILITA = 1;
//</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="Constantes de Eco de digitos no teclado">
    public static final int ECOA_DIGITADO = 0;
    public static final int ECOA_ASTERISCO = 1;
//</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="Constantes de Tipo de Leitor">
    public static final int CODIGO_DE_BARRAS = 0;
    public static final int MAGNETICO = 1;
    public static final int PROXIMIDADE_ABATRACK2 = 2;
    public static final int WIEGAND = 3;
    public static final int PROXIMIDADE_SMART_CARD = 4;
    public static final int CODIGO_BARRAS_SERIAL = 5;
    public static final int WIEGAND_FC_SEM_SEPARADOR = 6;
    public static final int WIEGAND_FC_COM_SEPARADOR = 7;
    public static final int QRCODE = 8;
    public static final int BARRAS_PROX_QRCODE = 7;
//</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="Constantes de Operação">
    public static final int DESATIVADO = 0;
    public static final int SOMENTE_ENTRADA = 1;
    public static final int SOMENTE_SAIDA = 2;
    public static final int ENTRADA_E_SAIDA = 3;
    public static final int ENTRADA_E_SAIDA_INVERTIDAS = 4;
//</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="Constantes de Opção">
    public static final int Opcao_NAO = 0;
    public static final int Opcao_SIM = 1;
//</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="Constantes de Modo">
    public static final int MODO_OFF_LINE = 0;
    public static final int MODO_ON_LINE = 1;
//</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="Constantes de Tipo de Cartão">
    public static final int PADRAO_TOPDATA = 0;
    public static final int PADRAO_LIVRE = 1;
//</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="Constantes do Método EntradasMudancaOnline">
    public static final int NAO_ACEITA_ENTRADA_DADOS = 0;
    public static final int ACEITA_TECLADO = 1;
    public static final int ACEITA_LEITURA_LEITOR1 = 2;
    public static final int ACEITA_LEITURA_LEITOR2 = 3;
    public static final int TECLADO_E_LEITOR1 = 4;
    public static final int TECLADO_E_LEITOR2 = 5;
    public static final int LEITOR1_E_LEITOR2 = 6;
    public static final int TECLADO_LEITOR1_LEITOR2 = 7;
    public static final int TECLADO_E_VERI_BIOMETRICA = 10;
    public static final int LEITOR1_E_VERI_BIOMETRICA = 11;
    public static final int TECLADO_E_LEITOR1_E_VERI_BIOMETRICA = 12;
    public static final int LEITOR1_E_VERI_BIOMETRICA_LEITOR2_SEM_VERI_BIOMETRICA = 13;
    public static final int LEITOR1_E_VERI_BIOMETRICA_LEITOR2_SEM_VERI_BIOMETRICA_E_TECLADO_SEM_VERI_BIOMETRICA = 14;
    public static final int LEITOR1_E_IDENTIFICACAO_BIOMETRICA = 100;
    public static final int LEITOR1_E_TECLADO_IDENTIFICACAO_BIOMETRICA = 101;
    public static final int LEITOR1_E_LEITOR2_E_IDENTIFICACAO_BIOMETRICA = 102;
    public static final int LEITOR1_E_LEITOR2_E_TECLADO_E_IDENTIFICACAO_BIOMETRICA = 103;
    public static final int LEITOR1_INVERTIDO_IDENTIFICACAO_BIOMETRICA = 104;
    public static final int LEITOR1_INVERTIDO_E_TECLADO_E_IDENTIFICACAO_BIOMETRICA = 105;

    public static final int NAO_ACEITA_DADOS_TECLADO = 0;
    public static final int ACEITA_DADOS_TECLADO = 1;
    public static final int LEITOR1_DESABILITADO = 0;
    public static final int LEITOR1_SOMENTE_ENTRADA = 1;
    public static final int LEITOR1_SOMENTE_SAIDA = 2;
    public static final int LEITOR1_ENTRADA_SAIDA = 3;
    public static final int LEITOR1_SAIDA_ENTRADA = 4;
    public static final int LEITOR2_SOMENTE_ENTRADA = 1;
    public static final int LEITOR2_SOMENTE_SAIDA = 2;
    public static final int LEITOR2_ENTRADA_SAIDA = 3;
    public static final int LEITOR2_SAIDA_ENTRADA = 4;
    public static final int RESERVADO_USO_FUTURO = 0;
//</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="Constantes de Origem do Método Receber Dados Online">
    public static final int VIA_TECLADO = 1;
    public static final int VIA_LEITOR1 = 2;
    public static final int VIA_LEITOR2 = 3;
    public static final int SENSOR_DA_CATRACA_OBSOLETO_ = 4;
    public static final int FIM_TEMPO_ACIONAMENTO = 5;
    public static final int GIRO_DA_CATRACA_TOPDATA = 6;
    public static final int URNA = 7;
    public static final int EVENTO_SENSOR1 = 8;
    public static final int EVENTO_SENSOR2 = 9;
    public static final int EVENTO_SENSOR3 = 10;
    public static final int SENSOR_BIOMETRICO = 12;
    public static final int TECLA_FUNCAO = 65;
    public static final int TECLA_ANULA = 42;
    public static final int TECLA_ENTRADA = 66;
    public static final int TECLA_SAIDA = 67;
    public static final int TECLA_CONFIRMA = 35;
    public static final int PRESENCA_DEDO = 37;
    public static final int RET_REQUISICAO_BIO = 13;
    public static final int URNA_CHEIA = 20;
    
//</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="Constantes de Tipo de aciomanento">
    public static final int Acionamento_Coletor = 0;
    public static final int Acionamento_Catraca_Entrada_E_Saida = 1;
    public static final int Acionamento_Catraca_Entrada = 2;
    public static final int Acionamento_Catraca_Saida = 3;
    public static final int Acionamento_Catraca_Saida_Liberada = 4;
    public static final int Acionamento_Catraca_Entrada_Liberada = 5;
    public static final int Acionamento_Catraca_Liberada_2_Sentidos = 6;
    public static final int Acionamento_Catraca_Sentido_Giro = 7;
    public static final int Acionamento_Catraca_Urna = 8;
//</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="Constantes de Função de aciomanento">
    public static final int NAO_UTILIZADO = 0;
    public static final int ACIONA_REGISTRO_ENTRADA_OU_SAIDA = 1;
    public static final int ACIONA_REGISTRO_ENTRADA = 2;
    public static final int ACIONA_REGISTRO_SAIDA = 3;
    public static final int CONECTADO_SIRENE = 4;
    public static final int REVISTA_USUARIOS = 5;
    public static final int CATRACA_SAIDA_LIBERADA = 6;
    public static final int CATRACA_ENTRADA_LIBERADA = 7;
    public static final int CATRACA_LIBERADA_DOIS_SENTIDOS = 8;
    public static final int CATRACA_LIBERADA_DOIS_SENTIDOS_MARCACAO_REGISTRO = 9;
//</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="Constantes retorno bilhetes">
    public static final int RET_BILHETE_OK = 1;
    public static final int RET_BILHETE_FALHA_NA_COMUNICACAO = 2;
    public static final int RET_BILHETE_BIO_REQUISICAO_DESCONEHCIDA = 251;
    public static final int RET_BILHETE_BIO_FALHA_COMUNICACAO = 253;
    public static final int RET_BILHETE_INNER_BIO_NAO_ESTA_EM_MODO_MASTER = 3;
    public static final int RET_BILHETE_BIO_USR_JA_CADASTRADO = 4;
    public static final int RET_BILHETE_BIO_USR_NAO_CADASTRADO = 5;
    public static final int RET_BILHETE_BIO_BASE_CHEIA = 6;
    public static final int RET_BILHETE_BIO_DIG_NAO_CONFERE = 14;
    public static final int RET_BILHETE_BIO_INVALIDA = 135;
    public static final int RET_BILHETE_BIO_TEMPLATE_INVALIDO = 22;
    public static final int RET_BILHETE_BIO_PARAMETRO_INVALIDO = 9;
    public static final int RET_BILHETE_BIO_TIMEOUT_COMUNICACAO = 7;
//</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="Constantes retorno Bio">
    public static final int RET_BIO_OK = 0;
    public static final int RET_FALHA_NA_COMUNICACAO = 1;
    public static final int RET_BIO_PROCESSANDO = 128;
    public static final int RET_BIO_FALHA_CAPTURA = 129;
    public static final int RET_BIO_FALHA_COMUNICACAO = 129;
    public static final int RET_INNER_BIO_NAO_ESTA_EM_MODO_MASTER = 130;
    public static final int RET_BIO_USR_JA_CADASTRADO = 131;
    public static final int RET_BIO_USR_NAO_CADASTRADO = 132;
    public static final int RET_BIO_BASE_CHEIA = 133;
    public static final int RET_BIO_DIG_NAO_CONFERE = 134;
    public static final int RET_BIO_INVALIDA = 135;
    public static final int RET_BIO_TEMPLATE_INVALIDO = 136;
    public static final int RET_BIO_PARAMETRO_INVALIDO = 137;
    public static final int RET_BIO_MODULO_INCORRETO = 250;
//</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="Constantes Funcao Default Leitores Proximidade e Bio">
    public static final int FUNCAO_DEFAULT_DESABILITADA = 0;
    public static final int REGISTRAR_FUNCAO_TECLADO_1 = 1;
    public static final int REGISTRAR_FUNCAO_TECLADO_2 = 2;
    public static final int REGISTRAR_FUNCAO_TECLADO_3 = 3;
    public static final int REGISTRAR_FUNCAO_TECLADO_4 = 4;
    public static final int REGISTRAR_FUNCAO_TECLADO_5 = 5;
    public static final int REGISTRAR_FUNCAO_TECLADO_6 = 6;
    public static final int REGISTRAR_FUNCAO_TECLADO_7 = 7;
    public static final int REGISTRAR_FUNCAO_TECLADO_8 = 8;
    public static final int REGISTRAR_FUNCAO_TECLADO_9 = 9;
    public static final int REGISTRAR_SEMPRE_ENTRADA = 10;
    public static final int REGISTRAR_SEMPRE_SAIDA = 11;
    public static final int REGISTRAR_CONFORME_GIRO = 12;
//</editor-fold>

    public static final int Maximo_Tentativas = 5;

    /**
     *
     */
    public static final int Limpar = -1;

}
