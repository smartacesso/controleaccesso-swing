//******************************************************************************
//A Topdata Sistemas de AutomaÃ§Ã£o Ltda nÃ£o se responsabiliza por qualquer
//tipo de dano que este software possa causar, este exemplo deve ser utilizado
//apenas para demonstrar a comunicaÃ§Ã£o com os equipamentos da linha
//inner e nÃ£o deve ser alterado, por este motivo ele nÃ£o deve ser incluso em
//suas aplicaÃ§Ãµes comerciais.
//
//Desenvolvido em Java.
//                                           Topdata Sistemas de AutomaÃ§Ã£o Ltda.
//******************************************************************************
package com.topdata;

/**
 * ComunicaÃ§Ã£o com a DLL "EasyInner.dll" Todos os mÃ©todos estÃ£o descritos no
 * manual do desenvolvedor que acompanha a instalaÃ§Ã£o da SDK
 */
public class EasyInner {

    static {
        System.loadLibrary("EasyInner");
    }

    //<editor-fold defaultstate="collapsed" desc="Constantes de Retorno EasyInner">
    public final int RET_COMANDO_OK = 0;
    public final int RET_COMANDO_ERRO = 1;
    public final int RET_PORTA_NAOABERTA = 2;
    public final int RET_PORTA_JAABERTA = 3;
    public final int RET_DLL_INNER2K_NAO_ENCONTRADA = 4;
    public final int RET_DLL_INNERTCP_NAO_ENCONTRADA = 5;
    public final int RET_DLL_INNERTCP2_NAO_ENCONTRADA = 6;
    public final int RET_ERRO_GPF = 8;
    public final int RET_TIPO_CONEXAO_INVALIDA = 9;
//</editor-fold>

    /**
     *
     * @param Inner
     * @return
     */
    public static native int SolicitarListaUsuariosComDigital(int Inner);

    /**
     *
     * @param Inner
     * @param Usuario
     * @return
     */
    public static native int SolicitarDigitalUsuario(int Inner, String Usuario);

    /**
     *
     * @param Inner
     * @param Usuario
     * @return
     */
    public static native int ReceberDigitalUsuario(int Inner, String Usuario);

    /**
     *
     * @param Inner
     * @param OnLine
     * @param Template
     * @return
     */
    public static native int ReceberDigitalUsuario(int Inner, byte[] Template, int TamResposta);
    
    /*
    ReceberRespostaRequisicaoBio
    */
    public static native int ReceberRespostaRequisicaoBio(int Inner, int[] TamResposta);

    /**
     *
     * @param Inner
     * @return
     */
    public static native int ConfigurarComportamentoIndexSearch(int Inner);

    /**
     * Habilita para utilizar usuarios com 16 digitos para biometria
     *
     * @param habilitado
     * @return
     */
    public static native int ConfigurarBioVariavel(int habilitado);

    /**
     *
     * @param Usuario
     * @return
     */
    public static native int ReceberUsuarioComDigital(byte[] Usuario);

    public static native int CMDTESTE(int i);

    /**
     * Ler giros realizados na catraca
     *
     * @param Inner
     * @param entradas
     * @param saidas
     * @param desistencias
     * @return
     */
    public static native int LerContadorGiro(int Inner, byte[] entradas, byte[] saidas, byte[] desistencias);

    /**
     * Atribui valor ao contador de giros da catraca
     *
     * @param Inner
     * @param entradas
     * @param saidas
     * @param desistencias
     * @return
     */
    public static native int AtribuirContadorGiro(int Inner, byte[] entradas, byte[] saidas, byte[] desistencias);

    /**
     *
     * @param Inner
     * @param Setor
     * @param cartao
     * @param bloco_0
     * @param bloco_1
     * @param bloco_2
     * @return
     */
    public static native int LerSmartCard(int Inner, int Setor, byte[] cartao, byte[] bloco_0, byte[] bloco_1, byte[] bloco_2);

    /**
     * Comando Novo Envia com template para o Inner Bio cadastrar no seu banco
     * de dados. O resultado do cadastro deve ser verificado no retorno da
     * funÃ§Ã£o UsuarioFoiEnviado. Inner: 1 a 32 â€“ Para comunicaÃ§Ã£o serial. 1 a 99
     * â€“ Para comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel. 1 a ... â€“ Para comunicaÃ§Ã£o
     * TCP/IP porta fixa. Template: O cadastro do usuÃ¡rio jÃ¡ contendo as duas
     * digitais e o nÃºmero do usuÃ¡rio. Ã‰ um array de bytes com o tamanho de 844
     * bytes.
     *
     * @param Inner
     * @param usuario
     * @param Template1
     * @param Template2
     * @return
     */
    public static native int EnviarDigitalUsuario(int Inner, String usuario, byte[] Template1, byte[] Template2);

    /**
     * Solicita uma digital diretaqmente do leitor do Inner
     *
     * @param Inner
     * @return
     */
    public static native int SolicitarTemplateLeitor(int Inner);

    /**
     * recebe a digital previamente solicitada
     *
     * @param Inner
     * @param OnLine
     * @param Template
     * @return
     */
    public static native int ReceberTemplateLeitor(int Inner, int OnLine, byte[] Template);

    /**
     * Configura qual serÃ¡ o nÃºmero do cartÃ£o master que o Inner irÃ¡ aceitar.
     * VÃ¡lido somente para o padrÃ£o livre de cartÃ£o. Para o padrÃ£o Topdata o
     * nÃºmero do master sempre Ã© 0.
     *
     * Master 0 a 99999999999999 (MÃ¡ximo de 14 dÃ­gitos) 0(Default).
     *
     * @param Master
     * @return
     */
    public static native int DefinirNumeroCartaoMaster(String Master);

    /**
     *
     * @param Sensor
     * @param Evento
     * @param Tempo
     * @return
     */
    public static native int DefinirEventoSensor(int Sensor, int Evento, int Tempo);

    /**
     * Recebe o status atual dos sensores do Inner. Essa funÃ§Ã£o deverÃ¡ ser
     * utilizada somente em casos muito especÃ­ficos, por exemplo, quando vocÃª
     * possui um Inner Plus/NET conectado a um sensor de presenÃ§a e deseja saber
     * se existe alguma pessoa naquele local Retorna o status do respectivo
     * sensor: 0 â€“ em nÃ­vel baixo (fechado) 1 â€“ em nÃ­vel alto (aberto)
     *
     * @param Inner
     * @param Sensores
     * @return
     */
    public static native int LerSensoresInner(int Inner, int[] Sensores);

    /**
     * Para usar cartoes com diferentes quantidade de digitos
     *
     * @param Digito
     * @return
     */
    public static native int InserirQuantidadeDigitoVariavel(int Digito);

    /**
     * Este comando funciona somente com o Inner no modo Off-Line. Recebe a
     * quantidade de bilhetes que hÃ¡ no Inner para coletar.
     *
     * @param Inner
     * @param QtdeBilhetes
     * @return
     */
    public static native int ReceberQuantidadeBilhetes(int Inner, int[] QtdeBilhetes);

    /**
     * Este comando recebe do Inner o relogio com os parametros abaixo. TabÃ©m
     * utiliza-se para verificar se o Inner esta conectado
     *
     *
     * @param Inner
     * @param dataHora
     * @return
     */
    public static native int ReceberRelogio(int Inner, int[] dataHora);

    /**
     * aciona rele 1 para coletor
     *
     * @param Inner
     * @return
     */
    public static native int AcionarRele1(int Inner);

    /**
     * aciona rele 2 para coletor
     *
     * @param Inner
     * @return
     */
    public static native int AcionarRele2(int Inner);

    /**
     * Define qual serÃ¡ o tipo de conexÃ£o(meio de comunicaÃ§Ã£o) que serÃ¡
     * utilizada pela dll para comunicar com os Inners. Essa funÃ§Ã£o deverÃ¡ ser
     * chamada antes de iniciar o processo de comunicaÃ§Ã£o e antes da funÃ§Ã£o
     * AbrirPortaComunicacao. 0 - ComunicaÃ§Ã£o serial, RS-232/485. 1 -
     * ComunicaÃ§Ã£o TCP/IP com porta variÃ¡vel. 2 - ComunicaÃ§Ã£o TCP/IP com porta
     * fixa (Default). 3 - ComunicaÃ§Ã£o via modem. 4 â€“ ComunicaÃ§Ã£o via
     * TopPendrive.
     *
     * @param Tipo
     * @return
     */
    public static native int DefinirTipoConexao(int Tipo);

    /**
     * Abre a porta de comunicaÃ§Ã£o. essa funÃ§Ã£o deverÃ¡ ser chamada antes de
     * iniciar qualquer processo de transmissÃ£o ou recepÃ§Ã£o de dados com o
     * Inner. Esta funÃ§Ã£o deve ser chamada apenas uma vez e no inÃ­cio da
     * comunicaÃ§Ã£o, e nÃ£o deve ser chamada para cada Inner. NÃºmero da porta
     * serial ou TCP/IP: - Para comunicaÃ§Ã£o TCP/IP o valor padrÃ£o da porta Ã©
     * 3570 (Default). - Para comunicaÃ§Ã£o Serial/Modem o valor padrÃ£o da porta Ã©
     * 1, COM 1(Default). - Para a comunicaÃ§Ã£o TopPendrive o valor Ã© 3(Default).
     *
     * @param Porta
     * @return int
     */
    public static native int AbrirPortaComunicacao(int Porta);

    /**
     * Fecha a porta de comunicaÃ§Ã£o previamente aberta, seja ela serial, Modem
     * ou TCP/IP. Em modo Off-Line normalmente Ã© chamada apÃ³s enviar/receber
     * todos os dados do Inner. Em modo On-Line Ã© chamada somente no ecerramento
     * do software do software.
     */
    public static native void FecharPortaComunicacao();

    /**
     * Define qual padrÃ£o de cartÃ£o serÃ¡ utilizado pelos Inners, padrÃ£o Topdata
     * ou padrÃ£o livre. O padrÃ£o Topdata de cartÃ£o estÃ¡ descrito no manual dos
     * equipamentos e Ã© utilizado somente com os Inners em modo Off-Line. No
     * padrÃ£o livre todos os dÃ­gitos do cartÃ£o sÃ£o considerados como matrÃ­cula,
     * ele pode ser utilizado no modo On Line ou no modo Off Line. Ao chamar
     * essa funÃ§Ã£o, a quantidade de dÃ­gitos Ã© setada para 14. 0 - PadrÃ£o
     * Topdata. 1 - PadrÃ£o livre (Default).
     *
     * @param Padrao
     * @return int
     */
    public static native int DefinirPadraoCartao(int Padrao);

    /**
     * Faz com que o Inner emita um bip curto(aviso sonoro).
     *
     * @param Inner
     * @return
     */
    public static native int AcionarBipCurto(int Inner);

    /**
     * Faz com que o Inner emita um bip long(aviso sonoro).
     *
     * @param Inner
     * @return
     */
    public static native int AcionarBipLongo(int Inner);

    /**
     * Testa a comunicaÃ§Ã£o com o Inner, tambÃ©m utilizado para efetuar a conexÃ£o
     * com o Inner. Para efetuar a conexÃ£o com o Inner, essa funÃ§Ã£o deve ser
     * executada em um loop atÃ© retornar 0(zero), executado com sucesso.
     *
     * @param Inner
     * @return
     */
    public static native int Ping(int Inner);

    /**
     * Liga a luz emitida pelo display do Inner. Essa funÃ§Ã£o nÃ£o deve ser
     * utilizada nas catracas.
     *
     * @param Inner
     * @return
     */
    public static native int LigarBackLite(int Inner);

    /**
     * Desliga a luz emitida pelo display do Inner. Essa funÃ§Ã£o nÃ£o deve ser
     * utilizada nas catracas.
     *
     * @param Inner
     * @return
     */
    public static native int DesligarBackLite(int Inner);

    /**
     * Permite que os dados sejam inseridos no Inner atravÃ©s do teclado do
     * equipamento. Habilitando o parÃ¢metro ecoar, o teclado irÃ¡ ecoar
     * asteriscos no display do Inner. Habilita: 0 - Desabilita o teclado
     * (Default). 1 - Habilita o teclado. Ecoar: 0 â€“ Ecoa o que Ã© digitado no
     * display do Inner (Default). 1 â€“ Ecoa asteriscos no display do Inner.
     *
     * @param Habilita
     * @param Ecoar
     * @return
     */
    public static native int HabilitarTeclado(int Habilita, int Ecoar);

    /**
     * Configura como irÃ¡ funcionar o acionamento(rele) 1 do Inner, e por quanto
     * tempo ele serÃ¡ acionado. FunÃ§Ã£o: 0 â€“ NÃ£o utilizado(Default). 1 â€“ Aciona
     * ao registrar uma entrada ou saÃ­da. 2 â€“ Aciona ao registrar uma entrada. 3
     * â€“ Aciona ao registrar uma saÃ­da. 4 â€“ EstÃ¡ conectado a uma sirene(Ver as
     * funÃ§Ãµes de sirene). 5 â€“ Utilizado para a revista de usuÃ¡rios(Ver a funÃ§Ã£o
     * DefinirPorcentagemRevista). 6 â€“ Catraca com a saÃ­da liberada. 7 â€“ Catraca
     * com a entrada liberada 8 â€“ Catraca liberada nos dois sentidos. 9 â€“
     * Catraca liberada nos dois sentidos e a marcaÃ§Ã£o(registro) Ã© gerada de
     * acordo com o sentido do giro.</param>
     * Tempo - 0 a 50 segundos. 0(Default).
     *
     * @param Funcao
     * @param Tempo
     * @return
     */
    public static native int ConfigurarAcionamento1(int Funcao, int Tempo);

    /**
     * Configura como irÃ¡ funcionar o acionamento(rele) 2 do Inner, e por quanto
     * tempo ele serÃ¡ acionado. FunÃ§Ã£o: 0 â€“ NÃ£o utilizado(Default). 1 â€“ Aciona
     * ao registrar uma entrada ou saÃ­da. 2 â€“ Aciona ao registrar uma entrada. 3
     * â€“ Aciona ao registrar uma saÃ­da. 4 â€“ EstÃ¡ conectado a uma sirene(Ver as
     * funÃ§Ãµes de sirene). 5 â€“ Utilizado para a revista de usuÃ¡rios(Ver a funÃ§Ã£o
     * DefinirPorcentagemRevista). 6 â€“ Catraca com a saÃ­da liberada. 7 â€“ Catraca
     * com a entrada liberada 8 â€“ Catraca liberada nos dois sentidos. 9 â€“
     * Catraca liberada nos dois sentidos e a marcaÃ§Ã£o(registro) Ã© gerada de
     * acordo com o sentido do giro.</param>
     * Tempo - 0 a 50 segundos. 0(Default).
     *
     * @param Funcao
     * @param Tempo
     * @return
     */
    public static native int ConfigurarAcionamento2(int Funcao, int Tempo);

    /**
     * Configura o tipo do leitor que o Inner estÃ¡ utilizando, se Ã© um leitor de
     * cÃ³digo de barras, magnÃ©tico ou proximidade. Tipo: 0 â€“ Leitor de cÃ³digo de
     * barras(Default). 1 â€“ Leitor MagnÃ©tico. 2 â€“ Leitor proximidade AbaTrack2.
     * 3 â€“ Leitor proximidade Wiegand e Wiegand Facility Code. 4 â€“ Leitor
     * proximidade Smart Card.
     *
     * @param Tipo
     * @return
     */
    public static native int ConfigurarTipoLeitor(int Tipo);

    /**
     * Configura as operaÃ§Ãµes que o leitor irÃ¡ executar. Se irÃ¡ registrar os
     * dados somente como entrada independente do sentido em que o cartÃ£o for
     * passado, somente como saÃ­da ou como entrada e saÃ­da. OperaÃ§Ã£o: 0 â€“ Leitor
     * desativado(Default). 1 â€“ Somente para entrada. 2 â€“ Somente para saÃ­da. 3
     * â€“ Entrada e saÃ­da. 4 â€“ Entrada e saÃ­da invertidas
     *
     * @param Operacao
     * @return
     */
    public static native int ConfigurarLeitor1(int Operacao);

    /**
     * Configura as operaÃ§Ãµes que o leitor irÃ¡ executar. Se irÃ¡ registrar os
     * dados somente como entrada independente do sentido em que o cartÃ£o for
     * passado, somente como saÃ­da ou como entrada e saÃ­da. OperaÃ§Ã£o: 0 â€“ Leitor
     * desativado(Default). 1 â€“ Somente para entrada. 2 â€“ Somente para saÃ­da. 3
     * â€“ Entrada e saÃ­da. 4 â€“ Entrada e saÃ­da invertidas
     *
     * @param Operacao
     * @return
     */
    public static native int ConfigurarLeitor2(int Operacao);

    /**
     * Define a quantidade de dÃ­gitos dos cartÃµes a serem lidos pelo Inner.
     * Quantidade - 1 a 16 dÃ­gitos. 14(Default).
     *
     * @param Quantidade
     * @return
     */
    public static native int DefinirQuantidadeDigitosCartao(int Quantidade);

    /**
     * Configura o Inner para registrar as tentativa de acesso negado. O Inner
     * irÃ¡ rgistrar apenas os acessos negados em relaÃ§Ã£o a lista de acesso
     * configurada para o modo Off-Line, ver as funÃ§Ãµes DefinirTipoListaAcesso e
     * ColetarBilhete. TipoRegistro: 0 â€“ NÃ£o registrar o acesso negado. 1 â€“
     * Apenas o acesso negado. 2 â€“ Falha na validaÃ§Ã£o da digital. 3 â€“ Acesso
     * negado e falha na validaÃ§Ã£o da digital.
     *
     * @param TipoRegistro
     * @return
     */
    public static native int RegistrarAcessoNegado(int TipoRegistro);

    /**
     * Define qual serÃ¡ o tipo do registro realizado pelo Inner ao aproximar um
     * cartÃ£o do tipo proximidade no leitor do Inner, sem que o usuÃ¡rio tenha
     * pressionado a tecla entrada, saÃ­da ou funÃ§Ã£o. Funcao: 0 â€“
     * Desablitado(Default). 1 a 9 â€“ Registrar como uma funÃ§Ã£o do teclado do
     * Inner. 10 â€“ Registrar sempre como entrada. 11 â€“ Registrar sempre como
     * saÃ­da. 12 â€“ Libera a catraca nos dois sentidos e registra o bilhete
     * conforme o sentido giro.
     *
     * @param Funcao
     * @return
     */
    public static native int DefinirFuncaoDefaultLeitoresProximidade(int Funcao);

    /**
     * Habilita os leitores wiegand para o primeiro leitor e o segundo leitor do
     * Inner, e configura se o segundo leitor irÃ¡ exibir as mensagens
     * configuradas. Habilita: 0 â€“ NÃ£o habilita o segundo leitor como
     * wiegand(Default). 1 â€“ Habilita o segundo leitor como wiegand.
     * ExibirMensagem: 0 â€“ NÃ£o exibe mensagem segundo leitor(Default). 1 â€“ Exibe
     * mensagem segundo leitor.
     *
     * @param Habilita
     * @param ExibirMensagem
     * @return
     */
    public static native int ConfigurarWiegandDoisLeitores(int Habilita, int ExibirMensagem);

    /**
     * Configura o tipo de registro que serÃ¡ associado a uma marcaÃ§Ã£o, quando
     * for inserido o dedo no Inner bio sem que o usuÃ¡rio tenha definido se Ã© um
     * entrada, saÃ­da, funÃ§Ã£o, etc. Funcao: 0 â€“ desabilitada(Default). 1 a 9 â€“
     * funÃ§Ãµes de 1 a 9. 10 â€“ entrada. 11 â€“ saÃ­da. 12 â€“ libera catraca para os
     * dois lados e registra bilhete conforme o giro.
     *
     * @param Funcao
     * @return
     */
    public static native int DefinirFuncaoDefaultSensorBiometria(int Funcao);

    /**
     * Envia o buffer interno da dll que contÃ©m todas as configuraÃ§Ãµes das
     * funÃ§Ãµes anteriores para o Inner, apÃ³s o envio esse buffer Ã© limpo sendo
     * necessÃ¡rio chamar novamentes as funÃ§Ãµes acima para reconfigurÃ¡-lo. Inner:
     * 1 a 32 â€“ Para comunicaÃ§Ã£o serial. 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP com
     * porta variÃ¡vel. 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP porta fixa.
     *
     * @param Inner
     * @return
     */
    public static native int EnviarConfiguracoes(int Inner);

    /**
     * Configura o relÃ³gio(data/hora) do Inner. Inner: 1 a 32 â€“ Para comunicaÃ§Ã£o
     * serial. 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel. 1 a ... â€“
     * Para comunicaÃ§Ã£o TCP/IP porta fixa. Dia - 1 a 31 Mes - 1 a 12. Ano - 0 a
     * 99 Hora - 0 a 23 Minuto - 0 a 59 Segundo - 0 a 59
     *
     * @param Inner
     * @param Dia
     * @param Mes
     * @param Ano
     * @param Hora
     * @param Minuto
     * @param Segundo
     * @return
     */
    public static native int EnviarRelogio(int Inner, int Dia, int Mes, int Ano, int Hora, int Minuto, int Segundo);

    /**
     * Solicita a versÃ£o do firmware do Inner e dados como o Idioma, se Ã© uma
     * versÃ£o especial. Inner: NÃºmero do Inner desejado. Linha: 01 â€“ Inner Plus.
     * 02 â€“ Inner Disk. 03 â€“ Inner Verid. 06 â€“ Inner Bio. 07 â€“ Inner NET.
     * Variacao - Depende da versÃ£o, existe somente em versÃµes customizadas.
     * VersaoAlta - 00 a 99 VersaoBaixa - 00 a 99 VersaoSufixo - Indica o idioma
     * do firmware: 01 â€“ PortuguÃªs. 02 â€“ Espanhol. 03 â€“ InglÃªs. 04 â€“ FrancÃªs.
     *
     * @param Inner
     * @param Versao
     * @return
     */
    public static native int ReceberVersaoFirmware(int Inner, int[] Versao);

    /**
     * Apaga o buffer com a lista de horÃ¡rios de acesso e envia automaticamente
     * para o Inner. Inner: 1 a 32 â€“ Para comunicaÃ§Ã£o serial. 1 a 99 â€“ Para
     * comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel. 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP
     * porta fixa.
     *
     * @param Inner
     * @return
     */
    public static native int ApagarHorariosAcesso(int Inner);

    /**
     * Insere no buffer da dll um horÃ¡rio de acesso. O Inner possui uma tabela
     * de 100 horÃ¡rios de acesso, para cada horÃ¡rio Ã© possÃ­vel definir 4 faixas
     * de acesso para cada dia da semana. Horario - 1 a 100 â€“ NÃºmero da tabela
     * de horÃ¡rios. DiaSemana - 1 a 7 â€“ Dia da semana a qual pertence a faixa de
     * horÃ¡rio. FaixaDia - 1 a 4 â€“ Para cada dia da semana existem 4 faixas de
     * horÃ¡rio. Hora - 0 a 23 Minuto - 0 a 59
     *
     * @param Horario
     * @param DiaSemana
     * @param FaixaDia
     * @param Hora
     * @param Minuto
     * @return
     */
    public static native int InserirHorarioAcesso(int Horario, int DiaSemana, int FaixaDia, int Hora, int Minuto);

    /**
     * Envia para o Inner o buffer com a lista de horÃ¡rios de acesso, apÃ³s
     * executar o comando o buffer Ã© limpo pela dll automaticamente. Inner: 1 a
     * 32 â€“ Para comunicaÃ§Ã£o serial. 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP com porta
     * variÃ¡vel. 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP porta fixa.
     *
     * @param Inner
     * @return
     */
    public static native int EnviarHorariosAcesso(int Inner);

    /**
     * Limpar o buffer com a lista de usuÃ¡rios cadastrados e envia
     * automaticamente para o Inner. Inner: 1 a 32 â€“ Para comunicaÃ§Ã£o serial. 1
     * a 99 â€“ Para comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel. 1 a ... â€“ Para
     * comunicaÃ§Ã£o TCP/IP porta fixa.
     *
     *
     * @param Inner
     * @return
     */
    public static native int ApagarListaAcesso(int Inner);

    /**
     * Insere no buffer da dll um usuÃ¡rio da lista e a qual horÃ¡rio de acesso
     * ele estÃ¡ associado. Os horÃ¡rios jÃ¡ deverÃ£o ter sido cadastrados atravÃ©s
     * das funÃ§Ãµes InserirHorarioAcesso e enviados atravÃ©s da funÃ§Ã£o
     * EnviarHorariosAcesso para a lista ter o efeito correto. Cartao - 1 a ...
     * â€“ Dependo do padrÃ£o de cartÃ£o definido e da quantidade de dÃ­gitos
     * definida. Horario - 1 a 100 â€“ NÃºmero do horÃ¡rio jÃ¡ cadastrado no Inner.
     * 101 â€“ Acesso sempre liberado para o usuÃ¡rio. 102 â€“ Acesso sempre negado
     * para o usuÃ¡rio.
     *
     * @param Cartao
     * @param Horario
     * @return
     */
    public static native int InserirUsuarioListaAcesso(String Cartao, int Horario);

    /**
     * Envia o buffer com os usuÃ¡rios da lista de acesso para o Inner, apÃ³s
     * executar o comando o buffer Ã© limpo pela dll automaticamente. Inner: 1 a
     * 32 â€“ Para comunicaÃ§Ã£o serial. 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP com porta
     * variÃ¡vel. 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP porta fixa.
     *
     * @param Inner
     * @return
     */
    public static native int EnviarListaAcesso(int Inner);

    /**
     * Define qual tipo de lista(controle) de acesso o Inner vai utilizar. ApÃ³s
     * habilitar a lista de acesso Ã© necessÃ¡rio preencher a lista e os horÃ¡rios
     * de acesso, verificar os as funÃ§Ãµes de â€œHorÃ¡rios de Acessoâ€� e as funÃ§Ãµes
     * da â€œLista de Acessoâ€�. Tipo: 0 â€“ NÃ£o utilizar a lista de acesso. 1 â€“
     * Utilizar lista branca(cartÃµes fora da lista tem o acesso negado). 2 â€“
     * Utilizar lista negra(bloqueia apenas os cartÃµes da lista).
     *
     * @param Tipo
     * @return
     */
    public static native int DefinirTipoListaAcesso(int Tipo);

    /**
     * Testa comunicaÃ§Ã£o com o Inner e mantÃ©m o Inner em OnLine quando a mudanÃ§a
     * automÃ¡tica estÃ¡ configurada. Especialmente indicada para a verificaÃ§Ã£o da
     * conexÃ£o em comunicaÃ§Ã£o TCP/IP.
     *
     * @param Inner
     * @return
     */
    public static native int PingOnLine(int Inner);

    /**
     * Insere um horÃ¡rio de toque de sirene e configura em quais dias da semana
     * esses horÃ¡rio irÃ£o tocar. Ã‰ possÃ­vel inserir no mÃ¡ximo 100 horÃ¡rios para
     * a sirene. Hora - 0 a 23 Minuto - 0 a 59 Segunda: 0 â€“ Desabilita o toque
     * nesse dia. 1 â€“ Habilita o toque nesse dia. Terca: 0 â€“ Desabilita o toque
     * nesse dia. 1 â€“ Habilita o toque nesse dia. Quarta: 0 â€“ Desabilita o toque
     * nesse dia. 1 â€“ Habilita o toque nesse dia. Quinta: 0 â€“ Desabilita o toque
     * nesse dia. 1 â€“ Habilita o toque nesse dia. Sexta: 0 â€“ Desabilita o toque
     * nesse dia. 1 â€“ Habilita o toque nesse dia. Sabado: 0 â€“ Desabilita o toque
     * nesse dia. 1 â€“ Habilita o toque nesse dia. DomingoFeriado: 0 â€“ Desabilita
     * o toque nesse dia. 1 â€“ Habilita o toque nesse dia.
     *
     * @param Hora
     * @param Minuto
     * @param Segunda
     * @param Terca
     * @param Quarta
     * @param Quinta
     * @param Sexta
     * @param Sabado
     * @param DomingoFeriado
     * @return
     */
    public static native int InserirHorarioSirene(int Hora, int Minuto, int Segunda, int Terca, int Quarta, int Quinta, int Sexta, int Sabado, int DomingoFeriado);

    /**
     * Envia o buffer com os horÃ¡rio de sirene cadastrados para o Inner. ApÃ³s
     * executar a funÃ§Ã£o o buffer Ã© limpo automaticamente pela dll. Inner: 1 a
     * 32 â€“ Para comunicaÃ§Ã£o serial. 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP com porta
     * variÃ¡vel. 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP porta fixa.
     *
     * @param Inner
     * @return
     */
    public static native int EnviarHorariosSirene(int Inner);

    /**
     * Libera a catraca para o usuÃ¡rio pode efetuar o giro na catraca em ambos
     * os sentidos. Em modo On-Line, na funÃ§Ã£o ReceberDadosOnLine o valor
     * retornado no parÃ¢metro â€œComplementoâ€� serÃ¡ do tipo entrada ou saÃ­da,
     * dependendo do sentido em que o usuÃ¡rio passar pela catraca. Essa funÃ§Ã£o
     * deve ser utilizada somente com Inners Catraca.
     *
     * @param Inner
     * @return
     */
    public static native int LiberarCatracaDoisSentidos(int Inner);
    /**/

    /**
     * Libera a catraca no sentido de entrada padrÃ£o do Inner, para o usuÃ¡rio
     * poder efetuar o giro na catraca. Em modo On-Line, na funÃ§Ã£o
     * ReceberDadosOnLine o valor retornado no parÃ¢metro â€œComplementoâ€� serÃ¡ do
     * tipo entrada.
     *
     * @param Inner
     * @return
     */
    public static native int LiberarCatracaEntrada(int Inner);
    /**/

    /**
     * Libera a catraca no sentido de saÃ­da padrÃ£o do Inner, para o usuÃ¡rio
     * poder efetuar o giro na catraca. Em modo On-Line, na funÃ§Ã£o
     * ReceberDadosOnLine o valor retornado no parÃ¢metro â€œComplementoâ€� serÃ¡ do
     * tipo saÃ­da. Essa funÃ§Ã£o deve ser utilizada somente com Inners Catraca.
     *
     * @param Inner
     * @return
     */
    public static native int LiberarCatracaSaida(int Inner);
    /**/

    /**
     * Libera a catraca no sentido contrÃ¡rio a entrada padrÃ£o do Inner, para o
     * usuÃ¡rio poder efetuar o giro na catraca. Em modo On-Line, na funÃ§Ã£o
     * ReceberDadosOnLine o valor retornado no parÃ¢metro â€œComplementoâ€� serÃ¡ do
     * tipo saÃ­da. Essa funÃ§Ã£o deve ser utilizada somente com Inners Catraca.
     *
     * @param Inner
     * @return
     */
    public static native int LiberarCatracaEntradaInvertida(int Inner);
    /**/

    /**
     * Libera a catraca no sentido contrÃ¡rio a saÃ­da padrÃ£o do Inner, para o
     * usuÃ¡rio poder efetuar o giro na catraca. Em modo On-Line, na funÃ§Ã£o
     * ReceberDadosOnLine o valor retornado no parÃ¢metro â€œComplementoâ€� serÃ¡ do
     * tipo entrada. Essa funÃ§Ã£o deve ser utilizada somente com Inners Catraca.
     *
     * @param Inner
     * @return
     */
    public static native int LiberarCatracaSaidaInvertida(int Inner);
    /**/

    /**
     * Prepara o Inner para trabalhar no modo Off-Line, porÃ©m essa funÃ§Ã£o ainda
     * nÃ£o envia essa informaÃ§Ã£o para o equipamento.
     *
     * @return
     */
    public static native int ConfigurarInnerOffLine();
    /**/

    /**
     * Configura a mensagem a ser exibida quando o usuÃ¡rio passar o cartÃ£o no
     * sentido de entrada do Inner. Caso a mensagem passe de 32 caracteres a DLL
     * irÃ¡ utilizar os primeiros 32 caracteres. O Inner nÃ£o aceita caracteres
     * com acentuaÃ§Ã£o, padrÃ£o UNICODE ou padrÃ£o ANSI. O Inner aceita apenas os
     * caracteres do padrÃ£o ASCII. ExibirData: 0 â€“ NÃ£o exibe a data/hora na
     * linha superior do display. 1 â€“ Exibe a data/hora na linha superior do
     * display(Default). Mensagem: String com a mensagem a ser exibida. Caso
     * esteja exibindo a data/hora o tamanho da mensagem passa a ser 16 ao invÃ©s
     * de 32. Caso seja passado uma string vazia o Inner exibirÃ¡ a mensagem em
     * branco no display. Entrada OK(Default).
     *
     * @param ExibirData
     * @param Mensagem
     * @return
     */
    public static native int DefinirMensagemEntradaOffLine(int ExibirData, String Mensagem);
    /**/

    /**
     * Configura a mensagem a ser exibida quando o usuÃ¡rio passar o cartÃ£o no
     * sentido de saÃ­da do Inner. Caso a mensagem passe de 32 caracteres a DLL
     * irÃ¡ utilizar os primeiros 32 caracteres. O Inner nÃ£o aceita caracteres
     * com acentuaÃ§Ã£o, padrÃ£o UNICODE ou padrÃ£o ANSI. O Inner aceita apenas os
     * caracteres do padrÃ£o ASCII. ExibirData: 0 â€“ NÃ£o exibe a data/hora na
     * linha superior do display. 1 â€“ Exibe a data/hora na linha superior do
     * display(Default). Mensagem String com a mensagem a ser exibida. Caso
     * esteja exibindo a data/hora o tamanho da mensagem passa a ser 16 ao invÃ©s
     * de 32. Caso seja passado uma string vazia o Inner exibirÃ¡ a mensagem em
     * branco no display. Entrada OK(Default).
     *
     * @param ExibirData
     * @param Mensagem
     * @return
     */
    public static native int DefinirMensagemSaidaOffLine(int ExibirData, String Mensagem);
    /**/

    /**
     * Configura a mensagem a ser exibida pelo Inner quando ele estiver ocioso.
     * Caso a mensagem passe de 32 caracteres a DLL irÃ¡ utilizar os primeiros 32
     * caracteres. O Inner nÃ£o aceita caracteres com acentuaÃ§Ã£o, padrÃ£o UNICODE
     * ou padrÃ£o ANSI. O Inner aceita apenas os caracteres do padrÃ£o ASCII.
     * ExibirData: 0 â€“ NÃ£o exibe a data/hora na linha superior do display. 1 â€“
     * Exibe a data/hora na linha superior do display(Default). Mensagem: String
     * com a mensagem a ser exibida. Caso esteja exibindo a data/hora o tamanho
     * da mensagem passa a ser 16 ao invÃ©s de 32. Caso seja passado uma string
     * vazia o Inner exibirÃ¡ a mensagem em branco no display. Passe o
     * cartÃ£o(Default).
     *
     * @param ExibirData
     * @param Mensagem
     * @return
     */
    public static native int DefinirMensagemPadraoOffLine(int ExibirData, String Mensagem);
    /**/

    /**
     * Envia o buffer com todas as mensagens off line configuradas
     * anteriormente, para o Inner. ApÃ³s executar a funÃ§Ã£o, o buffer com as
     * mensagens Ã© limpo automaticamente pela dll. Inner: 1 a 32 â€“ Para
     * comunicaÃ§Ã£o serial. 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel.
     * 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP porta fixa.
     *
     * @param Inner
     * @return
     */
    public static native int EnviarMensagensOffLine(int Inner);
    /**/

    /**
     * Habilita/Desabilita a mudanÃ§Ã£o automÃ¡tica do modo OffLine do Inner para
     * OnLine e viceversa. Configura o tempo apÃ³s a comunicaÃ§Ã£o ser interrompida
     * que estÃ¡ mudanÃ§a irÃ¡ ocorrer. Habilita: 0 â€“ Desabilita a
     * mudanÃ§a(Default). 1 â€“ Habilita a mudanÃ§a. 2 â€“ Habilita a mudanÃ§a
     * automÃ¡tica para o modo OnLine TCP com Ping, onde o Inner precisa receber
     * o comando PingOnLine para manter-se OnLine. Tempo - Tempo em segundos
     * para ocorrer a mudanÃ§a. 1 a 50.
     *
     * @param Habilita
     * @param Tempo
     * @return
     */
    public static native int HabilitarMudancaOnLineOffLine(int Habilita, int Tempo);

    /**
     * Configura as formas de entradas de dados para quando o Inner mudar para o
     * modo Off-Line. Para aplicaÃ§Ãµes com biometria verifique a prÃ³xima funÃ§Ã£o
     * â€œDefinirEntradasMudanÃ§aOffLineComBiometriaâ€�. Teclado: 0 â€“ NÃ£o aceita
     * dados pelo teclado. 1 â€“ Aceita dados pelo teclado. Leitor1: 0 â€“
     * desativado 1 â€“ somente para entrada 2 â€“ somente para saÃ­da 3 â€“ entrada e
     * saÃ­da 4 â€“ saÃ­da e entrada Leitor2: 0 â€“ desativado 1 â€“ somente para
     * entrada 2 â€“ somente para saÃ­da 3 â€“ entrada e saÃ­da 4 â€“ saÃ­da e entrada
     * Catraca - 0 â€“ reservado para uso futuro.
     *
     * @param Teclado
     * @param Leitor1
     * @param Leitor2
     * @param Catraca
     * @return
     */
    public static native int DefinirEntradasMudancaOffLine(int Teclado, int Leitor1, int Leitor2, int Catraca);

    /**
     * Configura a mensagem padrÃ£o exibido pelo Inner quando entrar para on line
     * apÃ³s uma queda para off line. 0 â€“ NÃ£o exibe a data/hora na linha superior
     * do display. 1 â€“ Exibe a data/hora na linha superior do display. String
     * com a mensagem a ser exibida. Caso esteja exibindo a data/hora o tamanho
     * da mensagem passa a ser 16 ao invÃ©s de 32. Caso seja passado uma string
     * vazia o Inner nÃ£o exibirÃ¡ a mensagem no display
     *
     * @param ExibirData
     * @param Mensagem
     * @return
     */
    public static native int DefinirMensagemPadraoMudancaOnLine(int ExibirData, String Mensagem);

    /**
     * Configura a mensagem padrÃ£o a ser exibida pelo Inner quando ele mudar
     * para Off-line. ExibirData: 0 â€“ NÃ£o exibe a data/hora na linha superior do
     * display. 1 â€“ Exibe a data/hora na linha superior do display. Mensagem:
     * String com a mensagem a ser exibida. Caso esteja exibindo a data/hora o
     * tamanho da mensagem passa a ser 16 ao invÃ©s de 32. Caso seja passado uma
     * string vazia o Inner nÃ£o exibirÃ¡ a mensagem no display
     *
     * @param ExibirData
     * @param Mensagem
     * @return
     */
    public static native int DefinirMensagemPadraoMudancaOffLine(int ExibirData, String Mensagem);
    /**/

    /**
     * Envia o buffer com as configuraÃ§Ãµes de mudanÃ§a automÃ¡tica do modo OnLine
     * para OffLine . Inner: 1 a 32 â€“ Para comunicaÃ§Ã£o serial. 1 a 99 â€“ Para
     * comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel. 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP
     * porta fixa.
     *
     * @param Inner
     * @return
     */
    public static native int EnviarConfiguracoesMudancaAutomaticaOnLineOffLine(int Inner);

    /**
     * Configura a mensagem a ser exibida quando o usuÃ¡rio passar o cartÃ£o
     * utilizando uma das funÃ§Ãµes do Inner(de 0 a 9) e a habilita ou desabilita
     * essas funÃ§Ãµes. Caso a mensagem passe de 32 caracteres a DLL irÃ¡ utilizar
     * os primeiros 32 caracteres. O Inner nÃ£o aceita caracteres com acentuaÃ§Ã£o,
     * padrÃ£o UNICODE ou padrÃ£o ANSI. O Inner aceita apenas os caracteres do
     * padrÃ£o ASCII. Mensagem: String com a mensagem a ser exibida. Caso esteja
     * exibindo a data/hora o tamanho da mensagem passa a ser 16 ao invÃ©s de 32.
     * Caso seja passado uma string vazia o Inner nÃ£o exibirÃ¡ a mensagem no
     * display. Funcao - 0 a 9. Habilitada: 0 â€“ Desabilita a funÃ§Ã£o do
     * Inner(Default). 1 â€“ Habilita a funÃ§Ã£o do Inner.
     *
     * @param Mensagem
     * @param Funcao
     * @param Habilitada
     * @return
     */
    public static native int DefinirMensagemFuncaoOffLine(String Mensagem, int Funcao, int Habilitada);

    /**
     * Coleta um bilhete Off-Line que estÃ¡ armazenado na memÃ³ria do Inner, os
     * dados do bilhete sÃ£o retornados por referÃªncia nos parÃ¢metros da funÃ§Ã£o.
     * Ates de chamar esta funÃ§Ã£o pela primeira vez Ã© preciso chamar
     * obrigatoriamente as funÃ§Ãµes DefinirPadraoCartao e
     * DefinirQuantidadeDigitosCartao nessa ordem para que o nÃºmero do cartÃ£o
     * seja calculado de forma correta. Inner: 1 a 32 â€“ Para comunicaÃ§Ã£o serial.
     * 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel. 1 a ... â€“ Para
     * comunicaÃ§Ã£o TCP/IP porta fixa. Tipo - Tipo da marcaÃ§Ã£o registrada. 0 a 9
     * â€“ FunÃ§Ãµes registradas pelo cartÃ£o. 10 â€“ Entrada pelo cartÃ£o. 11 â€“ SaÃ­da
     * pelo cartÃ£o. 12 â€“ Tentativa de entrada negada pelo cartÃ£o. 13 â€“ Tentativa
     * de saÃ­da negada pelo cartÃ£o. 100 a 109 â€“ FunÃ§Ãµes registradas pelo
     * teclado. 110 â€“ Entrada pelo teclado. 111 â€“ SaÃ­da pelo teclado. 112 â€“
     * Tentativa de entrada negada pelo teclado. 113 â€“ Tentativa de saÃ­da negada
     * pelo teclado. Dia - 1 a 31 Mes - 1 a 12 Ano - 0 a 99 Hora - 0 a 23 Minuto
     * - 0 a 59 Cartao - NÃºmero do cartÃ£o do usuÃ¡rio.
     *
     * @param Inner
     * @param Dados
     * @param Cartao
     * @return
     */
    public static native int ColetarBilhete(int Inner, int[] Dados, StringBuffer Cartao);

    /**
     * Prepara o Inner para trabalhar no modo On-Line, porÃ©m essa funÃ§Ã£o ainda
     * nÃ£o envia essa informaÃ§Ã£o para o equipamento.
     *
     * @return
     */
    public static native int ConfigurarInnerOnLine();

    /**
     * Configura o Inner para enviar as informaÃ§Ãµes de data/hora nos bilhete on
     * line, esses dados serÃ£o retornados nos parÃ¢metros da funÃ§Ã£o
     * ReceberDadosOnLine. Recebe: 0 â€“ NÃ£o receber a data/hora do
     * bilhete(Default). 1 â€“ Recebe a data/hora do bilhete.
     *
     * @param Recebe
     * @return
     */
    public static native int ReceberDataHoraDadosOnLine(int Recebe);

    /**
     * Envia para o Inner a mensagem padrÃ£o(fixa) que serÃ¡ sempre exibida pelo
     * Inner. Essa mensagem Ã© exibida enquanto o Inner estiver ocioso. Caso a
     * mensagem passe de 32 caracteres a DLL irÃ¡ utilizar os primeiros 32
     * caracteres. O Inner nÃ£o aceita caracteres com acentuaÃ§Ã£o, padrÃ£o UNICODE
     * ou padrÃ£o ANSI. O Inner aceita apenas os caracteres do padrÃ£o ASCII.
     * Inner: 1 a 32 â€“ Para comunicaÃ§Ã£o serial. 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP
     * com porta variÃ¡vel. 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP porta fixa.
     * ExibirData: 0 â€“ NÃ£o exibe a data/hora na linha superior do display. 1 â€“
     * Exibe a data/hora na linha superior do display. Mensagem - String com a
     * mensagem a ser exibida. Caso esteja exibindo a data/hora o tamanho da
     * mensagem passa a ser 16 ao invÃ©s de 32. Caso seja passado uma string
     * vazia o Inner exibirÃ¡ a mensagem em branco no display.
     *
     * @param Inner
     * @param ExibirData
     * @param Mensagem
     * @return
     */
    public static native int EnviarMensagemPadraoOnLine(int Inner, int ExibirData, String Mensagem);

    /**
     * Configura as formas de entrada de dados do Inner no modo OnLine. Cada vez
     * que alguma informaÃ§Ã£o for recebida no modo OnLine atravÃ©s da funÃ§Ã£o
     * ReceberDadosOnLine, a funÃ§Ã£o EnviarFormasEntradasOnLine deverÃ¡ ser
     * chamada novamente para reconfigurar o Inner. Inner: 1 a 32 â€“ Para
     * comunicaÃ§Ã£o serial. 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel.
     * 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP porta fixa QtdeDigitosTeclado - 0 a 20
     * dÃ­gitos. EcoTeclado: 0 â€“ para nÃ£o 1 â€“ para sim 2 â€“ ecoar FormaEntrada: 0
     * â€“ nÃ£o aceita entrada de dados 1 â€“ aceita teclado 2 â€“ aceita leitura no
     * leitor 1 3 â€“ aceita leitura no leitor 2 4 â€“ teclado e leitor 1 5 â€“
     * teclado e leitor 2 6 â€“ leitor 1 e leitor 2 7 â€“ teclado, leitor 1 e leitor
     * 2 10 â€“ teclado + verificaÃ§Ã£o biomÃ©trica 11 â€“ leitor1 + verificaÃ§Ã£o
     * biomÃ©trica 12 â€“ teclado + leitor1 + verificaÃ§Ã£o biomÃ©trica 13 â€“ leitor1
     * com verificaÃ§Ã£o biomÃ©trica + leitor2 sem verificaÃ§Ã£o biomÃ©trica 14 â€“
     * leitor1 com verificaÃ§Ã£o biomÃ©trica + leitor2 sem verificaÃ§Ã£o biomÃ©trica +
     * teclado sem verificaÃ§Ã£o biomÃ©trica 100 â€“ Leitor 1 + IdentificaÃ§Ã£o
     * BiomÃ©trica (sem VerificaÃ§Ã£o) 101 â€“ Leitor 1 + Teclado + IdentificaÃ§Ã£o
     * BiomÃ©trica (sem VerificaÃ§Ã£o) 102 â€“ Leitor 1 + Leitor 2 + IdentificaÃ§Ã£o
     * BiomÃ©trica (sem VerificaÃ§Ã£o) 103 â€“ Leitor 1 + Leitor 2 + Teclado +
     * IdentificaÃ§Ã£o BiomÃ©trica (sem VerificaÃ§Ã£o) 104 â€“ Leitor 1 invertido +
     * IdentificaÃ§Ã£o BiomÃ©trica (sem VerificaÃ§Ã£o) 105 â€“ Leitor 1 invertido +
     * Teclado + IdentificaÃ§Ã£o BiomÃ©trica (sem VerificaÃ§Ã£o) TempoTeclado - 1 a
     * 50 PosicaoCursorTeclado - 1 a 32
     *
     * @param Inner
     * @param QtdeDigitosTeclado
     * @param EcoTeclado
     * @param FormaEntrada
     * @param TempoTeclado
     * @param PosicaoCursorTeclado
     * @return
     */
    public static native int EnviarFormasEntradasOnLine(int Inner, int QtdeDigitosTeclado, int EcoTeclado, int FormaEntrada, int TempoTeclado, int PosicaoCursorTeclado);

    /**
     * Coleta um bilhete OnLine, caso o usuÃ¡rio tenha passado ou digitado algum
     * cartÃ£o no Inner retorna as informaÃ§Ãµes do cartÃ£o nos parÃ¢metros da
     * funÃ§Ã£o. Para que a data/hora do bilhete OnLine seja retornada, o Inner
     * deverÃ¡ ter sido previamente configurado atravÃ©s da funÃ§Ã£o
     * ReceberDataHoraDadosOnLine. Inner: 1 a 32 â€“ Para comunicaÃ§Ã£o serial. 1 a
     * 99 â€“ Para comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel. 1 a ... â€“ Para
     * comunicaÃ§Ã£o TCP/IP porta fixa. Origem - Origem dos dados recebidos. 1 â€“
     * via teclado 2 â€“ via leitor 1 3 â€“ via leitor 2 4 â€“ sensor da
     * catraca(obsoleto) 5 â€“ fim do tempo de acionamento 6 â€“ giro da catraca
     * Topdata (sensor Ã³tico) 7 â€“ Urna (catraca Millenium) 8 â€“ Evento no Sensor
     * 1 9 â€“ Evento no Sensor 2 10 â€“ Evento no Sensor 3 Complemento -
     * InformaÃ§Ãµes adicionais sobre os dados recebidos. 0 â€“ saÃ­da (com cartÃ£o) 1
     * â€“ entrada (com cartÃ£o) 35 â€“ # via teclado (1Â° tecla) 42 â€“ * via teclado
     * (1Â° tecla) 65 â€“ â€œFunÃ§Ã£oâ€� via teclado 66 â€“ â€œEntradaâ€� via teclado 67 â€“
     * â€œSaÃ­daâ€� via teclado 255 â€“ Inseriu todos os dÃ­gitos permitidos pelo
     * teclado. Evento do Sensor 0/1 â€“ NÃ­vel atual do sensor Cartao - NÃºmero do
     * cartÃ£o recebido. Dia - 1 a 31. Mes - 1 a 12. Ano - 0 a 99 Hora - 0 a 23
     * Minuto - 0 a 59 Segundo - 0 a 59
     *
     * @param Inner
     * @param Dados
     * @param Cartao
     * @return
     */
    public static native int ReceberDadosOnLine(int Inner, int[] Dados, StringBuffer Cartao);

    /**
     * Configura as formas de entrada dos dados quando o Inner voltar para o
     * modo On Line apÃ³s uma queda para OffLine. Entrada: 0 â€“ NÃ£o aceita entrada
     * de dados. 1 â€“ Aceita teclado. 2 â€“ Aceita leitor 1. 3 â€“ Aceita leitor 2. 4
     * â€“ Teclado e leitor 1. 5 â€“ Teclado e leitor 2. 6 â€“ Leitor 1 e leitor 2. 7
     * â€“ Teclado, leitor 1 e 2. 8 â€“ Sensor da catraca.
     *
     * @param Entrada
     * @return
     */
    public static native int DefinirEntradasMudancaOnLine(int Entrada);

    /**
     * Configura o teclado para quando o Inner voltar para OnLine apÃ³s uma queda
     * para OffLine. Digitos - 0 a 20 dÃ­gitos. EcoDisplay: 0 â€“ para nÃ£o ecoar 1
     * â€“ para sim 2 â€“ ecoar '*' Tempo - 1 a 50. PosicaoCursor - 1 a 32.
     *
     * @param Digitos
     * @param EcoDisplay
     * @param Tempo
     * @param PosicaoCursor
     * @return
     */
    public static native int DefinirConfiguracaoTecladoOnLine(int Digitos, int EcoDisplay, int Tempo, int PosicaoCursor);

    /**
     * Habilita/Desabilita a identificaÃ§Ã£o biomÃ©trica e/ou a verificaÃ§Ã£o
     * biomÃ©trica do Inner bio. O resultado da configuraÃ§Ã£o deve ser obtivo
     * atravÃ©s do comando ResultadoConfiguracaoBio. Inner: 1 a 32 â€“ Para
     * comunicaÃ§Ã£o serial. 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel.
     * 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP porta fixa. HabilitaIdentificacao: 0 â€“
     * Desabilita. 1 â€“ Habilita. HabilitaVerificacao: 0 â€“ Desabilita. 1 â€“
     * Habilita.
     *
     * @param Inner
     * @param HabilitaIdentificacao
     * @param HabilitaVerificacao
     * @return
     */
    public static native int ConfigurarBio(int Inner, int HabilitaIdentificacao, int HabilitaVerificacao);

    /**
     * Retorna o resultado da configuraÃ§Ã£o do Inner Bio, funÃ§Ã£o ConfigurarBio.
     * Se o retorno for igual a 0 Ã© poque o Inner bio foi configurado com
     * sucesso. Inner: 1 a 32 â€“ Para comunicaÃ§Ã£o serial. 1 a 99 â€“ Para
     * comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel. 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP
     * porta fixa. OnLine: 0 â€“ O comando foi utilizado com o Inner em OffLine. 1
     * â€“ O comando foi utilizado com o Inner em OnLine.
     *
     * @param Inner
     * @param OnLine
     * @return
     */
    public static native int ResultadoConfiguracaoBio(int Inner, int OnLine);

    /**
     * Insere o nÃºmero do cartÃ£o na lista de usuÃ¡rios sem digital do Inner bio.
     * Este nÃºmero ficara armazenado em um buffer interno dentro da dll e
     * somente serÃ¡ enviado para o Inner apÃ³s a chamada a funÃ§Ã£o
     * EnviarListaUsuariosSemDigitalBio. O nÃºmero mÃ¡ximo de dÃ­gitos para o
     * cartÃ£o Ã© 10, caso os cartÃµes tenham mais de 10 dÃ­gitos, utilizar os 10
     * dÃ­gitos menos significativos do cartÃ£o. Cartao - 1 a 9999999999 â€“ NÃºmero
     * do cartÃ£o do usuÃ¡rio.
     *
     * @param Cartao
     * @return
     */
    public static native int IncluirUsuarioSemDigitalBio(String Cartao);
    public static native int IncluirUsuarioSemDigitalBioInnerAcesso(String Cartao);

    /**
     * Solicita a versÃ£o do firmware da placa do Inner Bio, a placa que armazena
     * as digitais. Inner: 1 a 32 â€“ Para comunicaÃ§Ã£o serial. 1 a 99 â€“ Para
     * comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel. 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP
     * porta fixa.
     *
     * @param Inner
     * @return
     */
    public static native int SolicitarVersaoBio(int Inner);

    /**
     * Envia o buffer com a lista de usuÃ¡rios sem digital para o Inner. ApÃ³s a
     * execuÃ§Ã£o do comando, o buffer Ã© limpo pela dll. Inner: 1 a 32 â€“ Para
     * comunicaÃ§Ã£o serial. 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel.
     * 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP porta fixa.
     *
     * @param Inner
     * @return
     */
    public static native int EnviarListaUsuariosSemDigitalBio(int Inner);
    /**
     * Envia o buffer com a lista de usuÃ¡rios sem digital para o Inner. ApÃ³s a
     * execuÃ§Ã£o do comando, o buffer Ã© limpo pela dll. Inner: 1 a 32 â€“ Para
     * comunicaÃ§Ã£o serial. 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel.
     * 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP porta fixa.
     *
     * @param Inner
     * @param QtdDigitos
     * @return
     */
    public static native int EnviarListaUsuariosSemDigitalBioVariavel(int Inner, int QtdDigitos);

    /**
     * Solicita o modelo do Inner bio. Para receber o resultado dessa operaÃ§Ã£o
     * vocÃª deverÃ¡ chamar a funÃ§Ã£o ReceberModeloBio enquanto o retorno for
     * processando a operaÃ§Ã£o. Inner: 1 a 32 â€“ Para comunicaÃ§Ã£o serial. 1 a 99 â€“
     * Para comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel. 1 a ... â€“ Para comunicaÃ§Ã£o
     * TCP/IP porta fixa.
     *
     * @param Inner
     * @return
     */
    public static native int SolicitarModeloBio(int Inner);

    /**
     * Retorna o resultado do comando SolicitarModeloBio, o modelo do Inner Bio
     * Ã© retornado por referÃªncia no parÃ¢metro da funÃ§Ã£o. Inner: 1 a 32 â€“ Para
     * comunicaÃ§Ã£o serial. 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel.
     * 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP porta fixa. OnLine: 0 â€“ O comando foi
     * utilizado com o Inner em OffLine. 1 â€“ O comando foi utilizado com o Inner
     * em OnLine Modelo: 2 â€“ Bio Light 100 usuÃ¡rios. 4 â€“ Bio 1000/4000 usuÃ¡rios.
     * 255 â€“ VersÃ£o desconhecida.
     *
     * @param Inner
     * @param OnLine
     * @param Modelo
     * @return
     */
    public static native int ReceberModeloBio(int Inner, int OnLine, int[] Modelo);

    /**
     * Retorna o resultado do comando SolicitarVersaoBio, a versÃ£o do Inner Bio
     * Ã© retornado por referÃªncia nos parÃ¢metros da funÃ§Ã£o. Inner: 1 a 32 â€“ Para
     * comunicaÃ§Ã£o serial. 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel.
     * 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP porta fixa. OnLine: 0 â€“ O comando foi
     * utilizado com o Inner em OffLine. 1 â€“ O comando foi utilizado com o Inner
     * em OnLine VersaoAlta - Parte alta da versÃ£o. VersaoBaixa - Parte baixa da
     * versÃ£o.
     *
     * @param Inner
     * @param OnLine
     * @param Versao
     * @return
     */
    public static native int ReceberVersaoBio(int Inner, int OnLine, int[] Versao);

    /**
     * Define que o Inner utilizado no momento Ã© um Inner bio light ao invÃ©s de
     * um Inner bio 1000/4000. Essa funÃ§Ã£o deverÃ¡ ser chamada sempre que
     * necessÃ¡rio antes das funÃ§Ãµes SolicitarUsuarioCadastradoBio,
     * SolicitarExclusaoUsuario, InserirUsuarioLeitorBio e
     * FazerVerificacaoBiometricaBio. Light: 1 â€“ Ã‰ um Inner bio light 0 â€“ Ã‰ um
     * Inner bio 1000/4000(valor default)
     *
     * @param Light
     */
    public static native void SetarBioLight(int Light);

    /**
     *
     * @param habilitado
     */
    public static native void SetarBioVariavel(int habilitado);

    /**
     * Solicita para o Inner Bio inserir um usuÃ¡rio diretamente pelo leitor
     * biomÃ©trico. O leitor irÃ¡ acender a luz vermelho e apÃ³s o usuÃ¡rio inserir
     * a digital, automaticamente o usuÃ¡rio serÃ¡ cadastrado no nner bio com o
     * nÃºmero do cartÃ£o passado no parÃ¢metro UsuÃ¡rio. Inner: 1 a 32 â€“ Para
     * comunicaÃ§Ã£o serial. 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel.
     * 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP porta fixa. Tipo: 0 â€“ para solicitar o
     * primeiro template 1 â€“ para solicitar o segundo template (mesmo dedo) e
     * salvar. 2 â€“ para solicitar o segundo template (outro dedo) e salvar.
     * Usuario - NÃºmero do cartÃ£o que o usuÃ¡rio terÃ¡.
     *
     * @param Inner
     * @param Tipo
     * @param Usuario
     * @return
     */
    public static native int InserirUsuarioLeitorBio(int Inner, int Tipo, String Usuario);

    /**
     * Retorna o resultado do comando InserirUsuarioLeitorBio. Se o retorno for
     * igual a 0 Ã© porque o usuÃ¡rio foi cadastrado com sucesso. Inner: 1 a 32 â€“
     * Para comunicaÃ§Ã£o serial. 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP com porta
     * variÃ¡vel. 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP porta fixa. OnLine: 0 â€“ O
     * comando foi utilizado com o Inner em OffLine. 1 â€“ O comando foi utilizado
     * com o Inner em OnLine.
     *
     * @param Inner
     * @param OnLine
     * @return
     */
    public static native int ResultadoInsercaoUsuarioLeitorBio(int Inner, int OnLine);

    /**
     * Solicita para o Inner bio excluir o cadastro do usuÃ¡rio desejado. O
     * Retorno da exclusÃ£o Ã© verificado atravÃ©s da funÃ§Ã£o UsuarioFoiExcluido
     * Inner: 1 a 32 â€“ Para comunicaÃ§Ã£o serial. 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP
     * com porta variÃ¡vel. 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP porta fixa. Usuario
     * - NÃºmero do cartÃ£o do usuÃ¡rio cadastrado.
     *
     * @param Inner
     * @param Usuario
     * @return
     */
    public static native int SolicitarExclusaoUsuario(int Inner, String Usuario);

    /**
     * Retorna o resultado do comando SolicitarExclusaoUsuario, se o retorno da
     * funÃ§Ã£o for igual a 0 Ã© porque o usuÃ¡rio foi excluÃ­do com sucesso. Inner:
     * 1 a 32 â€“ Para comunicaÃ§Ã£o serial. 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP com
     * porta variÃ¡vel. 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP porta fixa. OnLine: 0 â€“
     * O comando foi utilizado com o Inner em OffLine. 1 â€“ O comando foi
     * utilizado com o Inner em OnLine.
     *
     * @param Inner
     * @param OnLine
     * @return
     */
    public static native int UsuarioFoiExcluido(int Inner, int OnLine);

    /**
     * Solicita do Inner Bio, o template com as duas digitais do Usuario
     * desejado. Inner: 1 a 32 â€“ Para comunicaÃ§Ã£o serial. 1 a 99 â€“ Para
     * comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel. 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP
     * porta fixa. Usuario - NÃºmero do cartÃ£o do usuÃ¡rio cadastrado.
     *
     * @param Inner
     * @param Usuario
     * @return
     */
    public static native int SolicitarUsuarioCadastradoBio(int Inner, String Usuario);

    /**
     * Retorna o resultado do comando SolicitarUsuarioCadastradoBio e o template
     * com as duas digitais do usuÃ¡rio cadastrado no Inner Bio. O template Ã©
     * retornado por referÃªncia nos parÃ¢metros da funÃ§Ã£o. Inner: 1 a 32 â€“ Para
     * comunicaÃ§Ã£o serial. 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel.
     * 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP porta fixa. OnLine: 0 â€“ O comando foi
     * utilizado com o Inner em OffLine. 1 â€“ O comando foi utilizado com o Inner
     * em OnLine Template: Cadastro do usuÃ¡rio com as duas digitais, o dado estÃ¡
     * em binÃ¡rio e nÃ£o deve ser alterado nunca. O tamanho do template Ã© de 844
     * bytes.
     *
     * @param Inner
     * @param OnLine
     * @param Template
     * @return
     */
    public static native int ReceberUsuarioCadastradoBio(int Inner, int OnLine, byte[] Template);

    /**
     * Solicita a quantidade de usuÃ¡rios cadastrados no Inner Bio.
     *
     * @param Inner
     * @return
     */
    public static native int SolicitarQuantidadeUsuariosBio(int Inner);

    /**
     * Retorna o resultado do comando SolicitarQuantidadeUsuariosBio, a
     * quantidade de usuÃ¡rios cadastrados no Inner Bio Ã© retornado por
     * referÃªncia nos parÃ¢metros da funÃ§Ã£o. Inner: 1 a 32 â€“ Para comunicaÃ§Ã£o
     * serial. 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel. 1 a ... â€“
     * Para comunicaÃ§Ã£o TCP/IP porta fixa. OnLine: 0 â€“ O comando foi utilizado
     * com o Inner em OffLine. 1 â€“ O comando foi utilizado com o Inner em OnLine
     * Quantidade - Total de usuÃ¡rios cadastrados no Inner Bio.
     *
     * @param Inner
     * @param OnLine
     * @param Quantidade
     * @return
     */
    public static native int ReceberQuantidadeUsuariosBio(int Inner, int OnLine, int[] Quantidade);

    /**
     * Prepara a dll para iniciar a coleta dos usuÃ¡rios do Inner bio, essa
     * funÃ§Ã£o deve ser chamada obrigatoriamente no inÃ­cio do processo.
     */
    public static native void InicializarColetaListaUsuariosBio();

    /**
     * Retorna 1 se ainda existe mais pacotes da lista de usuÃ¡rios, a ser
     * recebido do Inner Bio.
     *
     * @return
     */
    public static native int TemProximoPacote();

    /**
     * Recebe um usuÃ¡rio por vez do pacote recebido anteriormente. O nÃºmero do
     * usuÃ¡rio Ã© retornado pelo parÃ¢metro da funÃ§Ã£o. Inner: 1 a 32 â€“ Para
     * comunicaÃ§Ã£o serial. 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel.
     * 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP porta fixa. Usuario - NÃºmero do cartÃ£o
     * do usuÃ¡rio cadastrado.
     *
     * @param Inner
     * @param Usuario
     * @return
     */
    public static native int ReceberUsuarioLista(int Inner, StringBuffer Usuario);

    /**
     * Retorna 1 se ainda existe usuÃ¡rios no pacote recebido da lista.
     *
     * @return
     */
    public static native int TemProximoUsuario();

    /**
     * Solicita o pacote(a parte) atual da lista de usuÃ¡rios do Inner bio.
     * Inner: 1 a 32 â€“ Para comunicaÃ§Ã£o serial. 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP
     * com porta variÃ¡vel. 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP porta fixa.
     *
     * @param Inner
     * @return
     */
    public static native int SolicitarListaUsuariosBio(int Inner);

    /**
     * Solicita o pacote(a parte) atual da lista de usuÃ¡rios do Inner bio.
     * Inner: 1 a 32 â€“ Para comunicaÃ§Ã£o serial. 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP
     * com porta variÃ¡vel. 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP porta fixa.
     *
     * @param Inner
     * @return
     */
    public static native int SolicitarListaUsuariosBioVariavel(int Inner);

    /**
     * Receber o pacote solicitado pela funÃ§Ã£o SolicitarListaUsuariosBio. Inner:
     * 1 a 32 â€“ Para comunicaÃ§Ã£o serial. 1 a 99 â€“ Para comunicaÃ§Ã£o TCP/IP com
     * porta variÃ¡vel. 1 a ... â€“ Para comunicaÃ§Ã£o TCP/IP porta fixa.
     *
     * @param Inner
     * @return
     */
    public static native int ReceberPacoteListaUsuariosBio(int Inner);

    /**
     * Envia um template com duas digitais para o Inner Bio cadastrar no seu
     * banco de dados. O resultado do cadastro deve ser verificado no retorno da
     * funÃ§Ã£o UsuarioFoiEnviado. Inner: 1 a 32 â€“ Para comunicaÃ§Ã£o serial. 1 a 99
     * â€“ Para comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel. 1 a ... â€“ Para comunicaÃ§Ã£o
     * TCP/IP porta fixa. Template: O cadastro do usuÃ¡rio jÃ¡ contendo as duas
     * digitais e o nÃºmero do usuÃ¡rio. Ã‰ um array de bytes com o tamanho de 844
     * bytes.
     *
     * @param Inner
     * @param Template
     * @return
     */
    public static native int EnviarUsuarioBio(int Inner, byte[] Template);

    /**
     * Retorna o resultado do cadastro do Template no Inner Bio, atravÃ©s da
     * funÃ§Ã£o EnviarUsuarioBio. Se o retorno for igual a 0 Ã© porque o template
     * foi cadastrado com sucesso. Inner: 1 a 32 â€“ Para comunicaÃ§Ã£o serial. 1 a
     * 99 â€“ Para comunicaÃ§Ã£o TCP/IP com porta variÃ¡vel. 1 a ... â€“ Para
     * comunicaÃ§Ã£o TCP/IP porta fixa. OnLine: 0 â€“ O comando foi utilizado com o
     * Inner em OffLine. 1 â€“ O comando foi utilizado com o Inner em OnLine.
     *
     * @param Inner
     * @param OnLine
     * @return
     */
    public static native int UsuarioFoiEnviado(int Inner, int OnLine);

    /**
     * Configura as formas de entradas de dados para quando o Inner mudar para o
     * modo Off-Line. Esse comando difere do anterior por permitir a
     * configuraÃ§Ã£o de biometria. AtravÃ©s dessa funÃ§Ã£o o Inner pode ser
     * configurado para trabalhar com verificaÃ§Ã£o ou identificaÃ§Ã£o biomÃ©trica,
     * quando ocorrer uma mudanÃ§a automÃ¡tica de On-Line para Off-Line. Teclado:
     * 0 â€“ NÃ£o aceita dados pelo teclado. 1 â€“ Aceita dados pelo teclado.
     * Leitor1: 0 â€“ desativado 3 â€“ entrada e saÃ­da 4 â€“ saÃ­da e entrada (nesse
     * caso forÃ§a Leitor2 igual a zero)</param>
     * Leitor2: 0 â€“ desativado 3 â€“ entrada e saÃ­da Verificacao: 0 â€“ desativada 1
     * â€“ ativada Identificacao: 0 â€“ desativada 1 â€“ ativada
     *
     * @param Teclado
     * @param Leitor1
     * @param Leitor2
     * @param Verificacao
     * @param Identificacao
     * @return
     */
    public static native int DefinirEntradasMudancaOffLineComBiometria(int Teclado, int Leitor1, int Leitor2, int Verificacao, int Identificacao);

    /**
     * Liga led vermelho InnerAcesso
     *
     * @param Inner
     * @return
     */
    public static native int LigarLedVermelho(int Inner);

    /**
     * Desligar led vermelho InnerAcesso
     *
     * @param Inner
     * @return
     */
    public static native int DesligarLedVermelho(int Inner);

    /**
     * MÃ©todo que retorna os segundos do Sistema.
     *
     * @return
     */

    public static native int ConfigurarAjustesSensibilidadeBio(int Inner, byte Ganho, byte Brilho, byte Contraste);
   
    public static native int ConfigurarAjustesQualidadeBio(int Inner, byte Registro, byte Verificacao);
    
    public static native int ConfigurarAjustesSegurancaBio(int Inner, byte Identificacao, byte Verificacao);
    
    public static native int ConfigurarCapturaAdaptativaBio(int Inner, byte Capturar, byte Total, byte Tempo);
    
    public static native int ConfigurarFiltroBio(int Inner, byte Habilitar);
    
    public static native int ConfigurarTimeoutIdentificacao(byte TimeoutIdentificacao);

    public static native int ConfigurarNivelLFD(byte NivelLFD);    
    
    public static native int EnviarAjustesBio(int Inner);

    public static native int DefinirMensagemApresentacaoEntrada(int ExibirData, String msg);
    
    public static native int DefinirMensagemApresentacaoSaida(int ExibirData, String msg);

    public static native int InserirHorarioMudancaEntrada(byte Hora1, byte Minuto1, byte Hora2, byte Minuto2, byte Hora3, byte Minuto3);
    
    public static native int InserirHorarioMudancaSaida(byte Hora1, byte Minuto1, byte Hora2, byte Minuto2, byte Hora3, byte Minuto3);
    
    public static native int EnviarBufferEventosMudancaAuto(int Inner);
    
    ///Novos comandos versÃ£o 6xx 4.0.0
    public static native int ReceberVersaoFirmware6xx(int Inner, int[] Versao);
    
    public static native int ReceberConfiguracoesInner(int Inner, byte[] ConfiguracoesInner);

    public static native int RequisitarQuantidadeUsuariosBio(int Inner, int TipoComBio);

    public static native int RespostaQuantidadeUsuariosBio(int Inner, int[] QtdUsuariosBio);

    public static native int RequisitarListarUsuariosBio(int Inner, int TipoComBio, int NumPacote);

    public static native int RespostaListarUsuariosBio(int Inner, int[] QtdPacotes, int[] Tamanho);

    public static native int ReceberListaPacUsuariosBio(int Inner, byte[] ListaUsuarios, int TamanhoBuffer);

    public static native int EnviarDigitalUsuarioBio(int Inner, int TipoModBio, String Usuario, byte[] Digital1, byte[] Digital2);

    public static native int RespostaEnviarDigitalUsuarioBio(int Inner);

    public static native int RequisitarModeloBio(int Inner, int TipoComBio);

    public static native int RespostaModeloBio(int Inner, byte[] ModeloBio);

    public static native int RequisitarVersaoBio(int Inner, int TipoComBio);

    public static native int RespostaVersaoBio(int Inner, byte[] VersaoBio);

    public static native int RequisitarExcluirUsuarioBio(int Inner, int TipoComBio, String Usuario);

    public static native int RespostaExcluirUsuarioBio(int Inner);

    public static native int RequisitarExcluirTodosUsuariosBio(int Inner, int TipoComBio);

    public static native int RespostaExcluirTodosUsuariosBio(int Inner);

    public static native int RequisitarUsuarioCadastradoBio(int Inner, int TipoModBio, String Usuario);

    public static native int RespostaUsuarioCadastradoBio(int Inner, int[] TamanhoReceber);

    public static native int ReceberDigitalUsuarioBio(int Inner, byte[] BufferTemplate, int TamanhoReceber);

    public static native int RequisitarEnviarAjustesBio(int Inner, int TipoModBio, byte[] AjustesBio);

    public static native int RespostaEnviarAjustesBio(int Inner);

    public static native int RequisitarVerificarDigitalBio(int Inner, int TipoModBio, String Usuario);

    public static native int RespostaVerificarDigitalBio(int Inner);

    public static native int RequisitarIdentificarUsuarioLeitorBio(int Inner, int TipoModBio);

    public static native int RespostaIdentificarUsuarioLeitorBio(int Inner, byte[] UsuarioIdentificado);

    public static native int RequisitarReceberTemplateLeitorInnerBio(int Inner, int TipoModBio);

    public static native int RespostaReceberTemplateLeitorInnerBio(int Inner, int[] Tamanho);  

    public static native int ReceberTemplateLeitorInnerBio(int Inner, byte[] TemplateRecebido, int Tamanho);

    public static native int RequisitarVerificarCadastroUsuarioBio(int Inner, int TipoModBio, String Usuario);

    public static native int RespostaVerificarCadastroUsuarioBio(int Inner);

    public static native int RequisitarHabilitarIdentificacaoVerificacao(int Inner, int TipoModBio, int Identificacao, int Verificacao);

    public static native int RespostaHabilitarIdentificacaoVerificacao(int Inner);
    
    public static native int LigarLedVerde(int Inner);
    
    public static native int DesabilitarRele1(int Inner);
    
    public static native int DesabilitarRele2(int Inner);
    
    public static native int ReceberDadosOnLineComLetras(int Inner, int[] Dados, StringBuffer Cartao);
    
    public static native int ReceberDadosOnLine_QRCodeComLetras(int Inner, int[] Dados, StringBuffer Cartao);
    
    public static native int ReceberDadosOnLineQRCodeComLetras(int Inner, int[] Dados, StringBuffer Cartao);
    
    public static native int EnviarSinalizacao (int Inner, int TipoSinalizacao, int ModoSinalizacao, int Terminal);

    public static native int DesabilitarWebServer (int Desabilita);

    public static native int DefinirSensorPortaOffline (int Logica);

    public static native int DesabilitarBipColetor (int Desabilita);

    public static native int ConfigurarBotaoExternoOffline (int Funcao);

    public static native int ReceberVersaoFirmware6xx_ComComplementar(int Inner, int[] Versao);
}
