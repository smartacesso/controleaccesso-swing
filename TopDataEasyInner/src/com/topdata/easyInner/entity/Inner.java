//******************************************************************************
//A Topdata Sistemas de Automação Ltda não se responsabiliza por qualquer
//tipo de dano que este software possa causar, este exemplo deve ser utilizado
//apenas para demonstrar a comunica��o com os equipamentos da linha
//inner e não deve ser alterado, por este motivo ele não deve ser incluso em
//suas aplicações comerciais.
//
//Desenvolvido em Java.
//                                           Topdata Sistemas de Automação Ltda.
//******************************************************************************.
package com.topdata.easyInner.entity;

import com.topdata.easyInner.enumeradores.Enumeradores;

public class Inner {
   
    public int PadraoCartao;
    public int CountTentativasEnvioComando;
    public int CountRepeatPingOnline;
    public int Verificacao;
    public int Identificacao;
    public int CountPingFail;
    public int Numero;
    public int QtdDigitos;
    public int CntDoEvents;
    public int TipoLeitor;
    public int ValorLeitor1;
    public int ValorLeitor2;
    public int TentativasColeta;
    public int Porta;
    public int Acionamento;
    public int TipoConexao;
    public int Tentativas;
    public int BilhetesAReceber;
    public int TipoComBio;
    public int TimeOutAjustes;
    public int NivelLFD;
    public int DedoDuplicado;

    public long TempoInicialPingOnLine;
    public long TempoColeta;
    public long Temporizador;
    public long TempoInicialMensagem;
    public long VariacaoInner;

    public Enumeradores.EstadosTeclado EstadoTeclado;
    public Enumeradores.EstadosInner EstadoAtual;
    public Enumeradores.EstadosInner EstadoSolicitacaoPingOnLine;

    public boolean DoisLeitores;
    public boolean Catraca;
    public boolean Biometrico;
    public boolean Teclado;
    public boolean Lista;
    public boolean ListaBio;
    public boolean InnerNetAcesso;
    public boolean rele;
    public boolean CatInvertida;
    public boolean Horarios;
    public boolean DuasDigitais;
    public boolean CartMaster;

    public String LinhaInner;
    public String VersaoInner;
    public String ModeloBioInner;
    public String VersaoBio;
    public String CartaoTemplateLeitor;
    public static String CaminhoDados;
    public String Master;
    
    public Bilhete BilheteInner;
    public VersaoFirware VersaoFW;
    
    /*Initializing/Assigning pointers. (Dynamic) */
    public Inner()
    {
        BilheteInner = new Bilhete();
        VersaoFW = new VersaoFirware();
    }
}
