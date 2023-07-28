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

public class Bilhete {

    public Bilhete()
    {
        Cartao = new StringBuilder();
    }
    
    //Declaração Tipo de Bilhete
    public int Tipo;
    public int Dia;
    public int Mes;
    public int Ano;
    public int Hora;
    public int Minuto;
    public StringBuilder Cartao;
    public int Origem;
    public int Complemento;
    public int Segundo;
}
