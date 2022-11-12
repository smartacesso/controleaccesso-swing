package com.topdata.easyInner.utils;

import com.topdata.easyInner.enumeradores.Enumeradores;

/**
 *
 * @author juliano.ezequiel
 */
public class EasyInnerUtils {

    private static long Timeout;
    /**
     * Converte de Byte para HEX
     *
     * @param P
     * @return
     */
    public static String ArrayByteToHex(byte[] ValorByte, int indexBuff, int LengthBuff) {
        int iVlrAsc = 0;
        StringBuffer sSaida = new StringBuffer();
        int i = 0;

        for (int index = indexBuff; index < indexBuff + LengthBuff; index++) {
            iVlrAsc = (ValorByte[index] < 0 ? (ValorByte[index] + 256) : ValorByte[index]);

            sSaida.append(completaString(Integer.toHexString(iVlrAsc), 2, "0"));
        }

        return (sSaida.toString());
    }

    /**
     * Completa string de acordo com os parâmetros enviados
     *
     * @param var1
     * @param Len
     * @param complemento
     * @return
     */
    public static String completaString(String var1, int Len, String complemento) {
        while (var1.length() < Len) {
            var1 = complemento + var1;
        }
        return (var1);
    }
    
    public static byte[] HexToByteArray(String ValorHex, int TamanhoArray, int Total){
        byte[] ValorByte = new byte[TamanhoArray];
        int ibyte = 0;
        for (int index = 0; index < Total - 1; index += 2) {
            ValorByte[ibyte] = (byte) Long.parseLong(ValorHex.substring(index, index + 2), 16);
            ibyte++;
        }
        return ValorByte;
    }
    public static int HexToDecimal(Byte ValorHex)
    {
        String valor = ValorHex.toString();
        int valorInt = (int)Long.parseLong(valor, 16);
        return valorInt;
    }

    /**
     * Seta Timeout Bio
     */
    public static void setarTimeoutBio() {
        Timeout = System.currentTimeMillis() + 7000;
    }
    
    /**
     * Espera Resposta Bio
     *
     * @param Retorno
     * @return
     * @throws InterruptedException
     */
    public static boolean isEsperaRespostaBio(int Retorno) throws InterruptedException {
        Thread.sleep(300);
        return ((Retorno != Enumeradores.RET_COMANDO_OK) && (System.currentTimeMillis() <= Timeout));
    }
    
    /**
     * Remove zeros a esquerda
     * @param valor
     * @return 
     */
    public static String remZeroEsquerda(String valor) {
        boolean blnNum = false;
        String str = new String();
        for (int i = 0; i < valor.length(); i++) {
            if (!"0".equals(valor.substring(i, i + 1))) {
                blnNum = true;
            }
            if (blnNum) {
                str += valor.substring(i, i + 1);
            }
        }
        return str;
    }
    /**
     * Realiza a conversão Binário para Decimal
     *
     * @param valorBinario
     * @return
     */
    public static int BinarioParaDecimal(String valorBinario) {
        int length_bin = 0, aux = 0, retorno = 0, i;
        length_bin = valorBinario.length();

        for (i = 0; i < length_bin; i++) {
            aux = Integer.parseInt(valorBinario.substring(i, i + 1));
            retorno += aux * (int) Math.pow(2, (length_bin - i)) / 2;
        }
        return (retorno);
    }
    public static String BCDtoString(byte bcd) {
        StringBuffer sb = new StringBuffer();

        byte high = (byte) (bcd & 0xf0);
        high >>>= (byte) 4;
        high = (byte) (high & 0x0f);
        byte low = (byte) (bcd & 0x0f);

        sb.append(high);
        sb.append(low);

        return sb.toString();
    }
    public static String BCDToDecimalSub(byte[] ValorBCD, int indexValor, int TamanhoValor) {
        StringBuffer sb = new StringBuffer();  
        for(int index = indexValor; index < indexValor + TamanhoValor; index++)
        {
            Integer vl = ValorBCD[index] - 1;
            String st = BCDtoString(vl.byteValue());
            sb.append(st);
        }
        return sb.toString();
    }
    
    public static String ASCIIToDecimal(byte[] ValorArray)
    {
        StringBuilder ValorRet = new StringBuilder();
        for(int index = 0; index < ValorArray.length; index++){
            ValorRet.append(new Character((char) ValorArray[index]).toString());
        }
        return ValorRet.toString();
    }
    
    public native long GetTickCount();

    /**
     * Métodos Auxiliares para recuperar os segundos da máquina e para Sleep em
     * Milisegundos..
     *
     * @param ms
     * @return
     */
    public native long Sleep(long ms);
}
