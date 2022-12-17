package com.protreino.services.entity;

import com.sun.jna.Library;
import com.sun.jna.Memory;
import com.sun.jna.ptr.ByteByReference;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.LongByReference;

public interface Aso15DefJNA extends Library {
		
	public final int RES_OK = 0;
	public final int IS_EMPTY_ID = 1;
	public final int IS_EMPTY_FINGER = 1;
	public final int IS_MANAGER = 1;
	public final int IS_USER = 0;

	public final int ERR_MEMORY = -1;
	public final int ERR_INVALID_ID = -2;
	public final int ERR_INVALID_TEMPLATE = -3;
	public final int ERR_NOT_EMPTY = -4;
	public final int ERR_DISK_SIZE = -5;
	public final int ERR_NOT_USBDEV = -6;
	public final int ERR_INVALID_PARAMETER = -7;
	public final int ERR_FAIL_INIT_ENGINE = -8;
	public final int ERR_FAIL_MATCH_PROC = -9;
	public final int ERR_FP_TIMEOUT = -10;
	public final int ERR_BAD_QUALITY = -11;
	public final int ERR_FAIL_GEN = -12;
	public final int ERR_FAIL_FILE_IO = -13;
	public final int ERR_ENROLLED_ID = -14;
	public final int ERR_ENROLLED_FINGER = -15;
	public final int ERR_INVALID_FINGERNUM = -16;
	public final int ERR_FAIL_MATCH = -17;
	public final int ERR_NOT_ENROLLED = -18;
	public final int ERR_FAIL_SET = -19;
	public final int ERR_DUPLICATED = -20;
	public final int ERR_CANCELED = -21;
	public final int ERR_UNKNOWN = -30;

	public final int SFEPDB_REG_MAX = 100000;
	public final int MAX_IDNUMBER = SFEPDB_REG_MAX;
	public final int MAX_FPNUMBER = 10;

	public final int SFEP_UFPDATA_SIZE = 498;

	public final int IMAGE_WIDTH = 600;
	public final int IMAGE_HEIGHT = 400;

	public final int IMAGE_FULL_WIDTH = 636;
	public final int IMAGE_FULL_HEIGHT = 478;
	
	int SFEP_Initialize();
	int SFEP_Uninitialize();
	int SFEP_SetDatabasePath(String szPath);
	int SFEP_SetConfig(int hWnd);
	int SFEP_SetBrightness(byte bBVal);
	int SFEP_GetBrightness(Byte pbBVal);
	int SFEP_GetLiveImage();
	int SFEP_CalcBrightness();
	int SFEP_CalcBrightnessInFullImage();
	int SFEP_CurrentSaveBMP(String szFileName, int nWidth, int nHeight);
	boolean SFEP_IsFingerPress();
	int SFEP_CaptureFingerImage(int dwTimeOut);
	void SFEP_FpCancel();
	int SFEP_CreateTemplate(Memory pTemplate);
	int SFEP_GetTemplateForRegister(Memory memorySpaceStTemplates, Memory memorySpaceStRegTem);
	int SFEP_Enroll(Memory pTemplate, LongByReference pdwID, ByteByReference dedo, ByteByReference user);
	int SFEP_Identify(Memory pTemplate, IntByReference pdwID, ByteByReference fingerNum, ByteByReference manager, byte bSecLevel);
	int SFEP_Verify(Memory pTemplate, int dwID, byte bFingerNum, byte bSecLevel);
	int SFEP_Match2Template(Memory pTemplate1, Memory pTemplate2, byte bSecLevel);
	int SFEP_RemoveTemplate(int dwID, byte bFingerNum);
	int SFEP_RemoveAll();
	int SFEP_ReadTemplate(int dwID, byte bFingerNum, String szFileName);
	int SFEP_WriteTemplate(int dwID, byte bFingerNum, byte bManager, String szFileName);
	int SFEP_GetEnrollCount();
	int SFEP_CheckID(int dwID);
	int SFEP_CheckFingerNum(int dwID, byte bFingerNum);
	int SFEP_SearchID(Integer pdwID);
	int SFEP_SearchFingerNumber(int pdwID, Byte pbFingerNum);
	int SFEP_SetCameraType(Byte pbType);
}

