package com.protreino.services.services;

import java.net.ConnectException;
import java.net.HttpURLConnection;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JOptionPane;

import org.apache.commons.codec.binary.Base64;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.protreino.services.to.DetectFaceResult;
import com.protreino.services.to.Face;
import com.protreino.services.to.FeedFrameRequest;
import com.protreino.services.utils.FSDK;
import com.protreino.services.utils.HttpConnection;
import com.protreino.services.utils.Utils;

import Luxand.*;
import Luxand.FSDK.*;
import Luxand.FSDKCam.*;

public class LuxandService {
	
	private static LuxandService uniqueInstance = new LuxandService();
	
	public static final String PATH_FILE_MASKS_BIN = "fd_masks1.bin";
	
    private HTracker tracker = new HTracker();
    public boolean serviceInitialized = false;
    private String serverRecognizerUrl;
	
	private LuxandService() {
	}
	
	public static synchronized LuxandService getInstance() {
		return uniqueInstance;
	}
	
	public void initializeSDK(String key) {
		try {
			//String binPath = System.getProperty("user.dir");
			//System.setProperty("jna.library.path", binPath + "\\bin\\win32_x86");
			
			String url = Utils.getPreference("serverRecognizerIP"); 
			if(url == null || "".equals(url))
				url = "localhost:8080";
			serverRecognizerUrl = "http://"+url + "";
			
			int ret = FSDK.ActivateLibrary(key);
			if (ret != FSDK.FSDKE_OK)
				throw new Exception("Não foi possível ativar a Luxand FaceSDK. Verifique a chave de licença.");
			
			ret = FSDK.Initialize();
			if (ret != FSDK.FSDKE_OK)
				throw new Exception("Não foi possível inicializar a Luxand FaceSDK.");
			
			serviceInitialized = true;
			
	        FSDK.CreateTracker(tracker);
	        
	        Map<String, String> configurationsMap = getConfigurationsMap();
	        
	        ret = FSDK.SetFaceDetectionParameters(Boolean.valueOf(configurationsMap.get("HandleArbitraryRotations")), 
	        		Boolean.valueOf(configurationsMap.get("DetermineFaceRotationAngle")),
	        		Integer.valueOf(configurationsMap.get("InternalResizeWidth")));
        	if (ret != FSDK.FSDKE_OK)
	        	throw new Exception("Não foi possível setar os parâmetros de detecção da Luxand FaceSDK.");
        	
        	ret = FSDK.SetFaceDetectionThreshold(Integer.valueOf(configurationsMap.get("FaceDetectionThreshold")));
        	if (ret != FSDK.FSDKE_OK)
	        	throw new Exception("Não foi possível setar o parâmetro threshold da Luxand FaceSDK.");
        	
        	if(Boolean.valueOf(configurationsMap.get("FacilitateRecognizeWithMask"))) {
        		int [] erros = new int [10];
        		ret = FSDK.SetParameters("FaceDetectionModel="+PATH_FILE_MASKS_BIN+";TrimFacesWithUncertainFacialFeatures=false", erros);
        		if(ret != FSDK.FSDKE_OK)
        			throw new Exception("Não foi possível setar o parâmetro modelo de detecção de faces.");
        	}
	        
        	ret = FSDKCam.SetCameraNaming(true); // UseDevicePathAsName
	        if (ret != FSDK.FSDKE_OK)
	        	throw new Exception("Não foi possível setar o padrão de nomenclatura da Luxand FaceSDK.");
	        
		}
		catch(Exception e) {
			e.printStackTrace();
            JOptionPane.showMessageDialog(null, e.toString().substring(0, 150) + "...", "Erro ao iniciar Luxand FaceSDK", JOptionPane.PLAIN_MESSAGE);
        }
	}
	
	public void FinalizeSDK() {
		if (tracker != null) {
			FSDK.FreeTracker(tracker);
		}
		FSDK.Finalize();
	}
	
	public String[] getCameraDescriptionList() {
		TCameras cameraNameList = new TCameras();
		TCameras cameraDevicePathList = new TCameras();
        int count[] = new int[1];
        
        FSDKCam.InitializeCapturing();
		int ret = FSDKCam.GetCameraListEx(cameraNameList, cameraDevicePathList, count);
		FSDKCam.FinalizeCapturing();
		
		if (ret != FSDK.FSDKE_OK) {
        	JOptionPane.showMessageDialog(null, "Não foi possível retornar lista de nomes das câmeras.", "Erro na Luxand FaceSDK", JOptionPane.PLAIN_MESSAGE);
        	return new String[0];
        }
        
        String[] cameras = new String[count[0]];
        
        if (cameras.length > 0) {
        	for (int i = 0; i < cameras.length; i++) {
            	String cameraId = extractCameraIdFromDevicePath(cameraDevicePathList.cameras[i]);
            	cameras[i] = cameraNameList.cameras[i] + " (" + cameraId + ")";
            }
		}
        else {
        	JOptionPane.showMessageDialog(null, "Nenhuma câmera encontrada.", "Erro na Luxand FaceSDK", JOptionPane.PLAIN_MESSAGE);
        }
        
        return cameras;
	}
	
	public FSDK_VideoFormatInfo.ByValue[] getVideoFormatList(String devicePath){
		FSDK_VideoFormats videoFormatList = new FSDK_VideoFormats();
		int count[] = new int[1];
		
		FSDKCam.InitializeCapturing();
		int ret = FSDKCam.GetVideoFormatList(devicePath, videoFormatList, count);
		FSDKCam.FinalizeCapturing();
		
		if (ret != FSDK.FSDKE_OK)
			return new FSDK_VideoFormatInfo.ByValue[0];
		
		return videoFormatList.formats;
	}
	
	public String getDevicePathFromDeviceId(String cameraId) {
		TCameras cameraNameList = new TCameras();
		TCameras cameraDevicePathList = new TCameras();
        int count[] = new int[1];
        FSDKCam.InitializeCapturing();
		int ret = FSDKCam.GetCameraListEx(cameraNameList, cameraDevicePathList, count);
		FSDKCam.FinalizeCapturing();
		if (ret == FSDK.FSDKE_OK) {
        	for (String s : cameraDevicePathList.cameras) {
            	if (cameraId.equals(extractCameraIdFromDevicePath(s))) {
            		return s;
            	}
            }
        }
        return null;
	}
	
//	public String getDevicePathFromIP(String ipCamera, String usuario, String senha) {
//		if (CameraOpened)
//			FSDKCam.CloseVideoCamera(cameraHandle);
//		else
//			cameraHandle = new HCamera();
//
//		int r = FSDKCam.OpenIPVideoCamera(FSDK.FSDK_VIDEOCOMPRESSIONTYPE.FSDK_MJPEG, ipCamera, usuario, 
//								senha, 5, cameraHandle);
//		
//		if (FSDK.FSDKE_OK !=  r){
//			System.out.println("Falha ao conectar.");
//		}
//		
//		if (!CameraOpened) {
//			CameraOpened = true;
//			drawingTimer.start();
//		}
//		
//		return null;
//	}
	
	boolean testeGrab = false;
	public synchronized HImage grabFrame(HCamera cameraHandle) throws Exception {
		
		if (testeGrab) {
			return null;
		}
		testeGrab = true;
		HImage copyImageHandle = null;
		HImage sharedImageHandle = new HImage();

		int ret = FSDKCam.GrabFrame(cameraHandle, sharedImageHandle);
		
		if (ret != FSDK.FSDKE_OK)
			throw new Exception("Erro no GrabFrame: " + ret);
			
		copyImageHandle = new HImage();
		FSDK.CreateEmptyImage(copyImageHandle);
		
		ret = FSDK.CopyImage(sharedImageHandle, copyImageHandle);
		if (ret != FSDK.FSDKE_OK)
			copyImageHandle = null;
		
		FSDK.FreeImage(sharedImageHandle);
		
		testeGrab = false;
		return copyImageHandle;
	}
	
	public synchronized DetectFaceResult detectFace(HImage imageHandle) {
		DetectFaceResult resultado = new DetectFaceResult();
		
		try {
			TFacePosition.ByReference facePosition = new TFacePosition.ByReference();
			
			int ret = FSDK.DetectFace(imageHandle, facePosition);
			if (ret == FSDK.FSDKE_OK) {
				
				int[] bufferSize = new int[1];
				FSDK.GetImageBufferSize(imageHandle, bufferSize, FSDK_IMAGEMODE.FSDK_IMAGE_COLOR_24BIT);
				
				byte[] buffer = new byte[bufferSize[0]];
				ret = FSDK.SaveImageToBuffer(imageHandle, buffer, FSDK_IMAGEMODE.FSDK_IMAGE_COLOR_24BIT);
				
				if (ret == FSDK.FSDKE_OK) {
					resultado.setFace(new Face(facePosition, buffer));
					resultado.setResultCode(FSDK.FSDKE_OK);
				}
				else {
					resultado.setResultCode(FSDK.FSDKE_IMAGE_TO_BUFFER_ERROR);
				}
				
			}
			else {
				resultado.setResultCode(ret);
			}
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		
		return resultado;
	}
	
	public DetectFaceResult recognize(byte[] image, int imageWidth, int imageHeight) {
		DetectFaceResult resultado = new DetectFaceResult();
		
		try {
			
			List<String> imagesBase64 = new ArrayList<String>();
			imagesBase64.add(Base64.encodeBase64String(image));
			
			FeedFrameRequest feedFrameRequest = new FeedFrameRequest();
			feedFrameRequest.setImageWidth(imageWidth);
			feedFrameRequest.setImageHeight(imageHeight);
			feedFrameRequest.setImagesBase64(imagesBase64);
			
			HttpConnection httpConnection = new HttpConnection(serverRecognizerUrl + "/recognition/recognize");
			int status = httpConnection.sendResponse(new ObjectMapper().writeValueAsString(feedFrameRequest));
			
		    if (status == HttpURLConnection.HTTP_OK) {
		    	String response = httpConnection.getResponseString();
		    	resultado = new ObjectMapper().readValue(response, DetectFaceResult.class);
		    }
		    else if (status == HttpURLConnection.HTTP_UNAUTHORIZED) {
		    	resultado.setResultCode(FSDK.FSDKE_USER_NOT_LOGGED_IN);
		    }
		    else {
		    	System.out.println(httpConnection.getErrorString());
		    	resultado.setResultCode(FSDK.FSDKE_SERVER_EXCEPTION);
		    }
		    
		} catch (ConnectException e) {
			System.err.println("Não foi possível conectar com o servidor");
			resultado.setResultCode(FSDK.FSDKE_LOCAL_EXCEPTION);

		} catch (Exception e) {
			e.printStackTrace();
			resultado.setResultCode(FSDK.FSDKE_LOCAL_EXCEPTION);
		}
		
		return resultado;
	}
	
	public DetectFaceResult enroll(List<byte[]> images, int imageWidth, int imageHeight, String nameToAssign) {
		DetectFaceResult resultado = new DetectFaceResult();
		
		try {
			
			List<String> imagesBase64 = new ArrayList<String>();
			for (byte[] image : images)
				imagesBase64.add(Base64.encodeBase64String(image));
			
			FeedFrameRequest feedFrameRequest = new FeedFrameRequest();
			feedFrameRequest.setImageWidth(imageWidth);
			feedFrameRequest.setImageHeight(imageHeight);
			feedFrameRequest.setNameToAssign(nameToAssign);
			feedFrameRequest.setImagesBase64(imagesBase64);
			
			HttpConnection httpConnection = new HttpConnection(serverRecognizerUrl + "/recognition/enroll");
			int status = httpConnection.sendResponse(new ObjectMapper().writeValueAsString(feedFrameRequest));
			
		    if (status == HttpURLConnection.HTTP_OK) {
		    	String response = httpConnection.getResponseString();
		    	resultado = new ObjectMapper().readValue(response, DetectFaceResult.class);
		    }
		    else {
		    	System.out.println(httpConnection.getErrorString());
		    	resultado.setResultCode(FSDK.FSDKE_SERVER_EXCEPTION);
		    }
		}
		catch (Exception e) {
			e.printStackTrace();
			resultado.setResultCode(FSDK.FSDKE_LOCAL_EXCEPTION);
		}
		
		return resultado;
	}
	
	public String clearName(long identifier) {
		try {
			
			HttpConnection httpConnection = new HttpConnection(serverRecognizerUrl + "/recognition/identifiers/" + identifier);
			httpConnection.setRequestMethod("DELETE");
			
			int status = httpConnection.getResponseCode();
			
			if (status != HttpURLConnection.HTTP_OK) {
				String errorResponse = null;
				if (status != HttpURLConnection.HTTP_UNAUTHORIZED) {
					errorResponse = "Usuário não logado no servidor.";
				}
				else {
					errorResponse = httpConnection.getErrorString();
				}
				System.out.println(errorResponse);
		    	return errorResponse;
		    }
			
		    return "";
		}
		catch (Exception e) {
			e.printStackTrace();
			return e.getMessage();
		}
	}
	
	private String extractCameraIdFromDevicePath(String uniqueDevicePath) {
		return uniqueDevicePath.split("#")[2].split("&")[1].toUpperCase();
	}
	
	private Map<String, String> getConfigurationsMap() {
		
		Map<String, String> configurationsMap = new HashMap<String, String>();
		
		try {
			
			HttpConnection httpConnection = new HttpConnection(serverRecognizerUrl + "/recognition/configurations");
			
			int status = httpConnection.getResponseCode();
			
		    if (status == HttpURLConnection.HTTP_OK) {
		    	String response = httpConnection.getResponseString();
		    	TypeReference<HashMap<String, String>> typeRef = new TypeReference<HashMap<String, String>>() {};
		    	configurationsMap = new ObjectMapper().readValue(response, typeRef);
		    }
		    else {
		    	throw new Exception(httpConnection.getErrorString());
		    }
			
		}
		catch (Exception e) {
			System.err.println("Não foi possível se conectar com o servidor facial.");
			
			// Usa valores padrao
			configurationsMap.put("HandleArbitraryRotations", "false");
			configurationsMap.put("DetermineFaceRotationAngle", "false");
			configurationsMap.put("InternalResizeWidth", "100");
			configurationsMap.put("FaceDetectionThreshold", "5");
			configurationsMap.put("FacilitateRecognizeWithMask", "false");
		}
		
		return configurationsMap;
	}
	
}
