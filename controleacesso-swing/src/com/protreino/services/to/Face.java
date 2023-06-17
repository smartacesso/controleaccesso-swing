package com.protreino.services.to;

import Luxand.FSDK.TFacePosition.ByReference;

public class Face {
	
	private Long identifier;
	private String name;
	private FacePosition facePosition;
	private byte[] bytes;
	
	public Face() {
	}
	
	public Face(ByReference byReference, byte[] bytes) {
		this.facePosition = new FacePosition(byReference);
		this.bytes = bytes;
	}
	
	public Long getIdentifier() {
		return identifier;
	}
	
	public void setIdentifier(Long identifier) {
		this.identifier = identifier;
	}
	
	public String getName() {
		return name;
	}
	
	public void setName(String name) {
		this.name = name;
	}
	
	public FacePosition getFacePosition() {
		return facePosition;
	}
	
	public void setFacePosition(FacePosition facePosition) {
		this.facePosition = facePosition;
	}

	public byte[] getBytes() {
		return bytes;
	}

	public void setBytes(byte[] bytes) {
		this.bytes = bytes;
	}

}
