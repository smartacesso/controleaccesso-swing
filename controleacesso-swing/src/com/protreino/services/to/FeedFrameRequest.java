package com.protreino.services.to;

import java.util.List;

public class FeedFrameRequest {
	
	private String nameToAssign;
	private int imageWidth;
	private int imageHeight;
	private List<String> imagesBase64;
	
	public FeedFrameRequest() {
	}

	public String getNameToAssign() {
		return nameToAssign;
	}

	public void setNameToAssign(String nameToAssign) {
		this.nameToAssign = nameToAssign;
	}

	public int getImageWidth() {
		return imageWidth;
	}

	public void setImageWidth(int imageWidth) {
		this.imageWidth = imageWidth;
	}

	public int getImageHeight() {
		return imageHeight;
	}

	public void setImageHeight(int imageHeight) {
		this.imageHeight = imageHeight;
	}

	public List<String> getImagesBase64() {
		return imagesBase64;
	}

	public void setImagesBase64(List<String> imagesBase64) {
		this.imagesBase64 = imagesBase64;
	}

}
