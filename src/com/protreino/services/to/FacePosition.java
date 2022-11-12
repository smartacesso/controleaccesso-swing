package com.protreino.services.to;

import Luxand.FSDK.TFacePosition.ByReference;

public class FacePosition {
	
	public int w;
	public int xc;
	public int yc;
	
	public FacePosition() {
	}
	
	public FacePosition(ByReference byReference) {
		this.w = byReference.w;
		this.xc = byReference.xc;
		this.yc = byReference.yc;
	}

	public int getW() {
		return w;
	}

	public void setW(int w) {
		this.w = w;
	}

	public int getXc() {
		return xc;
	}

	public void setXc(int xc) {
		this.xc = xc;
	}

	public int getYc() {
		return yc;
	}

	public void setYc(int yc) {
		this.yc = yc;
	}

}
