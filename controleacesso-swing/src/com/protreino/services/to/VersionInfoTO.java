package com.protreino.services.to;

public class VersionInfoTO {
	
	private Double version;
	private String downloadUrl;
	private String description;
	private Boolean fullInstaller;
	
	public VersionInfoTO(){
	}

	public Double getVersion() {
		return version;
	}

	public void setVersion(Double version) {
		this.version = version;
	}

	public String getDownloadUrl() {
		return downloadUrl;
	}

	public void setDownloadUrl(String downloadUrl) {
		this.downloadUrl = downloadUrl;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Boolean getFullInstaller() {
		return fullInstaller;
	}

	public void setFullInstaller(Boolean fullInstaller) {
		this.fullInstaller = fullInstaller;
	}

}
