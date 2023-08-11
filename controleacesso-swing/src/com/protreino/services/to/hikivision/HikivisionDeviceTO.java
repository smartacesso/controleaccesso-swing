package com.protreino.services.to.hikivision;

import java.util.List;

public class HikivisionDeviceTO {

	private SearchResult SearchResult;
	
	public SearchResult getSearchResult() {
		return SearchResult;
	}

	public void setSearchResult(SearchResult searchResult) {
		SearchResult = searchResult;
	}
	
    public static class SearchResult {
    	
    	private List<MatchList> MatchList;
    	private Integer numOfMatches;
    	private Integer totalMatches;

		public List<MatchList> getMatchList() {
			return MatchList;
		}

		public void setMatchList(List<MatchList> matchList) {
			MatchList = matchList;
		}

		public Integer getNumOfMatches() {
			return numOfMatches;
		}

		public void setNumOfMatches(Integer numOfMatches) {
			this.numOfMatches = numOfMatches;
		}

		public Integer getTotalMatches() {
			return totalMatches;
		}

		public void setTotalMatches(Integer totalMatches) {
			this.totalMatches = totalMatches;
		}
    }
    
    public static class MatchList {
    	private Device Device;

		public Device getDevice() {
			return Device;
		}

		public void setDevice(Device device) {
			Device = device;
		}
    }
    
    public static class Device {
    	private EhomeParams EhomeParams;
        private Boolean ISAPIPortBound;
        private String devIndex;
        private String devMode;
        private String devName;
        private String devStatus;
        private String devType;
        private String devVersion;
        private Integer offlineHint;
        private String protocolType;
        private Integer videoChannelNum;
        
        public EhomeParams getEhomeParams() {
    		return EhomeParams;
    	}
    	public void setEhomeParams(EhomeParams ehomeParams) {
    		EhomeParams = ehomeParams;
    	}
    	public Boolean getISAPIPortBound() {
    		return ISAPIPortBound;
    	}
    	public void setISAPIPortBound(Boolean iSAPIPortBound) {
    		ISAPIPortBound = iSAPIPortBound;
    	}
    	public String getDevIndex() {
    		return devIndex;
    	}
    	public void setDevIndex(String devIndex) {
    		this.devIndex = devIndex;
    	}
    	public String getDevMode() {
    		return devMode;
    	}
    	public void setDevMode(String devMode) {
    		this.devMode = devMode;
    	}
    	public String getDevName() {
    		return devName;
    	}
    	public void setDevName(String devName) {
    		this.devName = devName;
    	}
    	public String getDevStatus() {
    		return devStatus;
    	}
    	public void setDevStatus(String devStatus) {
    		this.devStatus = devStatus;
    	}
    	public String getDevType() {
    		return devType;
    	}
    	public void setDevType(String devType) {
    		this.devType = devType;
    	}
    	public String getDevVersion() {
    		return devVersion;
    	}
    	public void setDevVersion(String devVersion) {
    		this.devVersion = devVersion;
    	}
    	public Integer getOfflineHint() {
    		return offlineHint;
    	}
    	public void setOfflineHint(Integer offlineHint) {
    		this.offlineHint = offlineHint;
    	}
    	public String getProtocolType() {
    		return protocolType;
    	}
    	public void setProtocolType(String protocolType) {
    		this.protocolType = protocolType;
    	}
    	public Integer getVideoChannelNum() {
    		return videoChannelNum;
    	}
    	public void setVideoChannelNum(Integer videoChannelNum) {
    		this.videoChannelNum = videoChannelNum;
    	}
    }
    
    public static class EhomeParams {
    	private String EhomeID;

		public String getEhomeID() {
			return EhomeID;
		}

		public void setEhomeID(String ehomeID) {
			EhomeID = ehomeID;
		}
    }

}
