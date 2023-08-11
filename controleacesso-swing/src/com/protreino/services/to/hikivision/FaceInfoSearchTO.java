package com.protreino.services.to.hikivision;

import java.util.List;

public class FaceInfoSearchTO {

	public FaceInfoSearch FaceInfoSearch;
	
	public class FaceInfoSearch{
	    public String responseStatusStrg;
	    public Object numOfMatches;
	    public Object totalMatches;
	    public List<MatchList> MatchList;
	}

	public class MatchList{
	    public String faceURL;
	    public String FPID;
	}

}
