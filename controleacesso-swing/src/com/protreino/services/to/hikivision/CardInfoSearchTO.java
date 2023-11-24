package com.protreino.services.to.hikivision;

import java.util.List;

public class CardInfoSearchTO {

	public CardInfoSearch CardInfoSearch;
	
	public class CardInfo {
	    public String cardNo;
	    public String cardType;
	    public String employeeNo;
	}

	public class CardInfoSearch {
	    public List<CardInfo> CardInfo;
	    public int numOfMatches;
	    public String responseStatusStrg;
	    public String searchID;
	    public int totalMatches;
	}
}
