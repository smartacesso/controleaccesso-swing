package com.protreino.services.to.hikivision;

public class HikivisionUserInfoTO {

	public UserInfoSearch UserInfoSearch;

	public class UserInfoSearch{
	    public String searchID;
	    public String responseStatusStrg;
	    public int numOfMatches;
	    public int totalMatches;
	}
}
