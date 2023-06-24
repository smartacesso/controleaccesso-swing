package com.protreino.services.to.hikivision;

import java.util.List;

public class UserInfoTO {

    public UserInfoOutList UserInfoOutList;

    public class UserInfoOutList{
	    public List<UserInfoOut> UserInfoOut;
	}
    
	public class UserInfoOut{
	    public String employeeNo;
	    public int errorCode;
	    public String errorMsg;
	    public int statusCode;
	    public String statusString;
	    public String subStatusCode;
	}

}
