package com.protreino.services.to.hikivision;

public class UserEditTo {

    /**
     * optional, request URL
     */
    public String requestURL;

    /**
     * required, status code
     */
    public int statusCode;

    /**
     * required, status description
     */
    public String statusString;

    /**
     * required, sub status code
     */
    public String subStatusCode;

    /**
     * optional, error code (required when statusCode != 1)
     */
    public Integer errorCode;

    /**
     * optional, error message (required when statusCode != 1)
     */
    public String errorMsg;

    /**
     * optional, reboot is required: 1=yes, other=no
     */
    public Integer rebootRequired;

    // GETTERS & SETTERS

    public String getRequestURL() {
        return requestURL;
    }

    public void setRequestURL(String requestURL) {
        this.requestURL = requestURL;
    }

    public int getStatusCode() {
        return statusCode;
    }

    public void setStatusCode(int statusCode) {
        this.statusCode = statusCode;
    }

    public String getStatusString() {
        return statusString;
    }

    public void setStatusString(String statusString) {
        this.statusString = statusString;
    }

    public String getSubStatusCode() {
        return subStatusCode;
    }

    public void setSubStatusCode(String subStatusCode) {
        this.subStatusCode = subStatusCode;
    }

    public Integer getErrorCode() {
        return errorCode;
    }

    public void setErrorCode(Integer errorCode) {
        this.errorCode = errorCode;
    }

    public String getErrorMsg() {
        return errorMsg;
    }

    public void setErrorMsg(String errorMsg) {
        this.errorMsg = errorMsg;
    }

    public Integer getRebootRequired() {
        return rebootRequired;
    }

    public void setRebootRequired(Integer rebootRequired) {
        this.rebootRequired = rebootRequired;
    }
}
