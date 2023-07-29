package com.protreino.services.exceptions;

public class ErrorOnSendLogsToWebException  extends RuntimeException {

    public ErrorOnSendLogsToWebException(String message) {
        super(message);
    }
}
