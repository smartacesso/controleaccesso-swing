package com.protreino.services.utils;

import java.io.BufferedReader;
import java.util.Hashtable;

public class HttpRequestParser {

	private String _requestLine;
    private Hashtable<String, String> _requestHeaders;
    private StringBuffer _messagetBody;

    public HttpRequestParser() {
        _requestHeaders = new Hashtable<String, String>();
        _messagetBody = new StringBuffer();
    }

    
    /**
     * Parse and HTTP request.
     * 
     * @param reader
     * 		Reader holding http request.
     * @throws Exception
     * 		If an I/O error occurs reading the input stream 
     * 		or 
     * 		If HTTP Request is malformed
     */
    public void parseRequest(BufferedReader reader) throws Exception {
    	
    	// Request line
    	String linha = reader.readLine();
    	System.out.println("<-- " + linha);
    	setRequestLine(linha);
    	
    	Boolean isPost = linha.contains("POST") ? true : false;
    	Integer postDataLength = null;
		
    	// Header parameters
		while ((linha = reader.readLine()) != null && (linha.length() != 0)) {
		    //System.out.println("<-- " + linha);
		    appendHeaderParameter(linha);
		    if (linha.toLowerCase().contains("content-length:")) {
		    	postDataLength = new Integer(linha.substring(linha.toLowerCase().indexOf("content-length:") + 16,
		    		linha.length())).intValue();
		    }
		}
		
		// Post data
		String postData = "";
		if (isPost && postDataLength != null && postDataLength > 0) {
		    char[] charArray = new char[postDataLength];
		    reader.read(charArray, 0, postDataLength);
		    postData = new String(charArray);
		    appendMessageBody(postData);
		}
		
		//System.out.println("<-- " + postData);
		System.out.println("----------------------------------------------------");
    	
    }
    

    /**
     * 
     * 5.1 Request-Line The Request-Line begins with a method token, followed by
     * the Request-URI and the protocol version, and ending with CRLF. The
     * elements are separated by SP characters. No CR or LF is allowed except in
     * the final CRLF sequence.
     * 
     * @return String with Request-Line
     */
    public String getRequestLine() {
        return _requestLine;
    }

    private void setRequestLine(String requestLine) throws Exception {
        if (requestLine == null || requestLine.length() == 0) {
            throw new Exception("Invalid Request-Line: " + requestLine);
        }
        _requestLine = requestLine;
    }

    private void appendHeaderParameter(String header) throws Exception {
        int idx = header.indexOf(":");
        if (idx == -1) {
            throw new Exception("Invalid Header Parameter: " + header);
        }
        _requestHeaders.put(header.substring(0, idx), header.substring(idx + 1, header.length()));
    }

    /**
     * The message-body (if any) of an HTTP message is used to carry the
     * entity-body associated with the request or response. The message-body
     * differs from the entity-body only when a transfer-coding has been
     * applied, as indicated by the Transfer-Encoding header field (section
     * 14.41).
     * @return String with message-body
     */
    public String getMessageBody() {
        return _messagetBody.toString();
    }

    private void appendMessageBody(String bodyLine) {
        _messagetBody.append(bodyLine);
    }

    /**
     * For list of available headers refer to sections: 4.5, 5.3, 7.1 of RFC 2616
     * @param headerName Name of header
     * @return String with the value of the header or null if not found.
     */
    public String getHeaderParam(String headerName){
        return _requestHeaders.get(headerName);
    }
    
    
    public Hashtable<String, String> getAllHeaderParam(){
    	return _requestHeaders;
    }
	
}
