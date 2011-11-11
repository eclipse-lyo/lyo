/*******************************************************************************
 * Copyright (c) 2011 IBM Corporation.
 *
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the Eclipse Public License v1.0
 *  and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *  
 *  The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 *  and the Eclipse Distribution License is available at
 *  http://www.eclipse.org/org/documents/edl-v10.php.
 *  
 *  Contributors:
 *  
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.samples.bugzilla.resources;

import org.eclipse.lyo.samples.bugzilla.exception.RestException;

import thewebsemantic.Namespace;
import thewebsemantic.RdfProperty;
import thewebsemantic.RdfType;

/**
 * Encapsulates an OSLC-CM 2.0 error response.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 * @see <a href="http://open-services.net/bin/view/Main/OslcCoreSpecification?sortcol=table;up=#Error_Responses">OSLC 2.0 Error Responses</a>
 */
@Namespace("http://open-services.net/ns/core#")
@RdfType("Error")
public class Error {
	@RdfProperty("http://open-services.net/ns/core#statusCode")
	private int statusCode;
	@RdfProperty("http://open-services.net/ns/core#message")
	private String message;
	
	public Error() {}
	
	public static Error fromRestException(RestException e) {
		Error error = new Error();
		error.setMessage(e.getMessage());
		error.setStatusCode(e.getStatusCode());
		
		return error;
	}
	
	public Error(int statusCode, String message) {
		this.statusCode = statusCode;
		this.message = message;
	}
	
	public int getStatusCode() {
		return statusCode;
	}
	
	public void setStatusCode(int statusCode) {
		this.statusCode = statusCode;
	}
	
	public String getMessage() {
		return message;
	}
	
	public void setMessage(String message) {
		this.message = message;
	}
}