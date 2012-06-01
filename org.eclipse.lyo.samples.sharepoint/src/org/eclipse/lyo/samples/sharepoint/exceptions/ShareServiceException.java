/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *  
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 *     Keith Wells             - initial API and implementation
 *     Sam Padgett           - initial API and Implementation
 *     Jim Conallen           - initial API and implementation
 *
 *******************************************************************************/
package org.eclipse.lyo.samples.sharepoint.exceptions;

import javax.servlet.ServletException;

import org.eclipse.lyo.samples.sharepoint.core.IConstants;





public class ShareServiceException extends ServletException {

	private static final long serialVersionUID = -4695857098965707720L;
	private int status = IConstants.SC_INTERNAL_ERROR; 
	public int getStatus(){
		return status;
	}
	
	public ShareServiceException(Exception e) {
		super(e);
	}

	public ShareServiceException(int status) {
		super();
		this.status = status;
	}

	public ShareServiceException(int status, String msg) {
		super(msg);
		this.status = status;
	}

	public ShareServiceException(int status, Exception e) {
		super(e);
		this.status = status;
	}

	public ShareServiceException(String msg) {
		super(msg);
	}

}
