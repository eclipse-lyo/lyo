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
 *	   Russell Boykin		- initial API and implementation
 *	   Alberto Giammaria	- initial API and implementation
 *	   Chris Peters			- initial API and implementation
 *	   Gianluca Bernardini	- initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.exception;

import java.net.URI;

public final class OslcCoreRelativeURIException extends OslcCoreApplicationException {
	private static final long serialVersionUID = -1238625637837216499L;

	private static final String MESSAGE_KEY = "RelativeURIException";

	private final String   methodName;
	private final URI	   relativeURI;
	private final Class<?> resourceClass;

	public OslcCoreRelativeURIException(final Class<?> resourceClass, final String methodName, final URI relativeURI) {
		super(MESSAGE_KEY, new Object[] {resourceClass.getName(), methodName, relativeURI.toString()});

		this.methodName	   = methodName;
		this.relativeURI   = relativeURI;
		this.resourceClass = resourceClass;
	}

	public String getMethodName() {
		return methodName;
	}

	public URI getRelativeURI() {
		return relativeURI;
	}

	public Class<?> getResourceClass() {
		return resourceClass;
	}
}