/*******************************************************************************
 * Copyright (c) 2011, 2014 IBM Corporation.
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
 *     Michael Fiedler     - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.client.exception.exception;


/**
 * Exception indicating a Jazz authentication or credentials problem
 *
 */
@SuppressWarnings("serial")
public class ResourceNotFoundException extends OslcClientApplicationException {

	private static final String MESSAGE_KEY = "ResourceNotFoundException";

	private final String resource;
	private final String value;

	public ResourceNotFoundException(final String resource, final String value) {
		super(MESSAGE_KEY, new Object[] {resource, value});
		this.resource = resource;
		this.value = value;
	}

	public String getResource() {
		return resource;
	}

	public String getValue() {
		return value;
	}


}
