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
package org.eclipse.lyo.oslc4j.core.model;

import java.net.URI;

public enum Representation {
	Reference(OslcConstants.OSLC_CORE_NAMESPACE + "Reference"),
	Inline(OslcConstants.OSLC_CORE_NAMESPACE + "Inline");
//	Either(OslcConstants.OSLC_CORE_NAMESPACE + "Either"); // Either not supported by OSLC4J

	private String uri;

	Representation(final String uri) {
		this.uri = uri;
	}

	@Override
	public String toString() {
		return uri;
	}

	public static Representation fromString(final String string) {
		final Representation[] values = Representation.values();
		for (final Representation value : values) {
			if (value.uri.equals(string)) {
				return value;
			}
		}
		return null;
	}

	public static Representation fromURI(final URI uri) {
		return fromString(uri.toString());
	}
}
