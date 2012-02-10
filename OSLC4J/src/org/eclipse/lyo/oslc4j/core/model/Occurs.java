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
 *     Russell Boykin       - initial API and implementation
 *     Alberto Giammaria    - initial API and implementation
 *     Chris Peters         - initial API and implementation
 *     Gianluca Bernardini  - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.model;

import java.net.URI;

public enum Occurs {
	ExactlyOne(OslcConstants.OSLC_CORE_ENUM_NAMESPACE + "Exactly-one"),
	ZeroOrOne(OslcConstants.OSLC_CORE_ENUM_NAMESPACE + "Zero-or-one"),
	ZeroOrMany(OslcConstants.OSLC_CORE_ENUM_NAMESPACE + "Zero-or-many"),
	OneOrMany(OslcConstants.OSLC_CORE_ENUM_NAMESPACE + "One-or-many");

	private String uri;

	Occurs(final String uri) {
		this.uri = uri;
	}

	@Override
    public String toString() {
		return uri;
	}

	public static Occurs fromString(final String string) {
	    final Occurs[] values = Occurs.values();
	    for (final Occurs value : values) {
	        if (value.uri.equals(string)) {
	            return value;
	        }
	    }
	    return null;
	}
	
	public static Occurs fromURI(final URI uri) {
		return fromString(uri.toString());
	}
}
