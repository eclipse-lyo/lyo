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

import java.net.URI;

import thewebsemantic.Id;
import thewebsemantic.Namespace;
import thewebsemantic.RdfProperty;

/**
 * A FOAF Person.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 * @see <a href="http://xmlns.com/foaf/spec/">FOAF Vocabulary Specification</a>
 */
@Namespace("http://xmlns.com/foaf/0.1/")
public class Person {
	private URI uri = null;
	private String name = null;
	@RdfProperty("http://xmlns.com/foaf/0.1/mbox")
	private String email = null;

	@Id
	public URI getUri() {
		return uri;
	}

	public void setUri(URI uri) {
		this.uri = uri;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}
}
