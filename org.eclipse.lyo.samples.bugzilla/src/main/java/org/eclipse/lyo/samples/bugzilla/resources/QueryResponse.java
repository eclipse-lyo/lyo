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
import java.util.ArrayList;
import java.util.Collection;

import thewebsemantic.Id;
import thewebsemantic.Namespace;
import thewebsemantic.RdfProperty;
import thewebsemantic.RdfType;

/**
 * An OSLC query response.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
@Namespace("http://www.bugzilla.org/rdf#")
@RdfType("QueryResponse")
public class QueryResponse {
	@Id
	private URI uri;
	
	public URI getUri() {
		return uri;
	}

	public void setUri(URI uri) {
		this.uri = uri;
	}

	@RdfProperty("http://www.w3.org/2000/01/rdf-schema#member")
	private Collection<ChangeRequest> members = new ArrayList<ChangeRequest>();

	public Collection<ChangeRequest> getMembers() {
		return members;
	}
}
