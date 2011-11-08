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
import thewebsemantic.RdfProperty;

/**
 * Some common properties for OSLC resources.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 * @see <a href="http://open-services.net/bin/view/Main/OSLCCoreSpecAppendixA">OSLC Core 2.0 Specification: Common Properties</a>
 */
public abstract class OslcResource {
	@Id
	protected URI uri = null;
	@RdfProperty("http://purl.org/dc/terms/title")
	protected String title = null;
	@RdfProperty("http://purl.org/dc/terms/description")
	protected String description = null;

	public URI getUri() {
		return uri;
	}

	public void setUri(URI uri) {
		this.uri = uri;
	}
	
	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}
}
