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

/**
 * Encapsulates information about an OSLC query response.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
@Namespace("http://open-services.net/ns/core#")
public class ResponseInfo extends OslcResource {
	@Id
	private URI uri;
	private Integer totalCount;
	private URI nextPage;
	
	public URI getUri() {
		return uri;
	}
	
	public void setUri(URI uri) {
		this.uri = uri;
	}
	
	public Integer getTotalCount() {
		return totalCount;
	}
	
	public void setTotalCount(Integer totalCount) {
		this.totalCount = totalCount;
	}
	
	public URI getNextPage() {
		return nextPage;
	}
	
	public void setNextPage(URI nextPage) {
		this.nextPage = nextPage;
	}
}
