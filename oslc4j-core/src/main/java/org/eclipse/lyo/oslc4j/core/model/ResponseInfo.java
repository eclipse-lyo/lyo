/*******************************************************************************
 * Copyright (c) 2012, 2013 IBM Corporation.
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
 *	   Steve Pitschke - initial API and implementation
 *******************************************************************************/

package org.eclipse.lyo.oslc4j.core.model;

import java.net.URI;
import java.util.Map;

/**
 * Wrapper for a collection or resources returned from an HTTP GET
 * to wrap that collection in a oslc:ResponseInfo element
 */
public abstract class ResponseInfo<T extends Object>
	extends FilteredResource<T>
{
	public
	ResponseInfo(
		T resource,
		Map<String, Object> properties,
		Integer totalCount,
		String nextPage
	)
	{
		super(resource, properties);
		
		this.totalCount = totalCount;
		this.nextPage = nextPage;
		this.container = new FilteredResource<T>(resource, properties);
	}
	
	public
	ResponseInfo(
		T resource,
		Map<String, Object> properties,
		Integer totalCount,
		URI nextPage
	)
	{
		this(resource, properties, totalCount,
			 nextPage == null ? null : nextPage.toString());
		this.container = new FilteredResource<T>(resource, properties);
	}
	
	public Integer
	totalCount() { return totalCount; }
	
	public String
	nextPage() { return nextPage; }
	
	private final Integer totalCount;
	private final String nextPage;
	private FilteredResource<T> container;
	
	public FilteredResource<T> getContainer() {
		return container;
	}
}
