/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
 *
 * See the NOTICE file(s) distributed with this work for additional
 * information regarding copyright ownership.
 *
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0, or the Eclipse Distribution License 1.0
 * which is available at http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause
 */

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
