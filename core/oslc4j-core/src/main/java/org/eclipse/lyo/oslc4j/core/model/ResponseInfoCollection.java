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
import java.util.Collection;
import java.util.Map;

/**
 * Concrete ResponseInfo collection wrapper where resource are sent
 * as an {@link Collection}
 */
public class ResponseInfoCollection<T extends Object> extends ResponseInfo<Collection<T>>
{
	public
	ResponseInfoCollection(
		Collection<T> collection,
		Map<String, Object> properties,
		Integer totalCount,
		String nextPage
	)
	{
		super(collection, properties, totalCount, nextPage);
	}
	
	public
	ResponseInfoCollection(
		Collection<T> collection,
		Map<String, Object> properties,
		Integer totalCount,
		URI nextPage
	)
	{
		super(collection, properties, totalCount, nextPage);
	}
	
	public Collection<T>
	collection() { return resource(); }
}
