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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Simple
 */

package org.eclipse.lyo.oslc4j.core.model;

import java.net.URI;
import java.util.Map;

/**
 * Concrete ResponseInfo collection wrapper where resource are sent
 * as an array
 */
public class ResponseInfoArray<T extends Object> extends ResponseInfo<T[]>
{
	public
	ResponseInfoArray(
		T[] array,
		Map<String, Object> properties,
		Integer totalCount,
		String nextPage
	)
	{
		super(array, properties, totalCount, nextPage);
	}
	
	public
	ResponseInfoArray(
		T[] array,
		Map<String, Object> properties,
		Integer totalCount,
		URI nextPage
	)
	{
		super(array, properties, totalCount, nextPage);
	}
	
	public T[]
	array() { return resource(); }
}
