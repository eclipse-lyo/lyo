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
package org.eclipse.lyo.oslc4j.core;

import java.util.Map;

/**
 * Marker interface applied to {@link Map} (parametrised with &lt;String, Object&gt;) to
 * indicate that when selecting properties for output all immediate,
 * resource properties of the resource should be output with entries
 * in the {@link NestedWildcardProperties#commonNestedProperties()}
 */
public interface NestedWildcardProperties
{
	/**
	 * @return map of all member properties of nested resources to be
	 * output
	 */
	Map<String, Object> commonNestedProperties();
}
