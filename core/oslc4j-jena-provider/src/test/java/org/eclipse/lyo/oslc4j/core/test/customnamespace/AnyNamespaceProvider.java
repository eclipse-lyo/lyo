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

package org.eclipse.lyo.oslc4j.core.test.customnamespace;

import static org.eclipse.lyo.oslc4j.core.test.customnamespace.CustomNamespaceConstants.CUSTOM_PREFIX;
import static org.eclipse.lyo.oslc4j.core.test.customnamespace.CustomNamespaceConstants.CUSTOM_URL;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.lyo.oslc4j.core.model.IOslcCustomNamespaceProvider;

/**
 * Used to test the custom namespace mappings feature.
 * This class just return a custom prefix to ensure that this 
 * prefix will be added when Jena or Json Helper is reading
 * the default annotation mappings.
 * 
 * @author Daniel Figueiredo Caetano
 *
 */
public class AnyNamespaceProvider implements IOslcCustomNamespaceProvider {

	/**
	 * Creates a map with a custom prefix, used for tests.
	 * 
	 * @return always a new map.
	 */
	@Override
	public Map<String, String> getCustomNamespacePrefixes() {
		Map<String, String> customNamespacePrefixes = new HashMap<String, String>(1);
		customNamespacePrefixes.put(CUSTOM_PREFIX, CUSTOM_URL);
		return customNamespacePrefixes;
	}
	
}