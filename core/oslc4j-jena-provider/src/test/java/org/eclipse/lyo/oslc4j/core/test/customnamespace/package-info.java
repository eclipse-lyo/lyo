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

/**
 * Used to test the custom namespce prefix mappings.
 * The namespace provider implementation {@link org.eclipse.lyo.oslc4j.core.test.customnamespace.AnyNamespaceProvider}
 * is passed in the {@link org.eclipse.lyo.oslc4j.core.annotation.OslcSchema#customNamespaceProvider()} 
 * field. 
 * 
 * @author Daniel Figueiredo Caetano
 *
 */
@OslcSchema(
	customNamespaceProvider= AnyNamespaceProvider.class,
	value={
		@OslcNamespaceDefinition(prefix=TEST1_PREFIX,namespaceURI=TEST1_URL),
		@OslcNamespaceDefinition(prefix=TEST2_PREFIX,namespaceURI=TEST2_URL)
	}
)
package org.eclipse.lyo.oslc4j.core.test.customnamespace;

import static org.eclipse.lyo.oslc4j.core.test.customnamespace.CustomNamespaceConstants.TEST1_PREFIX;
import static org.eclipse.lyo.oslc4j.core.test.customnamespace.CustomNamespaceConstants.TEST1_URL;
import static org.eclipse.lyo.oslc4j.core.test.customnamespace.CustomNamespaceConstants.TEST2_PREFIX;
import static org.eclipse.lyo.oslc4j.core.test.customnamespace.CustomNamespaceConstants.TEST2_URL;

import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespaceDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcSchema;