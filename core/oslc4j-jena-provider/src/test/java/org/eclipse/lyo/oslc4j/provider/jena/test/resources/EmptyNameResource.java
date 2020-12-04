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
package org.eclipse.lyo.oslc4j.provider.jena.test.resources;

import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;

@OslcName("")
@OslcNamespace(EmptyNameResource.TEST_NAMESPACE)
@OslcResourceShape(title = "Test Resource", describes = EmptyNameResource.TEST_RESOURCE_TYPE)
public class EmptyNameResource extends AbstractResource {
	public final static String TEST_NAMESPACE = "http://example.com/ns#";
	public final static String TEST_RESOURCE_TYPE = TEST_NAMESPACE + "Test";
}
