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
package org.eclipse.lyo.oslc4j.core.test.customnamespace;

import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;

/**
 * Resource shape to be used by test classes.
 *
 * @author Daniel Figueiredo Caetano
 *
 */
@OslcName("CustomNamespace")
@OslcNamespace(CustomNamespaceResource.CUSTOM_NAMESPACE)
@OslcResourceShape(
        title = "Test Resource",
        describes = CustomNamespaceResource.CUSTOM_RESOURCE_TYPE)
public class CustomNamespaceResource extends AbstractResource {

    public static final String CUSTOM_NAMESPACE = "http://customnamespace.oslc.com/ns#";
    public static final String CUSTOM_RESOURCE_TYPE = CUSTOM_NAMESPACE + "Test";
}
