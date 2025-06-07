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
package org.eclipse.lyo.client.oslc.resources;

@Deprecated
public interface ArchitectureConstants {
    public static final String ARCHITECTURE_DOMAIN = "http://open-services.net/ns/am#";
    public static final String ARCHITECTURE_NAMESPACE = "http://open-services.net/ns/am#";
    public static final String ARCHITECTURE_PREFIX = "oslc_am";
    public static final String ARCHITECTURE_RESOURCE = "Resource";
    public static final String ARCHITECTURE_LINK_TYPE = "LinkType";
    public static final String FOAF_NAMESPACE = "http://xmlns.com/foaf/0.1/";
    public static final String FOAF_NAMESPACE_PREFIX = "foaf";

    public static final String TYPE_ARCHITECTURE_RESOURCE =
            ARCHITECTURE_NAMESPACE + ARCHITECTURE_RESOURCE;
    public static final String TYPE_ARCHITECTURE_LINK_TYPE =
            ARCHITECTURE_NAMESPACE + ARCHITECTURE_LINK_TYPE;
    public static final String TYPE_PERSON = FOAF_NAMESPACE + "Person";
}
