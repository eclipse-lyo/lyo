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
 * @since 2.3.0
 */

package org.eclipse.lyo.validation;

public interface SampleAdaptorConstants {
    public static String SAMPLEDOMAIN_DOMAIN = "http://www.sampledomain.org/sam#";
    public static String SAMPLEDOMAIN_NAMSPACE = "http://www.sampledomain.org/sam#";
    public static String SAMPLEDOMAIN_NAMSPACE_PREFIX = "sam";

    public static String ARESOURCE = "AResource";
    public static String PATH_ARESOURCE = "aResource";
    public static String TYPE_ARESOURCE = SAMPLEDOMAIN_NAMSPACE + "AResource";

    public static String ANOSLCRESOURCE = "AnOslcResource";
    public static String PATH_ANOSLCRESOURCE = "anOslcResource";
    public static String TYPE_ANOSLCRESOURCE = SAMPLEDOMAIN_NAMSPACE + "AnOslcResource";

    public static String ANOTHERRESOURCE = "AnotherResource";
    public static String PATH_ANOTHERRESOURCE = "anotherResource";
    public static String TYPE_ANOTHERRESOURCE = SAMPLEDOMAIN_NAMSPACE + "AnotherResource";

    public static final String HDR_OSLC_VERSION = "OSLC-Core-Version";

}

