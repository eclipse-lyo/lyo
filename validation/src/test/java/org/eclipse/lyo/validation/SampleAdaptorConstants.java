/*-*****************************************************************************
 * Copyright (c) 2017 Yash Khatri.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 *
 * Contributors:
 *    Yash Khatri - initial API and implementation and/or initial documentation
 *******************************************************************************/

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

