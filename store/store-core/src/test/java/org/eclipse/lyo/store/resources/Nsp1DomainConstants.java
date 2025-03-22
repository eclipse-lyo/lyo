// Start of user code Copyright
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
// End of user code

package org.eclipse.lyo.store.resources;

// Start of user code imports
// End of user code

public interface Nsp1DomainConstants
{
    // Start of user code user constants
    // End of user code

    public static String TESTDOMAIN_DOMAIN = "http://your.organisation.test/nsp1#";
    public static String TESTDOMAIN_NAMSPACE = "http://your.organisation.test/nsp1#";
    public static String TESTDOMAIN_NAMSPACE_PREFIX = "nsp1";

    public static String BLANKRESOURCE_PATH = "blankResource";
    public static String BLANKRESOURCE_NAMESPACE = TESTDOMAIN_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String BLANKRESOURCE_LOCALNAME = "BlankResource"; //localName of the rdfs:class the resource describes
    public static String BLANKRESOURCE_TYPE = BLANKRESOURCE_NAMESPACE + BLANKRESOURCE_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String WITHBLANKRESOURCE_PATH = "withBlankResource";
    public static String WITHBLANKRESOURCE_NAMESPACE = TESTDOMAIN_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String WITHBLANKRESOURCE_LOCALNAME = "WithBlankResource"; //localName of the rdfs:class the resource describes
    public static String WITHBLANKRESOURCE_TYPE = WITHBLANKRESOURCE_NAMESPACE + WITHBLANKRESOURCE_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String WITHTWODEPTHBLANKRESOURCE_PATH = "withTwoDepthBlankResource";
    public static String WITHTWODEPTHBLANKRESOURCE_NAMESPACE = TESTDOMAIN_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String WITHTWODEPTHBLANKRESOURCE_LOCALNAME = "WithTwoDepthBlankResource"; //localName of the rdfs:class the resource describes
    public static String WITHTWODEPTHBLANKRESOURCE_TYPE = WITHTWODEPTHBLANKRESOURCE_NAMESPACE + WITHTWODEPTHBLANKRESOURCE_LOCALNAME; //fullname of the rdfs:class the resource describes
}
