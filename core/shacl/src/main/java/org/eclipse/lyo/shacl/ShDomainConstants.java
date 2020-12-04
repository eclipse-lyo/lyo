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

package org.eclipse.lyo.shacl;


// Start of user code imports
// End of user code

public interface ShDomainConstants
{
    // Start of user code user constants
    // End of user code

    public static String SHACL_DOMAIN = "http://www.w3.org/ns/shacl#";
    public static String SHACL_NAMSPACE = "http://www.w3.org/ns/shacl#";
    public static String SHACL_NAMSPACE_PREFIX = "sh";

    public static String VALIDATIONREPORT_PATH = "validationReport";
    public static String VALIDATIONREPORT_NAMESPACE = SHACL_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String VALIDATIONREPORT_LOCALNAME = "ValidationReport"; //localName of the rdfs:class the resource describes
    public static String VALIDATIONREPORT_TYPE = VALIDATIONREPORT_NAMESPACE + VALIDATIONREPORT_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String VALIDATIONRESULT_PATH = "validationResult";
    public static String VALIDATIONRESULT_NAMESPACE = SHACL_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String VALIDATIONRESULT_LOCALNAME = "ValidationResult"; //localName of the rdfs:class the resource describes
    public static String VALIDATIONRESULT_TYPE = VALIDATIONRESULT_NAMESPACE + VALIDATIONRESULT_LOCALNAME; //fullname of the rdfs:class the resource describes
}
