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

package org.eclipse.lyo.oslc.domains.am;

// Start of user code imports
// End of user code

public interface Oslc_amDomainConstants {
    // Start of user code user constants
    // End of user code

    /**
     * @deprecated use {@link Oslc_amDomainConstants#ARCHITECTURE_MANAGEMENT_NAMSPACE} or {@link Oslc_amDomainConstants#ARCHITECTURE_MANAGEMENT_DOMAIN_NAME} instead
     */
    @Deprecated(since = "5.0.1")
    public static String ARCHITECTURE_MANAGEMENT_DOMAIN = "http://open-services.net/ns/am#";

    public static String ARCHITECTURE_MANAGEMENT_DOMAIN_NAME = "Architecture Management";
    public static String ARCHITECTURE_MANAGEMENT_NAMSPACE =
            "http://open-services.net/ns/am#"; // Vocabulary namespace for the resources and
    // resource properties, when no explicit vocabulary
    // (describes, or propertyDefinition) is defined
    public static String ARCHITECTURE_MANAGEMENT_NAMSPACE_PREFIX =
            "oslc_am"; // Vocabulary prefix for the resources and resource properties, when no
    // explicit vocabulary (describes, or propertyDefinition) is defined

    public static String LINKTYPE_PATH = "linkType"; // the relative path of the resource shape URL.
    public static String LINKTYPE_NAMESPACE =
            Oslc_amDomainConstants
                    .ARCHITECTURE_MANAGEMENT_NAMSPACE; // namespace of the rdfs:class the resource
    // describes
    public static String LINKTYPE_LOCALNAME =
            "LinkType"; // localName of the rdfs:class the resource describes
    public static String LINKTYPE_TYPE =
            LINKTYPE_NAMESPACE
                    + LINKTYPE_LOCALNAME; // fullname of the rdfs:class the resource describes
    public static String RESOURCE_PATH = "resource"; // the relative path of the resource shape URL.
    public static String RESOURCE_NAMESPACE =
            Oslc_amDomainConstants
                    .ARCHITECTURE_MANAGEMENT_NAMSPACE; // namespace of the rdfs:class the resource
    // describes
    public static String RESOURCE_LOCALNAME =
            "Resource"; // localName of the rdfs:class the resource describes
    public static String RESOURCE_TYPE =
            RESOURCE_NAMESPACE
                    + RESOURCE_LOCALNAME; // fullname of the rdfs:class the resource describes
}
