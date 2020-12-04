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

package org.eclipse.lyo.oslc.domains.config;

import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc.domains.Oslc_configVocabularyConstants;

// Start of user code imports
// End of user code

public interface Oslc_configDomainConstants
{
    // Start of user code user constants
    // End of user code

    public static String CONFIGURATION_MANAGEMENT_DOMAIN = "http://open-services.net/ns/config#";
    public static String CONFIGURATION_MANAGEMENT_NAMSPACE = "http://open-services.net/ns/config#";
    public static String CONFIGURATION_MANAGEMENT_NAMSPACE_PREFIX = "oslc_config";

    public static String CHANGESET_PATH = "changeSet";
    public static String CHANGESET_NAMESPACE = Oslc_configVocabularyConstants.OSLC_CONFIGURATION_MANAGEMENT_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String CHANGESET_LOCALNAME = "ChangeSet"; //localName of the rdfs:class the resource describes
    public static String CHANGESET_TYPE = CHANGESET_NAMESPACE + CHANGESET_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String VERSIONRESOURCE_PATH = "versionResource";
    public static String VERSIONRESOURCE_NAMESPACE = CONFIGURATION_MANAGEMENT_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String VERSIONRESOURCE_LOCALNAME = "VersionResource"; //localName of the rdfs:class the resource describes
    public static String VERSIONRESOURCE_TYPE = VERSIONRESOURCE_NAMESPACE + VERSIONRESOURCE_LOCALNAME; //fullname of the rdfs:class the resource describes
}
