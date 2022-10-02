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
import org.eclipse.lyo.oslc.domains.DctermsVocabularyConstants;
import org.eclipse.lyo.oslc.domains.Oslc_configVocabularyConstants;
import org.eclipse.lyo.oslc.domains.OsclVocabularyConstants;
import org.eclipse.lyo.oslc.domains.ProvVocabularyConstants;

// Start of user code imports
// End of user code

public interface Oslc_configDomainConstants
{
    // Start of user code user constants
    // End of user code

    public static String CONFIGURATION_MANAGEMENT_DOMAIN = "Configuration Management";
    public static String CONFIGURATION_MANAGEMENT_NAMSPACE = "http://open-services.net/ns/config#"; //Vocabulary namespace for the resources and resource properties, when no explicit vocabulary (describes, or propertyDefinition) is defined 
    public static String CONFIGURATION_MANAGEMENT_NAMSPACE_PREFIX = "oslc_config"; //Vocabulary prefix for the resources and resource properties, when no explicit vocabulary (describes, or propertyDefinition) is defined

    public static String BASELINE_PATH = "baseline";  //the relative path of the resource shape URL.
    public static String BASELINE_NAMESPACE = Oslc_configDomainConstants.CONFIGURATION_MANAGEMENT_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String BASELINE_LOCALNAME = "Baseline"; //localName of the rdfs:class the resource describes
    public static String BASELINE_TYPE = BASELINE_NAMESPACE + BASELINE_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String CHANGESET_PATH = "changeSet";  //the relative path of the resource shape URL.
    public static String CHANGESET_NAMESPACE = Oslc_configVocabularyConstants.OSLC_CONFIGURATION_MANAGEMENT_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String CHANGESET_LOCALNAME = Oslc_configVocabularyConstants.CHANGESET; //localName of the rdfs:class the resource describes
    public static String CHANGESET_TYPE = CHANGESET_NAMESPACE + CHANGESET_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String COMPONENT_PATH = "component";  //the relative path of the resource shape URL.
    public static String COMPONENT_NAMESPACE = Oslc_configDomainConstants.CONFIGURATION_MANAGEMENT_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String COMPONENT_LOCALNAME = "Component"; //localName of the rdfs:class the resource describes
    public static String COMPONENT_TYPE = COMPONENT_NAMESPACE + COMPONENT_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String CONCEPTRESOURCE_PATH = "conceptResource";  //the relative path of the resource shape URL.
    public static String CONCEPTRESOURCE_NAMESPACE = Oslc_configDomainConstants.CONFIGURATION_MANAGEMENT_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String CONCEPTRESOURCE_LOCALNAME = "ConceptResource"; //localName of the rdfs:class the resource describes
    public static String CONCEPTRESOURCE_TYPE = CONCEPTRESOURCE_NAMESPACE + CONCEPTRESOURCE_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String CONFIGURATION_PATH = "configuration";  //the relative path of the resource shape URL.
    public static String CONFIGURATION_NAMESPACE = Oslc_configVocabularyConstants.OSLC_CONFIGURATION_MANAGEMENT_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String CONFIGURATION_LOCALNAME = Oslc_configVocabularyConstants.CONFIGURATION; //localName of the rdfs:class the resource describes
    public static String CONFIGURATION_TYPE = CONFIGURATION_NAMESPACE + CONFIGURATION_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String CONTRIBUTION_PATH = "contribution";  //the relative path of the resource shape URL.
    public static String CONTRIBUTION_NAMESPACE = Oslc_configDomainConstants.CONFIGURATION_MANAGEMENT_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String CONTRIBUTION_LOCALNAME = "Contribution"; //localName of the rdfs:class the resource describes
    public static String CONTRIBUTION_TYPE = CONTRIBUTION_NAMESPACE + CONTRIBUTION_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String SELECTIONS_PATH = "selections";  //the relative path of the resource shape URL.
    public static String SELECTIONS_NAMESPACE = Oslc_configDomainConstants.CONFIGURATION_MANAGEMENT_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String SELECTIONS_LOCALNAME = "Selections"; //localName of the rdfs:class the resource describes
    public static String SELECTIONS_TYPE = SELECTIONS_NAMESPACE + SELECTIONS_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String STREAM_PATH = "stream";  //the relative path of the resource shape URL.
    public static String STREAM_NAMESPACE = Oslc_configDomainConstants.CONFIGURATION_MANAGEMENT_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String STREAM_LOCALNAME = "Stream"; //localName of the rdfs:class the resource describes
    public static String STREAM_TYPE = STREAM_NAMESPACE + STREAM_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String VERSIONRESOURCE_PATH = "versionResource";  //the relative path of the resource shape URL.
    public static String VERSIONRESOURCE_NAMESPACE = Oslc_configDomainConstants.CONFIGURATION_MANAGEMENT_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String VERSIONRESOURCE_LOCALNAME = "VersionResource"; //localName of the rdfs:class the resource describes
    public static String VERSIONRESOURCE_TYPE = VERSIONRESOURCE_NAMESPACE + VERSIONRESOURCE_LOCALNAME; //fullname of the rdfs:class the resource describes
}
