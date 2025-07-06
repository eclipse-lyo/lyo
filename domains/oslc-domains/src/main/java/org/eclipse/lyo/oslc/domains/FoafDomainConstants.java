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

package org.eclipse.lyo.oslc.domains;

import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc.domains.FoafVocabularyConstants;

// spotless:off
// Start of user code imports
// End of user code
// spotless:on

public interface FoafDomainConstants
{
    // Start of user code user constants
    // End of user code

    /**
     * @deprecated use {@link FoafDomainConstants#FOAF_NAMSPACE} or {@link FoafDomainConstants#FOAF_DOMAIN_NAME} instead
     */
    @Deprecated(since = "5.0.1")
    public static String FOAF_DOMAIN = "http://xmlns.com/foaf/0.1/#";
    public static String FOAF_DOMAIN_NAME = "FOAF";
    public static String FOAF_NAMSPACE = "http://xmlns.com/foaf/0.1/#"; //Vocabulary namespace for the resources and resource properties, when no explicit vocabulary (describes, or propertyDefinition) is defined 
    public static String FOAF_NAMSPACE_PREFIX = "foaf"; //Vocabulary prefix for the resources and resource properties, when no explicit vocabulary (describes, or propertyDefinition) is defined

    public static String AGENT_PATH = "agent";  //the relative path of the resource shape URL.
    public static String AGENT_NAMESPACE = FoafVocabularyConstants.FOAF_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String AGENT_LOCALNAME = FoafVocabularyConstants.AGENT; //localName of the rdfs:class the resource describes
    public static String AGENT_TYPE = AGENT_NAMESPACE + AGENT_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String PERSON_PATH = "person";  //the relative path of the resource shape URL.
    public static String PERSON_NAMESPACE = FoafVocabularyConstants.FOAF_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String PERSON_LOCALNAME = FoafVocabularyConstants.PERSON; //localName of the rdfs:class the resource describes
    public static String PERSON_TYPE = PERSON_NAMESPACE + PERSON_LOCALNAME; //fullname of the rdfs:class the resource describes
}
