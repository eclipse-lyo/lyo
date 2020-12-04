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

// Start of user code imports
// End of user code

public interface FoafDomainConstants
{
    // Start of user code user constants
    // End of user code

    public static String FOAF_DOMAIN = "http://xmlns.com/foaf/0.1/#";
    public static String FOAF_NAMSPACE = "http://xmlns.com/foaf/0.1/#";
    public static String FOAF_NAMSPACE_PREFIX = "foaf";

    public static String AGENT_PATH = "agent";
    public static String AGENT_NAMESPACE = FoafVocabularyConstants.FOAF_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String AGENT_LOCALNAME = "Agent"; //localName of the rdfs:class the resource describes
    public static String AGENT_TYPE = AGENT_NAMESPACE + AGENT_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String PERSON_PATH = "person";
    public static String PERSON_NAMESPACE = FoafVocabularyConstants.FOAF_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String PERSON_LOCALNAME = "Person"; //localName of the rdfs:class the resource describes
    public static String PERSON_TYPE = PERSON_NAMESPACE + PERSON_LOCALNAME; //fullname of the rdfs:class the resource describes
}
