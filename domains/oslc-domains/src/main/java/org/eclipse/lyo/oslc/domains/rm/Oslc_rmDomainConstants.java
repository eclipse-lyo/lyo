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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Simple
 */
// End of user code

package org.eclipse.lyo.oslc.domains.rm;

import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc.domains.Oslc_rmVocabularyConstants;

// Start of user code imports
// End of user code

public interface Oslc_rmDomainConstants
{
    // Start of user code user constants
    // End of user code

    public static String REQUIREMENTS_MANAGEMENT_SHAPES_DOMAIN = "http://open-services.net/ns/rm#";
    public static String REQUIREMENTS_MANAGEMENT_SHAPES_NAMSPACE = "http://open-services.net/ns/rm#";
    public static String REQUIREMENTS_MANAGEMENT_SHAPES_NAMSPACE_PREFIX = "oslc_rm";

    public static String REQUIREMENT_PATH = "requirement";
    public static String REQUIREMENT_NAMESPACE = Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String REQUIREMENT_LOCALNAME = "Requirement"; //localName of the rdfs:class the resource describes
    public static String REQUIREMENT_TYPE = REQUIREMENT_NAMESPACE + REQUIREMENT_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String REQUIREMENTCOLLECTION_PATH = "requirementCollection";
    public static String REQUIREMENTCOLLECTION_NAMESPACE = Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String REQUIREMENTCOLLECTION_LOCALNAME = "RequirementCollection"; //localName of the rdfs:class the resource describes
    public static String REQUIREMENTCOLLECTION_TYPE = REQUIREMENTCOLLECTION_NAMESPACE + REQUIREMENTCOLLECTION_LOCALNAME; //fullname of the rdfs:class the resource describes
}
