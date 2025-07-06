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

package org.eclipse.lyo.oslc.domains.cm;

// spotless:off
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc.domains.Oslc_cmVocabularyConstants;

// Start of user code imports
// End of user code
// spotless:on

public interface Oslc_cmDomainConstants
{
    // Start of user code user constants
    // End of user code

    /**
     * @deprecated use {@link Oslc_cmDomainConstants#CHANGE_MANAGEMENT_SHAPES_NAMSPACE} or {@link Oslc_cmDomainConstants#CHANGE_MANAGEMENT_SHAPES_DOMAIN_NAME} instead
     */
    @Deprecated(since = "5.0.1")
    public static String CHANGE_MANAGEMENT_SHAPES_DOMAIN = "http://open-services.net/ns/cm#";
    public static String CHANGE_MANAGEMENT_SHAPES_DOMAIN_NAME = "Change Management shapes";
    public static String CHANGE_MANAGEMENT_SHAPES_NAMSPACE = "http://open-services.net/ns/cm#"; //Vocabulary namespace for the resources and resource properties, when no explicit vocabulary (describes, or propertyDefinition) is defined 
    public static String CHANGE_MANAGEMENT_SHAPES_NAMSPACE_PREFIX = "oslc_cm"; //Vocabulary prefix for the resources and resource properties, when no explicit vocabulary (describes, or propertyDefinition) is defined

    public static String CHANGENOTICE_PATH = "changeNotice";  //the relative path of the resource shape URL.
    public static String CHANGENOTICE_NAMESPACE = Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String CHANGENOTICE_LOCALNAME = Oslc_cmVocabularyConstants.CHANGENOTICE; //localName of the rdfs:class the resource describes
    public static String CHANGENOTICE_TYPE = CHANGENOTICE_NAMESPACE + CHANGENOTICE_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String CHANGEREQUEST_PATH = "changeRequest";  //the relative path of the resource shape URL.
    public static String CHANGEREQUEST_NAMESPACE = Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String CHANGEREQUEST_LOCALNAME = Oslc_cmVocabularyConstants.CHANGEREQUEST; //localName of the rdfs:class the resource describes
    public static String CHANGEREQUEST_TYPE = CHANGEREQUEST_NAMESPACE + CHANGEREQUEST_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String DEFECT_PATH = "defect";  //the relative path of the resource shape URL.
    public static String DEFECT_NAMESPACE = Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String DEFECT_LOCALNAME = Oslc_cmVocabularyConstants.DEFECT; //localName of the rdfs:class the resource describes
    public static String DEFECT_TYPE = DEFECT_NAMESPACE + DEFECT_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String ENHANCEMENT_PATH = "enhancement";  //the relative path of the resource shape URL.
    public static String ENHANCEMENT_NAMESPACE = Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String ENHANCEMENT_LOCALNAME = Oslc_cmVocabularyConstants.ENHANCEMENT; //localName of the rdfs:class the resource describes
    public static String ENHANCEMENT_TYPE = ENHANCEMENT_NAMESPACE + ENHANCEMENT_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String PRIORITY_PATH = "priority";  //the relative path of the resource shape URL.
    public static String PRIORITY_NAMESPACE = Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String PRIORITY_LOCALNAME = Oslc_cmVocabularyConstants.PRIORITY; //localName of the rdfs:class the resource describes
    public static String PRIORITY_TYPE = PRIORITY_NAMESPACE + PRIORITY_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String REVIEWTASK_PATH = "reviewTask";  //the relative path of the resource shape URL.
    public static String REVIEWTASK_NAMESPACE = Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String REVIEWTASK_LOCALNAME = Oslc_cmVocabularyConstants.REVIEWTASK; //localName of the rdfs:class the resource describes
    public static String REVIEWTASK_TYPE = REVIEWTASK_NAMESPACE + REVIEWTASK_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String STATE_PATH = "state";  //the relative path of the resource shape URL.
    public static String STATE_NAMESPACE = Oslc_cmDomainConstants.CHANGE_MANAGEMENT_SHAPES_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String STATE_LOCALNAME = "State"; //localName of the rdfs:class the resource describes
    public static String STATE_TYPE = STATE_NAMESPACE + STATE_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String TASK_PATH = "task";  //the relative path of the resource shape URL.
    public static String TASK_NAMESPACE = Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String TASK_LOCALNAME = Oslc_cmVocabularyConstants.TASK; //localName of the rdfs:class the resource describes
    public static String TASK_TYPE = TASK_NAMESPACE + TASK_LOCALNAME; //fullname of the rdfs:class the resource describes
}
