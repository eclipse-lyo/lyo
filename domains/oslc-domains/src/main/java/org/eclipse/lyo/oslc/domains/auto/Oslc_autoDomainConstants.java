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

package org.eclipse.lyo.oslc.domains.auto;

import org.eclipse.lyo.oslc4j.core.model.OslcConstants;


// Start of user code imports
// End of user code

public interface Oslc_autoDomainConstants
{
    // Start of user code user constants
    // End of user code

    public static String AUTOMATION_DOMAIN = "http://open-services.net/ns/auto#";
    public static String AUTOMATION_NAMSPACE = "http://open-services.net/ns/auto#";
    public static String AUTOMATION_NAMSPACE_PREFIX = "oslc_auto";

    public static String AUTOMATIONPLAN_PATH = "automationPlan";
    public static String AUTOMATIONPLAN_NAMESPACE = AUTOMATION_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String AUTOMATIONPLAN_LOCALNAME = "AutomationPlan"; //localName of the rdfs:class the resource describes
    public static String AUTOMATIONPLAN_TYPE = AUTOMATIONPLAN_NAMESPACE + AUTOMATIONPLAN_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String AUTOMATIONREQUEST_PATH = "automationRequest";
    public static String AUTOMATIONREQUEST_NAMESPACE = AUTOMATION_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String AUTOMATIONREQUEST_LOCALNAME = "AutomationRequest"; //localName of the rdfs:class the resource describes
    public static String AUTOMATIONREQUEST_TYPE = AUTOMATIONREQUEST_NAMESPACE + AUTOMATIONREQUEST_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String AUTOMATIONRESULT_PATH = "automationResult";
    public static String AUTOMATIONRESULT_NAMESPACE = AUTOMATION_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String AUTOMATIONRESULT_LOCALNAME = "AutomationResult"; //localName of the rdfs:class the resource describes
    public static String AUTOMATIONRESULT_TYPE = AUTOMATIONRESULT_NAMESPACE + AUTOMATIONRESULT_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String PARAMETERINSTANCE_PATH = "parameterInstance";
    public static String PARAMETERINSTANCE_NAMESPACE = AUTOMATION_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String PARAMETERINSTANCE_LOCALNAME = "ParameterInstance"; //localName of the rdfs:class the resource describes
    public static String PARAMETERINSTANCE_TYPE = PARAMETERINSTANCE_NAMESPACE + PARAMETERINSTANCE_LOCALNAME; //fullname of the rdfs:class the resource describes
}
