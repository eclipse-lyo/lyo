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
package org.eclipse.lyo.client.oslc.resources;

import org.eclipse.lyo.oslc4j.core.model.OslcConstants;

@Deprecated
public interface CmConstants
{
    public static String CHANGE_MANAGEMENT_DOMAIN                    = "http://open-services.net/ns/cm#";
    public static String CHANGE_MANAGEMENT_NAMESPACE                 = "http://open-services.net/ns/cm#";
    public static String CHANGE_MANAGEMENT_NAMESPACE_PREFIX          = "oslc_cm";
    public static String FOAF_NAMESPACE                              = "http://xmlns.com/foaf/0.1/";
    public static String FOAF_NAMESPACE_PREFIX                       = "foaf";
    public static String QUALITY_MANAGEMENT_NAMESPACE                = "http://open-services.net/ns/qm#";
    public static String QUALITY_MANAGEMENT_PREFIX                   = "oslc_qm";
    public static String REQUIREMENTS_MANAGEMENT_NAMESPACE           = "http://open-services.net/ns/rm#";
    public static String REQUIREMENTS_MANAGEMENT_PREFIX              = "oslc_rm";
    public static String SOFTWARE_CONFIGURATION_MANAGEMENT_NAMESPACE = "http://open-services.net/ns/scm#";
    public static String SOFTWARE_CONFIGURATION_MANAGEMENT_PREFIX    = "oslc_scm";


    public static String TYPE_CHANGE_REQUEST        = CHANGE_MANAGEMENT_NAMESPACE + "ChangeRequest";
    public static String TYPE_CHANGE_SET            = SOFTWARE_CONFIGURATION_MANAGEMENT_NAMESPACE + "ChangeSet";
    public static String TYPE_DISCUSSION            = OslcConstants.OSLC_CORE_NAMESPACE + "Discussion";
    public static String TYPE_PERSON                = FOAF_NAMESPACE + "Person";
    public static String TYPE_REQUIREMENT           = REQUIREMENTS_MANAGEMENT_NAMESPACE + "Requirement";
    public static String TYPE_TEST_CASE             = QUALITY_MANAGEMENT_NAMESPACE + "TestCase";
    public static String TYPE_TEST_EXECUTION_RECORD = QUALITY_MANAGEMENT_NAMESPACE + "TestExecutionRecord";
    public static String TYPE_TEST_PLAN             = QUALITY_MANAGEMENT_NAMESPACE + "TestPlan";
    public static String TYPE_TEST_RESULT           = QUALITY_MANAGEMENT_NAMESPACE + "TestResult";
    public static String TYPE_TEST_SCRIPT           = QUALITY_MANAGEMENT_NAMESPACE + "TestScript";

    public static String PATH_CHANGE_REQUEST = "changeRequest";

    public static String USAGE_LIST = CHANGE_MANAGEMENT_NAMESPACE + "list";
}
