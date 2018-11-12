/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 *     Paul McMahan         - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.resources;

import org.eclipse.lyo.oslc4j.core.model.OslcConstants;

public interface QmConstants
{
    public static String CHANGE_MANAGEMENT_DOMAIN                    = "http://open-services.net/ns/cm#";
    public static String CHANGE_MANAGEMENT_NAMESPACE                 = "http://open-services.net/ns/cm#";
    public static String CHANGE_MANAGEMENT_NAMESPACE_PREFIX          = "oslc_cm";
    public static String FOAF_NAMESPACE                              = "http://xmlns.com/foaf/0.1/";
    public static String FOAF_NAMESPACE_PREFIX                       = "foaf";
    public static String QUALITY_MANAGEMENT_DOMAIN                    = "http://open-services.net/ns/qm#";
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
	public static String TYPE_REQUIREMENT_COLLECTION = REQUIREMENTS_MANAGEMENT_NAMESPACE + "RequirementCollection";
    public static String TYPE_TEST_CASE             = QUALITY_MANAGEMENT_NAMESPACE + "TestCase";
    public static String TYPE_TEST_EXECUTION_RECORD = QUALITY_MANAGEMENT_NAMESPACE + "TestExecutionRecord";
    public static String TYPE_TEST_PLAN             = QUALITY_MANAGEMENT_NAMESPACE + "TestPlan";
    public static String TYPE_TEST_RESULT           = QUALITY_MANAGEMENT_NAMESPACE + "TestResult";
    public static String TYPE_TEST_SCRIPT           = QUALITY_MANAGEMENT_NAMESPACE + "TestScript";

    public static String PATH_TEST_PLAN = "testPlan";
    public static String PATH_TEST_CASE = "testCase";
    public static String PATH_TEST_SCRIPT = "testScript";
    public static String PATH_TEST_EXECUTION_RECORD = "testExecutionRecord";
    public static String PATH_TEST_RESULT = "testResult";

    public static String USAGE_LIST = QUALITY_MANAGEMENT_NAMESPACE + "list";
}
