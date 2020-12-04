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

package org.eclipse.lyo.oslc.domains.qm;

import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc.domains.Oslc_qmVocabularyConstants;

// Start of user code imports
// End of user code

public interface Oslc_qmDomainConstants
{
    // Start of user code user constants
    // End of user code

    public static String QUALITY_MANAGEMENT_DOMAIN = "http://open-services.net/ns/qm#";
    public static String QUALITY_MANAGEMENT_NAMSPACE = "http://open-services.net/ns/qm#";
    public static String QUALITY_MANAGEMENT_NAMSPACE_PREFIX = "oslc_qm";

    public static String TESTCASE_PATH = "testCase";
    public static String TESTCASE_NAMESPACE = Oslc_qmVocabularyConstants.QUALITY_MANAGEMENT_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String TESTCASE_LOCALNAME = "TestCase"; //localName of the rdfs:class the resource describes
    public static String TESTCASE_TYPE = TESTCASE_NAMESPACE + TESTCASE_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String TESTEXECUTIONRECORD_PATH = "testExecutionRecord";
    public static String TESTEXECUTIONRECORD_NAMESPACE = Oslc_qmVocabularyConstants.QUALITY_MANAGEMENT_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String TESTEXECUTIONRECORD_LOCALNAME = "TestExecutionRecord"; //localName of the rdfs:class the resource describes
    public static String TESTEXECUTIONRECORD_TYPE = TESTEXECUTIONRECORD_NAMESPACE + TESTEXECUTIONRECORD_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String TESTPLAN_PATH = "testPlan";
    public static String TESTPLAN_NAMESPACE = Oslc_qmVocabularyConstants.QUALITY_MANAGEMENT_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String TESTPLAN_LOCALNAME = "TestPlan"; //localName of the rdfs:class the resource describes
    public static String TESTPLAN_TYPE = TESTPLAN_NAMESPACE + TESTPLAN_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String TESTRESULT_PATH = "testResult";
    public static String TESTRESULT_NAMESPACE = Oslc_qmVocabularyConstants.QUALITY_MANAGEMENT_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String TESTRESULT_LOCALNAME = "TestResult"; //localName of the rdfs:class the resource describes
    public static String TESTRESULT_TYPE = TESTRESULT_NAMESPACE + TESTRESULT_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String TESTSCRIPT_PATH = "testScript";
    public static String TESTSCRIPT_NAMESPACE = Oslc_qmVocabularyConstants.QUALITY_MANAGEMENT_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String TESTSCRIPT_LOCALNAME = "TestScript"; //localName of the rdfs:class the resource describes
    public static String TESTSCRIPT_TYPE = TESTSCRIPT_NAMESPACE + TESTSCRIPT_LOCALNAME; //fullname of the rdfs:class the resource describes
}
