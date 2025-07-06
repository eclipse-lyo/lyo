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

// spotless:off
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;

// Start of user code imports
// End of user code
// spotless:on

public interface Oslc_qmVocabularyConstants
{
    // Start of user code user constants
    // End of user code

    public static String QUALITY_MANAGEMENT_NAMSPACE = "http://open-services.net/ns/qm#";
    public static String QUALITY_MANAGEMENT_NAMSPACE_PREFIX = "oslc_qm";

    public static String TESTCASE = "TestCase";
    public static String TYPE_TESTCASE = QUALITY_MANAGEMENT_NAMSPACE + TESTCASE;
    public static String TESTEXECUTIONRECORD = "TestExecutionRecord";
    public static String TYPE_TESTEXECUTIONRECORD = QUALITY_MANAGEMENT_NAMSPACE + TESTEXECUTIONRECORD;
    public static String TESTPLAN = "TestPlan";
    public static String TYPE_TESTPLAN = QUALITY_MANAGEMENT_NAMSPACE + TESTPLAN;
    public static String TESTRESULT = "TestResult";
    public static String TYPE_TESTRESULT = QUALITY_MANAGEMENT_NAMSPACE + TESTRESULT;
    public static String TESTSCRIPT = "TestScript";
    public static String TYPE_TESTSCRIPT = QUALITY_MANAGEMENT_NAMSPACE + TESTSCRIPT;
}
