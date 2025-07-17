/*
 * Copyright (c) 2025 Contributors to the Eclipse Foundation
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

package org.eclipse.lyo.validation;

import org.junit.jupiter.api.DisplayName;
import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;

/**
 * A test suite for SHACL validation functionality.
 */
@Suite
@SelectClasses({
    ShaclPatternValidationTest.class,
    ShaclMinLengthValidationTest.class,
    ShaclMaxLengthValidationTest.class,
    ShaclMinCountValidationTest.class,
    ShaclMaxCountValidationTest.class,
    ShaclMinInclusiveValidationTest.class,
    ShaclMaxInclusiveValidationTest.class,
    ShaclMinExclusiveValidationTest.class,
    ShaclMaxExclusiveValidationTest.class,
    ShaclInValidationTest.class,
    ShaclClosedValidationTest.class,
    OslcAnnotationsBasedValidationTest.class
})
@DisplayName("SHACL Validation Test Suite")
public class ValidationTestSuite {

}
