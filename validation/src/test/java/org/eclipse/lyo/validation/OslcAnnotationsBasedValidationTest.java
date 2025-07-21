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

/**
 * @since 2.3.0
 */
package org.eclipse.lyo.validation;

import static org.junit.jupiter.api.Assertions.*;

import java.math.BigInteger;
import java.net.URI;
import java.util.Date;
import org.eclipse.lyo.shacl.ValidationReport;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * Tests for OSLC annotation-based validation.
 *
 * @author Yash Khatri
 * @since 2.3.0
 */
@DisplayName("OSLC Annotations Based Validation Tests")
public class OslcAnnotationsBasedValidationTest {

    private AnOslcResource anOslcResource;

    @BeforeEach
    void setUp() throws Exception {
        anOslcResource =
                new AnOslcResource(new URI("http://www.sampledomain.org/sam#AnOslcResource"));
        anOslcResource.setAnotherIntegerProperty(new BigInteger("12"));
        anOslcResource.setAnIntegerProperty(new BigInteger("12"));
        anOslcResource.setIntegerProperty3(new BigInteger("12"));
        anOslcResource.setAStringProperty("Cat");
        anOslcResource.addASetOfDates(new Date());
    }

    /**
     * Tests that validation fails when required properties are missing.
     * This test fails because a required property (integerProperty2) is not set,
     * violating the MinCount constraint.
     */
    @Test
    @DisplayName("Should fail validation when required properties are missing")
    public void OslcBasedNegativetest() {

        try {
            ValidationReport vr = TestHelper.performTest(anOslcResource);
            TestHelper.assertNegative(vr, "MinCount violation. Expected 1, obtained: 0");

        } catch (Exception e) {
            e.printStackTrace();
            fail("Exception should not be thrown");
        }
    }

    /**
     * Tests that validation passes when all required properties are present.
     */
    @Test
    @DisplayName("Should pass validation when all required properties are present")
    public void OslcBasedPositivetest() {

        try {
            anOslcResource.setIntegerProperty2(new BigInteger("12"));

            ValidationReport vr = TestHelper.performTest(anOslcResource);
            TestHelper.assertPositive(vr);

        } catch (Exception e) {
            e.printStackTrace();
            fail("Exception should not be thrown");
        }
    }
}
