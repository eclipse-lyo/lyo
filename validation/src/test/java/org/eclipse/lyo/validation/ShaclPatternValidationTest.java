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

package org.eclipse.lyo.validation;

import static org.junit.jupiter.api.Assertions.*;

import java.math.BigInteger;
import java.net.URI;
import java.util.Date;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * Tests for SHACL pattern validation constraints.
 */
@DisplayName("SHACL Pattern Validation Tests")
public class ShaclPatternValidationTest {

    private AResource aResource;

    @BeforeEach
    void setUp() throws Exception {
        aResource = new AResource(new URI("http://www.sampledomain.org/sam#AResource"));
        aResource.setAnotherIntegerProperty(new BigInteger("12"));
        aResource.addASetOfDates(new Date());
    }

    /**
     * Tests that validation fails when string property does not match the required pattern.
     * The pattern requires strings to start with "B", but "Catalyzer" starts with "C".
     */
    @Test
    @DisplayName("Should fail validation when string does not match pattern")
    void shouldFailValidationWhenStringDoesNotMatchPattern() {
        // Given - Invalid value that should start with 'B' to be valid
        aResource.setAStringProperty("Catalyzer");

        // When & Then
        assertDoesNotThrow(
                () -> {
                    TestHelper.assertNegative(
                            TestHelper.performTest(aResource),
                            "pattern violation. Expected \"Catalyzer\" to match '^B'");
                });
    }

    /**
     * Tests that validation passes when string property matches the required pattern.
     */
    @Test
    @DisplayName("Should pass validation when string matches pattern")
    void shouldPassValidationWhenStringMatchesPattern() {
        // Given - Valid value that starts with 'B'
        aResource.setAStringProperty("Between");

        // When & Then
        assertDoesNotThrow(
                () -> {
                    TestHelper.assertPositive(TestHelper.performTest(aResource));
                });
    }
}
