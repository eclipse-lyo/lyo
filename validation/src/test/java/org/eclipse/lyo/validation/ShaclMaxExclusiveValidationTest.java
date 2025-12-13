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
import org.junit.jupiter.api.Test;

/**
 * The Class ShaclMaxExclusiveValidationTest.
 */
public class ShaclMaxExclusiveValidationTest {

    /** The a resource. */
    AResource aResource;

    /**
     * Shacl max exclusive negativetest.
     *
     * This test is failing because maximum allowed value for integerproperty2 is 14.
     */
    @Test
    public void ShaclMaxExclusiveNegativetest() {

        try {
            aResource = new AResource(new URI("http://www.sampledomain.org/sam#AResource"));
            aResource.setAStringProperty("Between");
            aResource.setAnotherIntegerProperty(new BigInteger("12"));
            // Invalid Value. Maximum allowed value is 14.
            aResource.setIntegerProperty2(new BigInteger("16"));
            aResource.addASetOfDates(new Date());

            TestHelper.assertNegative(
                    TestHelper.performTest(aResource), "Data value 16 is not less than 15");

        } catch (Exception e) {
            e.printStackTrace();
            fail("Exception should not be thrown");
        }
    }

    /**
     * Shacl max exclusive positivetest.
     */
    @Test
    public void ShaclMaxExclusivePositivetest() {

        try {
            aResource = new AResource(new URI("http://www.sampledomain.org/sam#AResource"));
            aResource.setAnotherIntegerProperty(new BigInteger("12"));
            aResource.setIntegerProperty2(new BigInteger("14"));
            aResource.setAStringProperty("Between");
            aResource.addASetOfDates(new Date());

            TestHelper.assertPositive(TestHelper.performTest(aResource));

        } catch (Exception e) {
            e.printStackTrace();
            fail("Exception should not be thrown");
        }
    }
}
