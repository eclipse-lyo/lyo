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

import java.math.BigInteger;
import java.net.URI;
import java.util.Date;

import org.junit.Assert;
import org.junit.Test;

/**
 * The Class ShaclMaxInclusiveValidationTest.
 */
public class ShaclMaxInclusiveValidationTest {

    /** The a resource. */
    AResource aResource;

    /**
     * Shacl max inclusive negativetest.
     *
     * This test is failing because the maximum allowed value for the integerproperty3 is 15.
     */
    @Test
    public void ShaclMaxInclusiveNegativetest() {

        try {
            aResource = new AResource(new URI("http://www.sampledomain.org/sam#AResource"));
            aResource.setAStringProperty("Between");
            aResource.setAnotherIntegerProperty(new BigInteger("12"));
            //Invalid value. Maximum allowed value is 15.
            aResource.setIntegerProperty3(new BigInteger("16"));
            aResource.addASetOfDates(new Date());

            TestHelper.assertNegative(TestHelper.performTest(aResource), "maxInclusive violation. Expected 16 <= 15");

        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail("Exception should not be thrown");
        }

    }

    /**
     * Shacl max inclusive positivetest.
     */
    @Test
    public void ShaclMaxInclusivePositivetest() {

        try {
            aResource = new AResource(new URI("http://www.sampledomain.org/sam#AResource"));
            aResource.setAnotherIntegerProperty(new BigInteger("12"));
            aResource.setIntegerProperty3(new BigInteger("15"));
            aResource.setAStringProperty("Between");
            aResource.addASetOfDates(new Date());

            TestHelper.assertPositive(TestHelper.performTest(aResource));

        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail("Exception should not be thrown");
        }
    }

}
