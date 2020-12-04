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

import org.eclipse.lyo.shacl.ValidationReport;
import org.junit.Assert;
import org.junit.Test;

/**
 * The Class ShaclMaxCountValidationTest.
 */
public class ShaclMaxCountValidationTest {

    /**
     * The a resource.
     */
    public AResource aResource;

    /**
     * Shacl max count negativetest.
     * <p>
     * This test is failing because the max allowed cardinality of anintegerproperty is 0.
     */
    @Test
    public void ShaclMaxCountNegativetest() {

        try {
            aResource = new AResource(new URI("http://www.sampledomain.org/sam#AResource"));
            //Invalid. Cardinality should be 0.
            aResource.setAnIntegerProperty(new BigInteger("1"));
            aResource.setAnotherIntegerProperty(new BigInteger("12"));
            aResource.setAStringProperty("Between");
            aResource.addASetOfDates(new Date());

            ValidationReport vr = TestHelper.performTest(aResource);
            TestHelper.assertNegative(vr, "MaxCount violation. Expected 0, obtained: 1");

        } catch (Exception e) {
            Assert.fail("Exception should be thrown.");
            e.printStackTrace();
        }
    }

    /**
     * Shacl max count positivetest.
     */
    @Test
    public void ShaclMaxCountPositivetest() {

        try {
            aResource = new AResource(new URI("http://www.sampledomain.org/sam#AResource"));
            aResource.setAnotherIntegerProperty(new BigInteger("12"));
            aResource.setAStringProperty("Between");
            aResource.addASetOfDates(new Date());

            ValidationReport vr = TestHelper.performTest(aResource);
            TestHelper.assertPositive(vr);

        } catch (Exception e) {
            Assert.fail("Exception should be thrown.");
            e.printStackTrace();
        }

    }

}
