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

/**
 *
 * @author Yash Khatri
 * @version $version-stub$
 * @since 2.3.0
 */

package org.eclipse.lyo.validation;

import java.math.BigInteger;
import java.net.URI;
import java.util.Date;

import org.junit.Assert;
import org.junit.Test;

/**
 * The Class ShaclPatternValidationTest.
 */
public class ShaclPatternValidationTest {

    /** The a resource. */
    AResource aResource;

    /**
     * Shacl pattern negative test.
     *
     * This test will fail because the pattern for StringProperty does not satisfy.
     * It should start with "B" to be valid. But Here in this example, it starts
     * with "C".
     */
    @Test
    public void ShaclPatternNegativetest() {

        try {
            aResource = new AResource(new URI("http://www.sampledomain.org/sam#AResource"));
            aResource.setAnotherIntegerProperty(new BigInteger("12"));
            // Invalid Value. Should Start with 'B' to be valid.
            aResource.setAStringProperty("Catalyzer");
            aResource.addASetOfDates(new Date());

            TestHelper.assertNegative(TestHelper.performTest(aResource),
                    "pattern violation. Expected \"Catalyzer\" to match '^B'");

        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail("Exception should not be thrown");
        }

    }

    /**
     * Shacl pattern positive test.
     *
     */
    @Test
    public void ShaclPatternPositivetest() {

        try {
            aResource = new AResource(new URI("http://www.sampledomain.org/sam#AResource"));
            aResource.setAnotherIntegerProperty(new BigInteger("12"));
            aResource.setAStringProperty("Between");
            aResource.addASetOfDates(new Date());

            TestHelper.assertPositive(TestHelper.performTest(aResource));

        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail("Exception should not be thrown");
        }
    }

}
