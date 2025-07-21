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
 *
 * @author Yash Khatri
 * @version $version-stub$
 * @since 2.3.0
 */
package org.eclipse.lyo.validation;

import static org.junit.jupiter.api.Assertions.*;

import java.math.BigInteger;
import java.net.URI;
import java.util.Date;
import org.junit.jupiter.api.Test;

/**
 * The Class ShaclMinLengthValidationTest.
 */
public class ShaclMinLengthValidationTest {

    /** The a resource. */
    AResource aResource;

    /**
     * Shacl min length negativetest.
     *
     * This test will fail becasue the AStringProperty have the constraint set as
     * ShaclMinLength =  7. The minimum length of the value "Betwee" is 6 therefore invalid.
     *
     *
     */
    @Test
    public void ShaclMinLengthNegativetest() {

        try {
            aResource = new AResource(new URI("http://www.sampledomain.org/sam#AResource"));
            // Invalid Value. Length should be at least 7.
            aResource.setAStringProperty("Betwee");
            aResource.setAnotherIntegerProperty(new BigInteger("12"));
            aResource.addASetOfDates(new Date());

            TestHelper.assertNegative(
                    TestHelper.performTest(aResource),
                    "minLength violation. Expected length(\"Betwee\") >= 7");

        } catch (Exception e) {
            e.printStackTrace();
            fail("Exception should not be thrown");
        }
    }

    /**
     * Shacl min length positivetest.
     *
     */
    @Test
    public void ShaclMinLengthPositivetest() {

        try {
            aResource = new AResource(new URI("http://www.sampledomain.org/sam#AResource"));
            aResource.setAnotherIntegerProperty(new BigInteger("12"));
            aResource.setAStringProperty("Between");
            aResource.addASetOfDates(new Date());

            TestHelper.assertPositive(TestHelper.performTest(aResource));

        } catch (Exception e) {
            e.printStackTrace();
            fail("Exception should not be thrown");
        }
    }
}
