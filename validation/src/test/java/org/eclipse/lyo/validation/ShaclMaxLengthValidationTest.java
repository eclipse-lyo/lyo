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
import org.junit.jupiter.api.Test;

/**
 * The Class ShaclMaxLengthValidationTest.
 *
 * @author Yash Khatri
 * @version $version-stub$
 * @since 2.3.0
 */
public class ShaclMaxLengthValidationTest {

    /**
     * The a resource.
     */
    AResource aResource;

    /**
     * Shacl max length negativetest.
     * <p>
     * This test is failing because the allowed max length of the StringProperty is 10 but here it
     * is more than 10
     */
    @Test
    public void ShaclMaxLengthNegativetest() {

        try {
            aResource = new AResource(new URI("http://www.sampledomain.org/sam#AResource"));
            // Invalid Value. The max allowed length of String is 10.
            aResource.setAStringProperty("Between two and four");
            aResource.setAnotherIntegerProperty(new BigInteger("12"));
            aResource.addASetOfDates(new Date());

            TestHelper.assertNegative(
                    TestHelper.performTest(aResource),
                    "maxLength violation. Expected length(\"Between two and four\") <= 10");

        } catch (Exception e) {
            e.printStackTrace();
            fail("Exception should not be thrown");
        }
    }

    /**
     * Shacl max length positivetest.
     */
    @Test
    public void ShaclMaxLengthPositivetest() {

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
