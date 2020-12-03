/*-*****************************************************************************
 * Copyright (c) 2017 Yash Khatri.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 *
 * Contributors:
 *    Yash Khatri - initial API and implementation and/or initial documentation
 *******************************************************************************/

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
 * The Class ShaclInValidationTest.
 */
public class ShaclInValidationTest {

    /** The a resource. */
    AResource aResource;

    /**
     * Shacl in negativetest.
     *
     * This test is failing because the allowed values for the
     * anotherintegerproperty are 5, 7, 9 or 12.
     */
    @Test
    public void ShaclInNegativetest() {

        try {
            aResource = new AResource(new URI("http://www.sampledomain.org/sam#AResource"));
            aResource.setAStringProperty("Between");
            aResource.addASetOfDates(new Date());
            aResource.setAnotherIntegerProperty(new BigInteger("6"));
            // Invalid value. Allowed values are "A" or "B" or "C".
            aResource.setAnotherStringProperty("D");

            TestHelper.assertNegative(TestHelper.performTest(aResource),
                    "In violation. Expected \"D\" to be in List(LiteralValue(\"C\"), LiteralValue(\"B\"), LiteralValue(\"A\"))");

        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail("Exception should not be thrown");
        }

    }

    /**
     * Shacl in positivetest.
     */
    @Test
    public void ShaclInPositivetest() {

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
