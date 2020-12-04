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

import java.math.BigInteger;
import java.net.URI;
import java.util.Date;

import org.junit.Assert;
import org.junit.Test;

/**
 * The Class ShaclMinExclusiveValidationTest.
 *
 * @author Yash Khatri
 * @version $version-stub$
 * @since 2.3.0
 */
public class ShaclMinExclusiveValidationTest {

    /**
     * The a resource.
     */
    AResource aResource;

    /**
     * Shacl min exclusive negativetest.
     * <p>
     * This test will fail because the value of integerproperty2 can be minimum 6.
     */
    @Test
    public void ShaclMinExclusiveNegativetest() {

        try {
            aResource = new AResource(new URI("http://www.sampledomain.org/sam#AResource"));
            aResource.setAStringProperty("Between");
            aResource.setAnotherIntegerProperty(new BigInteger("12"));
            //Invalid Value. Min allowed value is 6.
            aResource.setIntegerProperty2(new BigInteger("5"));
            aResource.addASetOfDates(new Date());

            TestHelper.assertNegative(TestHelper.performTest(aResource), "minExclusive violation. Expected 5 > 5");

        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail("Exception should not be thrown");
        }

    }

    /**
     * Shacl min exclusive positivetest.
     */
    @Test
    public void ShaclMinExclusivePositivetest() {

        try {
            aResource = new AResource(new URI("http://www.sampledomain.org/sam#AResource"));
            aResource.setAnotherIntegerProperty(new BigInteger("12"));
            aResource.setIntegerProperty2(new BigInteger("6"));
            aResource.setAStringProperty("Between");
            aResource.addASetOfDates(new Date());

            TestHelper.assertPositive(TestHelper.performTest(aResource));

        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail("Exception should not be thrown");
        }
    }

}
