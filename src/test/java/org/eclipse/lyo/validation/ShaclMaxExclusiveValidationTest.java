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

import org.eclipse.lyo.validation.shacl.ValidationResult;
import org.junit.Assert;
import org.junit.Test;

import java.math.BigInteger;
import java.net.URI;
import java.util.Date;

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
            //Invalid Value. Maximum allowed value is 14.
            aResource.setIntegerProperty2(new BigInteger("16"));
            aResource.addASetOfDates(new Date());

            ValidationResult vr = TestHelper.performTest(aResource);
            TestHelper.assertNegative(vr, "sh:MaxExclusiveConstraintComponent");

        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail("Exception should not be thrown");
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

            ValidationResult vr = TestHelper.performTest(aResource);
            TestHelper.assertPositive(vr);

        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail("Exception should not be thrown");
        }

    }

}
