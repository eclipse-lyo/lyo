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
