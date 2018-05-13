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

package org.eclipse.lyo.validation;

import org.eclipse.lyo.validation.shacl.ValidationResult;
import org.junit.Assert;
import org.junit.Test;

import java.math.BigInteger;
import java.net.URI;
import java.util.Date;

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
            //Invalid Value. The max allowed length of String is 10.
            aResource.setAStringProperty("Between two and four");
            aResource.setAnotherIntegerProperty(new BigInteger("12"));
            aResource.addASetOfDates(new Date());

            TestHelper.assertNegative(TestHelper.performTest(aResource), "maxLength violation. Expected length(\"Between two and four\") <= 10");


        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail("Exception should not be thrown");
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
            Assert.fail("Exception should not be thrown");
        }

    }

}
