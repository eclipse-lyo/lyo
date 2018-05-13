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
