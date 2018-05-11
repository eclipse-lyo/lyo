/**
 * Copyright (c) 2017 Yash Khatri.
 * <p>
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 * <p>
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 * <p>
 * <p>
 * Contributors:
 * Yash Khatri - initial API and implementation and/or initial documentation
 *
 * @since 2.3.0
 * @since 2.3.0
 */

/**
 *
 * @author Yash Khatri
 * @version $version-stub$
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
            //Invalid Value. Length should be at least 7.
            aResource.setAStringProperty("Betwee");
            aResource.setAnotherIntegerProperty(new BigInteger("12"));
            aResource.addASetOfDates(new Date());

            ValidationResult vr = TestHelper.performTest(aResource);
            TestHelper.assertNegative(vr, "sh:MinLengthConstraintComponent");

        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail("Exception should not be thrown");
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

            ValidationResult vr = TestHelper.performTest(aResource);
            TestHelper.assertPositive(vr);

        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail("Exception should not be thrown");
        }
    }

}
