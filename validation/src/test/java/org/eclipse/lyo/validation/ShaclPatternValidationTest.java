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
