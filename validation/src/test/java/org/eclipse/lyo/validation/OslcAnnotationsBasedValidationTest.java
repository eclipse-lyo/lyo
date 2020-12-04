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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Simple
 */

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
 * The Class OslcAnnotationsBasedValidationTest.
 *
 *
 * @author Yash Khatri 
 * 
 */

public class OslcAnnotationsBasedValidationTest {

    /** The a resource. */
    AnOslcResource anOslcResource;

    /**
     * OslcBased negative test.
     *
     * This test will fail because the pattern for StringProperty does not satisfy.
     * It should start with "B" to be valid. But Here in this example, it starts
     * with "C".
     */
    @Test
    public void OslcBasedNegativetest() {

        try {
            anOslcResource = new AnOslcResource(new URI("http://www.sampledomain.org/sam#AnOslcResource"));
            anOslcResource.setAnotherIntegerProperty(new BigInteger("12"));
            anOslcResource.setAnIntegerProperty(new BigInteger("12"));
            anOslcResource.setIntegerProperty3(new BigInteger("12"));
            anOslcResource.setAStringProperty("Cat");
            anOslcResource.addASetOfDates(new Date());

            ValidationReport vr = TestHelper.performTest(anOslcResource);
            TestHelper.assertNegative(vr, "MinCount violation. Expected 1, obtained: 0");

        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail("Exception should not be thrown");
        }
    }

    /**
     * OslcBased positive test.
     *
     */
    @Test
    public void OslcBasedPositivetest() {

        try {
            anOslcResource = new AnOslcResource(new URI("http://www.sampledomain.org/sam#anOslcResource"));
            anOslcResource.setAnotherIntegerProperty(new BigInteger("12"));
            anOslcResource.setAnIntegerProperty(new BigInteger("12"));
            anOslcResource.setIntegerProperty3(new BigInteger("12"));
            anOslcResource.setAStringProperty("Cat");
            anOslcResource.addASetOfDates(new Date());
            anOslcResource.setIntegerProperty2(new BigInteger("12"));

            ValidationReport vr = TestHelper.performTest(anOslcResource);
            TestHelper.assertPositive(vr);

        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail("Exception should not be thrown");
        }
    }

}
