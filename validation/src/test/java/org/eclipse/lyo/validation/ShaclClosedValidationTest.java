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

/**
 * @since 2.3.0
 */

package org.eclipse.lyo.validation;

import java.math.BigInteger;
import java.net.URI;
import java.util.Date;

import org.apache.jena.rdf.model.Model;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.shacl.ShaclShapeFactory;
import org.eclipse.lyo.shacl.Shape;
import org.eclipse.lyo.shacl.ValidationReport;
import org.junit.Assert;
import org.junit.Test;

/**
 * The Class ShaclClosedValidationTest.
 */
public class ShaclClosedValidationTest {

    /** The a resource. */
    AResource aResource;

    /**
     * Shacl closed negative test.
     *
     * This test will fail because the shacl shape have closed set to true. This
     * means that any extra property apart from those allowed in the shacl shape wil
     * lead to the shacl closed validation error.
     */
    @Test
    public void ShaclClosedNegativetest() {

        try {
            aResource = new AResource(new URI("http://www.sampledomain.org/sam#AResource"));
            // According to the Shacl shape this is an extra property. Hence, it will show
            // shacl
            // closed error.
            aResource.setAnIntegerProperty(new BigInteger("2"));
            aResource.setAnotherIntegerProperty(new BigInteger("12"));
            aResource.setAStringProperty("Between");
            aResource.addASetOfDates(new Date());

            Model dataModel = JenaModelHelper.createJenaModel(new Object[] { aResource });
            Shape shaclShape = ShaclShapeFactory.createShaclShape(AResource.class);

            // Removing "anIntegerProperty" from the ShaclShape properties.
            shaclShape.removeProperty(new URI(SampleAdaptorConstants.SAMPLEDOMAIN_NAMSPACE + "anIntegerProperty"));
            shaclShape.setClosed(true);
            Model shapeModel = JenaModelHelper.createJenaModel(new Object[] { shaclShape });

            Validator validator = ValidatorFactory.createShaclExValidator();
            ValidationReport vr = validator.validate(dataModel, shapeModel);

            TestHelper.assertNegative(vr, "closed violation.");

        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail("Exception should not be thrown");
        }

    }

    /**
     * Shacl closed positive test.
     *
     */
    @Test
    public void ShaclClosedPositivetest() {

        try {
            aResource = new AResource(new URI("http://www.sampledomain.org/sam#AResource"));
            aResource.setAnotherIntegerProperty(new BigInteger("12"));
            aResource.setAStringProperty("Between");
            aResource.addASetOfDates(new Date());

            ValidationReport vr = TestHelper.performTest(aResource);
            TestHelper.assertPositive(vr);

        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail("Exception should not be thrown");
        }
    }

}
