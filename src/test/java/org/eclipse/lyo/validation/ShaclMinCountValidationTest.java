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

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import java.math.BigInteger;
import java.net.URI;
import java.util.Date;
import org.apache.jena.rdf.model.Model;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.validation.impl.ValidatorImpl;
import org.eclipse.lyo.validation.model.ResourceModel;
import org.eclipse.lyo.validation.model.ValidationResultModel;
import org.eclipse.lyo.validation.shacl.ShaclShape;
import org.eclipse.lyo.validation.shacl.ShaclShapeFactory;
import org.junit.Assert;
import org.junit.Test;

/**
 * The Class ShaclMinCountValidationTest.
 *
 * @author Yash Khatri
 * @version $version-stub$
 * @since 2.3.0
 */
public class ShaclMinCountValidationTest {

    /**
     * The a resource.
     */
    AResource aResource;

    /**
     * Shacl min count negativetest.
     * <p>
     * This test will fail because the shacl min count for AnotherIntegerProperety is 1 but it is
     * 0 here.
     */
    @Test
    public void ShaclMinCountNegativetest() {

        try {
            aResource = new AResource(new URI("http://www.sampledomain.org/sam#AResource"));
            aResource.setAStringProperty("Between");
            aResource.addASetOfDates(new Date());
            //not setting anotherIntegerProperty

            Model dataModel = JenaModelHelper.createJenaModel(new Object[]{aResource});
            ShaclShape shaclShape = ShaclShapeFactory.createShaclShape(AResource.class);
            Model shapeModel = JenaModelHelper.createJenaModel(new Object[]{shaclShape});

            Validator validator = new ValidatorImpl();
            ValidationResultModel vr = validator.validate(dataModel, shapeModel);

            for (ResourceModel rm : vr.getInvalidResources()) {

                JsonElement jelement = new JsonParser().parse(rm.getResult().toJsonString2spaces());
                JsonObject obj = jelement.getAsJsonObject();
                String actualError = obj.getAsJsonArray("errors").get(0).getAsJsonObject().get(
                        "error").toString().replaceAll("\"", "").split(" ")[0];

                Assert.assertFalse(rm.getResult().isValid());
                String expectedError = "sh:minCountError";
                Assert.assertEquals(expectedError, actualError);
                Assert.assertEquals(1, rm.getResult().errors().size());
            }
        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail("Exception should not be thrown");
        }

    }

    /**
     * Shacl min count positivetest.
     */
    @Test
    public void ShaclMinCountPositivetest() {

        try {
            aResource = new AResource(new URI("http://www.sampledomain.org/sam#AResource"));
            aResource.setAnotherIntegerProperty(new BigInteger("12"));
            aResource.setAStringProperty("Between");
            aResource.addASetOfDates(new Date());

            Model dataModel = JenaModelHelper.createJenaModel(new Object[]{aResource});
            ShaclShape shaclShape = ShaclShapeFactory.createShaclShape(AResource.class);
            Model shapeModel = JenaModelHelper.createJenaModel(new Object[]{shaclShape});
            shapeModel.write(System.out, "TURTLE");
            Validator validator = new ValidatorImpl();
            ValidationResultModel vr = validator.validate(dataModel, shapeModel);

            for (ResourceModel rm : vr.getValidResources()) {

                Assert.assertTrue(rm.getResult().isValid());
                Assert.assertEquals(0, rm.getResult().errors().size());
            }
        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail("Exception should not be thrown");
        }

    }

}
