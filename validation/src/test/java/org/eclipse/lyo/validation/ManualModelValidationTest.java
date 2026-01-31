package org.eclipse.lyo.validation;

import static org.junit.jupiter.api.Assertions.fail;

import java.math.BigInteger;
import java.net.URI;
import java.util.Date;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Resource;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.shacl.ShaclShapeFactory;
import org.eclipse.lyo.shacl.Shape;
import org.eclipse.lyo.shacl.ValidationReport;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * Tests for OSLC Range and ValueType validation by manually constructing invalid models.
 */
@DisplayName("Manual Model Validation Tests for OSLC Constraints")
public class ManualModelValidationTest {

    @Test
    @DisplayName("Should fail when object property has wrong type (range violation)")
    public void testRangeViolation() {
        try {
            // Create a valid resource first
            AnOslcResource resource = new AnOslcResource(new URI("http://example.com/resource1"));
            resource.setAnIntegerProperty(new BigInteger("10"));
            resource.setIntegerProperty2(new BigInteger("10"));
            resource.setIntegerProperty3(new BigInteger("10"));
            resource.setAStringProperty("SomeString");
            resource.addASetOfDates(new Date());

            // Generate the model
            Model dataModel = JenaModelHelper.createJenaModel(new Object[] { resource });
            Resource resourceRes = dataModel.getResource("http://example.com/resource1");

            // Add a reference property pointing to a resource of wrong type
            // aReferenceProperty expects SampleAdaptorConstants.TYPE_ANOTHERRESOURCE
            // We create a resource of different type
            Resource wrongTypeRes = dataModel.createResource("http://example.com/wrongTypeRes");
            wrongTypeRes.addProperty(org.apache.jena.vocabulary.RDF.type,
                    dataModel.createResource("http://example.com/SomeOtherType"));

            resourceRes.addProperty(
                    dataModel.createProperty(SampleAdaptorConstants.SAMPLEDOMAIN_NAMSPACE + "aReferenceProperty"),
                    wrongTypeRes);

            // Create Shape
            Shape shaclShape = ShaclShapeFactory.createShaclShape(AnOslcResource.class);
            Model shapeModel = JenaModelHelper.createJenaModel(new Object[] { shaclShape });

            // Validate
            Validator validator = ValidatorFactory.createShaclExValidator();
            ValidationReport report = validator.validate(dataModel, shapeModel);

            // We expect a ClassConstraint violation (sh:class)
            // The message from Jena SHACL starts with "ClassConstraint"
            TestHelper.assertNegative(report, "ClassConstraint");

        } catch (Exception e) {
            e.printStackTrace();
            fail("Exception should not be thrown");
        }
    }

    @Test
    @DisplayName("Should fail when integer property has string value (datatype violation)")
    public void testDatatypeViolation() {
        try {
            // Create a valid resource first
            AnOslcResource resource = new AnOslcResource(new URI("http://example.com/resource2"));
            resource.setAnIntegerProperty(new BigInteger("10"));
            resource.setIntegerProperty2(new BigInteger("10"));
            resource.setIntegerProperty3(new BigInteger("10"));
            resource.setAStringProperty("SomeString");
            resource.addASetOfDates(new Date());

            // Generate the model
            Model dataModel = JenaModelHelper.createJenaModel(new Object[] { resource });
            Resource resourceRes = dataModel.getResource("http://example.com/resource2");

            // Add anotherIntegerProperty with a String value (should be Integer)
            resourceRes.addProperty(
                    dataModel.createProperty(SampleAdaptorConstants.SAMPLEDOMAIN_NAMSPACE + "anotherIntegerProperty"),
                    "NotAnInteger");

            // Create Shape
            Shape shaclShape = ShaclShapeFactory.createShaclShape(AnOslcResource.class);
            Model shapeModel = JenaModelHelper.createJenaModel(new Object[] { shaclShape });

            // Validate
            Validator validator = ValidatorFactory.createShaclExValidator();
            ValidationReport report = validator.validate(dataModel, shapeModel);

            // We expect a DatatypeConstraint violation (sh:datatype)
            // The message from Jena SHACL starts with "DatatypeConstraint"
            TestHelper.assertNegative(report, "DatatypeConstraint");

        } catch (Exception e) {
            e.printStackTrace();
            fail("Exception should not be thrown");
        }
    }
}
