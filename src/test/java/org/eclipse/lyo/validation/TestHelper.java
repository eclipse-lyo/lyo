package org.eclipse.lyo.validation;

import java.lang.reflect.InvocationTargetException;
import java.net.URISyntaxException;
import java.text.ParseException;

import javax.xml.datatype.DatatypeConfigurationException;

import org.apache.jena.rdf.model.Model;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.shacl.ShaclShapeFactory;
import org.eclipse.lyo.shacl.Shape;
import org.eclipse.lyo.shacl.ValidationReport;
import org.junit.Assert;

public class TestHelper {

    public static ValidationReport performTest(AbstractResource resource)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException,
            DatatypeConfigurationException, OslcCoreApplicationException, URISyntaxException, ParseException,
            InstantiationException, SecurityException, NoSuchMethodException {

        Model dataModel = JenaModelHelper.createJenaModel(new Object[] { resource });
        Shape shaclShape = ShaclShapeFactory.createShaclShape(resource.getClass());
        Model shapeModel = JenaModelHelper.createJenaModel(new Object[] { shaclShape });

        Validator validator = ValidatorFactory.createShaclExValidator();
        return validator.validate(dataModel, shapeModel);

    }

    public static void assertNegative(ValidationReport vr, String errorMessage) {
        if (vr.getResult().iterator().hasNext()) {
        Assert.assertTrue(vr.getResult().iterator().next().getMessage().startsWith(errorMessage));
        } else {
            Assert.fail("Validation Error should exist.");
        }
    }

    public static void assertPositive(ValidationReport vr) {
        Assert.assertTrue(vr.isConforms());
    }
}
