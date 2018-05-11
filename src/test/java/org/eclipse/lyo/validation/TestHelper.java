package org.eclipse.lyo.validation;

import es.weso.schema.ErrorInfo;
import org.apache.jena.rdf.model.Model;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.validation.shacl.ShaclShape;
import org.eclipse.lyo.validation.shacl.ShaclShapeFactory;
import org.eclipse.lyo.validation.shacl.ValidationResult;
import org.junit.Assert;
import org.junit.Test;
import scala.Option;

import static org.assertj.core.api.Assertions.assertThat;

public class TestHelper {

    public static ValidationResult performTest(AbstractResource resource) {

        try {

            Model dataModel = JenaModelHelper.createJenaModel(new Object[]{resource});
            ShaclShape shaclShape = ShaclShapeFactory.createShaclShape(resource.getClass());
            Model shapeModel = JenaModelHelper.createJenaModel(new Object[]{shaclShape});

            Validator validator = ValidatorFactory.createShaclExValidator();
            return validator.validate(dataModel, shapeModel);

        } catch (Exception e) {
            Assert.fail("Exception should not be thrown");
        }
        return null;
    }

    public static void assertNegative(ValidationResult vr, String errorMessage){
        final Option<ErrorInfo> errorInfoOption = vr.getResult().errors().headOption();
        if (errorInfoOption.isDefined()) {
            final ErrorInfo errorInfo = errorInfoOption.get();
            assertThat(errorInfo.msg()).startsWith(errorMessage);
        }
        Assert.assertEquals(1, vr.getErrors().size());
    }

    public static void assertPositive(ValidationResult vr) {
        Assert.assertTrue(vr.isValid());
        Assert.assertEquals(0, vr.getResult().errors().size());
    }
}
