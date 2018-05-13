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

import java.lang.reflect.InvocationTargetException;
import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.text.ParseException;

import javax.xml.datatype.DatatypeConfigurationException;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.util.FileUtils;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.validation.shacl.ValidationReport;

/**
 * @author Yash Khatri
 * @version $version-stub$
 * @since 2.3.0
 */
public class ValidationExample {

    /**
     * Loads an example SHACL file and validates all focus nodes against all shapes.
     * 
     * @throws URISyntaxException
     * @throws NoSuchMethodException
     * @throws SecurityException
     * @throws InstantiationException
     * @throws IllegalArgumentException
     */
    public static void validateModelAgainstShaclModel() throws IllegalAccessException, InvocationTargetException,
            DatatypeConfigurationException, OslcCoreApplicationException, IllegalArgumentException,
            InstantiationException, SecurityException, NoSuchMethodException, URISyntaxException {

        // Load the main data model
        Model dataModel = ModelFactory.createDefaultModel();
        dataModel.read(ValidationExample.class.getResourceAsStream("/aResource-Data.ttl"), "urn:dummy",
                FileUtils.langTurtle);

        // Load the SHACL shape model
        Model shapeModel = ModelFactory.createDefaultModel();
        shapeModel.read(ValidationExample.class.getResourceAsStream("/aResource-Shape.ttl"), "urn:dummy",
                FileUtils.langTurtle);

        Validator validator = ValidatorFactory.createShaclExValidator();
        // Validate the data model against the SHACL shapes model. You can have shape
        // and data in same model.
        ValidationReport vr = validator.validate(dataModel, shapeModel);
    }

    /**
     * Creates an OSLC resource, and validates it against its class annotations
     * defining its SHACL Shape.
     * 
     * @throws NoSuchMethodException
     * @throws SecurityException
     * @throws InstantiationException
     */
    public static void validateOslcResourceWithShaclAnnotations()
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException,
            OslcCoreApplicationException, URISyntaxException, ParseException, DatatypeConfigurationException,
            InstantiationException, SecurityException, NoSuchMethodException {

        // Create an OSLC resource
        AResource aResource = new AResource(new URI("http://www.sampledomain.org/sam#AResource"));
        aResource.setAnIntegerProperty(new BigInteger("101"));
        aResource.setAStringProperty("ABC");

        // Validate the OSLC resource against the SHACL Shape annotations - as defined
        // in the class @shacl annotations.
        Validator validator = ValidatorFactory.createShaclExValidator();
        ValidationReport vr = validator.validate(aResource);
    }

    /**
     * Creates an OSLC resource, and validates it against its class annotations
     * defining its OSLC Shape.
     * 
     * @throws NoSuchMethodException
     * @throws SecurityException
     * @throws InstantiationException
     */
    public static void validateOslcResourceWithOslcAnnotations()
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException,
            OslcCoreApplicationException, URISyntaxException, ParseException, DatatypeConfigurationException,
            InstantiationException, SecurityException, NoSuchMethodException {

        // Create an OSLC resource
        AnOslcResource anOslcResource = new AnOslcResource(new URI("http://www.sampledomain.org/sam#AnOslcResource"));
        anOslcResource.setAnIntegerProperty(new BigInteger("101"));

        // Validate the OSLC resource against the OSLC Shape annotations - as defined in
        // the class @oslc annotations.
        Validator validator = ValidatorFactory.createShaclExValidator();
        ValidationReport vr = validator.validate(anOslcResource);
    }

    public static void main(String[] args) throws Exception {
        validateModelAgainstShaclModel();
        validateOslcResourceWithShaclAnnotations();
        validateOslcResourceWithOslcAnnotations();
    }
}
