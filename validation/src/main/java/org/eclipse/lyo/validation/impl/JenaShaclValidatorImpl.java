/*
 * Copyright (c) 2021 Contributors to the Eclipse Foundation
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

package org.eclipse.lyo.validation.impl;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.InvocationTargetException;
import java.net.URISyntaxException;
import java.text.ParseException;

import javax.xml.datatype.DatatypeConfigurationException;

import org.apache.jena.graph.Graph;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.RDFLanguages;
import org.apache.jena.shacl.ShaclValidator;
import org.apache.jena.shacl.Shapes;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.shacl.ShaclShapeFactory;
import org.eclipse.lyo.shacl.Shape;
import org.eclipse.lyo.shacl.ValidationReport;
import org.eclipse.lyo.validation.Validator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class JenaShaclValidatorImpl implements Validator {

    private static final Logger log = LoggerFactory.getLogger(JenaShaclValidatorImpl.class);

    @Override
    public ValidationReport validate(AbstractResource resource) throws OslcCoreApplicationException, URISyntaxException,
            ParseException, IllegalAccessException, IllegalArgumentException, InvocationTargetException,
            DatatypeConfigurationException, InstantiationException, SecurityException, NoSuchMethodException {
        Shape shaclShape = ShaclShapeFactory.createShaclShape(resource.getClass());
        Model shapeModel = JenaModelHelper.createJenaModel(new Object[] { shaclShape });
        Model dataModel = JenaModelHelper.createJenaModel(new Object[] { resource });
        return getValidationResults(dataModel, shapeModel);
    }

    @Override
    public ValidationReport validate(Model dataModel, Model shapeModel) throws IllegalAccessException,
            IllegalArgumentException, InstantiationException, InvocationTargetException, SecurityException,
            NoSuchMethodException, DatatypeConfigurationException, OslcCoreApplicationException, URISyntaxException {
        return getValidationResults(dataModel, shapeModel);
    }

    @Override
    public ValidationReport validate(Model dataModel, Class<? extends AbstractResource> clazz)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException,
            DatatypeConfigurationException, OslcCoreApplicationException, URISyntaxException, ParseException,
            InstantiationException, SecurityException, NoSuchMethodException {
        Shape shaclShape = ShaclShapeFactory.createShaclShape(clazz);
        Model shapeModel = JenaModelHelper.createJenaModel(new Object[] { shaclShape });
        return getValidationResults(dataModel, shapeModel);
    }

    private ValidationReport getValidationResults(Model dataModel, Model shapeModel) throws IllegalAccessException,
            IllegalArgumentException, InstantiationException, InvocationTargetException, SecurityException,
            NoSuchMethodException, DatatypeConfigurationException, OslcCoreApplicationException, URISyntaxException {

        if (log.isDebugEnabled()) {
            log.debug("Data model: \n{}", dataModel.write(System.out, RDFLanguages.strLangTurtle));
            log.debug("Shape model: \n{}", shapeModel.write(System.out, RDFLanguages.strLangTurtle));
        }

        Graph shapesGraph = shapeModel.getGraph();
        Graph dataGraph = dataModel.getGraph();

        Shapes shapes = Shapes.parse(shapesGraph);
        org.apache.jena.shacl.ValidationReport report = ShaclValidator.get().validate(shapes, dataGraph);

        if (log.isDebugEnabled()) {
            log.debug("Validation report: \n{}", report.getModel().write(System.out, RDFLanguages.strLangTurtle));
        }

        return fromJenaReport(report);
    }

    private ValidationReport fromJenaReport(org.apache.jena.shacl.ValidationReport jenaReport) throws IllegalAccessException,
            IllegalArgumentException, InstantiationException, InvocationTargetException, SecurityException,
            NoSuchMethodException, DatatypeConfigurationException, OslcCoreApplicationException, URISyntaxException {

        // The Jena report has a getModel() method returning the report as a Jena Model.
        Model reportModel = jenaReport.getModel();

        // We use JenaModelHelper to convert the Jena Model back to Lyo's ValidationReport object.
        // JenaModelHelper expects the model to contain the resource.

        Object[] fromJenaModel = JenaModelHelper.fromJenaModel(reportModel, ValidationReport.class);

        if (fromJenaModel.length > 0) {
            return (ValidationReport) fromJenaModel[0];
        } else {
            // This might happen if the report is empty or mapped incorrectly.
            // A SHACL validation report always has at least one ValidationReport node.
            // However, JenaModelHelper might need the resource to be typed correctly in the model.
            // SHACL validation report is of type sh:ValidationReport.
            // I need to ensure that ValidationReport.class is mapped to sh:ValidationReport.
            throw new RuntimeException("Could not deserialize ValidationReport from Jena Model");
        }
    }
}
