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

package org.eclipse.lyo.validation.impl;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.InvocationTargetException;
import java.net.URISyntaxException;
import java.text.ParseException;

import javax.xml.datatype.DatatypeConfigurationException;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.RDFLanguages;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.validation.Validator;
import org.eclipse.lyo.validation.shacl.ShaclShape;
import org.eclipse.lyo.validation.shacl.ShaclShapeFactory;
import org.eclipse.lyo.validation.shacl.ValidationReport;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import es.weso.rdf.PrefixMap;
import es.weso.rdf.RDFReader;
import es.weso.rdf.jena.RDFAsJenaModel;
import es.weso.schema.Result;
import es.weso.schema.Schema;
import es.weso.schema.Schemas;
import scala.Option;
import scala.util.Either;

/**
 * @since 2.3.0
 */
public class ShaclExValidatorImpl implements Validator {

    private static final Option<String> OPTION_NONE = Option.apply(null);
    private static final String TRIGGER_MODE_TARGET_DECLS = "TargetDecls";
    private static final String SHACLEX = "SHACLex";
    private static final Logger log = LoggerFactory.getLogger(ShaclExValidatorImpl.class);
    private static final String EMPTY_MAP = "";

    @Override
    public ValidationReport validate(AbstractResource resource) throws OslcCoreApplicationException, URISyntaxException,
            ParseException, IllegalAccessException, IllegalArgumentException, InvocationTargetException,
            DatatypeConfigurationException, InstantiationException, SecurityException, NoSuchMethodException {
        ShaclShape shaclShape = ShaclShapeFactory.createShaclShape(resource.getClass());
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
        ShaclShape shaclShape = ShaclShapeFactory.createShaclShape(clazz);
        Model shapeModel = JenaModelHelper.createJenaModel(new Object[] { shaclShape });
        return getValidationResults(dataModel, shapeModel);
    }

    private ValidationReport getValidationResults(Model dataModel, Model shapeModel) throws IllegalAccessException,
            IllegalArgumentException, InstantiationException, InvocationTargetException, SecurityException,
            NoSuchMethodException, DatatypeConfigurationException, OslcCoreApplicationException, URISyntaxException {
        Model valResultJenaModel = ModelFactory.createDefaultModel();
        if (log.isDebugEnabled()) {
            log.debug("Data model: \n{}", dataModel.write(System.out, RDFLanguages.strLangTurtle));
            log.debug("Shape model: \n{}", shapeModel.write(System.out, RDFLanguages.strLangTurtle));
        }

        Result result = validateInternal(dataModel, shapeModel);

        final RDFReader valReport = result.validationReport().right().get();
        Either<String, String> valReportAsTurtle = valReport.serialize(RDFLanguages.strLangTurtle);

        if (log.isDebugEnabled()) {
            log.debug("Validation report: \n{}", valReportAsTurtle.right().get());
        }

        try {
            final InputStream in = new ByteArrayInputStream(valReportAsTurtle.right().get().getBytes("UTF-8"));
            valResultJenaModel.read(in, null, RDFLanguages.strLangTurtle);
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        }
        return populateValidationReport(result);
    }

    private Result validateInternal(Model resourceAsModel, Model shapeAsModel) throws IllegalArgumentException {
        RDFAsJenaModel resourceAsRDFReader = new RDFAsJenaModel(resourceAsModel);
        RDFAsJenaModel shapeAsRDFReader = new RDFAsJenaModel(shapeAsModel);
        return validate(resourceAsRDFReader, shapeAsRDFReader);
    }

    private Result validate(final RDFAsJenaModel rdf, final Schema schema) {
        PrefixMap nodeMap = rdf.getPrefixMap();
        PrefixMap shapesMap = schema.pm();
        return schema.validate(rdf, TRIGGER_MODE_TARGET_DECLS, EMPTY_MAP, OPTION_NONE, OPTION_NONE, nodeMap, shapesMap);
    }

    private Result validate(RDFAsJenaModel resourceAsRDFReader, RDFAsJenaModel shapeAsRDFReader) {
        final Either<String, Schema> schemaTry = Schemas.fromRDF(shapeAsRDFReader, SHACLEX);
        if (schemaTry.isRight()) {
            Schema schema = schemaTry.right().get();
            return validate(resourceAsRDFReader, schema);
        } else {
            throw new IllegalArgumentException("A given Shape cannot be used to create a correct " + "Schema");
        }
    }

    ValidationReport populateValidationReport(Result result) throws IllegalAccessException, IllegalArgumentException,
            InstantiationException, InvocationTargetException, SecurityException, NoSuchMethodException,
            DatatypeConfigurationException, OslcCoreApplicationException, URISyntaxException {
        Model valReportJenaModel = ModelFactory.createDefaultModel();
        final RDFReader valReport = result.validationReport().right().get();
        Either<String, String> valReportAsTurtle = valReport.serialize(RDFLanguages.strLangTurtle);

        if (log.isDebugEnabled()) {
            log.debug("Validation report: \n{}", valReportAsTurtle.right().get());
        }

        try {
            final InputStream in = new ByteArrayInputStream(valReportAsTurtle.right().get().getBytes("UTF-8"));
            valReportJenaModel.read(in, null, RDFLanguages.strLangTurtle);
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        }
        return desearializeJenaModelToJavaObject(valReportJenaModel);
    }

    ValidationReport desearializeJenaModelToJavaObject(Model valReportJenaModel) throws IllegalAccessException,
            IllegalArgumentException, InstantiationException, InvocationTargetException, SecurityException,
            NoSuchMethodException, DatatypeConfigurationException, OslcCoreApplicationException, URISyntaxException {

        Object[] fromJenaModel = JenaModelHelper.fromJenaModel(valReportJenaModel, ValidationReport.class);

        return (ValidationReport) fromJenaModel[0];

    }
}
