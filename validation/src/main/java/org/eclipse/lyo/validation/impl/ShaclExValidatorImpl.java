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

package org.eclipse.lyo.validation.impl;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.InvocationTargetException;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.text.ParseException;

import javax.xml.datatype.DatatypeConfigurationException;

import cats.effect.IO;
import cats.effect.IO$;
import cats.effect.kernel.Resource;
import cats.effect.kernel.Resource$;
import cats.effect.unsafe.IORuntime;
import cats.effect.unsafe.IORuntime$;
import es.weso.rdf.RDFBuilder;
import es.weso.rdf.jena.RDFAsJenaModel$;
import es.weso.schema.RDFReport;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.RDFLanguages;
import org.checkerframework.checker.units.qual.A;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.shacl.ShaclShapeFactory;
import org.eclipse.lyo.shacl.Shape;
import org.eclipse.lyo.shacl.ValidationReport;
import org.eclipse.lyo.validation.Validator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import es.weso.rdf.PrefixMap;
import es.weso.rdf.RDFReader;
import es.weso.rdf.jena.RDFAsJenaModel;
import es.weso.schema.Result;
import es.weso.schema.Schema;
import es.weso.schema.Schemas;
import scala.Option;
import scala.collection.immutable.HashMap;
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
    private static final IORuntime IO_RUNTIME = IORuntime$.MODULE$.global();

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
        Model valResultJenaModel = ModelFactory.createDefaultModel();
        if (log.isDebugEnabled()) {
            log.debug("Data model: \n{}", dataModel.write(System.out, RDFLanguages.strLangTurtle));
            log.debug("Shape model: \n{}", shapeModel.write(System.out, RDFLanguages.strLangTurtle));
        }

        Result result = validateInternal(dataModel, shapeModel);

        final RDFReport validationReport = result.validationReport();

        if (log.isDebugEnabled()) {
            log.debug("Validation report: \n{}", validationReport);
        }

        final String turtleReport = reportToTurtle(validationReport);

        final InputStream in = new ByteArrayInputStream(turtleReport.getBytes(StandardCharsets.UTF_8));
        valResultJenaModel.read(in, null, RDFLanguages.strLangTurtle);
        return populateValidationReport(result);
    }

    private String reportToTurtle(RDFReport validationReport) {
        final Model model = ModelFactory.createDefaultModel();
        final RDFAsJenaModel rdfAsJenaModel = getRdfAsJenaModel(model);
        final IO<RDFBuilder> builderIO = validationReport.toRDF(rdfAsJenaModel);
        final RDFBuilder rdfBuilder = builderIO.unsafeRunSync(IO_RUNTIME);
        final IO<String> stringIO = rdfBuilder.serialize(RDFLanguages.strLangNTriples, Option.apply(null));
        final String turtleReport = stringIO.unsafeRunSync(IO_RUNTIME);
        return turtleReport;
    }

    private RDFAsJenaModel getRdfAsJenaModel(Model model) {
        final IO<RDFAsJenaModel> modelIO = RDFAsJenaModel.fromModel(model, Option.apply(null),
            Option.apply(null), new HashMap<>(), new HashMap<>());
        final RDFAsJenaModel rdfAsJenaModel = modelIO.unsafeRunSync(IO_RUNTIME);
        return rdfAsJenaModel;
    }

    private Result validateInternal(Model resourceAsModel, Model shapeAsModel) throws IllegalArgumentException {
        RDFAsJenaModel resourceAsRDFReader = getRdfAsJenaModel(resourceAsModel);
        RDFAsJenaModel shapeAsRDFReader = getRdfAsJenaModel(shapeAsModel);
        return validate(resourceAsRDFReader, shapeAsRDFReader);
    }

    private Result validate(final RDFAsJenaModel rdf, final Schema schema) {
        PrefixMap nodeMap = rdf.getPrefixMap().unsafeRunSync(IO_RUNTIME);
        PrefixMap shapesMap = schema.pm();
        return schema.validate(rdf, TRIGGER_MODE_TARGET_DECLS, EMPTY_MAP, OPTION_NONE, OPTION_NONE, nodeMap, shapesMap, rdf)
            .unsafeRunSync(IO_RUNTIME);
    }

    private Result validate(RDFAsJenaModel resourceAsRDFReader, RDFAsJenaModel shapeAsRDFReader) {
        final Schema schema = Schemas.fromRDF(shapeAsRDFReader, SHACLEX)
            .onError(throwable -> {throw new IllegalArgumentException("A given Shape cannot be used to create a correct " + "Schema");})
            .unsafeRunSync(IO_RUNTIME);
        return validate(resourceAsRDFReader, schema);
    }

    ValidationReport populateValidationReport(Result result) throws IllegalAccessException, IllegalArgumentException,
            InstantiationException, InvocationTargetException, SecurityException, NoSuchMethodException,
            DatatypeConfigurationException, OslcCoreApplicationException, URISyntaxException {
        Model valReportJenaModel = ModelFactory.createDefaultModel();
        final RDFReport valReport = result.validationReport();
        String valReportAsTurtle = reportToTurtle(valReport);

        if (log.isDebugEnabled()) {
            log.debug("Validation report: \n{}", valReportAsTurtle);
        }

        try {
            final InputStream in = new ByteArrayInputStream(valReportAsTurtle.getBytes("UTF-8"));
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
