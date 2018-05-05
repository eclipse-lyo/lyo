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

import es.weso.rdf.PrefixMap;
import es.weso.rdf.RDFReader;
import es.weso.rdf.jena.RDFAsJenaModel;
import es.weso.schema.Result;
import es.weso.schema.Schema;
import es.weso.schema.Schemas;
import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.URISyntaxException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;
import javax.xml.datatype.DatatypeConfigurationException;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.ResIterator;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFFormat;
import org.apache.jena.shared.PropertyNotFoundException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.validation.Validator;
import org.eclipse.lyo.validation.model.ResourceModel;
import org.eclipse.lyo.validation.model.ValidationResultModel;
import org.eclipse.lyo.validation.shacl.ShaclShape;
import org.eclipse.lyo.validation.shacl.ShaclShapeFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.Option;
import scala.util.Either;

/**
 * @since 2.3.0
 */
public class ValidatorImpl implements Validator {

    private static final Option<String> OPTION_NONE               = Option.apply(null);
    private static final String         TRIGGER_MODE_TARGET_DECLS = "TargetDecls";
    private static final String         SHACLEX                   = "SHACLex";
    private static final Logger         log                       = LoggerFactory.getLogger(ValidatorImpl.class);
    private static final String         EMPTY_MAP                 = "";

    @Override
    public ValidationResultModel validate(AbstractResource resource)
            throws OslcCoreApplicationException, URISyntaxException, ParseException,
            IllegalAccessException, IllegalArgumentException, InvocationTargetException,
            DatatypeConfigurationException {
        ShaclShape shaclShape = ShaclShapeFactory.createShaclShape(resource.getClass());
        Model shapeModel = JenaModelHelper.createJenaModel(new Object[]{shaclShape});
        Model dataModel = JenaModelHelper.createJenaModel(new Object[]{resource});
        return getValidationResults(dataModel, shapeModel);
    }

    @Override
    public ValidationResultModel validate(Model dataModel, Model shapeModel) {
        return getValidationResults(dataModel, shapeModel);
    }

    @Override
    public ValidationResultModel validate(Model dataModel, Class<? extends AbstractResource> clazz)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException,
            DatatypeConfigurationException, OslcCoreApplicationException, URISyntaxException,
            ParseException {
        ShaclShape shaclShape = ShaclShapeFactory.createShaclShape(clazz);
        Model shapeModel = JenaModelHelper.createJenaModel(new Object[]{shaclShape});
        return getValidationResults(dataModel, shapeModel);
    }

    private ValidationResultModel getValidationResults(Model dataModel, Model shapeModel) {
        if(log.isDebugEnabled()) {
            log.debug("Data model: \n{}", modelToTurtle(dataModel));
            log.debug("Shape model: \n{}", modelToTurtle(shapeModel));
        }
        ResIterator iterator = dataModel.listSubjects();
        Model model = ModelFactory.createDefaultModel();
        List<ResourceModel> validResources = new ArrayList<>();
        List<ResourceModel> invalidResources = new ArrayList<>();
        while (iterator.hasNext()) {
            // Iterating over each resource in the model

            ResourceModel resourceModel = new ResourceModel();

            final Resource resource = iterator.next();
            model.add(resource.listProperties());

            Result validationResult = validateInternal(model, shapeModel);

            if(log.isDebugEnabled()) {
                final RDFReader valReport = validationResult.validationReport().right().get();
                log.debug("Validation report: \n{}", valReport.serialize("TURTLE"));
            }

            populateResourceModel(resourceModel, model, resource, validationResult);

            populateCounts(resourceModel, validationResult.isValid(), validResources, invalidResources);

            model.remove(resource.listProperties());

            log.info(
                    "Total Number Of Resources " + (validResources.size() + invalidResources.size
                            ()));
        }

        log.info("Validations Completed; Returning ValidationResultModel");
        return populateValidationModel(validResources, invalidResources);

    }

    private void populateCounts(ResourceModel resourceModel, boolean isValid,
            List<ResourceModel> validResources, List<ResourceModel> invalidResources) {
        if (isValid) {
            validResources.add(resourceModel);
            log.info("Datamodel valid.");
        } else {
            invalidResources.add(resourceModel);
            log.info("Datamodel Invalid");
        }
        log.debug("Valid Count:" + validResources.size());
        log.debug("InValid Count:" + invalidResources.size());
    }

    private ValidationResultModel populateValidationModel(List<ResourceModel> validResources,
            List<ResourceModel> invalidResources) {
        log.debug("Populating ValidationResultModel");
        return new ValidationResultModel(validResources, invalidResources);
    }

    private Result validateInternal(Model resourceAsModel, Model shapeAsModel)
            throws IllegalArgumentException {
        RDFAsJenaModel resourceAsRDFReader = new RDFAsJenaModel(resourceAsModel);
        RDFAsJenaModel shapeAsRDFReader = new RDFAsJenaModel(shapeAsModel);
        return validate(resourceAsRDFReader, shapeAsRDFReader);
    }

    private Result validate(final RDFAsJenaModel rdf, final Schema schema) {
        PrefixMap nodeMap = rdf.getPrefixMap();
        PrefixMap shapesMap = schema.pm();
        return schema.validate(
                rdf,
                TRIGGER_MODE_TARGET_DECLS,
                EMPTY_MAP,
                OPTION_NONE,
                OPTION_NONE,
                nodeMap,
                shapesMap);
    }

    private Result validate(RDFAsJenaModel resourceAsRDFReader, RDFAsJenaModel shapeAsRDFReader) {
        final Either<String, Schema> schemaTry = Schemas.fromRDF(shapeAsRDFReader, SHACLEX);
        if (schemaTry.isRight()) {
            Schema schema = schemaTry.right().get();
            return validate(resourceAsRDFReader, schema);
        } else {
            throw new IllegalArgumentException("A given Shape cannot be used to create a correct "
                                                       + "Schema");
        }
    }

    // TODO Andrew@2018-05-05: contrib to lyo-core JMH
    public static String modelToTurtle(final RDFAsJenaModel shaclexModel) {
        final Model jenaModel = shaclexModel.model();
        return modelToTurtle(jenaModel);
    }

    // TODO Andrew@2018-05-05: contrib to lyo-core JMH
    public static String modelToTurtle(final Model jenaModel) {
        final OutputStream out = new ByteArrayOutputStream();
        RDFDataMgr.write(out, jenaModel, RDFFormat.TURTLE);
        return out.toString();
    }

    private static void populateResourceModel(ResourceModel resourceModel, Model model,
            final Resource resource, Result validationResult) {
        try {
            log.debug("setting title");
            resourceModel.setTitle(resource.getRequiredProperty(
                    model.getProperty("http://purl.org/dc/terms/title")).getObject().toString());
        } catch (PropertyNotFoundException e) {
            try {
                log.debug("setting title");
                resourceModel.setTitle(resource.getRequiredProperty(
                        model.getProperty("http://purl.org/dc/terms#title"))
                        .getObject()
                        .toString());
            } catch (PropertyNotFoundException ex) {
                log.debug("title doesn't exist");
                resourceModel.setTitle("No title exists");
            }
        }

        try {
            log.debug("setting description");
            resourceModel.setDescription(resource.getRequiredProperty(
                    model.getProperty("http://purl.org/dc/terms/description"))
                    .getObject()
                    .toString());
        } catch (PropertyNotFoundException e) {
            try {
                log.debug("setting description");
                resourceModel.setDescription(resource.getRequiredProperty(
                        model.getProperty("http://purl.org/dc/terms#description"))
                        .getObject()
                        .toString());
            } catch (PropertyNotFoundException ex) {
                log.debug("description doesn't exist");
                resourceModel.setDescription("No desciption exists");
            }
        }
        try {
            log.debug("setting uri");
            resourceModel.setURI(model.getSeq(resource).toString());
        } catch (Exception e) {
            log.debug("uri doesn't exist");
            resourceModel.setURI("No uri exists");
        }

        resourceModel.setResult(validationResult);
    }
}
