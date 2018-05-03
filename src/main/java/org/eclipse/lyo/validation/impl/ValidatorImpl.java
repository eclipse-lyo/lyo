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
import es.weso.rdf.jena.RDFAsJenaModel;
import es.weso.schema.Result;
import es.weso.schema.Schema;
import es.weso.schema.Schemas;
import es.weso.shapeMaps.ShapeMap;
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
import scala.None;
import scala.Option;
import scala.util.Either;

/**
 * @since 2.3.0
 */
public class ValidatorImpl implements Validator {

    private static final Option<String> OPTION_NONE = Option.apply(null);
    private static final String TRIGGER_MODE = "TargetDecls";
    private static final String SHACLEX = "SHACLex";
    private static final Logger log = LoggerFactory.getLogger(ValidatorImpl.class);

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

        ResourceModel resourceModel;
        Result validationResult;
        ResIterator iterator = dataModel.listSubjects();
        Model model = ModelFactory.createDefaultModel();
        List<ResourceModel> validResources = new ArrayList<>();
        List<ResourceModel> invalidResources = new ArrayList<>();
        boolean isValid = false;
        while (iterator.hasNext()) {
            // Iterating over each resource in the model

            resourceModel = new ResourceModel();

            final Resource resource = iterator.next();
            model.add(resource.listProperties());

            validationResult = validateInternal(model, shapeModel);
            if (validationResult.isValid()) {
                isValid = true;
            }

            populateResourceModel(resourceModel, model, resource, validationResult);

            populateCounts(resourceModel, isValid, validResources, invalidResources);

            model.remove(resource.listProperties());
            isValid = false;

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
//        final ShapeMap empty = ShapeMap.empty();
//        final String shapeMap = empty.toString();
        return schema.validate(rdf, TRIGGER_MODE, "", OPTION_NONE, OPTION_NONE, nodeMap,
                               shapesMap);
    }

    private Result validate(RDFAsJenaModel resourceAsRDFReader, RDFAsJenaModel shapeAsRDFReader) {
        Schema schema = null;
        final Either<String, Schema> schemaTry = Schemas.fromRDF(shapeAsRDFReader, SHACLEX);
        if (schemaTry.isRight()) {
            schema = schemaTry.toOption().get();
        }
        return validate(resourceAsRDFReader, schema);
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
