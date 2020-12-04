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
package org.eclipse.lyo.validation;

import java.lang.reflect.InvocationTargetException;
import java.net.URISyntaxException;
import java.text.ParseException;

import javax.xml.datatype.DatatypeConfigurationException;

import org.apache.jena.rdf.model.Model;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.shacl.ShaclShapeFactory;
import org.eclipse.lyo.shacl.Shape;
import org.eclipse.lyo.shacl.ValidationReport;

/**
 * @author Yash Khatri
 * @version $version-stub$
 * @since 2.3.0
 */
public interface Validator {

    /**
     * Validate a single resource against the shape based on its class annotations.
     *
     * @param resource Resource to be validated
     *
     * @return {@link ValidationReport}
     * @throws NoSuchMethodException 
     * @throws SecurityException 
     * @throws InstantiationException 
     *
     * @see ShaclShapeFactory#createShaclShape(Class)
     */
    ValidationReport validate(AbstractResource resource)
            throws OslcCoreApplicationException, URISyntaxException, ParseException,
            IllegalAccessException, IllegalArgumentException, InvocationTargetException,
            DatatypeConfigurationException, InstantiationException, SecurityException, NoSuchMethodException;

    /**
     * Validate.
     * <p>
     * This method takes JenaModels as parameters, validates the
     * dataModel against shapeModel and return the
     * ValidationResultModel
     * <p>
     * It iterates on all the resources with in the <code>dataModel</code> and returns all the errors
     * found in all resources. 
     *
     * @param dataModel  the data model
     * @param shapeModel the shape model
     *
     * @return {@link ValidationReport}
     *
     * @throws IllegalAccessException         the illegal access exception
     * @throws InvocationTargetException      the invocation target exception
     * @throws DatatypeConfigurationException the datatype configuration exception
     * @throws OslcCoreApplicationException   the oslc core application exception
     * @throws URISyntaxException 
     * @throws NoSuchMethodException 
     * @throws SecurityException 
     * @throws InstantiationException 
     * @throws IllegalArgumentException 
     */
    ValidationReport validate(Model dataModel, Model shapeModel)
            throws IllegalAccessException, InvocationTargetException,
            DatatypeConfigurationException, OslcCoreApplicationException, IllegalArgumentException, InstantiationException, SecurityException, NoSuchMethodException, URISyntaxException;

    /**
     * Validate <code>dataModel</code> against the {@link Shape} that is constructed from
     * the shape annotations in the resource class passed in the
     * <code>clazz</code> variable. The target is set to the class type of the resource class.
     * <p>
     * It iterates on all the resources with in the <code>dataModel</code> and returns all
     * the errors in each resource.
     *
     * @param dataModel Data model to be validated
     * @param clazz     Resource class with shape annotations
     *
     * @return {@link ValidationReport}
     * @throws NoSuchMethodException 
     * @throws SecurityException 
     * @throws InstantiationException 
     */
    ValidationReport validate(Model dataModel, Class<? extends AbstractResource> clazz)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException,
            DatatypeConfigurationException, OslcCoreApplicationException, URISyntaxException,
            ParseException, InstantiationException, SecurityException, NoSuchMethodException;
}
