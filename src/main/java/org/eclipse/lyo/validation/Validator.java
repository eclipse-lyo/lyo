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
import java.net.URISyntaxException;
import java.text.ParseException;
import javax.xml.datatype.DatatypeConfigurationException;
import org.apache.jena.rdf.model.Model;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.validation.shacl.ShaclShape;
import org.eclipse.lyo.validation.shacl.ValidationResult;

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
     * @return the validation result model
     *
     * @see org.eclipse.lyo.validation.shacl.ShaclShapeFactory#createShaclShape(Class)
     */
    ValidationResult validate(AbstractResource resource)
            throws OslcCoreApplicationException, URISyntaxException, ParseException,
            IllegalAccessException, IllegalArgumentException, InvocationTargetException,
            DatatypeConfigurationException;

    /**
     * Validate.
     * <p>
     * This method takes JenaModels as parameters, validates the
     * dataModel against shapeModel and return the
     * ValidationResultModel
     * <p>
     * It iterates on all the resources with in the <code>dataModel</code> and returns the first
     * error in each resource.
     *
     * @param dataModel  the data model
     * @param shapeModel the shape model
     *
     * @return the validation result model
     *
     * @throws IllegalAccessException         the illegal access exception
     * @throws InvocationTargetException      the invocation target exception
     * @throws DatatypeConfigurationException the datatype configuration exception
     * @throws OslcCoreApplicationException   the oslc core application exception
     */
    ValidationResult validate(Model dataModel, Model shapeModel)
            throws IllegalAccessException, InvocationTargetException,
            DatatypeConfigurationException, OslcCoreApplicationException;

    /**
     * Validate <code>dataModel</code> against the {@link ShaclShape} that is constructed from
     * the shape annotations in the resource class passed in the
     * <code>clazz</code> variable. The target is set to the class type of the resource class.
     * <p>
     * It iterates on all the resources with in the <code>dataModel</code> and returns the first
     * error in each resource.
     *
     * @param dataModel Data model to be validated
     * @param clazz     Resource class with shape annotations
     *
     * @return Model with the validation results
     */
    ValidationResult validate(Model dataModel, Class<? extends AbstractResource> clazz)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException,
            DatatypeConfigurationException, OslcCoreApplicationException, URISyntaxException,
            ParseException;
}
