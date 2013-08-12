/*******************************************************************************
 * Copyright (c) 2013 IBM Corporation.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 *     Samuel Padgett - initial implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.test;

import java.net.URISyntaxException;

import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreMissingAnnotationException;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.ResourceShapeFactory;
import org.eclipse.lyo.oslc4j.core.test.resources.MissingResourceShapeAnnotation;
import org.junit.Test;

public class ShapeTest {
	@Test(expected = OslcCoreMissingAnnotationException.class)
    public void testResourceClassMissingResourceShapeAnnotation()
           throws OslcCoreApplicationException,
                  URISyntaxException
    {
        ResourceShapeFactory.createResourceShape("http://bogus",
                                                 OslcConstants.PATH_RESOURCE_SHAPES,
                                                 "bogus",
                                                 MissingResourceShapeAnnotation.class);
    }
}
