// Start of user code Copyright
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
// End of user code

package org.eclipse.lyo.oslc4j.core.model;

import java.net.URI;
import java.net.URISyntaxException;
import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;

// Start of user code imports
// End of user code

// Start of user code preClassCode
// End of user code

// Start of user code classAnnotations
// End of user code
@OslcNamespace(OslcDomainConstants.DISCUSSION_NAMESPACE)
@OslcName(OslcDomainConstants.DISCUSSION_LOCALNAME)
@OslcResourceShape(
        title = "Discussion Resource Shape",
        describes = OslcDomainConstants.DISCUSSION_TYPE)
public class Discussion extends AbstractResource implements IDiscussion {

    // Start of user code classAttributes
    // End of user code
    // Start of user code classMethods
    // End of user code
    public Discussion() {
        super();

        // Start of user code constructor1
        // End of user code
    }

    public Discussion(final URI about) {
        super(about);

        // Start of user code constructor2
        // End of user code
    }

    public static ResourceShape createResourceShape()
            throws OslcCoreApplicationException, URISyntaxException {
        return ResourceShapeFactory.createResourceShape(
                OSLC4JUtils.getServletURI(),
                OslcConstants.PATH_RESOURCE_SHAPES,
                OslcDomainConstants.DISCUSSION_PATH,
                Discussion.class);
    }

    public String toString() {
        return toString(false);
    }

    public String toString(boolean asLocalResource) {
        String result = "";
        // Start of user code toString_init
        // End of user code

        if (asLocalResource) {
            result =
                    result
                            + "{a Local Discussion Resource} - update Discussion.toString() to"
                            + " present resource as desired.";
            // Start of user code toString_bodyForLocalResource
            // End of user code
        } else {
            result = String.valueOf(getAbout());
        }

        // Start of user code toString_finalize
        // End of user code

        return result;
    }
}
