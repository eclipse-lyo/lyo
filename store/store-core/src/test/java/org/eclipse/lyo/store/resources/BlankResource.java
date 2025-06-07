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

package org.eclipse.lyo.store.resources;

import java.net.URI;
import java.net.URISyntaxException;
import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.ResourceShape;
import org.eclipse.lyo.oslc4j.core.model.ResourceShapeFactory;
import org.eclipse.lyo.oslc4j.core.model.ValueType;

// Start of user code imports
// End of user code

// Start of user code preClassCode
// End of user code

// Start of user code classAnnotations
// End of user code
@OslcNamespace(Nsp1DomainConstants.BLANKRESOURCE_NAMESPACE)
@OslcName(Nsp1DomainConstants.BLANKRESOURCE_LOCALNAME)
@OslcResourceShape(
        title = "BlankResource Resource Shape",
        describes = Nsp1DomainConstants.BLANKRESOURCE_TYPE)
public class BlankResource extends AbstractResource implements IBlankResource {
    // Start of user code attributeAnnotation:intProperty
    // End of user code
    private Integer intProperty;

    // Start of user code classAttributes
    // End of user code
    // Start of user code classMethods
    // End of user code
    public BlankResource() throws URISyntaxException {
        super();

        // Start of user code constructor1
        // End of user code
    }

    public BlankResource(final URI about) throws URISyntaxException {
        super(about);

        // Start of user code constructor2
        // End of user code
    }

    public static ResourceShape createResourceShape()
            throws OslcCoreApplicationException, URISyntaxException {
        return ResourceShapeFactory.createResourceShape(
                OSLC4JUtils.getServletURI(),
                OslcConstants.PATH_RESOURCE_SHAPES,
                Nsp1DomainConstants.BLANKRESOURCE_PATH,
                BlankResource.class);
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
                            + "{a Local BlankResource Resource} - update BlankResource.toString()"
                            + " to present resource as desired.";
            // Start of user code toString_bodyForLocalResource
            // End of user code
        } else {
            result = getAbout().toString();
        }

        // Start of user code toString_finalize
        // End of user code

        return result;
    }

    public String toHtml() {
        return toHtml(false);
    }

    public String toHtml(boolean asLocalResource) {
        String result = "";
        // Start of user code toHtml_init
        // End of user code

        if (asLocalResource) {
            result = toString(true);
            // Start of user code toHtml_bodyForLocalResource
            // End of user code
        } else {
            result =
                    "<a href=\""
                            + getAbout()
                            + "\" class=\"oslc-resource-link\">"
                            + toString()
                            + "</a>";
        }

        // Start of user code toHtml_finalize
        // End of user code

        return result;
    }

    // Start of user code getterAnnotation:intProperty
    // End of user code
    @OslcName("intProperty")
    @OslcPropertyDefinition(Nsp1DomainConstants.TESTDOMAIN_NAMSPACE + "intProperty")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.Integer)
    @OslcReadOnly(false)
    public Integer getIntProperty() {
        // Start of user code getterInit:intProperty
        // End of user code
        return intProperty;
    }

    // Start of user code setterAnnotation:intProperty
    // End of user code
    public void setIntProperty(final Integer intProperty) {
        // Start of user code setterInit:intProperty
        // End of user code
        this.intProperty = intProperty;

        // Start of user code setterFinalize:intProperty
        // End of user code
    }

    /*
        static public String intPropertyToHtmlForCreation (final HttpServletRequest httpServletRequest)
        {
            String s = "";

            // Start of user code "Init:intPropertyToHtmlForCreation(...)"
            // End of user code

            s = s + "<label for=\"intProperty\">intProperty: </LABEL>";

            // Start of user code "Mid:intPropertyToHtmlForCreation(...)"
            // End of user code

            s= s + "<input name=\"intProperty\" type=\"text\" style=\"width: 400px\" id=\"intProperty\" >";
            // Start of user code "Finalize:intPropertyToHtmlForCreation(...)"
            // End of user code

            return s;
        }
    */

    public String intPropertyToHtml() {
        String s = "";

        // Start of user code intPropertytoHtml_mid
        // End of user code

        try {
            if (intProperty == null) {
                s = s + "<em>null</em>";
            } else {
                s = s + intProperty.toString();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        // Start of user code intPropertytoHtml_finalize
        // End of user code

        return s;
    }
}
