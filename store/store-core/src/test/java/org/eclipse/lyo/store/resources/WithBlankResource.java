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
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
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
@OslcNamespace(Nsp1DomainConstants.WITHBLANKRESOURCE_NAMESPACE)
@OslcName(Nsp1DomainConstants.WITHBLANKRESOURCE_LOCALNAME)
@OslcResourceShape(
        title = "WithBlankResource Resource Shape",
        describes = Nsp1DomainConstants.WITHBLANKRESOURCE_TYPE)
public class WithBlankResource extends AbstractResource implements IWithBlankResource {
    // Start of user code attributeAnnotation:relatesToBlankResource
    // End of user code
    private BlankResource relatesToBlankResource = new BlankResource();
    // Start of user code attributeAnnotation:stringProperty
    // End of user code
    private String stringProperty;

    // Start of user code classAttributes
    // End of user code
    // Start of user code classMethods
    // End of user code
    public WithBlankResource() throws URISyntaxException {
        super();

        // Start of user code constructor1
        // End of user code
    }

    public WithBlankResource(final URI about) throws URISyntaxException {
        super(about);

        // Start of user code constructor2
        // End of user code
    }

    public static ResourceShape createResourceShape()
            throws OslcCoreApplicationException, URISyntaxException {
        return ResourceShapeFactory.createResourceShape(
                OSLC4JUtils.getServletURI(),
                OslcConstants.PATH_RESOURCE_SHAPES,
                Nsp1DomainConstants.WITHBLANKRESOURCE_PATH,
                WithBlankResource.class);
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
                            + "{a Local WithBlankResource Resource} - update"
                            + " WithBlankResource.toString() to present resource as desired.";
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

    // Start of user code getterAnnotation:relatesToBlankResource
    // End of user code
    @OslcName("relatesToBlankResource")
    @OslcPropertyDefinition(Nsp1DomainConstants.TESTDOMAIN_NAMSPACE + "relatesToBlankResource")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.LocalResource)
    @OslcRange({Nsp1DomainConstants.BLANKRESOURCE_TYPE})
    @OslcReadOnly(false)
    public BlankResource getRelatesToBlankResource() {
        // Start of user code getterInit:relatesToBlankResource
        // End of user code
        return relatesToBlankResource;
    }

    // Start of user code getterAnnotation:stringProperty
    // End of user code
    @OslcName("stringProperty")
    @OslcPropertyDefinition(Nsp1DomainConstants.TESTDOMAIN_NAMSPACE + "stringProperty")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public String getStringProperty() {
        // Start of user code getterInit:stringProperty
        // End of user code
        return stringProperty;
    }

    // Start of user code setterAnnotation:relatesToBlankResource
    // End of user code
    public void setRelatesToBlankResource(final BlankResource relatesToBlankResource) {
        // Start of user code setterInit:relatesToBlankResource
        // End of user code
        this.relatesToBlankResource = relatesToBlankResource;

        // Start of user code setterFinalize:relatesToBlankResource
        // End of user code
    }

    // Start of user code setterAnnotation:stringProperty
    // End of user code
    public void setStringProperty(final String stringProperty) {
        // Start of user code setterInit:stringProperty
        // End of user code
        this.stringProperty = stringProperty;

        // Start of user code setterFinalize:stringProperty
        // End of user code
    }

    public String relatesToBlankResourceToHtml() {
        String s = "";

        // Start of user code relatesToBlankResourcetoHtml_mid
        // End of user code

        try {
            if (relatesToBlankResource == null) {
                s = s + "<em>null</em>";
            } else {
                s = s + relatesToBlankResource.toHtml(true);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        // Start of user code relatesToBlankResourcetoHtml_finalize
        // End of user code

        return s;
    }

    public String stringPropertyToHtml() {
        String s = "";

        // Start of user code stringPropertytoHtml_mid
        // End of user code

        try {
            if (stringProperty == null) {
                s = s + "<em>null</em>";
            } else {
                s = s + stringProperty.toString();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        // Start of user code stringPropertytoHtml_finalize
        // End of user code

        return s;
    }
}
