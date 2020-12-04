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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.text.SimpleDateFormat;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.Iterator;
//import javax.servlet.http.HttpServletRequest;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import javax.ws.rs.core.UriBuilder;

import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.annotation.OslcAllowedValue;
import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcMemberProperty;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRepresentation;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.Representation;
import org.eclipse.lyo.oslc4j.core.model.ValueType;
import org.eclipse.lyo.oslc4j.core.model.ResourceShape;
import org.eclipse.lyo.oslc4j.core.model.ResourceShapeFactory;

import org.eclipse.lyo.store.resources.Nsp1DomainConstants;
import org.eclipse.lyo.store.resources.Nsp1DomainConstants;
import org.eclipse.lyo.store.resources.WithBlankResource;

// Start of user code imports
// End of user code

// Start of user code preClassCode
// End of user code

// Start of user code classAnnotations
// End of user code
@OslcNamespace(Nsp1DomainConstants.WITHTWODEPTHBLANKRESOURCE_NAMESPACE)
@OslcName(Nsp1DomainConstants.WITHTWODEPTHBLANKRESOURCE_LOCALNAME)
@OslcResourceShape(title = "WithTwoDepthBlankResource Resource Shape", describes = Nsp1DomainConstants.WITHTWODEPTHBLANKRESOURCE_TYPE)
public class WithTwoDepthBlankResource
    extends AbstractResource
    implements IWithTwoDepthBlankResource
{
    // Start of user code attributeAnnotation:relatesToBlankResourceTwoDepth
    // End of user code
    private WithBlankResource relatesToBlankResourceTwoDepth = new WithBlankResource();
    // Start of user code attributeAnnotation:stringProperty
    // End of user code
    private String stringProperty;
    // Start of user code attributeAnnotation:intProperty
    // End of user code
    private Integer intProperty;
    
    // Start of user code classAttributes
    // End of user code
    // Start of user code classMethods
    // End of user code
    public WithTwoDepthBlankResource()
           throws URISyntaxException
    {
        super();
    
        // Start of user code constructor1
        // End of user code
    }
    
    public WithTwoDepthBlankResource(final URI about)
           throws URISyntaxException
    {
        super(about);
    
        // Start of user code constructor2
        // End of user code
    }
    
    
    public static ResourceShape createResourceShape() throws OslcCoreApplicationException, URISyntaxException {
        return ResourceShapeFactory.createResourceShape(OSLC4JUtils.getServletURI(),
        OslcConstants.PATH_RESOURCE_SHAPES,
        Nsp1DomainConstants.WITHTWODEPTHBLANKRESOURCE_PATH,
        WithTwoDepthBlankResource.class);
    }
    
    
    public String toString()
    {
        return toString(false);
    }
    
    public String toString(boolean asLocalResource)
    {
        String result = "";
        // Start of user code toString_init
        // End of user code
    
        if (asLocalResource) {
            result = result + "{a Local WithTwoDepthBlankResource Resource} - update WithTwoDepthBlankResource.toString() to present resource as desired.";
            // Start of user code toString_bodyForLocalResource
            // End of user code
        }
        else {
            result = getAbout().toString();
        }
    
        // Start of user code toString_finalize
        // End of user code
    
        return result;
    }
    
    public String toHtml()
    {
        return toHtml(false);
    }
    
    public String toHtml(boolean asLocalResource)
    {
        String result = "";
        // Start of user code toHtml_init
        // End of user code
    
        if (asLocalResource) {
            result = toString(true);
            // Start of user code toHtml_bodyForLocalResource
            // End of user code
        }
        else {
            result = "<a href=\"" + getAbout() + "\" class=\"oslc-resource-link\">" + toString() + "</a>";
        }
    
        // Start of user code toHtml_finalize
        // End of user code
    
        return result;
    }
    
    
    // Start of user code getterAnnotation:relatesToBlankResourceTwoDepth
    // End of user code
    @OslcName("relatesToBlankResourceTwoDepth")
    @OslcPropertyDefinition(Nsp1DomainConstants.TESTDOMAIN_NAMSPACE + "relatesToBlankResourceTwoDepth")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.LocalResource)
    @OslcRange({Nsp1DomainConstants.WITHBLANKRESOURCE_TYPE})
    @OslcReadOnly(false)
    public WithBlankResource getRelatesToBlankResourceTwoDepth()
    {
        // Start of user code getterInit:relatesToBlankResourceTwoDepth
        // End of user code
        return relatesToBlankResourceTwoDepth;
    }
    
    // Start of user code getterAnnotation:stringProperty
    // End of user code
    @OslcName("stringProperty")
    @OslcPropertyDefinition(Nsp1DomainConstants.TESTDOMAIN_NAMSPACE + "stringProperty")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public String getStringProperty()
    {
        // Start of user code getterInit:stringProperty
        // End of user code
        return stringProperty;
    }
    
    // Start of user code getterAnnotation:intProperty
    // End of user code
    @OslcName("intProperty")
    @OslcPropertyDefinition(Nsp1DomainConstants.TESTDOMAIN_NAMSPACE + "intProperty")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.Integer)
    @OslcReadOnly(false)
    public Integer getIntProperty()
    {
        // Start of user code getterInit:intProperty
        // End of user code
        return intProperty;
    }
    
    
    // Start of user code setterAnnotation:relatesToBlankResourceTwoDepth
    // End of user code
    public void setRelatesToBlankResourceTwoDepth(final WithBlankResource relatesToBlankResourceTwoDepth )
    {
        // Start of user code setterInit:relatesToBlankResourceTwoDepth
        // End of user code
        this.relatesToBlankResourceTwoDepth = relatesToBlankResourceTwoDepth;
    
        // Start of user code setterFinalize:relatesToBlankResourceTwoDepth
        // End of user code
    }
    
    // Start of user code setterAnnotation:stringProperty
    // End of user code
    public void setStringProperty(final String stringProperty )
    {
        // Start of user code setterInit:stringProperty
        // End of user code
        this.stringProperty = stringProperty;
    
        // Start of user code setterFinalize:stringProperty
        // End of user code
    }
    
    // Start of user code setterAnnotation:intProperty
    // End of user code
    public void setIntProperty(final Integer intProperty )
    {
        // Start of user code setterInit:intProperty
        // End of user code
        this.intProperty = intProperty;
    
        // Start of user code setterFinalize:intProperty
        // End of user code
    }
    
    

    
    public String relatesToBlankResourceTwoDepthToHtml()
    {
        String s = "";
    
        // Start of user code relatesToBlankResourceTwoDepthtoHtml_mid
        // End of user code
    
        try {
            if (relatesToBlankResourceTwoDepth == null) {
                s = s + "<em>null</em>";
            }
            else {
                s = s + relatesToBlankResourceTwoDepth.toHtml(true);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    
        // Start of user code relatesToBlankResourceTwoDepthtoHtml_finalize
        // End of user code
    
        return s;
    }
    
    public String stringPropertyToHtml()
    {
        String s = "";
    
        // Start of user code stringPropertytoHtml_mid
        // End of user code
    
        try {
            if (stringProperty == null) {
                s = s + "<em>null</em>";
            }
            else {
                s = s + stringProperty.toString();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    
        // Start of user code stringPropertytoHtml_finalize
        // End of user code
    
        return s;
    }
    
    public String intPropertyToHtml()
    {
        String s = "";
    
        // Start of user code intPropertytoHtml_mid
        // End of user code
    
        try {
            if (intProperty == null) {
                s = s + "<em>null</em>";
            }
            else {
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
