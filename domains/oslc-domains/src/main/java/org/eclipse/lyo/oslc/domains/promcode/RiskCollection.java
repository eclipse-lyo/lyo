// Start of user code Copyright
/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
 *
 * See the NOTICE file(s) distributed with this work for additional
 * information regarding copyright ownership.
 *
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Distribution License 1.0 which is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: BSD-3-Simple
 *
 * This file is generated by Lyo Designer (https://www.eclipse.org/lyo/)
 */
// End of user code

package org.eclipse.lyo.oslc.domains.promcode;

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

import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.annotation.OslcAllowedValue;
import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcMemberProperty;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRdfCollectionType;
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

import org.eclipse.lyo.oslc.domains.promcode.Oslc_promcodeDomainConstants;
import org.eclipse.lyo.oslc.domains.promcode.ManagedItemCollection;
import org.eclipse.lyo.oslc.domains.promcode.Oslc_promcodeDomainConstants;
import org.eclipse.lyo.oslc.domains.promcode.Artifact;
import org.eclipse.lyo.oslc.domains.promcode.Issue;
import org.eclipse.lyo.oslc.domains.promcode.Project;
import org.eclipse.lyo.oslc.domains.promcode.Risk;
import org.eclipse.lyo.oslc.domains.promcode.ScopeItem;
import org.eclipse.lyo.oslc.domains.promcode.WorkItem;
// Start of user code imports
// End of user code

// Start of user code preClassCode
// End of user code

// Start of user code classAnnotations
// End of user code
@OslcNamespace(Oslc_promcodeDomainConstants.RISKCOLLECTION_NAMESPACE)
@OslcName(Oslc_promcodeDomainConstants.RISKCOLLECTION_LOCALNAME)
@OslcResourceShape(title = "RiskCollection Shape", description = "", describes = Oslc_promcodeDomainConstants.RISKCOLLECTION_TYPE)
public class RiskCollection
    extends ManagedItemCollection
    implements IRiskCollection
{
    // Start of user code attributeAnnotation:oslc_promcodeBelongsTo
    // End of user code
    private Link oslc_promcodeBelongsTo;
    // Start of user code attributeAnnotation:oslc_promcodeCollects
    // End of user code
    private Set<Link> oslc_promcodeCollects = new HashSet<Link>();
    
    // Start of user code classAttributes
    // End of user code
    // Start of user code classMethods
    // End of user code
    public RiskCollection()
    {
        super();
    
        // Start of user code constructor1
        // End of user code
    }
    
    public RiskCollection(final URI about)
    {
        super(about);
    
        // Start of user code constructor2
        // End of user code
    }
    
    public static ResourceShape createResourceShape() throws OslcCoreApplicationException, URISyntaxException {
        return ResourceShapeFactory.createResourceShape(OSLC4JUtils.getServletURI(),
        OslcConstants.PATH_RESOURCE_SHAPES,
        Oslc_promcodeDomainConstants.RISKCOLLECTION_PATH,
        RiskCollection.class);
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
            result = result + "{a Local RiskCollection Resource} - update RiskCollection.toString() to present resource as desired.";
            // Start of user code toString_bodyForLocalResource
            // End of user code
        }
        else {
            result = String.valueOf(getAbout());
        }
    
        // Start of user code toString_finalize
        // End of user code
    
        return result;
    }
    
    public void addOslc_promcodeCollects(final Link collects)
    {
        this.oslc_promcodeCollects.add(collects);
    }
    
    
    // Start of user code getterAnnotation:oslc_promcodeBelongsTo
    // End of user code
    @OslcName("belongsTo")
    @OslcPropertyDefinition(Oslc_promcodeDomainConstants.PROMCODE_NAMSPACE + "belongsTo")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.Resource)
    @OslcRange({Oslc_promcodeDomainConstants.PROJECT_TYPE})
    @OslcReadOnly(false)
    public Link getOslc_promcodeBelongsTo()
    {
        // Start of user code getterInit:oslc_promcodeBelongsTo
        // End of user code
        return oslc_promcodeBelongsTo;
    }

    // Start of user code getterAnnotation:oslc_promcodeCollects
    // End of user code
    @OslcName("collects")
    @OslcPropertyDefinition(Oslc_promcodeDomainConstants.PROMCODE_NAMSPACE + "collects")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRange({Oslc_promcodeDomainConstants.RISK_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getOslc_promcodeCollects()
    {
        // Start of user code getterInit:oslc_promcodeCollects
        // End of user code
        return oslc_promcodeCollects;
    }

    // Start of user code setterAnnotation:oslc_promcodeBelongsTo
    // End of user code
    public void setOslc_promcodeBelongsTo(final Link belongsTo )
    {
        // Start of user code setterInit:oslc_promcodeBelongsTo
        // End of user code
        this.oslc_promcodeBelongsTo = belongsTo;
        // Start of user code setterFinalize:oslc_promcodeBelongsTo
        // End of user code
    }

    // Start of user code setterAnnotation:oslc_promcodeCollects
    // End of user code
    public void setOslc_promcodeCollects(final Set<Link> collects )
    {
        // Start of user code setterInit:oslc_promcodeCollects
        // End of user code
        this.oslc_promcodeCollects.clear();
        if (collects != null)
        {
            this.oslc_promcodeCollects.addAll(collects);
        }
        // Start of user code setterFinalize:oslc_promcodeCollects
        // End of user code
    }

}
