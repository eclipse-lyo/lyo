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

package org.eclipse.lyo.oslc.domains;

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

import org.eclipse.lyo.oslc.domains.FoafDomainConstants;


import org.eclipse.lyo.oslc.domains.FoafDomainConstants;
import org.eclipse.lyo.oslc.domains.FoafVocabularyConstants;

// Start of user code imports
// End of user code

// Start of user code preClassCode
// End of user code

// Start of user code classAnnotations
// End of user code
@OslcNamespace(FoafDomainConstants.PERSON_NAMESPACE)
@OslcName(FoafDomainConstants.PERSON_LOCALNAME)
@OslcResourceShape(title = "Person Resource Shape", describes = FoafDomainConstants.PERSON_TYPE)
public class Person
    extends AbstractResource
    implements IPerson
{
    // Start of user code attributeAnnotation:familyName
    // End of user code
    private String familyName;
    // Start of user code attributeAnnotation:givenName
    // End of user code
    private String givenName;
    // Start of user code attributeAnnotation:name
    // End of user code
    private String name;
    
    // Start of user code classAttributes
    // End of user code
    // Start of user code classMethods
    // End of user code
    public Person()
    {
        super();
    
        // Start of user code constructor1
        // End of user code
    }
    
    public Person(final URI about)
    {
        super(about);
    
        // Start of user code constructor2
        // End of user code
    }
    
    public static ResourceShape createResourceShape() throws OslcCoreApplicationException, URISyntaxException {
        return ResourceShapeFactory.createResourceShape(OSLC4JUtils.getServletURI(),
        OslcConstants.PATH_RESOURCE_SHAPES,
        FoafDomainConstants.PERSON_PATH,
        Person.class);
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
            result = result + "{a Local Person Resource} - update Person.toString() to present resource as desired.";
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
    
    
    // Start of user code getterAnnotation:familyName
    // End of user code
    @OslcName("familyName")
    @OslcPropertyDefinition(FoafVocabularyConstants.FOAF_NAMSPACE + "familyName")
    @OslcDescription("Family name of person expressed as simple text string.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public String getFamilyName()
    {
        // Start of user code getterInit:familyName
        // End of user code
        return familyName;
    }
    
    // Start of user code getterAnnotation:givenName
    // End of user code
    @OslcName("givenName")
    @OslcPropertyDefinition(FoafVocabularyConstants.FOAF_NAMSPACE + "givenName")
    @OslcDescription("Given name of person expressed as simple text string. ")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public String getGivenName()
    {
        // Start of user code getterInit:givenName
        // End of user code
        return givenName;
    }
    
    // Start of user code getterAnnotation:name
    // End of user code
    @OslcName("name")
    @OslcPropertyDefinition(FoafVocabularyConstants.FOAF_NAMSPACE + "name")
    @OslcDescription("The full name of a person expressed as simple text string.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public String getName()
    {
        // Start of user code getterInit:name
        // End of user code
        return name;
    }
    
    
    // Start of user code setterAnnotation:familyName
    // End of user code
    public void setFamilyName(final String familyName )
    {
        // Start of user code setterInit:familyName
        // End of user code
        this.familyName = familyName;
    
        // Start of user code setterFinalize:familyName
        // End of user code
    }
    
    // Start of user code setterAnnotation:givenName
    // End of user code
    public void setGivenName(final String givenName )
    {
        // Start of user code setterInit:givenName
        // End of user code
        this.givenName = givenName;
    
        // Start of user code setterFinalize:givenName
        // End of user code
    }
    
    // Start of user code setterAnnotation:name
    // End of user code
    public void setName(final String name )
    {
        // Start of user code setterInit:name
        // End of user code
        this.name = name;
    
        // Start of user code setterFinalize:name
        // End of user code
    }
    
    
}
