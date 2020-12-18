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

package org.eclipse.lyo.oslc.domains.rm;

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

import org.eclipse.lyo.oslc.domains.rm.Oslc_rmDomainConstants;


import org.eclipse.lyo.oslc.domains.DctermsDomainConstants;
import org.eclipse.lyo.oslc.domains.FoafDomainConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcDomainConstants;
import org.eclipse.lyo.oslc.domains.rm.Oslc_rmDomainConstants;
import org.eclipse.lyo.oslc.domains.DctermsVocabularyConstants;
import org.eclipse.lyo.oslc.domains.Oslc_rmVocabularyConstants;
import org.eclipse.lyo.oslc.domains.Person;
// Start of user code imports
import org.eclipse.lyo.oslc.domains.Oslc_rmVocabularyConstants;
// End of user code

// Start of user code preClassCode
// End of user code

// Start of user code classAnnotations
// End of user code
@OslcNamespace(Oslc_rmDomainConstants.REQUIREMENTCOLLECTION_NAMESPACE)
@OslcName(Oslc_rmDomainConstants.REQUIREMENTCOLLECTION_LOCALNAME)
@OslcResourceShape(title = "RequirementCollection Resource Shape", describes = Oslc_rmDomainConstants.REQUIREMENTCOLLECTION_TYPE)
public class RequirementCollection
    extends AbstractResource
    implements IRequirementCollection
{
    // Start of user code attributeAnnotation:title
    // End of user code
    private String title;
    // Start of user code attributeAnnotation:description
    // End of user code
    private String description;
    // Start of user code attributeAnnotation:identifier
    // End of user code
    private String identifier;
    // Start of user code attributeAnnotation:shortTitle
    // End of user code
    private String shortTitle;
    // Start of user code attributeAnnotation:subject
    // End of user code
    private Set<String> subject = new HashSet<String>();
    // Start of user code attributeAnnotation:creator
    // End of user code
    private Set<Link> creator = new HashSet<Link>();
    // Start of user code attributeAnnotation:contributor
    // End of user code
    private Set<Link> contributor = new HashSet<Link>();
    // Start of user code attributeAnnotation:created
    // End of user code
    private Date created;
    // Start of user code attributeAnnotation:modified
    // End of user code
    private Date modified;
    // Start of user code attributeAnnotation:serviceProvider
    // End of user code
    private Set<Link> serviceProvider = new HashSet<Link>();
    // Start of user code attributeAnnotation:instanceShape
    // End of user code
    private Set<Link> instanceShape = new HashSet<Link>();
    // Start of user code attributeAnnotation:elaboratedBy
    // End of user code
    private Set<Link> elaboratedBy = new HashSet<Link>();
    // Start of user code attributeAnnotation:elaborates
    // End of user code
    private Set<Link> elaborates = new HashSet<Link>();
    // Start of user code attributeAnnotation:specifiedBy
    // End of user code
    private Set<Link> specifiedBy = new HashSet<Link>();
    // Start of user code attributeAnnotation:specifies
    // End of user code
    private Set<Link> specifies = new HashSet<Link>();
    // Start of user code attributeAnnotation:affectedBy
    // End of user code
    private Set<Link> affectedBy = new HashSet<Link>();
    // Start of user code attributeAnnotation:trackedBy
    // End of user code
    private Set<Link> trackedBy = new HashSet<Link>();
    // Start of user code attributeAnnotation:implementedBy
    // End of user code
    private Set<Link> implementedBy = new HashSet<Link>();
    // Start of user code attributeAnnotation:validatedBy
    // End of user code
    private Set<Link> validatedBy = new HashSet<Link>();
    // Start of user code attributeAnnotation:satisfiedBy
    // End of user code
    private Set<Link> satisfiedBy = new HashSet<Link>();
    // Start of user code attributeAnnotation:satisfies
    // End of user code
    private Set<Link> satisfies = new HashSet<Link>();
    // Start of user code attributeAnnotation:decomposedBy
    // End of user code
    private Set<Link> decomposedBy = new HashSet<Link>();
    // Start of user code attributeAnnotation:decomposes
    // End of user code
    private Set<Link> decomposes = new HashSet<Link>();
    // Start of user code attributeAnnotation:constrainedBy
    // End of user code
    private Set<Link> constrainedBy = new HashSet<Link>();
    // Start of user code attributeAnnotation:constrains
    // End of user code
    private Set<Link> constrains = new HashSet<Link>();
    // Start of user code attributeAnnotation:uses
    // End of user code
    private Set<Link> uses = new HashSet<Link>();
    
    // Start of user code classAttributes
    // End of user code
    // Start of user code classMethods
    // End of user code
    public RequirementCollection()
    {
        super();
    
        // Start of user code constructor1
        // End of user code
    }
    
    public RequirementCollection(final URI about)
    {
        super(about);
    
        // Start of user code constructor2
        // End of user code
    }
    
    public static ResourceShape createResourceShape() throws OslcCoreApplicationException, URISyntaxException {
        return ResourceShapeFactory.createResourceShape(OSLC4JUtils.getServletURI(),
        OslcConstants.PATH_RESOURCE_SHAPES,
        Oslc_rmDomainConstants.REQUIREMENTCOLLECTION_PATH,
        RequirementCollection.class);
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
            result = result + "{a Local RequirementCollection Resource} - update RequirementCollection.toString() to present resource as desired.";
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
    
    public void addSubject(final String subject)
    {
        this.subject.add(subject);
    }
    
    public void addCreator(final Link creator)
    {
        this.creator.add(creator);
    }
    
    public void addContributor(final Link contributor)
    {
        this.contributor.add(contributor);
    }
    
    public void addServiceProvider(final Link serviceProvider)
    {
        this.serviceProvider.add(serviceProvider);
    }
    
    public void addInstanceShape(final Link instanceShape)
    {
        this.instanceShape.add(instanceShape);
    }
    
    public void addElaboratedBy(final Link elaboratedBy)
    {
        this.elaboratedBy.add(elaboratedBy);
    }
    
    public void addElaborates(final Link elaborates)
    {
        this.elaborates.add(elaborates);
    }
    
    public void addSpecifiedBy(final Link specifiedBy)
    {
        this.specifiedBy.add(specifiedBy);
    }
    
    public void addSpecifies(final Link specifies)
    {
        this.specifies.add(specifies);
    }
    
    public void addAffectedBy(final Link affectedBy)
    {
        this.affectedBy.add(affectedBy);
    }
    
    public void addTrackedBy(final Link trackedBy)
    {
        this.trackedBy.add(trackedBy);
    }
    
    public void addImplementedBy(final Link implementedBy)
    {
        this.implementedBy.add(implementedBy);
    }
    
    public void addValidatedBy(final Link validatedBy)
    {
        this.validatedBy.add(validatedBy);
    }
    
    public void addSatisfiedBy(final Link satisfiedBy)
    {
        this.satisfiedBy.add(satisfiedBy);
    }
    
    public void addSatisfies(final Link satisfies)
    {
        this.satisfies.add(satisfies);
    }
    
    public void addDecomposedBy(final Link decomposedBy)
    {
        this.decomposedBy.add(decomposedBy);
    }
    
    public void addDecomposes(final Link decomposes)
    {
        this.decomposes.add(decomposes);
    }
    
    public void addConstrainedBy(final Link constrainedBy)
    {
        this.constrainedBy.add(constrainedBy);
    }
    
    public void addConstrains(final Link constrains)
    {
        this.constrains.add(constrains);
    }
    
    public void addUses(final Link uses)
    {
        this.uses.add(uses);
    }
    
    
    // Start of user code getterAnnotation:title
    // End of user code
    @OslcName("title")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "title")
    @OslcDescription("Title of the resource represented as rich text in XHTML content. SHOULD include only content that is valid inside an XHTML <span> element.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.XMLLiteral)
    @OslcReadOnly(false)
    public String getTitle()
    {
        // Start of user code getterInit:title
        // End of user code
        return title;
    }
    
    // Start of user code getterAnnotation:description
    // End of user code
    @OslcName("description")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "description")
    @OslcDescription("Descriptive text about resource represented as rich text in XHTML content. SHOULD include only content that is valid and suitable inside an XHTML <div> element.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.XMLLiteral)
    @OslcReadOnly(false)
    public String getDescription()
    {
        // Start of user code getterInit:description
        // End of user code
        return description;
    }
    
    // Start of user code getterAnnotation:identifier
    // End of user code
    @OslcName("identifier")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "identifier")
    @OslcDescription("A unique identifier for a resource. Typically read-only and assigned by the service provider when a resource is created. Not typically intended for end-user display.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public String getIdentifier()
    {
        // Start of user code getterInit:identifier
        // End of user code
        return identifier;
    }
    
    // Start of user code getterAnnotation:shortTitle
    // End of user code
    @OslcName("shortTitle")
    @OslcPropertyDefinition(OslcDomainConstants.OSLC_NAMSPACE + "shortTitle")
    @OslcDescription("Shorter form of dcterms:title for the resource represented as rich text in XHTML content. SHOULD include only content that is valid inside an XHTML <span> element.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.XMLLiteral)
    @OslcReadOnly(false)
    public String getShortTitle()
    {
        // Start of user code getterInit:shortTitle
        // End of user code
        return shortTitle;
    }
    
    // Start of user code getterAnnotation:subject
    // End of user code
    @OslcName("subject")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "subject")
    @OslcDescription("Tag or keyword for a resource. Each occurrence of a dcterms:subject property denotes an additional tag for the resource.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    @OslcTitle("")
    public Set<String> getSubject()
    {
        // Start of user code getterInit:subject
        // End of user code
        return subject;
    }
    
    // Start of user code getterAnnotation:creator
    // End of user code
    @OslcName("creator")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "creator")
    @OslcDescription("Creator or creators of the resource. It is likely that the target resource will be a foaf:Person but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRange({FoafDomainConstants.PERSON_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getCreator()
    {
        // Start of user code getterInit:creator
        // End of user code
        return creator;
    }
    
    // Start of user code getterAnnotation:contributor
    // End of user code
    @OslcName("contributor")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "contributor")
    @OslcDescription("Contributor or contributors to the resource. It is likely that the target resource will be a foaf:Person but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRange({FoafDomainConstants.PERSON_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getContributor()
    {
        // Start of user code getterInit:contributor
        // End of user code
        return contributor;
    }
    
    // Start of user code getterAnnotation:created
    // End of user code
    @OslcName("created")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "created")
    @OslcDescription("Timestamp of resource creation")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.DateTime)
    @OslcReadOnly(false)
    public Date getCreated()
    {
        // Start of user code getterInit:created
        // End of user code
        return created;
    }
    
    // Start of user code getterAnnotation:modified
    // End of user code
    @OslcName("modified")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "modified")
    @OslcDescription("Timestamp of latest resource modification")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.DateTime)
    @OslcReadOnly(false)
    public Date getModified()
    {
        // Start of user code getterInit:modified
        // End of user code
        return modified;
    }
    
    // Start of user code getterAnnotation:serviceProvider
    // End of user code
    @OslcName("serviceProvider")
    @OslcPropertyDefinition(OslcDomainConstants.OSLC_NAMSPACE + "serviceProvider")
    @OslcDescription("A link to the resource's OSLC Service Provider. There may be cases when the subject resource is available from a service provider that implements multiple domain specifications, which could result in multiple values for this property.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getServiceProvider()
    {
        // Start of user code getterInit:serviceProvider
        // End of user code
        return serviceProvider;
    }
    
    // Start of user code getterAnnotation:instanceShape
    // End of user code
    @OslcName("instanceShape")
    @OslcPropertyDefinition(OslcDomainConstants.OSLC_NAMSPACE + "instanceShape")
    @OslcDescription("The URI of a Resource Shape that describes the possible properties, occurrence, value types, allowed values and labels. This shape information is useful in displaying the subject resource as well as guiding clients in performing modifications. Instance shapes may be specific to the authenticated user associated with the request that retrieved the resource, the current state of the resource and other factors and thus should not be cached.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getInstanceShape()
    {
        // Start of user code getterInit:instanceShape
        // End of user code
        return instanceShape;
    }
    
    // Start of user code getterAnnotation:elaboratedBy
    // End of user code
    @OslcName("elaboratedBy")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "elaboratedBy")
    @OslcDescription("The subject is elaborated by the object. For example, a user requirement is elaborated by use case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getElaboratedBy()
    {
        // Start of user code getterInit:elaboratedBy
        // End of user code
        return elaboratedBy;
    }
    
    // Start of user code getterAnnotation:elaborates
    // End of user code
    @OslcName("elaborates")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "elaborates")
    @OslcDescription("The object is elaborated by the subject.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getElaborates()
    {
        // Start of user code getterInit:elaborates
        // End of user code
        return elaborates;
    }
    
    // Start of user code getterAnnotation:specifiedBy
    // End of user code
    @OslcName("specifiedBy")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "specifiedBy")
    @OslcDescription("The subject is specified by the object. For example, a requirement is elaborated by a model element .")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getSpecifiedBy()
    {
        // Start of user code getterInit:specifiedBy
        // End of user code
        return specifiedBy;
    }
    
    // Start of user code getterAnnotation:specifies
    // End of user code
    @OslcName("specifies")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "specifies")
    @OslcDescription("The object is specified by the subject.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getSpecifies()
    {
        // Start of user code getterInit:specifies
        // End of user code
        return specifies;
    }
    
    // Start of user code getterAnnotation:affectedBy
    // End of user code
    @OslcName("affectedBy")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "affectedBy")
    @OslcDescription("Requirement is affected by a resource, such as a defect or issue.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getAffectedBy()
    {
        // Start of user code getterInit:affectedBy
        // End of user code
        return affectedBy;
    }
    
    // Start of user code getterAnnotation:trackedBy
    // End of user code
    @OslcName("trackedBy")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "trackedBy")
    @OslcDescription("Resource, such as a change request, which tracks this requirement.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getTrackedBy()
    {
        // Start of user code getterInit:trackedBy
        // End of user code
        return trackedBy;
    }
    
    // Start of user code getterAnnotation:implementedBy
    // End of user code
    @OslcName("implementedBy")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "implementedBy")
    @OslcDescription("Resource, such as a change request, which implements this requirement.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getImplementedBy()
    {
        // Start of user code getterInit:implementedBy
        // End of user code
        return implementedBy;
    }
    
    // Start of user code getterAnnotation:validatedBy
    // End of user code
    @OslcName("validatedBy")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "validatedBy")
    @OslcDescription("Resource, such as a test case, which validates this requirement.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getValidatedBy()
    {
        // Start of user code getterInit:validatedBy
        // End of user code
        return validatedBy;
    }
    
    // Start of user code getterAnnotation:satisfiedBy
    // End of user code
    @OslcName("satisfiedBy")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "satisfiedBy")
    @OslcDescription("The subject is satisfied by the object. For example, a user requirement is satisfied by a system requirement.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getSatisfiedBy()
    {
        // Start of user code getterInit:satisfiedBy
        // End of user code
        return satisfiedBy;
    }
    
    // Start of user code getterAnnotation:satisfies
    // End of user code
    @OslcName("satisfies")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "satisfies")
    @OslcDescription("The object is satisfied by the subject.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getSatisfies()
    {
        // Start of user code getterInit:satisfies
        // End of user code
        return satisfies;
    }
    
    // Start of user code getterAnnotation:decomposedBy
    // End of user code
    @OslcName("decomposedBy")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "decomposedBy")
    @OslcDescription("The subject is decomposed by the object. For example, a system requirement is decomposed into a collection of system requirements.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getDecomposedBy()
    {
        // Start of user code getterInit:decomposedBy
        // End of user code
        return decomposedBy;
    }
    
    // Start of user code getterAnnotation:decomposes
    // End of user code
    @OslcName("decomposes")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "decomposes")
    @OslcDescription("The object is decomposed by the subject.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getDecomposes()
    {
        // Start of user code getterInit:decomposes
        // End of user code
        return decomposes;
    }
    
    // Start of user code getterAnnotation:constrainedBy
    // End of user code
    @OslcName("constrainedBy")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "constrainedBy")
    @OslcDescription("The subject is constrained by the object. For example, a functional requirement is constrained by a safety requirement.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getConstrainedBy()
    {
        // Start of user code getterInit:constrainedBy
        // End of user code
        return constrainedBy;
    }
    
    // Start of user code getterAnnotation:constrains
    // End of user code
    @OslcName("constrains")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "constrains")
    @OslcDescription("The object is constrained by the subject.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getConstrains()
    {
        // Start of user code getterInit:constrains
        // End of user code
        return constrains;
    }
    
    // Start of user code getterAnnotation:uses
    // End of user code
    @OslcName("uses")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "uses")
    @OslcDescription("A collection uses a resource - the resource is in the requirement collection. ")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    @OslcTitle("")
    public Set<Link> getUses()
    {
        // Start of user code getterInit:uses
        // End of user code
        return uses;
    }
    
    
    // Start of user code setterAnnotation:title
    // End of user code
    public void setTitle(final String title )
    {
        // Start of user code setterInit:title
        // End of user code
        this.title = title;
    
        // Start of user code setterFinalize:title
        // End of user code
    }
    
    // Start of user code setterAnnotation:description
    // End of user code
    public void setDescription(final String description )
    {
        // Start of user code setterInit:description
        // End of user code
        this.description = description;
    
        // Start of user code setterFinalize:description
        // End of user code
    }
    
    // Start of user code setterAnnotation:identifier
    // End of user code
    public void setIdentifier(final String identifier )
    {
        // Start of user code setterInit:identifier
        // End of user code
        this.identifier = identifier;
    
        // Start of user code setterFinalize:identifier
        // End of user code
    }
    
    // Start of user code setterAnnotation:shortTitle
    // End of user code
    public void setShortTitle(final String shortTitle )
    {
        // Start of user code setterInit:shortTitle
        // End of user code
        this.shortTitle = shortTitle;
    
        // Start of user code setterFinalize:shortTitle
        // End of user code
    }
    
    // Start of user code setterAnnotation:subject
    // End of user code
    public void setSubject(final Set<String> subject )
    {
        // Start of user code setterInit:subject
        // End of user code
        this.subject.clear();
        if (subject != null)
        {
            this.subject.addAll(subject);
        }
    
        // Start of user code setterFinalize:subject
        // End of user code
    }
    
    // Start of user code setterAnnotation:creator
    // End of user code
    public void setCreator(final Set<Link> creator )
    {
        // Start of user code setterInit:creator
        // End of user code
        this.creator.clear();
        if (creator != null)
        {
            this.creator.addAll(creator);
        }
    
        // Start of user code setterFinalize:creator
        // End of user code
    }
    
    // Start of user code setterAnnotation:contributor
    // End of user code
    public void setContributor(final Set<Link> contributor )
    {
        // Start of user code setterInit:contributor
        // End of user code
        this.contributor.clear();
        if (contributor != null)
        {
            this.contributor.addAll(contributor);
        }
    
        // Start of user code setterFinalize:contributor
        // End of user code
    }
    
    // Start of user code setterAnnotation:created
    // End of user code
    public void setCreated(final Date created )
    {
        // Start of user code setterInit:created
        // End of user code
        this.created = created;
    
        // Start of user code setterFinalize:created
        // End of user code
    }
    
    // Start of user code setterAnnotation:modified
    // End of user code
    public void setModified(final Date modified )
    {
        // Start of user code setterInit:modified
        // End of user code
        this.modified = modified;
    
        // Start of user code setterFinalize:modified
        // End of user code
    }
    
    // Start of user code setterAnnotation:serviceProvider
    // End of user code
    public void setServiceProvider(final Set<Link> serviceProvider )
    {
        // Start of user code setterInit:serviceProvider
        // End of user code
        this.serviceProvider.clear();
        if (serviceProvider != null)
        {
            this.serviceProvider.addAll(serviceProvider);
        }
    
        // Start of user code setterFinalize:serviceProvider
        // End of user code
    }
    
    // Start of user code setterAnnotation:instanceShape
    // End of user code
    public void setInstanceShape(final Set<Link> instanceShape )
    {
        // Start of user code setterInit:instanceShape
        // End of user code
        this.instanceShape.clear();
        if (instanceShape != null)
        {
            this.instanceShape.addAll(instanceShape);
        }
    
        // Start of user code setterFinalize:instanceShape
        // End of user code
    }
    
    // Start of user code setterAnnotation:elaboratedBy
    // End of user code
    public void setElaboratedBy(final Set<Link> elaboratedBy )
    {
        // Start of user code setterInit:elaboratedBy
        // End of user code
        this.elaboratedBy.clear();
        if (elaboratedBy != null)
        {
            this.elaboratedBy.addAll(elaboratedBy);
        }
    
        // Start of user code setterFinalize:elaboratedBy
        // End of user code
    }
    
    // Start of user code setterAnnotation:elaborates
    // End of user code
    public void setElaborates(final Set<Link> elaborates )
    {
        // Start of user code setterInit:elaborates
        // End of user code
        this.elaborates.clear();
        if (elaborates != null)
        {
            this.elaborates.addAll(elaborates);
        }
    
        // Start of user code setterFinalize:elaborates
        // End of user code
    }
    
    // Start of user code setterAnnotation:specifiedBy
    // End of user code
    public void setSpecifiedBy(final Set<Link> specifiedBy )
    {
        // Start of user code setterInit:specifiedBy
        // End of user code
        this.specifiedBy.clear();
        if (specifiedBy != null)
        {
            this.specifiedBy.addAll(specifiedBy);
        }
    
        // Start of user code setterFinalize:specifiedBy
        // End of user code
    }
    
    // Start of user code setterAnnotation:specifies
    // End of user code
    public void setSpecifies(final Set<Link> specifies )
    {
        // Start of user code setterInit:specifies
        // End of user code
        this.specifies.clear();
        if (specifies != null)
        {
            this.specifies.addAll(specifies);
        }
    
        // Start of user code setterFinalize:specifies
        // End of user code
    }
    
    // Start of user code setterAnnotation:affectedBy
    // End of user code
    public void setAffectedBy(final Set<Link> affectedBy )
    {
        // Start of user code setterInit:affectedBy
        // End of user code
        this.affectedBy.clear();
        if (affectedBy != null)
        {
            this.affectedBy.addAll(affectedBy);
        }
    
        // Start of user code setterFinalize:affectedBy
        // End of user code
    }
    
    // Start of user code setterAnnotation:trackedBy
    // End of user code
    public void setTrackedBy(final Set<Link> trackedBy )
    {
        // Start of user code setterInit:trackedBy
        // End of user code
        this.trackedBy.clear();
        if (trackedBy != null)
        {
            this.trackedBy.addAll(trackedBy);
        }
    
        // Start of user code setterFinalize:trackedBy
        // End of user code
    }
    
    // Start of user code setterAnnotation:implementedBy
    // End of user code
    public void setImplementedBy(final Set<Link> implementedBy )
    {
        // Start of user code setterInit:implementedBy
        // End of user code
        this.implementedBy.clear();
        if (implementedBy != null)
        {
            this.implementedBy.addAll(implementedBy);
        }
    
        // Start of user code setterFinalize:implementedBy
        // End of user code
    }
    
    // Start of user code setterAnnotation:validatedBy
    // End of user code
    public void setValidatedBy(final Set<Link> validatedBy )
    {
        // Start of user code setterInit:validatedBy
        // End of user code
        this.validatedBy.clear();
        if (validatedBy != null)
        {
            this.validatedBy.addAll(validatedBy);
        }
    
        // Start of user code setterFinalize:validatedBy
        // End of user code
    }
    
    // Start of user code setterAnnotation:satisfiedBy
    // End of user code
    public void setSatisfiedBy(final Set<Link> satisfiedBy )
    {
        // Start of user code setterInit:satisfiedBy
        // End of user code
        this.satisfiedBy.clear();
        if (satisfiedBy != null)
        {
            this.satisfiedBy.addAll(satisfiedBy);
        }
    
        // Start of user code setterFinalize:satisfiedBy
        // End of user code
    }
    
    // Start of user code setterAnnotation:satisfies
    // End of user code
    public void setSatisfies(final Set<Link> satisfies )
    {
        // Start of user code setterInit:satisfies
        // End of user code
        this.satisfies.clear();
        if (satisfies != null)
        {
            this.satisfies.addAll(satisfies);
        }
    
        // Start of user code setterFinalize:satisfies
        // End of user code
    }
    
    // Start of user code setterAnnotation:decomposedBy
    // End of user code
    public void setDecomposedBy(final Set<Link> decomposedBy )
    {
        // Start of user code setterInit:decomposedBy
        // End of user code
        this.decomposedBy.clear();
        if (decomposedBy != null)
        {
            this.decomposedBy.addAll(decomposedBy);
        }
    
        // Start of user code setterFinalize:decomposedBy
        // End of user code
    }
    
    // Start of user code setterAnnotation:decomposes
    // End of user code
    public void setDecomposes(final Set<Link> decomposes )
    {
        // Start of user code setterInit:decomposes
        // End of user code
        this.decomposes.clear();
        if (decomposes != null)
        {
            this.decomposes.addAll(decomposes);
        }
    
        // Start of user code setterFinalize:decomposes
        // End of user code
    }
    
    // Start of user code setterAnnotation:constrainedBy
    // End of user code
    public void setConstrainedBy(final Set<Link> constrainedBy )
    {
        // Start of user code setterInit:constrainedBy
        // End of user code
        this.constrainedBy.clear();
        if (constrainedBy != null)
        {
            this.constrainedBy.addAll(constrainedBy);
        }
    
        // Start of user code setterFinalize:constrainedBy
        // End of user code
    }
    
    // Start of user code setterAnnotation:constrains
    // End of user code
    public void setConstrains(final Set<Link> constrains )
    {
        // Start of user code setterInit:constrains
        // End of user code
        this.constrains.clear();
        if (constrains != null)
        {
            this.constrains.addAll(constrains);
        }
    
        // Start of user code setterFinalize:constrains
        // End of user code
    }
    
    // Start of user code setterAnnotation:uses
    // End of user code
    public void setUses(final Set<Link> uses )
    {
        // Start of user code setterInit:uses
        // End of user code
        this.uses.clear();
        if (uses != null)
        {
            this.uses.addAll(uses);
        }
    
        // Start of user code setterFinalize:uses
        // End of user code
    }
    
    
}
