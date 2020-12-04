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

package org.eclipse.lyo.oslc.domains.cm;

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

import org.eclipse.lyo.oslc.domains.cm.Oslc_cmDomainConstants;


import org.eclipse.lyo.oslc.domains.cm.Oslc_cmDomainConstants;
import org.eclipse.lyo.oslc.domains.config.Oslc_configDomainConstants;
import org.eclipse.lyo.oslc.domains.DctermsDomainConstants;
import org.eclipse.lyo.oslc.domains.FoafDomainConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcDomainConstants;
import org.eclipse.lyo.oslc.domains.rm.Oslc_rmDomainConstants;
import org.eclipse.lyo.oslc.domains.Oslc_cmVocabularyConstants;

import org.eclipse.lyo.oslc.domains.DctermsVocabularyConstants;
import org.eclipse.lyo.oslc.domains.cm.Defect;
import org.eclipse.lyo.oslc.domains.rm.Requirement;
import org.eclipse.lyo.oslc.domains.Agent;
import org.eclipse.lyo.oslc.domains.Person;
import org.eclipse.lyo.oslc.domains.Person;
import org.eclipse.lyo.oslc4j.core.model.Discussion;
import org.eclipse.lyo.oslc.domains.rm.Requirement;
import org.eclipse.lyo.oslc.domains.cm.ChangeRequest;
import org.eclipse.lyo.oslc.domains.cm.Priority;
import org.eclipse.lyo.oslc.domains.cm.State;
import org.eclipse.lyo.oslc.domains.config.ChangeSet;
import org.eclipse.lyo.oslc.domains.rm.Requirement;

// Start of user code imports
import org.eclipse.lyo.oslc.domains.Oslc_cmVocabularyConstants;
// End of user code

// Start of user code preClassCode
// End of user code

// Start of user code classAnnotations
// End of user code
@OslcNamespace(Oslc_cmDomainConstants.CHANGEREQUEST_NAMESPACE)
@OslcName(Oslc_cmDomainConstants.CHANGEREQUEST_LOCALNAME)
@OslcResourceShape(title = "ChangeRequest Resource Shape", describes = Oslc_cmDomainConstants.CHANGEREQUEST_TYPE)
public class ChangeRequest
    extends AbstractResource
    implements IChangeRequest
{
    // Start of user code attributeAnnotation:shortTitle
    // End of user code
    private String shortTitle;
    // Start of user code attributeAnnotation:description
    // End of user code
    private String description;
    // Start of user code attributeAnnotation:title
    // End of user code
    private String title;
    // Start of user code attributeAnnotation:identifier
    // End of user code
    private String identifier;
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
    // Start of user code attributeAnnotation:discussedBy
    // End of user code
    private Link discussedBy;
    // Start of user code attributeAnnotation:closeDate
    // End of user code
    private Date closeDate;
    // Start of user code attributeAnnotation:status
    // End of user code
    private String status;
    // Start of user code attributeAnnotation:closed
    // End of user code
    private Boolean closed;
    // Start of user code attributeAnnotation:inProgress
    // End of user code
    private Boolean inProgress;
    // Start of user code attributeAnnotation:fixed
    // End of user code
    private Boolean fixed;
    // Start of user code attributeAnnotation:approved
    // End of user code
    private Boolean approved;
    // Start of user code attributeAnnotation:reviewed
    // End of user code
    private Boolean reviewed;
    // Start of user code attributeAnnotation:verified
    // End of user code
    private Boolean verified;
    // Start of user code attributeAnnotation:relatedChangeRequest
    // End of user code
    private Set<Link> relatedChangeRequest = new HashSet<Link>();
    // Start of user code attributeAnnotation:affectsPlanItem
    // End of user code
    private Set<Link> affectsPlanItem = new HashSet<Link>();
    // Start of user code attributeAnnotation:affectedByDefect
    // End of user code
    private Set<Link> affectedByDefect = new HashSet<Link>();
    // Start of user code attributeAnnotation:tracksRequirement
    // End of user code
    private Set<Link> tracksRequirement = new HashSet<Link>();
    // Start of user code attributeAnnotation:implementsRequirement
    // End of user code
    private Set<Link> implementsRequirement = new HashSet<Link>();
    // Start of user code attributeAnnotation:affectsRequirement
    // End of user code
    private Set<Link> affectsRequirement = new HashSet<Link>();
    // Start of user code attributeAnnotation:tracksChangeSet
    // End of user code
    private Set<Link> tracksChangeSet = new HashSet<Link>();
    // Start of user code attributeAnnotation:parent
    // End of user code
    private Set<Link> parent = new HashSet<Link>();
    // Start of user code attributeAnnotation:priority
    // End of user code
    private Set<Link> priority = new HashSet<Link>();
    // Start of user code attributeAnnotation:state
    // End of user code
    private Link state;
    // Start of user code attributeAnnotation:authorizer
    // End of user code
    private Set<Link> authorizer = new HashSet<Link>();
    
    // Start of user code classAttributes
    // End of user code
    // Start of user code classMethods
    // End of user code
    public ChangeRequest()
    {
        super();
    
        // Start of user code constructor1
        // End of user code
    }
    
    public ChangeRequest(final URI about)
    {
        super(about);
    
        // Start of user code constructor2
        // End of user code
    }
    
    public static ResourceShape createResourceShape() throws OslcCoreApplicationException, URISyntaxException {
        return ResourceShapeFactory.createResourceShape(OSLC4JUtils.getServletURI(),
        OslcConstants.PATH_RESOURCE_SHAPES,
        Oslc_cmDomainConstants.CHANGEREQUEST_PATH,
        ChangeRequest.class);
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
            result = result + "{a Local ChangeRequest Resource} - update ChangeRequest.toString() to present resource as desired.";
            // Start of user code toString_bodyForLocalResource
            // End of user code
        }
        else {
            result = String.valueOf(getAbout());
        }
    
        // Start of user code toString_finalize
        result = String.format("[%s]: %s (Change Request; id=%s)", this.getShortTitle(), this.getTitle(), this.getIdentifier());
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
    
    public void addRelatedChangeRequest(final Link relatedChangeRequest)
    {
        this.relatedChangeRequest.add(relatedChangeRequest);
    }
    
    public void addAffectsPlanItem(final Link affectsPlanItem)
    {
        this.affectsPlanItem.add(affectsPlanItem);
    }
    
    public void addAffectedByDefect(final Link affectedByDefect)
    {
        this.affectedByDefect.add(affectedByDefect);
    }
    
    public void addTracksRequirement(final Link tracksRequirement)
    {
        this.tracksRequirement.add(tracksRequirement);
    }
    
    public void addImplementsRequirement(final Link implementsRequirement)
    {
        this.implementsRequirement.add(implementsRequirement);
    }
    
    public void addAffectsRequirement(final Link affectsRequirement)
    {
        this.affectsRequirement.add(affectsRequirement);
    }
    
    public void addTracksChangeSet(final Link tracksChangeSet)
    {
        this.tracksChangeSet.add(tracksChangeSet);
    }
    
    public void addParent(final Link parent)
    {
        this.parent.add(parent);
    }
    
    public void addPriority(final Link priority)
    {
        this.priority.add(priority);
    }
    
    public void addAuthorizer(final Link authorizer)
    {
        this.authorizer.add(authorizer);
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
    
    // Start of user code getterAnnotation:discussedBy
    // End of user code
    @OslcName("discussedBy")
    @OslcPropertyDefinition(OslcDomainConstants.OSLC_NAMSPACE + "discussedBy")
    @OslcDescription("A series of notes and comments about this resource.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Resource)
    @OslcRange({OslcDomainConstants.DISCUSSION_TYPE})
    @OslcReadOnly(false)
    public Link getDiscussedBy()
    {
        // Start of user code getterInit:discussedBy
        // End of user code
        return discussedBy;
    }
    
    // Start of user code getterAnnotation:closeDate
    // End of user code
    @OslcName("closeDate")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "closeDate")
    @OslcDescription("The date at which no further activity or work is intended to be conducted.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.DateTime)
    @OslcReadOnly(false)
    public Date getCloseDate()
    {
        // Start of user code getterInit:closeDate
        // End of user code
        return closeDate;
    }
    
    // Start of user code getterAnnotation:status
    // End of user code
    @OslcName("status")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "status")
    @OslcDescription("Used to indicate the status of the change request based on values defined by the service provider. Most often a read-only property. Some possible values may include: 'Submitted', 'Done', 'InProgress', etc.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public String getStatus()
    {
        // Start of user code getterInit:status
        // End of user code
        return status;
    }
    
    // Start of user code getterAnnotation:closed
    // End of user code
    @OslcName("closed")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "closed")
    @OslcDescription("Whether or not the Change Request is completely done, no further fixes or fix verification is needed.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Boolean)
    @OslcReadOnly(false)
    public Boolean isClosed()
    {
        // Start of user code getterInit:closed
        // End of user code
        return closed;
    }
    
    // Start of user code getterAnnotation:inProgress
    // End of user code
    @OslcName("inProgress")
    @OslcPropertyDefinition(Oslc_cmDomainConstants.CHANGE_MANAGEMENT_SHAPES_NAMSPACE + "inProgress")
    @OslcDescription("Whether or not the Change Request in a state indicating that active work is occurring. If oslc_cm:inprogress is true, then oslc_cm:fixed and oslc_cm:closed must also be false")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Boolean)
    @OslcReadOnly(false)
    public Boolean isInProgress()
    {
        // Start of user code getterInit:inProgress
        // End of user code
        return inProgress;
    }
    
    // Start of user code getterAnnotation:fixed
    // End of user code
    @OslcName("fixed")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "fixed")
    @OslcDescription("Whether or not the Change Request has been fixed.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Boolean)
    @OslcReadOnly(false)
    public Boolean isFixed()
    {
        // Start of user code getterInit:fixed
        // End of user code
        return fixed;
    }
    
    // Start of user code getterAnnotation:approved
    // End of user code
    @OslcName("approved")
    @OslcPropertyDefinition(Oslc_cmDomainConstants.CHANGE_MANAGEMENT_SHAPES_NAMSPACE + "approved")
    @OslcDescription("Whether or not the Change Request has been approved.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Boolean)
    @OslcReadOnly(false)
    public Boolean isApproved()
    {
        // Start of user code getterInit:approved
        // End of user code
        return approved;
    }
    
    // Start of user code getterAnnotation:reviewed
    // End of user code
    @OslcName("reviewed")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "reviewed")
    @OslcDescription("Whether or not the Change Request has been reviewed.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Boolean)
    @OslcReadOnly(false)
    public Boolean isReviewed()
    {
        // Start of user code getterInit:reviewed
        // End of user code
        return reviewed;
    }
    
    // Start of user code getterAnnotation:verified
    // End of user code
    @OslcName("verified")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "verified")
    @OslcDescription("Whether or not the resolution or fix of the Change Request has been verified.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Boolean)
    @OslcReadOnly(false)
    public Boolean isVerified()
    {
        // Start of user code getterInit:verified
        // End of user code
        return verified;
    }
    
    // Start of user code getterAnnotation:relatedChangeRequest
    // End of user code
    @OslcName("relatedChangeRequest")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "relatedChangeRequest")
    @OslcDescription("This relationship is loosely coupled and has no specific meaning. It is likely that the target resource will be an oslc_cm:ChangeRequest but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getRelatedChangeRequest()
    {
        // Start of user code getterInit:relatedChangeRequest
        // End of user code
        return relatedChangeRequest;
    }
    
    // Start of user code getterAnnotation:affectsPlanItem
    // End of user code
    @OslcName("affectsPlanItem")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "affectsPlanItem")
    @OslcDescription("Change request affects a plan item. It is likely that the target resource will be an oslc_cm:ChangeRequest but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getAffectsPlanItem()
    {
        // Start of user code getterInit:affectsPlanItem
        // End of user code
        return affectsPlanItem;
    }
    
    // Start of user code getterAnnotation:affectedByDefect
    // End of user code
    @OslcName("affectedByDefect")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "affectedByDefect")
    @OslcDescription("Change request is affected by a reported defect. It is likely that the target resource will be an oslc_cm:ChangeRequest but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcRange({Oslc_cmDomainConstants.DEFECT_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getAffectedByDefect()
    {
        // Start of user code getterInit:affectedByDefect
        // End of user code
        return affectedByDefect;
    }
    
    // Start of user code getterAnnotation:tracksRequirement
    // End of user code
    @OslcName("tracksRequirement")
    @OslcPropertyDefinition(Oslc_cmDomainConstants.CHANGE_MANAGEMENT_SHAPES_NAMSPACE + "tracksRequirement")
    @OslcDescription("Tracks the associated Requirement or Requirement ChangeSet resources. It is likely that the target resource will be an oslc_rm:Requirement but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRange({Oslc_rmDomainConstants.REQUIREMENT_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getTracksRequirement()
    {
        // Start of user code getterInit:tracksRequirement
        // End of user code
        return tracksRequirement;
    }
    
    // Start of user code getterAnnotation:implementsRequirement
    // End of user code
    @OslcName("implementsRequirement")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "implementsRequirement")
    @OslcDescription("Implements associated Requirement. It is likely that the target resource will be an oslc_rm:Requirement but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcRange({Oslc_rmDomainConstants.REQUIREMENT_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getImplementsRequirement()
    {
        // Start of user code getterInit:implementsRequirement
        // End of user code
        return implementsRequirement;
    }
    
    // Start of user code getterAnnotation:affectsRequirement
    // End of user code
    @OslcName("affectsRequirement")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "affectsRequirement")
    @OslcDescription("Change request affecting a Requirement. It is likely that the target resource will be an oslc_rm:Requirement but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcRange({Oslc_rmDomainConstants.REQUIREMENT_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getAffectsRequirement()
    {
        // Start of user code getterInit:affectsRequirement
        // End of user code
        return affectsRequirement;
    }
    
    // Start of user code getterAnnotation:tracksChangeSet
    // End of user code
    @OslcName("tracksChangeSet")
    @OslcPropertyDefinition(Oslc_cmDomainConstants.CHANGE_MANAGEMENT_SHAPES_NAMSPACE + "tracksChangeSet")
    @OslcDescription("Tracks SCM change set resource. It is likely that the target resource will be an oslc_scm:ChangeSet but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcRange({Oslc_configDomainConstants.CHANGESET_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getTracksChangeSet()
    {
        // Start of user code getterInit:tracksChangeSet
        // End of user code
        return tracksChangeSet;
    }
    
    // Start of user code getterAnnotation:parent
    // End of user code
    @OslcName("parent")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "parent")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRange({Oslc_cmDomainConstants.CHANGEREQUEST_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getParent()
    {
        // Start of user code getterInit:parent
        // End of user code
        return parent;
    }
    
    // Start of user code getterAnnotation:priority
    // End of user code
    @OslcName("priority")
    @OslcPropertyDefinition(Oslc_cmDomainConstants.CHANGE_MANAGEMENT_SHAPES_NAMSPACE + "priority")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRange({Oslc_cmDomainConstants.PRIORITY_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getPriority()
    {
        // Start of user code getterInit:priority
        // End of user code
        return priority;
    }
    
    // Start of user code getterAnnotation:state
    // End of user code
    @OslcName("state")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "state")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Resource)
    @OslcRange({Oslc_cmDomainConstants.STATE_TYPE})
    @OslcReadOnly(false)
    public Link getState()
    {
        // Start of user code getterInit:state
        // End of user code
        return state;
    }
    
    // Start of user code getterAnnotation:authorizer
    // End of user code
    @OslcName("authorizer")
    @OslcPropertyDefinition(Oslc_cmDomainConstants.CHANGE_MANAGEMENT_SHAPES_NAMSPACE + "authorizer")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRange({FoafDomainConstants.AGENT_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getAuthorizer()
    {
        // Start of user code getterInit:authorizer
        // End of user code
        return authorizer;
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
    
    // Start of user code setterAnnotation:discussedBy
    // End of user code
    public void setDiscussedBy(final Link discussedBy )
    {
        // Start of user code setterInit:discussedBy
        // End of user code
        this.discussedBy = discussedBy;
    
        // Start of user code setterFinalize:discussedBy
        // End of user code
    }
    
    // Start of user code setterAnnotation:closeDate
    // End of user code
    public void setCloseDate(final Date closeDate )
    {
        // Start of user code setterInit:closeDate
        // End of user code
        this.closeDate = closeDate;
    
        // Start of user code setterFinalize:closeDate
        // End of user code
    }
    
    // Start of user code setterAnnotation:status
    // End of user code
    public void setStatus(final String status )
    {
        // Start of user code setterInit:status
        // End of user code
        this.status = status;
    
        // Start of user code setterFinalize:status
        // End of user code
    }
    
    // Start of user code setterAnnotation:closed
    // End of user code
    public void setClosed(final Boolean closed )
    {
        // Start of user code setterInit:closed
        // End of user code
        this.closed = closed;
    
        // Start of user code setterFinalize:closed
        // End of user code
    }
    
    // Start of user code setterAnnotation:inProgress
    // End of user code
    public void setInProgress(final Boolean inProgress )
    {
        // Start of user code setterInit:inProgress
        // End of user code
        this.inProgress = inProgress;
    
        // Start of user code setterFinalize:inProgress
        // End of user code
    }
    
    // Start of user code setterAnnotation:fixed
    // End of user code
    public void setFixed(final Boolean fixed )
    {
        // Start of user code setterInit:fixed
        // End of user code
        this.fixed = fixed;
    
        // Start of user code setterFinalize:fixed
        // End of user code
    }
    
    // Start of user code setterAnnotation:approved
    // End of user code
    public void setApproved(final Boolean approved )
    {
        // Start of user code setterInit:approved
        // End of user code
        this.approved = approved;
    
        // Start of user code setterFinalize:approved
        // End of user code
    }
    
    // Start of user code setterAnnotation:reviewed
    // End of user code
    public void setReviewed(final Boolean reviewed )
    {
        // Start of user code setterInit:reviewed
        // End of user code
        this.reviewed = reviewed;
    
        // Start of user code setterFinalize:reviewed
        // End of user code
    }
    
    // Start of user code setterAnnotation:verified
    // End of user code
    public void setVerified(final Boolean verified )
    {
        // Start of user code setterInit:verified
        // End of user code
        this.verified = verified;
    
        // Start of user code setterFinalize:verified
        // End of user code
    }
    
    // Start of user code setterAnnotation:relatedChangeRequest
    // End of user code
    public void setRelatedChangeRequest(final Set<Link> relatedChangeRequest )
    {
        // Start of user code setterInit:relatedChangeRequest
        // End of user code
        this.relatedChangeRequest.clear();
        if (relatedChangeRequest != null)
        {
            this.relatedChangeRequest.addAll(relatedChangeRequest);
        }
    
        // Start of user code setterFinalize:relatedChangeRequest
        // End of user code
    }
    
    // Start of user code setterAnnotation:affectsPlanItem
    // End of user code
    public void setAffectsPlanItem(final Set<Link> affectsPlanItem )
    {
        // Start of user code setterInit:affectsPlanItem
        // End of user code
        this.affectsPlanItem.clear();
        if (affectsPlanItem != null)
        {
            this.affectsPlanItem.addAll(affectsPlanItem);
        }
    
        // Start of user code setterFinalize:affectsPlanItem
        // End of user code
    }
    
    // Start of user code setterAnnotation:affectedByDefect
    // End of user code
    public void setAffectedByDefect(final Set<Link> affectedByDefect )
    {
        // Start of user code setterInit:affectedByDefect
        // End of user code
        this.affectedByDefect.clear();
        if (affectedByDefect != null)
        {
            this.affectedByDefect.addAll(affectedByDefect);
        }
    
        // Start of user code setterFinalize:affectedByDefect
        // End of user code
    }
    
    // Start of user code setterAnnotation:tracksRequirement
    // End of user code
    public void setTracksRequirement(final Set<Link> tracksRequirement )
    {
        // Start of user code setterInit:tracksRequirement
        // End of user code
        this.tracksRequirement.clear();
        if (tracksRequirement != null)
        {
            this.tracksRequirement.addAll(tracksRequirement);
        }
    
        // Start of user code setterFinalize:tracksRequirement
        // End of user code
    }
    
    // Start of user code setterAnnotation:implementsRequirement
    // End of user code
    public void setImplementsRequirement(final Set<Link> implementsRequirement )
    {
        // Start of user code setterInit:implementsRequirement
        // End of user code
        this.implementsRequirement.clear();
        if (implementsRequirement != null)
        {
            this.implementsRequirement.addAll(implementsRequirement);
        }
    
        // Start of user code setterFinalize:implementsRequirement
        // End of user code
    }
    
    // Start of user code setterAnnotation:affectsRequirement
    // End of user code
    public void setAffectsRequirement(final Set<Link> affectsRequirement )
    {
        // Start of user code setterInit:affectsRequirement
        // End of user code
        this.affectsRequirement.clear();
        if (affectsRequirement != null)
        {
            this.affectsRequirement.addAll(affectsRequirement);
        }
    
        // Start of user code setterFinalize:affectsRequirement
        // End of user code
    }
    
    // Start of user code setterAnnotation:tracksChangeSet
    // End of user code
    public void setTracksChangeSet(final Set<Link> tracksChangeSet )
    {
        // Start of user code setterInit:tracksChangeSet
        // End of user code
        this.tracksChangeSet.clear();
        if (tracksChangeSet != null)
        {
            this.tracksChangeSet.addAll(tracksChangeSet);
        }
    
        // Start of user code setterFinalize:tracksChangeSet
        // End of user code
    }
    
    // Start of user code setterAnnotation:parent
    // End of user code
    public void setParent(final Set<Link> parent )
    {
        // Start of user code setterInit:parent
        // End of user code
        this.parent.clear();
        if (parent != null)
        {
            this.parent.addAll(parent);
        }
    
        // Start of user code setterFinalize:parent
        // End of user code
    }
    
    // Start of user code setterAnnotation:priority
    // End of user code
    public void setPriority(final Set<Link> priority )
    {
        // Start of user code setterInit:priority
        // End of user code
        this.priority.clear();
        if (priority != null)
        {
            this.priority.addAll(priority);
        }
    
        // Start of user code setterFinalize:priority
        // End of user code
    }
    
    // Start of user code setterAnnotation:state
    // End of user code
    public void setState(final Link state )
    {
        // Start of user code setterInit:state
        // End of user code
        this.state = state;
    
        // Start of user code setterFinalize:state
        // End of user code
    }
    
    // Start of user code setterAnnotation:authorizer
    // End of user code
    public void setAuthorizer(final Set<Link> authorizer )
    {
        // Start of user code setterInit:authorizer
        // End of user code
        this.authorizer.clear();
        if (authorizer != null)
        {
            this.authorizer.addAll(authorizer);
        }
    
        // Start of user code setterFinalize:authorizer
        // End of user code
    }
    
    
}
