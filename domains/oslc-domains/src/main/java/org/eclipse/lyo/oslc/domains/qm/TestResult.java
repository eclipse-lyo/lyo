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

package org.eclipse.lyo.oslc.domains.qm;

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

import org.eclipse.lyo.oslc.domains.qm.Oslc_qmDomainConstants;


import org.eclipse.lyo.oslc.domains.cm.Oslc_cmDomainConstants;
import org.eclipse.lyo.oslc.domains.DctermsDomainConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcDomainConstants;
import org.eclipse.lyo.oslc.domains.qm.Oslc_qmDomainConstants;
import org.eclipse.lyo.oslc.domains.RdfDomainConstants;
import org.eclipse.lyo.oslc.domains.DctermsVocabularyConstants;

import org.eclipse.lyo.oslc.domains.Oslc_qmVocabularyConstants;

import org.eclipse.lyo.oslc.domains.RdfVocabularyConstants;
import org.eclipse.lyo.oslc.domains.cm.ChangeRequest;
import org.eclipse.lyo.oslc.domains.qm.TestScript;
import org.eclipse.lyo.oslc.domains.qm.TestExecutionRecord;
import org.eclipse.lyo.oslc.domains.qm.TestCase;
import org.eclipse.lyo.oslc.domains.qm.TestPlan;

// Start of user code imports
import org.eclipse.lyo.oslc.domains.Oslc_qmVocabularyConstants;
// End of user code

// Start of user code preClassCode
// End of user code

// Start of user code classAnnotations
// End of user code
@OslcNamespace(Oslc_qmDomainConstants.TESTRESULT_NAMESPACE)
@OslcName(Oslc_qmDomainConstants.TESTRESULT_LOCALNAME)
@OslcResourceShape(title = "TestResult Resource Shape", describes = Oslc_qmDomainConstants.TESTRESULT_TYPE)
public class TestResult
    extends AbstractResource
    implements ITestResult
{
    // Start of user code attributeAnnotation:created
    // End of user code
    private Date created;
    // Start of user code attributeAnnotation:identifier
    // End of user code
    private String identifier;
    // Start of user code attributeAnnotation:modified
    // End of user code
    private Date modified;
    // Start of user code attributeAnnotation:instanceShape
    // End of user code
    private Set<Link> instanceShape = new HashSet<Link>();
    // Start of user code attributeAnnotation:title
    // End of user code
    private String title;
    // Start of user code attributeAnnotation:type
    // End of user code
    private Set<Link> type = new HashSet<Link>();
    // Start of user code attributeAnnotation:serviceProvider
    // End of user code
    private Set<Link> serviceProvider = new HashSet<Link>();
    // Start of user code attributeAnnotation:status
    // End of user code
    private String status;
    // Start of user code attributeAnnotation:affectedByChangeRequest
    // End of user code
    private Set<Link> affectedByChangeRequest = new HashSet<Link>();
    // Start of user code attributeAnnotation:executesTestScript
    // End of user code
    private Link executesTestScript;
    // Start of user code attributeAnnotation:producedByTestExecutionRecord
    // End of user code
    private Link producedByTestExecutionRecord;
    // Start of user code attributeAnnotation:reportsOnTestCase
    // End of user code
    private Link reportsOnTestCase;
    // Start of user code attributeAnnotation:reportsOnTestPlan
    // End of user code
    private Link reportsOnTestPlan;
    
    // Start of user code classAttributes
    // End of user code
    // Start of user code classMethods
    // End of user code
    public TestResult()
    {
        super();
    
        // Start of user code constructor1
        // End of user code
    }
    
    public TestResult(final URI about)
    {
        super(about);
    
        // Start of user code constructor2
        // End of user code
    }
    
    public static ResourceShape createResourceShape() throws OslcCoreApplicationException, URISyntaxException {
        return ResourceShapeFactory.createResourceShape(OSLC4JUtils.getServletURI(),
        OslcConstants.PATH_RESOURCE_SHAPES,
        Oslc_qmDomainConstants.TESTRESULT_PATH,
        TestResult.class);
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
            result = result + "{a Local TestResult Resource} - update TestResult.toString() to present resource as desired.";
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
    
    public void addInstanceShape(final Link instanceShape)
    {
        this.instanceShape.add(instanceShape);
    }
    
    public void addType(final Link type)
    {
        this.type.add(type);
    }
    
    public void addServiceProvider(final Link serviceProvider)
    {
        this.serviceProvider.add(serviceProvider);
    }
    
    public void addAffectedByChangeRequest(final Link affectedByChangeRequest)
    {
        this.affectedByChangeRequest.add(affectedByChangeRequest);
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
    
    // Start of user code getterAnnotation:type
    // End of user code
    @OslcName("type")
    @OslcPropertyDefinition(RdfVocabularyConstants.RDF_NAMSPACE + "type")
    @OslcDescription("The resource type URIs")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcReadOnly(false)
    public Set<Link> getType()
    {
        // Start of user code getterInit:type
        // End of user code
        return type;
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
    
    // Start of user code getterAnnotation:status
    // End of user code
    @OslcName("status")
    @OslcPropertyDefinition(Oslc_qmDomainConstants.QUALITY_MANAGEMENT_NAMSPACE + "status")
    @OslcDescription("Used to indicate the state of the Test Result based on values defined by the service provider. Most often a read-only property.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public String getStatus()
    {
        // Start of user code getterInit:status
        // End of user code
        return status;
    }
    
    // Start of user code getterAnnotation:affectedByChangeRequest
    // End of user code
    @OslcName("affectedByChangeRequest")
    @OslcPropertyDefinition(Oslc_qmVocabularyConstants.QUALITY_MANAGEMENT_NAMSPACE + "affectedByChangeRequest")
    @OslcDescription("Change request that affects the Test Result. It is likely that the target resource will be an oslc_cm:ChangeRequest but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcRange({Oslc_cmDomainConstants.CHANGEREQUEST_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getAffectedByChangeRequest()
    {
        // Start of user code getterInit:affectedByChangeRequest
        // End of user code
        return affectedByChangeRequest;
    }
    
    // Start of user code getterAnnotation:executesTestScript
    // End of user code
    @OslcName("executesTestScript")
    @OslcPropertyDefinition(Oslc_qmVocabularyConstants.QUALITY_MANAGEMENT_NAMSPACE + "executesTestScript")
    @OslcDescription("Test Script executed to produce the Test Result. It is likely that the target resource will be an oslc_qm:TestScript but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Resource)
    @OslcRange({Oslc_qmDomainConstants.TESTSCRIPT_TYPE})
    @OslcReadOnly(false)
    @OslcTitle("")
    public Link getExecutesTestScript()
    {
        // Start of user code getterInit:executesTestScript
        // End of user code
        return executesTestScript;
    }
    
    // Start of user code getterAnnotation:producedByTestExecutionRecord
    // End of user code
    @OslcName("producedByTestExecutionRecord")
    @OslcPropertyDefinition(Oslc_qmVocabularyConstants.QUALITY_MANAGEMENT_NAMSPACE + "producedByTestExecutionRecord")
    @OslcDescription("Test Execution Record that the Test Result was produced by. It is likely that the target resource will be an oslc_qm:TestExecutionRecord but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Resource)
    @OslcRange({Oslc_qmDomainConstants.TESTEXECUTIONRECORD_TYPE})
    @OslcReadOnly(false)
    @OslcTitle("")
    public Link getProducedByTestExecutionRecord()
    {
        // Start of user code getterInit:producedByTestExecutionRecord
        // End of user code
        return producedByTestExecutionRecord;
    }
    
    // Start of user code getterAnnotation:reportsOnTestCase
    // End of user code
    @OslcName("reportsOnTestCase")
    @OslcPropertyDefinition(Oslc_qmVocabularyConstants.QUALITY_MANAGEMENT_NAMSPACE + "reportsOnTestCase")
    @OslcDescription("Test Case that the Test Result reports on. It is likely that the target resource will be an oslc_qm:TestCase but that is not necessarily the case.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.Resource)
    @OslcRange({Oslc_qmDomainConstants.TESTCASE_TYPE})
    @OslcReadOnly(false)
    public Link getReportsOnTestCase()
    {
        // Start of user code getterInit:reportsOnTestCase
        // End of user code
        return reportsOnTestCase;
    }
    
    // Start of user code getterAnnotation:reportsOnTestPlan
    // End of user code
    @OslcName("reportsOnTestPlan")
    @OslcPropertyDefinition(Oslc_qmVocabularyConstants.QUALITY_MANAGEMENT_NAMSPACE + "reportsOnTestPlan")
    @OslcDescription("Test Plan that the Test Execution Record reports on. It is likely that the target resource will be an oslc_qm:TestPlan but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Resource)
    @OslcRange({Oslc_qmDomainConstants.TESTPLAN_TYPE})
    @OslcReadOnly(false)
    public Link getReportsOnTestPlan()
    {
        // Start of user code getterInit:reportsOnTestPlan
        // End of user code
        return reportsOnTestPlan;
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
    
    // Start of user code setterAnnotation:type
    // End of user code
    public void setType(final Set<Link> type )
    {
        // Start of user code setterInit:type
        // End of user code
        this.type.clear();
        if (type != null)
        {
            this.type.addAll(type);
        }
    
        // Start of user code setterFinalize:type
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
    
    // Start of user code setterAnnotation:affectedByChangeRequest
    // End of user code
    public void setAffectedByChangeRequest(final Set<Link> affectedByChangeRequest )
    {
        // Start of user code setterInit:affectedByChangeRequest
        // End of user code
        this.affectedByChangeRequest.clear();
        if (affectedByChangeRequest != null)
        {
            this.affectedByChangeRequest.addAll(affectedByChangeRequest);
        }
    
        // Start of user code setterFinalize:affectedByChangeRequest
        // End of user code
    }
    
    // Start of user code setterAnnotation:executesTestScript
    // End of user code
    public void setExecutesTestScript(final Link executesTestScript )
    {
        // Start of user code setterInit:executesTestScript
        // End of user code
        this.executesTestScript = executesTestScript;
    
        // Start of user code setterFinalize:executesTestScript
        // End of user code
    }
    
    // Start of user code setterAnnotation:producedByTestExecutionRecord
    // End of user code
    public void setProducedByTestExecutionRecord(final Link producedByTestExecutionRecord )
    {
        // Start of user code setterInit:producedByTestExecutionRecord
        // End of user code
        this.producedByTestExecutionRecord = producedByTestExecutionRecord;
    
        // Start of user code setterFinalize:producedByTestExecutionRecord
        // End of user code
    }
    
    // Start of user code setterAnnotation:reportsOnTestCase
    // End of user code
    public void setReportsOnTestCase(final Link reportsOnTestCase )
    {
        // Start of user code setterInit:reportsOnTestCase
        // End of user code
        this.reportsOnTestCase = reportsOnTestCase;
    
        // Start of user code setterFinalize:reportsOnTestCase
        // End of user code
    }
    
    // Start of user code setterAnnotation:reportsOnTestPlan
    // End of user code
    public void setReportsOnTestPlan(final Link reportsOnTestPlan )
    {
        // Start of user code setterInit:reportsOnTestPlan
        // End of user code
        this.reportsOnTestPlan = reportsOnTestPlan;
    
        // Start of user code setterFinalize:reportsOnTestPlan
        // End of user code
    }
    
    
}
