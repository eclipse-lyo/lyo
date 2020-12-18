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

import org.eclipse.lyo.oslc.domains.qm.Oslc_qmDomainConstants;
import org.eclipse.lyo.oslc.domains.cm.Oslc_cmDomainConstants;
import org.eclipse.lyo.oslc.domains.DctermsDomainConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcDomainConstants;
import org.eclipse.lyo.oslc.domains.qm.Oslc_qmDomainConstants;
import org.eclipse.lyo.oslc.domains.RdfDomainConstants;
import org.eclipse.lyo.oslc.domains.DctermsVocabularyConstants;
import org.eclipse.lyo.oslc.domains.Oslc_qmVocabularyConstants;
import org.eclipse.lyo.oslc.domains.RdfVocabularyConstants;
import org.eclipse.lyo.oslc.domains.cm.IChangeRequest;
import org.eclipse.lyo.oslc.domains.qm.ITestCase;
import org.eclipse.lyo.oslc.domains.qm.ITestExecutionRecord;
import org.eclipse.lyo.oslc.domains.qm.ITestPlan;
import org.eclipse.lyo.oslc.domains.qm.ITestScript;
// Start of user code imports
import org.eclipse.lyo.oslc.domains.Oslc_qmVocabularyConstants;
// End of user code

@OslcNamespace(Oslc_qmDomainConstants.TESTRESULT_NAMESPACE)
@OslcName(Oslc_qmDomainConstants.TESTRESULT_LOCALNAME)
@OslcResourceShape(title = "TestResult Resource Shape", describes = Oslc_qmDomainConstants.TESTRESULT_TYPE)
public interface ITestResult
{

    public void addInstanceShape(final Link instanceShape );
    public void addType(final Link type );
    public void addServiceProvider(final Link serviceProvider );
    public void addAffectedByChangeRequest(final Link affectedByChangeRequest );

    @OslcName("created")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "created")
    @OslcDescription("Timestamp of resource creation")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.DateTime)
    @OslcReadOnly(false)
    public Date getCreated();

    @OslcName("identifier")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "identifier")
    @OslcDescription("A unique identifier for a resource. Typically read-only and assigned by the service provider when a resource is created. Not typically intended for end-user display.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public String getIdentifier();

    @OslcName("modified")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "modified")
    @OslcDescription("Timestamp of latest resource modification")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.DateTime)
    @OslcReadOnly(false)
    public Date getModified();

    @OslcName("instanceShape")
    @OslcPropertyDefinition(OslcDomainConstants.OSLC_NAMSPACE + "instanceShape")
    @OslcDescription("The URI of a Resource Shape that describes the possible properties, occurrence, value types, allowed values and labels. This shape information is useful in displaying the subject resource as well as guiding clients in performing modifications. Instance shapes may be specific to the authenticated user associated with the request that retrieved the resource, the current state of the resource and other factors and thus should not be cached.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getInstanceShape();

    @OslcName("title")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "title")
    @OslcDescription("Title of the resource represented as rich text in XHTML content. SHOULD include only content that is valid inside an XHTML <span> element.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.XMLLiteral)
    @OslcReadOnly(false)
    public String getTitle();

    @OslcName("type")
    @OslcPropertyDefinition(RdfVocabularyConstants.RDF_NAMSPACE + "type")
    @OslcDescription("The resource type URIs")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcReadOnly(false)
    public Set<Link> getType();

    @OslcName("serviceProvider")
    @OslcPropertyDefinition(OslcDomainConstants.OSLC_NAMSPACE + "serviceProvider")
    @OslcDescription("A link to the resource's OSLC Service Provider. There may be cases when the subject resource is available from a service provider that implements multiple domain specifications, which could result in multiple values for this property.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getServiceProvider();

    @OslcName("status")
    @OslcPropertyDefinition(Oslc_qmDomainConstants.QUALITY_MANAGEMENT_NAMSPACE + "status")
    @OslcDescription("Used to indicate the state of the Test Result based on values defined by the service provider. Most often a read-only property.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public String getStatus();

    @OslcName("affectedByChangeRequest")
    @OslcPropertyDefinition(Oslc_qmVocabularyConstants.QUALITY_MANAGEMENT_NAMSPACE + "affectedByChangeRequest")
    @OslcDescription("Change request that affects the Test Result. It is likely that the target resource will be an oslc_cm:ChangeRequest but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcRange({Oslc_cmDomainConstants.CHANGEREQUEST_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getAffectedByChangeRequest();

    @OslcName("executesTestScript")
    @OslcPropertyDefinition(Oslc_qmVocabularyConstants.QUALITY_MANAGEMENT_NAMSPACE + "executesTestScript")
    @OslcDescription("Test Script executed to produce the Test Result. It is likely that the target resource will be an oslc_qm:TestScript but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Resource)
    @OslcRange({Oslc_qmDomainConstants.TESTSCRIPT_TYPE})
    @OslcReadOnly(false)
    @OslcTitle("")
    public Link getExecutesTestScript();

    @OslcName("producedByTestExecutionRecord")
    @OslcPropertyDefinition(Oslc_qmVocabularyConstants.QUALITY_MANAGEMENT_NAMSPACE + "producedByTestExecutionRecord")
    @OslcDescription("Test Execution Record that the Test Result was produced by. It is likely that the target resource will be an oslc_qm:TestExecutionRecord but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Resource)
    @OslcRange({Oslc_qmDomainConstants.TESTEXECUTIONRECORD_TYPE})
    @OslcReadOnly(false)
    @OslcTitle("")
    public Link getProducedByTestExecutionRecord();

    @OslcName("reportsOnTestCase")
    @OslcPropertyDefinition(Oslc_qmVocabularyConstants.QUALITY_MANAGEMENT_NAMSPACE + "reportsOnTestCase")
    @OslcDescription("Test Case that the Test Result reports on. It is likely that the target resource will be an oslc_qm:TestCase but that is not necessarily the case.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.Resource)
    @OslcRange({Oslc_qmDomainConstants.TESTCASE_TYPE})
    @OslcReadOnly(false)
    public Link getReportsOnTestCase();

    @OslcName("reportsOnTestPlan")
    @OslcPropertyDefinition(Oslc_qmVocabularyConstants.QUALITY_MANAGEMENT_NAMSPACE + "reportsOnTestPlan")
    @OslcDescription("Test Plan that the Test Execution Record reports on. It is likely that the target resource will be an oslc_qm:TestPlan but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Resource)
    @OslcRange({Oslc_qmDomainConstants.TESTPLAN_TYPE})
    @OslcReadOnly(false)
    public Link getReportsOnTestPlan();


    public void setCreated(final Date created );
    public void setIdentifier(final String identifier );
    public void setModified(final Date modified );
    public void setInstanceShape(final Set<Link> instanceShape );
    public void setTitle(final String title );
    public void setType(final Set<Link> type );
    public void setServiceProvider(final Set<Link> serviceProvider );
    public void setStatus(final String status );
    public void setAffectedByChangeRequest(final Set<Link> affectedByChangeRequest );
    public void setExecutesTestScript(final Link executesTestScript );
    public void setProducedByTestExecutionRecord(final Link producedByTestExecutionRecord );
    public void setReportsOnTestCase(final Link reportsOnTestCase );
    public void setReportsOnTestPlan(final Link reportsOnTestPlan );
}

