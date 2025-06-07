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

package org.eclipse.lyo.oslc.domains.auto;

import java.util.Date;
import java.util.Set;
import org.eclipse.lyo.oslc.domains.DctermsVocabularyConstants;
import org.eclipse.lyo.oslc.domains.FoafDomainConstants;
import org.eclipse.lyo.oslc.domains.RdfVocabularyConstants;
import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
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
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.OslcDomainConstants;
import org.eclipse.lyo.oslc4j.core.model.Representation;
import org.eclipse.lyo.oslc4j.core.model.ValueType;

// Start of user code imports
// End of user code

@OslcNamespace(Oslc_autoDomainConstants.AUTOMATIONPLAN_NAMESPACE)
@OslcName(Oslc_autoDomainConstants.AUTOMATIONPLAN_LOCALNAME)
@OslcResourceShape(
        title = "AutomationPlan Shape",
        describes = Oslc_autoDomainConstants.AUTOMATIONPLAN_TYPE)
public interface IAutomationPlan {

    public void addContributor(final Link contributor);

    public void addCreator(final Link creator);

    public void addType(final Link type);

    public void addSubject(final String subject);

    public void addInstanceShape(final Link instanceShape);

    public void addServiceProvider(final Link serviceProvider);

    public void addParameterDefinition(final Link parameterDefinition);

    public void addUsesExecutionEnvironment(final Link usesExecutionEnvironment);

    public void addFutureAction(final Link futureAction);

    @OslcName("contributor")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "contributor")
    @OslcDescription(
            "Contributor or contributors to the resource. It is likely that the target resource"
                    + " will be a foaf:Person but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRange({FoafDomainConstants.PERSON_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getContributor();

    @OslcName("created")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "created")
    @OslcDescription("Timestamp of resource creation")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.DateTime)
    @OslcReadOnly(false)
    public Date getCreated();

    @OslcName("creator")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "creator")
    @OslcDescription(
            "Creator or creators of the resource. It is likely that the target resource will be a"
                    + " foaf:Person but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRange({FoafDomainConstants.PERSON_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getCreator();

    @OslcName("description")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "description")
    @OslcDescription(
            "Descriptive text about resource represented as rich text in XHTML content. SHOULD"
                + " include only content that is valid and suitable inside an XHTML <div> element.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.XMLLiteral)
    @OslcReadOnly(false)
    public String getDescription();

    @OslcName("identifier")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "identifier")
    @OslcDescription(
            "A unique identifier for a resource. Typically read-only and assigned by the service"
                    + " provider when a resource is created. Not typically intended for end-user"
                    + " display.")
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

    @OslcName("type")
    @OslcPropertyDefinition(RdfVocabularyConstants.RDF_NAMSPACE + "type")
    @OslcDescription("The resource type URIs")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcReadOnly(false)
    public Set<Link> getType();

    @OslcName("subject")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "subject")
    @OslcDescription(
            "Tag or keyword for a resource. Each occurrence of a dcterms:subject property denotes"
                    + " an additional tag for the resource.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    @OslcTitle("")
    public Set<String> getSubject();

    @OslcName("title")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "title")
    @OslcDescription(
            "Title of the resource represented as rich text in XHTML content. SHOULD include only"
                    + " content that is valid inside an XHTML <span> element.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.XMLLiteral)
    @OslcReadOnly(false)
    public String getTitle();

    @OslcName("instanceShape")
    @OslcPropertyDefinition(OslcDomainConstants.OSLC_NAMSPACE + "instanceShape")
    @OslcDescription(
            "The URI of a Resource Shape that describes the possible properties, occurrence, value"
                + " types, allowed values and labels. This shape information is useful in"
                + " displaying the subject resource as well as guiding clients in performing"
                + " modifications. Instance shapes may be specific to the authenticated user"
                + " associated with the request that retrieved the resource, the current state of"
                + " the resource and other factors and thus should not be cached.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getInstanceShape();

    @OslcName("serviceProvider")
    @OslcPropertyDefinition(OslcDomainConstants.OSLC_NAMSPACE + "serviceProvider")
    @OslcDescription(
            "A link to the resource's OSLC Service Provider. There may be cases when the subject"
                + " resource is available from a service provider that implements multiple domain"
                + " specifications, which could result in multiple values for this property.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getServiceProvider();

    @OslcName("parameterDefinition")
    @OslcPropertyDefinition(Oslc_autoDomainConstants.AUTOMATION_NAMSPACE + "parameterDefinition")
    @OslcDescription(
            "The definition of a parameter for this Automation Plan. parameterDefinitions are"
                + " either a local (inline) or referenced resource and use the attributes (the"
                + " range) of the  oslc:Property resource with one exception. When used in the"
                + " context of an oslc_auto:parameterDefinition, the cardinality of"
                + " oslc:propertyDefinition becomes zero-or-one instead of exactly-one. Automation"
                + " consumers creating Automation Requests MUST use the oslc:occurs attribute of"
                + " the parameterDefinition, if present, to determine if a given parameter is"
                + " required when creating the Automation Request. If the oslc:occurs attribute"
                + " indicates the parameter is required (exactly-one or one-or-more), the service"
                + " provider must guarantee the named parameter will be present in the Automation"
                + " Result either as an oslc_auto:inputParmeter when unmodified during execution,"
                + " or as an oslc_auto:outputParameter when modified during execution.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcReadOnly(false)
    public Set<Link> getParameterDefinition();

    @OslcName("usesExecutionEnvironment")
    @OslcPropertyDefinition(
            Oslc_autoDomainConstants.AUTOMATION_NAMSPACE + "usesExecutionEnvironment")
    @OslcDescription(
            "A resource representing the environment(s) which this Automation Plan can be executed"
                + " in. The execution environment resource could represent a grouping of"
                + " environmental details such as operating system, database, browser, compiler,"
                + " etc. See also the execution environments section.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getUsesExecutionEnvironment();

    @OslcName("futureAction")
    @OslcPropertyDefinition(Oslc_autoDomainConstants.AUTOMATION_NAMSPACE + "futureAction")
    @OslcDescription(
            "A resource representing actions that will become available on Automation Results that"
                    + " result from execution of this Plan. The resource is likely to be of type"
                    + " oslc:Action, but it can be of any type. Automation defines"
                    + " oslc_auto:TeardownAction as one kind of future action.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getFutureAction();

    public void setContributor(final Set<Link> contributor);

    public void setCreated(final Date created);

    public void setCreator(final Set<Link> creator);

    public void setDescription(final String description);

    public void setIdentifier(final String identifier);

    public void setModified(final Date modified);

    public void setType(final Set<Link> type);

    public void setSubject(final Set<String> subject);

    public void setTitle(final String title);

    public void setInstanceShape(final Set<Link> instanceShape);

    public void setServiceProvider(final Set<Link> serviceProvider);

    public void setParameterDefinition(final Set<Link> parameterDefinition);

    public void setUsesExecutionEnvironment(final Set<Link> usesExecutionEnvironment);

    public void setFutureAction(final Set<Link> futureAction);
}
