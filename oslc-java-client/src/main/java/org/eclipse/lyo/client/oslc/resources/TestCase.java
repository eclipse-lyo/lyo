/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 *     Paul McMahan         - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.resources;

import java.net.URI;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.ValueType;

@OslcResourceShape(title = "Quality Management Resource Shape", describes = QmConstants.TYPE_TEST_CASE)
@OslcNamespace(QmConstants.QUALITY_MANAGEMENT_NAMESPACE)
/**
 * @see http://open-services.net/bin/view/Main/QmSpecificationV2#Resource_TestCase
 */
public final class TestCase
       extends QmResource
{
    private final Set<URI>      contributors                = new TreeSet<URI>();
    private final Set<URI>      creators                    = new TreeSet<URI>();
    private final Set<Link>     relatedChangeRequests       = new HashSet<Link>();
    private final Set<String>   subjects                    = new TreeSet<String>();
    private final Set<Link>     testsChangeRequests         = new HashSet<Link>();
    private final Set<Link>     usesTestScripts             = new HashSet<Link>();
    private final Set<Link>     validatesRequirements       = new HashSet<Link>();

    private String   description;

    public TestCase()
    {
        super();
    }

    protected URI getRdfType() {
    	return URI.create(QmConstants.TYPE_TEST_CASE);
    }

    public void addContributor(final URI contributor)
    {
        this.contributors.add(contributor);
    }

    public void addCreator(final URI creator)
    {
        this.creators.add(creator);
    }

    public void addRelatedChangeRequest(final Link relatedChangeRequest)
    {
        this.relatedChangeRequests.add(relatedChangeRequest);
    }

    public void addSubject(final String subject)
    {
        this.subjects.add(subject);
    }

    public void addTestsChangeRequest(final Link changeRequest)
    {
        this.testsChangeRequests.add(changeRequest);
    }

    public void addUsesTestScript(final Link testscript)
    {
        this.usesTestScripts.add(testscript);
    }

    public void addValidatesRequirement(final Link requirement)
    {
        this.validatesRequirements.add(requirement);
    }

    @OslcDescription("The person(s) who are responsible for the work needed to complete the test case.")
    @OslcName("contributor")
    @OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "contributor")
    @OslcRange(QmConstants.TYPE_PERSON)
    @OslcTitle("Contributors")
    public URI[] getContributors()
    {
        return contributors.toArray(new URI[contributors.size()]);
    }

    @OslcDescription("Creator or creators of resource.")
    @OslcName("creator")
    @OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "creator")
    @OslcRange(QmConstants.TYPE_PERSON)
    @OslcTitle("Creators")
    public URI[] getCreators()
    {
        return creators.toArray(new URI[creators.size()]);
    }

    @OslcDescription("Descriptive text (reference: Dublin Core) about resource represented as rich text in XHTML content.")
    @OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "description")
    @OslcTitle("Description")
    @OslcValueType(ValueType.XMLLiteral)
    public String getDescription()
    {
        return description;
    }

    @OslcDescription("A related change request.")
    @OslcName("relatedChangeRequest")
    @OslcPropertyDefinition(QmConstants.QUALITY_MANAGEMENT_NAMESPACE + "relatedChangeRequest")
    @OslcRange(QmConstants.TYPE_CHANGE_REQUEST)
    @OslcReadOnly(false)
    @OslcTitle("Related Change Requests")
    public Link[] getRelatedChangeRequests()
    {
        return relatedChangeRequests.toArray(new Link[relatedChangeRequests.size()]);
    }

    @OslcDescription("Tag or keyword for a resource. Each occurrence of a dcterms:subject property denotes an additional tag for the resource.")
    @OslcName("subject")
    @OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "subject")
    @OslcReadOnly(false)
    @OslcTitle("Subjects")
    public String[] getSubjects()
    {
        return subjects.toArray(new String[subjects.size()]);
    }

    @OslcDescription("Change Request tested by the Test Case.")
    @OslcName("testsChangeRequest")
    @OslcPropertyDefinition(QmConstants.QUALITY_MANAGEMENT_NAMESPACE + "testsChangeRequest")
    @OslcRange(QmConstants.TYPE_CHANGE_REQUEST)
    @OslcReadOnly(false)
    @OslcTitle("Tests Change Request")
    public Link[] getTestsChangeRequests()
    {
        return testsChangeRequests.toArray(new Link[testsChangeRequests.size()]);
    }

    @OslcDescription("Test Script used by the Test Case.")
    @OslcName("usesTestScript")
    @OslcPropertyDefinition(QmConstants.QUALITY_MANAGEMENT_NAMESPACE + "usesTestScript")
    @OslcRange(QmConstants.TYPE_TEST_SCRIPT)
    @OslcReadOnly(false)
    @OslcTitle("Uses Test Script")
    public Link[] getUsesTestScripts()
    {
        return usesTestScripts.toArray(new Link[usesTestScripts.size()]);
    }

    @OslcDescription("Requirement that is validated by the Test Case.")
    @OslcName("validatesRequirement")
    @OslcPropertyDefinition(QmConstants.QUALITY_MANAGEMENT_NAMESPACE + "validatesRequirement")
    @OslcRange(QmConstants.TYPE_REQUIREMENT)
    @OslcReadOnly(false)
    @OslcTitle("Validates Requirement")
    public Link[] getValidatesRequirements()
    {
        return validatesRequirements.toArray(new Link[validatesRequirements.size()]);
    }

    public void setContributors(final URI[] contributors)
    {
        this.contributors.clear();

        if (contributors != null)
        {
            this.contributors.addAll(Arrays.asList(contributors));
        }
    }

    public void setCreators(final URI[] creators)
    {
        this.creators.clear();

        if (creators != null)
        {
            this.creators.addAll(Arrays.asList(creators));
        }
    }

    public void setDescription(final String description)
    {
        this.description = description;
    }

    public void setRelatedChangeRequests(final Link[] relatedChangeRequests)
    {
        this.relatedChangeRequests.clear();

        if (relatedChangeRequests != null)
        {
            this.relatedChangeRequests.addAll(Arrays.asList(relatedChangeRequests));
        }
    }

    public void setSubjects(final String[] subjects)
    {
        this.subjects.clear();

        if (subjects != null)
        {
            this.subjects.addAll(Arrays.asList(subjects));
        }
    }

    public void setTestsChangeRequests(final Link[] testsChangeRequests)
    {
        this.testsChangeRequests.clear();

        if (testsChangeRequests != null)
        {
            this.testsChangeRequests.addAll(Arrays.asList(testsChangeRequests));
        }
    }

    public void setUsesTestScripts(final Link[] usesTestScripts)
    {
        this.usesTestScripts.clear();

        if (usesTestScripts != null)
        {
            this.usesTestScripts.addAll(Arrays.asList(usesTestScripts));
        }
    }

    public void setValidatesRequirements(final Link[] validatesRequirements)
    {
        this.validatesRequirements.clear();

        if (validatesRequirements != null)
        {
            this.validatesRequirements.addAll(Arrays.asList(validatesRequirements));
        }
    }
}
