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
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;

@OslcResourceShape(title = "Quality Management Resource Shape", describes = QmConstants.TYPE_TEST_EXECUTION_RECORD)
@OslcNamespace(QmConstants.QUALITY_MANAGEMENT_NAMESPACE)
/**
 * @see http://open-services.net/bin/view/Main/QmSpecificationV2#Resource_TestExecutionRecord
 */
public final class TestExecutionRecord
       extends QmResource
{
    private final Set<Link>     blockedByChangeRequests       = new HashSet<Link>();
    private final Set<URI>      contributors                = new TreeSet<URI>();
    private final Set<URI>      creators                    = new TreeSet<URI>();
    private final Set<Link>     relatedChangeRequests       = new HashSet<Link>();

    private Link     reportsOnTestPlan;
    private URI      runsOnTestEnvironment;
    private Link     runsTestCase;

    public TestExecutionRecord()
    {
        super();
    }

    protected URI getRdfType() {
    	return URI.create(QmConstants.TYPE_TEST_EXECUTION_RECORD);
    }

    public void addBlockedByChangeRequest(final Link blockingChangeRequest)
    {
        this.blockedByChangeRequests.add(blockingChangeRequest);
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

    @OslcDescription("The person(s) who are responsible for the work needed to complete the change request.")
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

    @OslcDescription("Change Request that prevents execution of the Test Execution Record.")
    @OslcName("blockedByChangeRequest")
    @OslcPropertyDefinition(QmConstants.QUALITY_MANAGEMENT_NAMESPACE + "blockedByChangeRequest")
    @OslcRange(QmConstants.TYPE_CHANGE_REQUEST)
    @OslcReadOnly(false)
    @OslcTitle("Blocked By Change Request")
    public Link[] getBlockedByChangeRequests()
    {
        return blockedByChangeRequests.toArray(new Link[blockedByChangeRequests.size()]);
    }

    @OslcDescription("This relationship is loosely coupled and has no specific meaning.")
    @OslcName("relatedChangeRequest")
    @OslcPropertyDefinition(QmConstants.QUALITY_MANAGEMENT_NAMESPACE + "relatedChangeRequest")
    @OslcRange(QmConstants.TYPE_CHANGE_REQUEST)
    @OslcReadOnly(false)
    @OslcTitle("Related Change Requests")
    public Link[] getRelatedChangeRequests()
    {
        return relatedChangeRequests.toArray(new Link[relatedChangeRequests.size()]);
    }

    @OslcDescription("Test Plan that the Test Execution Record reports on.")
    @OslcName("reportsOnTestPlan")
    @OslcPropertyDefinition(QmConstants.QUALITY_MANAGEMENT_NAMESPACE + "reportsOnTestPlan")
    @OslcRange(QmConstants.TYPE_TEST_PLAN)
    @OslcReadOnly(false)
    @OslcTitle("Reports On Test Plan")
    public Link getReportsOnTestPlan()
    {
        return reportsOnTestPlan;
    }

    @OslcDescription("Indicates the environment details of the test case for this execution record.")
    @OslcPropertyDefinition(QmConstants.QUALITY_MANAGEMENT_NAMESPACE + "runsOnTestEnvironment")
    @OslcTitle("Runs On Test Environment")
    public URI getRunsOnTestEnvironment()
    {
        return runsOnTestEnvironment;
    }

    @OslcDescription("Test Case run by the Test Execution Record.")
    @OslcName("runsTestCase")
    @OslcPropertyDefinition(QmConstants.QUALITY_MANAGEMENT_NAMESPACE + "runsTestCase")
    @OslcRange(QmConstants.TYPE_TEST_CASE)
    @OslcReadOnly(false)
    @OslcTitle("Runs Test Case")
    public Link getRunsTestCase()
    {
        return runsTestCase;
    }

    public void setBlockedByChangeRequests(final Link[] blockedByChangeRequests)
    {
        this.blockedByChangeRequests.clear();

        if (blockedByChangeRequests != null)
        {
            this.blockedByChangeRequests.addAll(Arrays.asList(blockedByChangeRequests));
        }
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

    public void setRelatedChangeRequests(final Link[] relatedChangeRequests)
    {
        this.relatedChangeRequests.clear();

        if (relatedChangeRequests != null)
        {
            this.relatedChangeRequests.addAll(Arrays.asList(relatedChangeRequests));
        }
    }

    public void setReportsOnTestPlan(final Link reportsOnTestPlan)
    {
        this.reportsOnTestPlan = reportsOnTestPlan;
    }

    public void setRunsOnTestEnvironment(final URI runsOnTestEnvironment)
    {
        this.runsOnTestEnvironment = runsOnTestEnvironment;
    }

    public void setRunsTestCase(final Link runsTestCase)
    {
        this.runsTestCase = runsTestCase;
    }

}
