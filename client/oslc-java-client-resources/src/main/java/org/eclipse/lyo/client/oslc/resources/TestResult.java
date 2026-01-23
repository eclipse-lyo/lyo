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
package org.eclipse.lyo.client.oslc.resources;

import java.net.URI;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.ValueType;

/**
 * @see <a href="https://docs.oasis-open-projects.org/oslc-op/qm/v2.1/os/quality-management-shapes.html#TestResultShape">https://docs.oasis-open-projects.org/oslc-op/qm/v2.1/os/quality-management-shapes.html#TestResultShape</a>
 */
@Deprecated
@OslcResourceShape(title = "Quality Management Resource Shape", describes = QmConstants.TYPE_TEST_RESULT)
@OslcNamespace(QmConstants.QUALITY_MANAGEMENT_NAMESPACE)
public class TestResult
       extends QmResource
{
    private final Set<Link>     affectedByChangeRequests       = new HashSet<>();

    private Link     executesTestScript;
    private Link     reportsOnTestCase;
    private Link     reportsOnTestPlan;
    private Link     producedByTestExecutionRecord;
    private String   status;

    public TestResult()
    {
        super();
    }

    protected URI getRdfType() {
    	return URI.create(QmConstants.TYPE_TEST_RESULT);
    }

    public void addAffectedByChangeRequest(final Link affectingChangeRequest)
    {
        this.affectedByChangeRequests.add(affectingChangeRequest);
    }

    @OslcDescription("Change request that affects the Test Result.")
    @OslcName("affectedByChangeRequest")
    @OslcPropertyDefinition(QmConstants.QUALITY_MANAGEMENT_NAMESPACE + "affectedByChangeRequest")
    @OslcRange(QmConstants.TYPE_CHANGE_REQUEST)
    @OslcReadOnly(false)
    @OslcTitle("Affected By Change Request")
    public Link[] getAffectedByChangeRequests()
    {
        return affectedByChangeRequests.toArray(new Link[affectedByChangeRequests.size()]);
    }

    @OslcDescription("Test Plan that the Test Result reports on.")
    @OslcName("reportsOnTestPlan")
    @OslcPropertyDefinition(QmConstants.QUALITY_MANAGEMENT_NAMESPACE + "reportsOnTestPlan")
    @OslcRange(QmConstants.TYPE_TEST_PLAN)
    @OslcReadOnly(false)
    @OslcTitle("Reports On Test Plan")
    public Link getReportsOnTestPlan()
    {
        return reportsOnTestPlan;
    }

    @OslcDescription("Test Case that the Test Result reports on.")
    @OslcName("reportsOnTestCase")
    @OslcPropertyDefinition(QmConstants.QUALITY_MANAGEMENT_NAMESPACE + "reportsOnTestCase")
    @OslcRange(QmConstants.TYPE_TEST_CASE)
    @OslcReadOnly(false)
    @OslcTitle("Reports On Test Case")
    public Link getReportsOnTestCase()
    {
        return reportsOnTestCase;
    }

    @OslcDescription("Test Script executed to produce the Test Result.")
    @OslcPropertyDefinition(QmConstants.QUALITY_MANAGEMENT_NAMESPACE + "executesTestScript")
    @OslcTitle("Executes Test Script")
    public Link getExecutesTestScript()
    {
        return executesTestScript;
    }

    @OslcDescription("Test Execution Record that the Test Result was produced by.")
    @OslcName("producedByTestExecutionRecord")
    @OslcPropertyDefinition(QmConstants.QUALITY_MANAGEMENT_NAMESPACE + "producedByTestExecutionRecord")
    @OslcRange(QmConstants.TYPE_TEST_EXECUTION_RECORD)
    @OslcReadOnly(false)
    @OslcTitle("Produced By Test Execution Record")
    public Link getProducedByTestExecutionRecord()
    {
        return producedByTestExecutionRecord;
    }

    @OslcDescription("Used to indicate the state of the Test Result based on values defined by the service provider.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcPropertyDefinition(QmConstants.QUALITY_MANAGEMENT_NAMESPACE + "status")
    @OslcTitle("Status")
    @OslcValueType(ValueType.XMLLiteral)
    public String getStatus()
    {
        return status;
    }

    public void setAffectedByChangeRequests(final Link[] affectedByChangeRequests)
    {
        this.affectedByChangeRequests.clear();

        if (affectedByChangeRequests != null)
        {
            this.affectedByChangeRequests.addAll(Arrays.asList(affectedByChangeRequests));
        }
    }

    public void setReportsOnTestPlan(final Link reportsOnTestPlan)
    {
        this.reportsOnTestPlan = reportsOnTestPlan;
    }

    public void setReportsOnTestCase(final Link reportsOnTestCase)
    {
        this.reportsOnTestCase = reportsOnTestCase;
    }

    public void setProducedByTestExecutionRecord(final Link producedByTestExecutionRecord)
    {
        this.producedByTestExecutionRecord = producedByTestExecutionRecord;
    }

    public void setExecutesTestScript(final Link executesTestScript)
    {
        this.executesTestScript = executesTestScript;
    }

    public void setStatus(final String status)
    {
        this.status = status;
    }

}
