/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
 *
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the Eclipse Public License v1.0
 *  and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *  
 *  The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 *  and the Eclipse Distribution License is available at
 *  http://www.eclipse.org/org/documents/edl-v10.php.
 *  
 *  Contributors:
 *  
 *     Paul McMahan <pmcmahan@us.ibm.com>     - initial implementation
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.samples.automation;

import javax.xml.namespace.QName;

import org.eclipse.lyo.client.oslc.OSLCConstants;

public interface IConstants {
	
	String NAMESPACE_URI_JAZZ_QM     = "http://jazz.net/ns/qm/rqm#"; //$NON-NLS-1$
	String NAMESPACE_URI_XHTML       = "http://www.w3.org/1999/xhtml"; //$NON-NLS-1$
	String NAMESPACE_URI_DC_ELEMENTS = "http://purl.org/dc/elements/1.1/"; //$NON-NLS-1$
	
    String TYPE_AUTOMATION_ADAPTER    = NAMESPACE_URI_JAZZ_QM + "AutomationAdapter";
    
	QName PROPERTY_DC_RELATION               = new QName(OSLCConstants.DC, "relation");
	
	QName PROPERTY_QM_REPORTS_ON_TEST_CASE   = new QName(OSLCConstants.OSLC_QM_V2, "reportsOnTestCase");
	QName PROPERTY_QM_RUNS_TEST_CASE         = new QName(OSLCConstants.OSLC_QM_V2, "runsTestCase");
	QName PROPERTY_QM_EXECUTES_TEST_SCRIPT   = new QName(OSLCConstants.OSLC_QM_V2, "executesTestScript");
	QName PROPERTY_QM_REPORTS_ON_TEST_PLAN   = new QName(OSLCConstants.OSLC_QM_V2, "reportsOnTestPlan");

	QName PROPERTY_RQM_TAKEN                 = new QName(IConstants.NAMESPACE_URI_JAZZ_QM, "taken");
	QName PROPERTY_RQM_PROGRESS              = new QName(IConstants.NAMESPACE_URI_JAZZ_QM, "progress");
	QName PROPERTY_RQM_EXECUTES_ON_ADAPTER   = new QName(IConstants.NAMESPACE_URI_JAZZ_QM, "executesOnAdapter");
	QName PROPERTY_RQM_REQUEST_TYPE          = new QName(IConstants.NAMESPACE_URI_JAZZ_QM, "requestType");
	QName PROPERTY_RQM_UPLOAD_ATTACHMENT_URL = new QName(IConstants.NAMESPACE_URI_JAZZ_QM, "uploadAttachmentUrl");
	QName PROPERTY_RQM_TEST_SUITE_RESULT     = new QName(IConstants.NAMESPACE_URI_JAZZ_QM, "testSuiteResult");
	QName PROPERTY_RQM_STATE_URL             = new QName(IConstants.NAMESPACE_URI_JAZZ_QM, "stateUrl");
	QName PROPERTY_RQM_START_TIME            = new QName(IConstants.NAMESPACE_URI_JAZZ_QM, "startTime");
	QName PROPERTY_RQM_END_TIME              = new QName(IConstants.NAMESPACE_URI_JAZZ_QM, "endTime");
	QName PROPERTY_RQM_ATTACHMENT            = new QName(IConstants.NAMESPACE_URI_JAZZ_QM, "attachment");
	QName PROPERTY_RQM_EXECUTED_ON_MACHINE   = new QName(IConstants.NAMESPACE_URI_JAZZ_QM, "executedOnMachine");
	
}
