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

import java.io.File;
import java.io.IOException;
import java.lang.Thread.UncaughtExceptionHandler;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Date;
import java.util.Properties;
import java.util.logging.Logger;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import net.oauth.OAuthException;

import org.eclipse.lyo.client.oslc.resources.AutomationConstants;
import org.eclipse.lyo.client.oslc.resources.AutomationRequest;
import org.eclipse.lyo.client.oslc.resources.AutomationResult;
import org.eclipse.lyo.client.oslc.resources.ParameterInstance;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Sample of registering an external agent (adapter) with an Automation Service
 * Provider and executing Automation Requests like an RQM test execution adapter.
 */
public class RQMAutomationSample implements IConstants, IAutomationRequestHandler, UncaughtExceptionHandler {

	private static final Logger logger = Logger.getLogger(RQMAutomationSample.class.getName());

	private AutomationAdapter adapter;

	/**
	 * Start the sample
	 *
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception {

		new RQMAutomationSample().begin();

	}

	/**
	 * Login to the Automation Service Provider and start polling for Automation
	 * Requests
	 *
	 * @param args
	 * @throws Exception
	 */
	private void begin() throws Exception {

		configureAdapter();

		try {

			logger.info("Starting heart beat thread for adapter at "
					+ adapter.getAbout());

			// create a heartbeat thread and start it
			Thread heartbeatThread = new Thread(
					adapter.new HeartbeatRunnable(), "Adapter Heartbeat Thread");
			heartbeatThread.setUncaughtExceptionHandler(this);
			heartbeatThread.start();

			logger.info("Starting adapter polling at "
					+ adapter.getAssignedWorkUrl());

			// start polling the service provider for Automation Requests.
			// this call will block until adapter.stop() is called in
			// handleAutomationRequest() below
			adapter.start(this);

		} finally {

			// stop the adapter, in case we are in this catch block due to
			// an exception being thrown
			adapter.stop();

			// logout of the Automation Service Provider in order to free server
			// side resources, licenses, etc.  Currently this is a no-op.
			adapter.logout();
		}
	}

	/**
	 * Configure the adapter by reading its properties from a Properties file
	 * and logging into the Service Provider.
	 *
	 * @throws Exception
	 */
	private void configureAdapter() throws Exception {

		// For the sake of simplicity this sample loads the adapter
		// properties from a file in the class loader. A real world adapter
		// would load the adapter properties from a stable location in the
		// filesystem or from a database.
		URI propertiesFileUri = RQMAutomationSample.class.getResource(
				"adapter.properties").toURI();

		logger.info("Loading cached adapter properties from " + propertiesFileUri.toString());

		Properties properties = new WriteThroughProperties(propertiesFileUri);

		adapter = new AutomationAdapter(properties);

		logger.info("Logging into service provider at "
				+ adapter.getServerUrl());

		// Login to the server using the properties provided in the constructor
		adapter.login();

		logger.info("Registering with service provider");

		// this call will establish the adapter's "about" property
		// as the URL for the adapter.
		adapter.register();

		properties.setProperty(AutomationAdapter.PROPERTY_ABOUT,
				adapter.getAbout().toString());

	}

	/**
	 * Example callback that demonstrates a few useful things an adapter can do.
	 *
	 * @see IAutomationRequestHandler#handleAutomationRequest(AutomationRequest, AutomationAdapter)
	 */
	public AutomationResult handleAutomationRequest(AutomationRequest request, AutomationAdapter adapter)
			throws AutomationException {

		logger.info("Adapter has been assigned an Automation Request at "
				+ request.getAbout());

		AutomationResult result = null;

		try {

			// Create a new automation result
			result = new AutomationResult();

			// Save the start time in the result
			result.getExtendedProperties().put(PROPERTY_RQM_START_TIME,
					new Date(System.currentTimeMillis()));

			// An example of how to get the script for the AutomationRequest.
			// The script might contain references to resources needed to
			// execute the test.
			adapter.sendMessageForRequest(new Message("LYO_1", "Downloading script document"), request);

			Document script = adapter.getScriptDocument(request);

			adapter.sendMessageForRequest(new Message("LYO_2", "Script document successfully downloaded"), request);

			// update progress indication
			adapter.sendProgressForRequest(50, request);

			// execute the script with the parameters from the Automation Request
			executeScript(script, request.getInputParameters(), adapter,
					request);

			// Upload an attachment for the result
			File attachment = getSampleFile();
			URI attachmentURI = adapter.uploadAttachment(attachment, request);

			// Set the attachment URI in the result
			result.getExtendedProperties().put(PROPERTY_RQM_ATTACHMENT, attachmentURI);

			// Add some rich text to the result
			Element xhtmlTableElement = createXhtmlTable();
			QName contributionQname = new QName(AutomationConstants.AUTOMATION_DOMAIN, "contribution");
			result.getExtendedProperties().put(contributionQname, xhtmlTableElement);

			// Set the verdict for the result
			result.addVerdict(new URI(AutomationConstants.VERDICT_PASSED));

			// Save the end time in the result
			result.getExtendedProperties().put(PROPERTY_RQM_END_TIME,
					new Date(System.currentTimeMillis()));

			// update progress indication
			adapter.sendProgressForRequest(99, request);

			logger.info("Returning a result with verdict "
					+ result.getVerdicts()[0]);


		} catch (AutomationRequestCanceledException e) {

			logger.info("Automation Request \""
					+ e.getCanceledRequest().getTitle() + "\" was canceled.");

			// clean up any resources created for test execution here

			result = null;

		} catch (Exception e) {

			// cancel the request since it could not be completed
			adapter.cancel(request);

			throw new AutomationException(e);

		}

		return result;
	}

	/**
	 * Execute the script with the provided input parameters.
	 *
	 * @param script
	 * @param inputParameters
	 * @throws InterruptedException
	 * @throws AutomationException
	 * @throws URISyntaxException
	 * @throws OAuthException
	 * @throws IOException
	 */
	private void executeScript(Document script,
			ParameterInstance[] inputParameters, AutomationAdapter adapter,
			AutomationRequest request) throws InterruptedException,
			AutomationException, IOException, OAuthException,
			URISyntaxException {

		String scriptTitle = script.getDocumentElement()
				.getElementsByTagNameNS(NAMESPACE_URI_DC_ELEMENTS, "title")
				.item(0).getTextContent();

		logger.info("Running script named '" + scriptTitle +"'");

		logger.info("Input parameters:");
		for (ParameterInstance parameter : inputParameters) {
			String paramStr = "\t" + parameter.getName() + ": "
					+ parameter.getValue();
			logger.info(paramStr);
		}

		/*
		 * Add code here to execute the test script
		 */
		Thread.sleep(1000);

		// Update the request status
		StatusResponse statusResponse = new StatusResponse(
				StatusResponse.STATUS_OK,
				"Script '" + scriptTitle + "' was executed successfully.");

		adapter.sendStatusForRequest(statusResponse, request);

	}

	/**
	 * Create an element for a simple XHTML table
	 *
	 * @return
	 * @throws ParserConfigurationException
	 */
	private Element createXhtmlTable() throws ParserConfigurationException {

		Document document = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();

		Element divElement = document.createElementNS(IConstants.NAMESPACE_URI_XHTML, "div");

		Element tableElement = document.createElementNS(IConstants.NAMESPACE_URI_XHTML, "table");
		divElement.appendChild(tableElement);
		tableElement.setAttribute("border", "1");

		Element tr1Element = document.createElementNS(IConstants.NAMESPACE_URI_XHTML, "tr");
		Element tr2Element = document.createElementNS(IConstants.NAMESPACE_URI_XHTML, "tr");
		tableElement.appendChild(tr1Element);
		tableElement.appendChild(tr2Element);

		Element th1Element = document.createElementNS(IConstants.NAMESPACE_URI_XHTML, "th");
		Element th2Element = document.createElementNS(IConstants.NAMESPACE_URI_XHTML, "th");
		tr1Element.appendChild(th1Element);
		tr1Element.appendChild(th2Element);
		th1Element.setTextContent("Column 1");
		th2Element.setTextContent("Column 2");

		Element td1Element = document.createElementNS(IConstants.NAMESPACE_URI_XHTML, "td");
		Element td2Element = document.createElementNS(IConstants.NAMESPACE_URI_XHTML, "td");
		tr2Element.appendChild(td1Element);
		tr2Element.appendChild(td2Element);
		td1Element.setTextContent("Value 1");
		td2Element.setTextContent("Value 2");

		return divElement;
	}

	/**
	 * Get a sample file from the class loader
	 *
	 * @return
	 * @throws URISyntaxException
	 */
	private File getSampleFile () throws URISyntaxException {

		String packagePath = getClass().getPackage().getName().replace('.', '/');

		String sampleFilePath = packagePath + "/sample.png";

		URL sampleURL = getClass().getClassLoader().getResource(sampleFilePath);

		File sampleImageFile = new File(sampleURL.toURI());

		return sampleImageFile;
	}

	/**
	 * Called when the heartbeat thread throws an uncaught Exception
	 *
	 * @param thread
	 * @param throwable
	 */
	public void uncaughtException(Thread thread, Throwable throwable) {

		logger.severe("Adapter heartbeat running in Thread " + thread.getName()
				+ " threw an uncaught exception.");

		throwable.printStackTrace(System.err);

		adapter.stop();

	}

}
