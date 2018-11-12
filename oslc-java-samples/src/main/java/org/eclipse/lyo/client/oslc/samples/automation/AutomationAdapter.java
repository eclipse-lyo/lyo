/*******************************************************************************
 * Copyright (c) 2012, 2015 IBM Corporation.
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
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.Date;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;
import java.util.UUID;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import net.oauth.OAuthException;

import org.apache.commons.io.FileUtils;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.apache.http.client.ClientProtocolException;
import org.apache.wink.client.ClientResponse;
import org.apache.wink.common.model.multipart.BufferedOutMultiPart;
import org.apache.wink.common.model.multipart.OutPart;
import org.eclipse.lyo.client.exception.JazzAuthErrorException;
import org.eclipse.lyo.client.exception.JazzAuthFailedException;
import org.eclipse.lyo.client.exception.ResourceNotFoundException;
import org.eclipse.lyo.client.exception.RootServicesException;
import org.eclipse.lyo.client.oslc.OSLCConstants;
import org.eclipse.lyo.client.oslc.OslcClient;
import org.eclipse.lyo.client.oslc.jazz.JazzFormAuthClient;
import org.eclipse.lyo.client.oslc.jazz.JazzRootServicesHelper;
import org.eclipse.lyo.client.oslc.resources.AutomationConstants;
import org.eclipse.lyo.client.oslc.resources.AutomationPlan;
import org.eclipse.lyo.client.oslc.resources.AutomationRequest;
import org.eclipse.lyo.client.oslc.resources.AutomationResult;
import org.eclipse.lyo.client.oslc.resources.TestScript;
import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.core.model.ValueType;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.vocabulary.RDF;

@OslcResourceShape(title = "Automation Adapter Resource Shape", describes = IConstants.TYPE_AUTOMATION_ADAPTER)
@OslcNamespace(IConstants.NAMESPACE_URI_JAZZ_AUTO_RQM)
public class AutomationAdapter extends AbstractResource implements IConstants {

	// connection properties for the adapter. These are not part of the
	// AutomationAdapter resource properties
	public static final String PROPERTY_SERVER_URL = "serverUrl";
	public static final String PROPERTY_USERNAME = "username";
	public static final String PROPERTY_PASSWORD = "password";
	public static final String PROPERTY_PROJECT_AREA = "projectArea";

	// resource properties
	private final Set<String> capabilities = new TreeSet<String>();

	public static final String PROPERTY_TITLE = "title";
	public static final String PROPERTY_DESCRIPTION = "description";
	public static final String PROPERTY_TYPE = "type";
	public static final String PROPERTY_POLLING_INTERVAL = "pollingInterval";
	public static final String PROPERTY_CAPABIILTY = "capability";
	public static final String PROPERTY_ABOUT = "about";

	// connection properties
	private String serverUrl;
	private String username;
	private String password;
	private String projectArea;

	// properties provided to the server
	private String title;
	private String description;
	private String type;
	private String hostname;
	private String ipAddress;
	private Integer pollingInterval;
	private String macAddress;
	private String fullyQualifiedDomainName;

	// properties provided by the server
	private URI relation;
	private URI workAvailableUrl;
	private Boolean workAvailable;
	private String identifier;
	private Date modified;
	private URI creator;
	private URI instanceShape;
	private URI runsOnMachine;
	private URI serviceProvider;
	private URI assignedWorkUrl;

	// state of the adapter
	private boolean isStopped = false;

	private JazzFormAuthClient client = null;
	private JazzRootServicesHelper rootServicesHelper = null;

	private DocumentBuilder documentBuilder;

	public AutomationAdapter() {
		this(new Properties());
	}

	public AutomationAdapter(Properties properties) {

		try {
			DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
			dbf.setNamespaceAware(true);
			documentBuilder = dbf.newDocumentBuilder();
		} catch (ParserConfigurationException e) {
			throw new RuntimeException(e);
		}

		serverUrl = properties.getProperty(PROPERTY_SERVER_URL);
		username = properties.getProperty(PROPERTY_USERNAME);
		password = properties.getProperty(PROPERTY_PASSWORD);
		projectArea = properties.getProperty(PROPERTY_PROJECT_AREA);

		title = properties.getProperty(PROPERTY_TITLE);
		description = properties.getProperty(PROPERTY_DESCRIPTION);
		type = properties.getProperty(PROPERTY_TYPE);
		
		String pollingIntervalStr = properties
				.getProperty(PROPERTY_POLLING_INTERVAL);
		if (pollingIntervalStr != null) {
			pollingInterval = Integer.valueOf(pollingIntervalStr);
		} else {
			pollingInterval = 120;
		}

		String capabilities = properties.getProperty(PROPERTY_CAPABIILTY);
		if (capabilities != null) {
			setCapabilities(capabilities.split(","));
		}

		String aboutStr = properties.getProperty(PROPERTY_ABOUT);
		if (aboutStr != null) {
			setAbout(URI.create(aboutStr));
		}
		MachineInformation machine = new MachineInformation();
		hostname = machine.getHostname();
		ipAddress = machine.getIpAddress();
		macAddress = machine.getMacAddress();
		fullyQualifiedDomainName = machine.getFQDN();
	}

	public String getServerUrl() {
		return serverUrl;
	}

	public void setServerUrl(String serverUrl) {
		this.serverUrl = serverUrl;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getProjectArea() {
		return projectArea;
	}

	public void setProjectArea(String projectArea) {
		this.projectArea = projectArea;
	}

	public OslcClient getClient() {
		return client;
	}

	@OslcDescription("Descriptive text (reference: Dublin Core) about resource represented as rich text in XHTML content.")
	@OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "description")
	@OslcTitle("Description")
	@OslcValueType(ValueType.XMLLiteral)
	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	@OslcDescription("Title (reference: Dublin Core) or often a single line summary of the resource represented as rich text in XHTML content.")
	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "title")
	@OslcTitle("Title")
	@OslcValueType(ValueType.XMLLiteral)
	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	@OslcDescription("Type (reference: Dublin Core) of adapter.")
	@OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "type")
	@OslcTitle("Type")
	@OslcValueType(ValueType.String)
	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	@OslcPropertyDefinition(NAMESPACE_URI_JAZZ_AUTO_RQM + "hostname")
	@OslcTitle("Hostname")
	@OslcValueType(ValueType.String)
	public String getHostname() {
		return hostname;
	}

	public void setHostname(String hostname) {
		this.hostname = hostname;
	}

	@OslcPropertyDefinition(NAMESPACE_URI_JAZZ_AUTO_RQM + "ipAddress")
	@OslcTitle("IP Address")
	@OslcValueType(ValueType.String)
	public String getIpAddress() {
		return ipAddress;
	}

	public void setIpAddress(String ipAddress) {
		this.ipAddress = ipAddress;
	}

	@OslcPropertyDefinition(NAMESPACE_URI_JAZZ_AUTO_RQM + "pollingInterval")
	@OslcTitle("Polling Interval")
	@OslcValueType(ValueType.Integer)
	public Integer getPollingInterval() {
		return pollingInterval;
	}

	public void setPollingInterval(Integer pollingInterval) {
		this.pollingInterval = pollingInterval;
	}

	@OslcPropertyDefinition(NAMESPACE_URI_JAZZ_AUTO_RQM + "macAddress")
	@OslcTitle("MAC Address")
	@OslcValueType(ValueType.String)
	public String getMacAddress() {
		return macAddress;
	}

	public void setMacAddress(String macAddress) {
		this.macAddress = macAddress;
	}

	@OslcPropertyDefinition(NAMESPACE_URI_JAZZ_AUTO_RQM + "fullyQualifiedDomainName")
	@OslcTitle("Fully Qualified Domain Name")
	@OslcValueType(ValueType.String)
	public String getFullyQualifiedDomainName() {
		return fullyQualifiedDomainName;
	}

	public void setFullyQualifiedDomainName(String fullyQualifiedDomainName) {
		this.fullyQualifiedDomainName = fullyQualifiedDomainName;
	}

	@OslcDescription("Capability of the adapter like Execute, Record, Import.")
	@OslcName("capability")
	@OslcPropertyDefinition(NAMESPACE_URI_JAZZ_AUTO_RQM + "capability")
	@OslcTitle("Capability")
	@OslcOccurs(Occurs.OneOrMany)
	public String[] getCapabilities() {
		return capabilities.toArray(new String[capabilities.size()]);
	}

	public void addCapability(final String capability) {
		this.capabilities.add(capability);
	}

	public void setCapabilities(final String[] capabilities) {
		this.capabilities.clear();

		if (capabilities != null) {
			this.capabilities.addAll(Arrays.asList(capabilities));
		}
	}

	@OslcDescription("Relation (reference: Dublin Core) of adapter to other QM resources.")
	@OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "relation")
	@OslcTitle("Relation")
	@OslcValueType(ValueType.Resource)
	public URI getRelation() {
		return relation;
	}

	public void setRelation(URI relation) {
		this.relation = relation;
	}

	@OslcDescription("URL to poll for work assigned to the adapter.")
	@OslcPropertyDefinition(NAMESPACE_URI_JAZZ_AUTO_RQM + "workAvailableUrl")
	@OslcTitle("Work Available URL")
	@OslcValueType(ValueType.Resource)
	public URI getWorkAvailableUrl() {
		return workAvailableUrl;
	}

	public void setWorkAvailableUrl(URI workAvailableUrl) {
		this.workAvailableUrl = workAvailableUrl;
	}

	@OslcDescription("Boolean indicating whether work is available for the adapter.")
	@OslcPropertyDefinition(NAMESPACE_URI_JAZZ_AUTO_RQM + "workAvailable")
	@OslcTitle("Is Work Available")
	@OslcValueType(ValueType.Boolean)
	public Boolean getWorkAvailable() {
		return workAvailable;
	}

	public void setWorkAvailable(Boolean workAvailable) {
		this.workAvailable = workAvailable;
	}

	@OslcDescription("Identifier (reference: Dublin Core) for the adapter.")
	@OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "identifier")
	@OslcTitle("Identifier")
	@OslcValueType(ValueType.String)
	public String getIdentifier() {
		return identifier;
	}

	public void setIdentifier(String identifier) {
		this.identifier = identifier;
	}

	@OslcDescription("Last modification date (reference: Dublin Core) of the adapter.")
	@OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "modified")
	@OslcTitle("Modified")
	@OslcValueType(ValueType.DateTime)
	public Date getModified() {
		return modified;
	}

	public void setModified(Date modified) {
		this.modified = modified;
	}

	@OslcDescription("Creator (reference: Dublin Core) of the adapter.")
	@OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "creator")
	@OslcTitle("Creator")
	@OslcValueType(ValueType.Resource)
	public URI getCreator() {
		return creator;
	}

	public void setCreator(URI creator) {
		this.creator = creator;
	}

	@OslcDescription("Resource Shape that provides hints as to resource property value-types and allowed values. ")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "instanceShape")
	@OslcRange(OslcConstants.TYPE_RESOURCE_SHAPE)
	@OslcTitle("Instance Shape")
	public URI getInstanceShape() {
		return instanceShape;
	}

	public void setInstanceShape(URI instanceShape) {
		this.instanceShape = instanceShape;
	}

	@OslcDescription("URL for the machine that the adapter is running on.")
	@OslcPropertyDefinition(NAMESPACE_URI_JAZZ_AUTO_RQM + "runsOnMachine")
	@OslcTitle("Runs On Machine")
	@OslcValueType(ValueType.Resource)
	public URI getRunsOnMachine() {
		return runsOnMachine;
	}

	public void setRunsOnMachine(URI runsOnMachine) {
		this.runsOnMachine = runsOnMachine;
	}

	@OslcDescription("The scope of a resource is a URI for the resource's OSLC Service Provider.")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE
			+ "serviceProvider")
	@OslcRange(OslcConstants.TYPE_SERVICE_PROVIDER)
	@OslcTitle("Service Provider")
	public URI getServiceProvider() {
		return serviceProvider;
	}

	public void setServiceProvider(URI serviceProvider) {
		this.serviceProvider = serviceProvider;
	}

	@OslcDescription("URL for the work assigned to the adapter.")
	@OslcPropertyDefinition(NAMESPACE_URI_JAZZ_AUTO_RQM + "assignedWorkUrl")
	@OslcTitle("Work Available URL")
	@OslcValueType(ValueType.Resource)
	public URI getAssignedWorkUrl() {
		return assignedWorkUrl;
	}

	public void setAssignedWorkUrl(URI assignedWorkUrl) {
		this.assignedWorkUrl = assignedWorkUrl;
	}

	/**
	 * Start this adapter. It will begin polling the server for assigned
	 * Automation Requests based on the polling interval. When the server has an
	 * Automation Request for the adapter then control will be passed back to
	 * the caller of this method via the
	 * {@link IAutomationRequestHandler#handleAutomationRequest(AutomationRequest, AutomationAdapter)}
	 * interface. The AutomationResult returned by the IAutomationRequestHandler
	 * will be sent back to the Automation Service Provider, and polling will
	 * resume.
	 *
	 * Before calling this method the adapter needs to be logged in and
	 * registered with the Automation Service Provider.
	 *
	 * @param requestHandler
	 * @throws AutomationException
	 * @throws InterruptedException
	 * @throws IOException
	 * @throws OAuthException
	 * @throws URISyntaxException
	 * @throws ResourceNotFoundException
	 * @see {@link IAutomationRequestHandler}
	 * @see #login()
	 * @see #registerWithServiceProvider()
	 */
	public void start(IAutomationRequestHandler requestHandler)
			throws AutomationException, InterruptedException, IOException,
			URISyntaxException, OAuthException, ResourceNotFoundException {

		if (client == null) {

			throw new AutomationException(
					"Adapter is not logged into the Automation Service Provider.");

		}

		if (getAbout() == null) {

			throw new AutomationException(
					"Adapter is not registered with the Automation Service Provider.");

		}

		isStopped = false;

		// continue to poll the automation provider until the adapter is stopped
		while (true) {

			if (isStopped) {
				break;
			}

			String nextAssignmentUrl = getNextAssignmentUrl();

			if (nextAssignmentUrl != null) {

				AutomationRequest request = takeAssignment(nextAssignmentUrl);

				AutomationResult result = requestHandler
						.handleAutomationRequest(request, this);

				if (result != null) {

					populateResultFromRequest(request, result);

					saveResult(result, request);

					completeRequest(request);

				}

			}

			if (!isStopped) {
				Thread.sleep(1000L * pollingInterval);
			}
		}
	}

	/**
	 * Stop the adapter. Calling this method will cause the adapter to stop
	 * polling the Automation Service Provider and eventually return control of
	 * the execution thread back to the caller of this adapter's
	 * {@link #start(IAutomationRequestHandler)} method.
	 */
	public void stop() {
		isStopped = true;
	}

	/**
	 * Send the Automation Result to the Automation Service Provider
	 *
	 * @param result
	 * @param request
	 * @throws IOException
	 * @throws OAuthException
	 * @throws URISyntaxException
	 * @throws AutomationException
	 * @throws ResourceNotFoundException
	 */
	private void saveResult(AutomationResult result, AutomationRequest request) throws IOException,
			OAuthException, URISyntaxException, AutomationException,
			ResourceNotFoundException {

		ClientResponse response;
		String resultCreationFactoryUrl;

		synchronized (client) {
			Object resultLink = request.getExtendedProperties().get(PROPERTY_QM_PRODUCES_TEST_RESULT);
			if(resultLink != null){
				URI resultUri = (URI) resultLink;
				resultCreationFactoryUrl = resultUri.toString();
			}else{

				// Find the OSLC Service Provider for the project area we want to
				// work with
				String serviceProviderUrl = client.lookupServiceProviderUrl(
						rootServicesHelper.getCatalogUrl(), projectArea);
				
	
				resultCreationFactoryUrl = client.lookupCreationFactory(
						serviceProviderUrl, AutomationConstants.AUTOMATION_DOMAIN,
						AutomationConstants.TYPE_AUTOMATION_RESULT);
			}

			response = client.createResource(resultCreationFactoryUrl, result,
					OslcMediaType.APPLICATION_RDF_XML);

			response.consumeContent();

		}

		if (response.getStatusCode() != HttpStatus.SC_CREATED) {

			throw new AutomationException(
					"Failed to create an AutomationResult at "
							+ resultCreationFactoryUrl + ". "
							+ response.getStatusCode() + ": "
							+ response.getMessage());

		}

	}

	/**
	 * Set the Automation Request's properties as complete and update the
	 * Automation Service Provider
	 *
	 * @param request
	 * @throws URISyntaxException
	 * @throws AutomationException
	 * @throws OAuthException
	 * @throws IOException
	 */
	private void completeRequest(AutomationRequest request)
			throws AutomationException, IOException, OAuthException,
			URISyntaxException {

		assertNotCanceled(request);

		request.setStates(new URI[] { URI.create(
				AutomationConstants.STATE_COMPLETE) });

		request.getExtendedProperties().remove(PROPERTY_RQM_PROGRESS);

		request.getExtendedProperties().put(PROPERTY_RQM_PROGRESS,
				new Integer(100));

		URI updateUri = appendOslcProperties(request.getAbout(),
				"oslc_auto:state", "rqm_auto:progress");

		ClientResponse response;

		synchronized (client) {

			response = client.updateResource(updateUri.toString(), request,
					OslcMediaType.APPLICATION_RDF_XML);

			response.consumeContent();

		}

		if (response.getStatusCode() != HttpStatus.SC_OK) {

			throw new AutomationException(
					"Failed to update AutomationResult at "
							+ updateUri.toString() + ". "
							+ response.getStatusCode() + ": "
							+ response.getMessage());

		}
	}

	/**
	 * Check for any queued Automation Requests at the Automation Service
	 * Provider. If an AutomationRequest is queued then return its URL.
	 *
	 * @return
	 * @throws AutomationException
	 * @throws IOException
	 * @throws OAuthException
	 * @throws URISyntaxException
	 */
	private String getNextAssignmentUrl() throws AutomationException,
			IOException, OAuthException, URISyntaxException {

		if (assignedWorkUrl == null) {

			throw new AutomationException(
					"The assignedWorkUrl property must be set in order to poll the automation provider.");

		}

		Model model;

		synchronized (client) {

			ClientResponse response = client.getResource(
					assignedWorkUrl.toString(),
					OslcMediaType.APPLICATION_RDF_XML);

			InputStream is = response.getEntity(InputStream.class);

			model = ModelFactory.createDefaultModel();

			model.read(is, assignedWorkUrl.toString());

		}

		StmtIterator stmtIter = model.listStatements(null, RDF.type, model
				.createResource(AutomationConstants.TYPE_AUTOMATION_REQUEST));

		if (stmtIter.hasNext()) {
			return stmtIter.next().getSubject().getURI();
		}

		return null;

	}

	/**
	 * Notify the Automation Service Provider that the request has been taken by
	 * this adapter.
	 *
	 * @param requestUrl
	 * @return
	 * @throws IOException
	 * @throws OAuthException
	 * @throws URISyntaxException
	 * @throws AutomationException
	 */
	private AutomationRequest takeAssignment(String requestUrl)
			throws IOException, OAuthException, URISyntaxException,
			AutomationException {

		AutomationRequest request;
		URI updateUri;
		ClientResponse response;

		synchronized (client) {

			request = client.getResource(requestUrl,
					OslcMediaType.APPLICATION_RDF_XML).getEntity(
					AutomationRequest.class);

			request.getExtendedProperties().put(PROPERTY_RQM_TAKEN,
					Boolean.TRUE);

			request.setStates(new URI[] { URI.create(
					AutomationConstants.STATE_IN_PROGRESS) });

			updateUri = appendOslcProperties(URI.create(requestUrl),
					"oslc_auto:state", "rqm_auto:taken");

			response = client.updateResource(updateUri.toString(), request,
					OslcMediaType.APPLICATION_RDF_XML);

			response.consumeContent();

		}

		if (response.getStatusCode() != HttpStatus.SC_OK) {

			throw new AutomationException(
					"Failed to update AutomationRequest at "
							+ updateUri.toString() + ". "
							+ response.getStatusCode() + ": "
							+ response.getMessage());

		}

		return request;
	}

	/**
	 * Register with the Automation Service Provider if necessary. Before
	 * calling this method the adapter needs to be logged in to the Automation
	 * Service Provider.
	 *
	 * If the adapter's <code>about</code> property is not null then it is
	 * assumed that the adapter is already created and its properties will be
	 * refreshed from the service provider instead of recreating it again.
	 *
	 * @throws AutomationException
	 * @throws IOException
	 * @throws OAuthException
	 * @throws URISyntaxException
	 * @throws ResourceNotFoundException
	 * @see #login()
	 */
	public void register() throws AutomationException, IOException,
			OAuthException, URISyntaxException, ResourceNotFoundException {

		if (client == null) {

			throw new AutomationException(
					"Adapter is not logged into the Automation Service Provider.");

		}

		if (getAbout() == null) {

			String adapterCreationFactoryUrl;
			ClientResponse response;

			synchronized (client) {

				// Find the OSLC Service Provider for the project area we want
				// to work with
				String serviceProviderUrl = client.lookupServiceProviderUrl(
						rootServicesHelper.getCatalogUrl(), projectArea);

				adapterCreationFactoryUrl = client.lookupCreationFactory(
						serviceProviderUrl,
						AutomationConstants.AUTOMATION_DOMAIN,
						TYPE_AUTOMATION_ADAPTER);

				response = client.createResource(adapterCreationFactoryUrl,
						this, OslcMediaType.APPLICATION_RDF_XML);

				response.consumeContent();

			}

			if (response.getStatusCode() != HttpStatus.SC_CREATED) {

				throw new AutomationException(
						"Failed to register the adapter at "
								+ adapterCreationFactoryUrl + ". "
								+ response.getStatusCode() + ": "
								+ response.getMessage());

			}

			String adapterUrl = response.getHeaders().getFirst(
					HttpHeaders.LOCATION);

			setAbout(URI.create(adapterUrl));

		}

		reloadPropertiesFromServiceProvider();

	}

	/**
	 * Reload the adapter's properties from the service Provider. This ensures
	 * that the adapter has the properties it needs such as the URL to poll for
	 * assigned work. Before calling this method the adapter needs to be logged
	 * in and registered with the Automation Service Provider.
	 *
	 * @throws IOException
	 * @throws OAuthException
	 * @throws URISyntaxException
	 * @throws AutomationException
	 * @see #login()
	 */
	public void reloadPropertiesFromServiceProvider() throws IOException,
			OAuthException, URISyntaxException, AutomationException {

		if (client == null) {

			throw new AutomationException(
					"Adapter is not logged into the Automation Service Provider.");

		}

		if (getAbout() == null) {

			throw new AutomationException(
					"Adapter is not registered with the Automation Service Provider.");

		}

		AutomationAdapter registeredAdapter;

		synchronized (client) {

			ClientResponse response = client.getResource(getAbout().toString(),
					OslcMediaType.APPLICATION_RDF_XML);

			if (response.getStatusCode() != HttpStatus.SC_OK) {

				response.consumeContent();

				throw new AutomationException(
						"The Adapter has an 'about' property that it is not acknowledged by the Automation Service Provider. The Adapter may have been deleted.");
			}

			registeredAdapter = response.getEntity(AutomationAdapter.class);

		}

		// copy the server side properties into this local adapter instance
		copyServerProperties(registeredAdapter);

	}

	/**
	 * Unregister with the Automation Service Provider. Before calling this
	 * method the adapter needs to be logged in and registered with the
	 * Service Provider.
	 *
	 * @throws AutomationException
	 * @throws IOException
	 * @throws OAuthException
	 * @throws URISyntaxException
	 * @see #login()
	 */
	public void unregister() throws AutomationException, IOException,
			OAuthException, URISyntaxException {

		if (client == null) {

			throw new AutomationException(
					"Adapter is not logged into the Automation Service Provider.");

		}

		if (getAbout() == null) {

			throw new AutomationException(
					"Adapter is not registered with the Automation Service Provider.");

		}

		ClientResponse response;

		synchronized (client) {

			response = client.deleteResource(getAbout().toString());

			response.consumeContent();

		}

		int statusCode = response.getStatusCode();

		if ((statusCode != HttpStatus.SC_OK)
				&& (statusCode != HttpStatus.SC_NOT_FOUND)) {

			throw new AutomationException(
					"Failed to unregister the adapter at "
							+ getAbout().toString() + ". "
							+ response.getStatusCode() + ": "
							+ response.getMessage());

		}

		setAbout(null);
	}

	/**
	 * Copy the server-side properties from a registered adapter into this
	 * adapter
	 *
	 * @param registeredAdapter
	 */
	private void copyServerProperties(AutomationAdapter registeredAdapter) {
		setAbout(registeredAdapter.getAbout());
		setRelation(registeredAdapter.getRelation());
		setWorkAvailableUrl(registeredAdapter.getWorkAvailableUrl());
		setWorkAvailable(registeredAdapter.getWorkAvailable());
		setIdentifier(registeredAdapter.getIdentifier());
		setModified(registeredAdapter.getModified());
		setCreator(registeredAdapter.getCreator());
		setInstanceShape(registeredAdapter.getInstanceShape());
		setRunsOnMachine(registeredAdapter.getRunsOnMachine());
		setServiceProvider(registeredAdapter.getServiceProvider());
		setAssignedWorkUrl(registeredAdapter.getAssignedWorkUrl());
	}

	/**
	 * Login to the Automation Service Provider with the username, password, and
	 * project area currently set on this adapter
	 *
	 * @throws RootServicesException
	 * @throws JazzAuthErrorException
	 * @throws JazzAuthFailedException
	 * @throws IOException 
	 * @throws ClientProtocolException 
	 */
	public void login() throws RootServicesException, JazzAuthFailedException,
			JazzAuthErrorException, ClientProtocolException, IOException {

		login(getServerUrl(), getUsername(), getPassword(), getProjectArea());

	}

	/**
	 * Login to the Automation Service Provider
	 *
	 * @param serverUrl
	 * @param username
	 * @param password
	 * @param projectArea
	 * @throws RootServicesException
	 * @throws JazzAuthErrorException
	 * @throws JazzAuthFailedException
	 * @throws IOException 
	 * @throws ClientProtocolException 
	 */
	public void login(String serverUrl, String username, String password,
			String projectArea) throws RootServicesException,
			JazzAuthFailedException, JazzAuthErrorException, ClientProtocolException, IOException {

		assert serverUrl != null;
		assert username != null;
		assert projectArea != null;

		// Initialize a Jazz rootservices helper and indicate we're looking for
		// the Automation catalog
		rootServicesHelper = new JazzRootServicesHelper(serverUrl,
				OSLCConstants.OSLC_AUTO);

		// Create a new Form Auth client with the supplied user/password
		client = rootServicesHelper.initFormClient(username, password);

		synchronized (client) {

			client.login();

		}

	}

	/**
	 * Logout of the Automation Service Provider
	 *
	 * <strong>NOT IMPLEMENTED</strong>
	 */
	public void logout() {

		if (client != null) {
			// TODO implement when logout support is added to OSLC client
			// client.logout();
		}

		client = null;

	}

	/**
	 * Get the Test Script document associated with the Automation Request's
	 * oslc_qm:executesTestScript property.
	 *
	 * Before calling this method the adapter must be logged into the server.
	 *
	 * @param request
	 * @return
	 * @throws IOException
	 * @throws OAuthException
	 * @throws URISyntaxException
	 * @throws AutomationException
	 * @throws SAXException
	 */
	public Document getScriptDocument(AutomationRequest request)
			throws IOException, OAuthException, URISyntaxException,
			AutomationException, SAXException {

		if (client == null) {

			throw new AutomationException(
					"The adapter has not logged into the server.");

		}

		assertNotCanceled(request);

		URI scriptURI = (URI) request.getExtendedProperties().get(
				PROPERTY_QM_EXECUTES_TEST_SCRIPT);

		if (scriptURI == null) {

			throw new AutomationException(
					"The AutomationRequest does not have a value for the property : "
							+ PROPERTY_QM_EXECUTES_TEST_SCRIPT);

		}

		scriptURI = appendOslcProperties(scriptURI, "dcterms:relation");

		InputStream is;

		synchronized (client) {

			TestScript script = client.getResource(scriptURI.toString(),
					OslcMediaType.APPLICATION_RDF_XML).getEntity(
					TestScript.class);

			URI scriptUri = (URI) script.getExtendedProperties().get(
					PROPERTY_DC_RELATION);

			is = client.getResource(scriptUri.toString(),
					OslcMediaType.APPLICATION_XML).getEntity(InputStream.class);

		}

		Document document = documentBuilder.parse(is);

		return document;
	}

	/**
	 * Utility method to add the oslc.properties request parameter onto a URI
	 *
	 * @param uri
	 * @param properties
	 * @return
	 */
	private URI appendOslcProperties(URI uri, String... properties) {

		String str = uri.toString();

		if (str.indexOf("?") < 0) {
			str += "?";
		} else {
			str += "&";
		}

		str += "oslc.properties=";

		boolean first = true;
		for (String property : properties) {
			if (!first) {
				str += ",";
			}
			str += property;
			first = false;
		}

		return URI.create(str);

	}

	/**
	 * Populate the AutomationResult's properties from the Automation Request
	 *
	 * @param request
	 * @param result
	 * @throws AutomationException
	 * @throws URISyntaxException
	 * @throws OAuthException
	 * @throws IOException
	 */
	private void populateResultFromRequest(AutomationRequest request,
			AutomationResult result) throws AutomationException, IOException,
			OAuthException, URISyntaxException {



		result.setProducedByAutomationRequest(new Link(request.getAbout()));

		Link automationPlan = request.getExecutesAutomationPlan();

		result.setInputParameters(request.getInputParameters());

		Map<QName, Object> requestExtProperties = request
				.getExtendedProperties();

		Map<QName, Object> resultExtProperties = result.getExtendedProperties();

		resultExtProperties.put(PROPERTY_QM_EXECUTES_TEST_SCRIPT,
				requestExtProperties.get(PROPERTY_QM_EXECUTES_TEST_SCRIPT));

		if (automationPlan != null && automationPlan.getValue() != null) {

			result.setReportsOnAutomationPlan(request
					.getExecutesAutomationPlan());

			AutomationPlan plan;

			synchronized (client) {

				plan = client.getResource(automationPlan.getValue().toString(),
						OslcMediaType.APPLICATION_RDF_XML).getEntity(
						AutomationPlan.class);

			}

			URI testcase = (URI) plan.getExtendedProperties().get(
					PROPERTY_QM_RUNS_TEST_CASE);

			result.getExtendedProperties().put(
					PROPERTY_QM_REPORTS_ON_TEST_CASE, testcase);
		}

	}

	/**
	 * Send a heartbeat to the AutomationProvider to let it know that the
	 * adapter is still available for work.
	 *
	 * @throws URISyntaxException
	 * @throws AutomationException
	 */
	private void sendHeartbeat() throws URISyntaxException, AutomationException {

		if (client == null) {

			throw new AutomationException(
					"Adapter is not logged into the Automation Service Provider.");

		}

		if (getAbout() == null) {

			throw new AutomationException(
					"Adapter is not registered with the Automation Service Provider");

		}

		URI updateUrl = appendOslcProperties(getAbout(),
				"rqm_auto:pollingInterval");

		ClientResponse response;

		synchronized (client) {

			response = client.updateResource(updateUrl.toString(), this,
					OslcMediaType.APPLICATION_RDF_XML);

			response.consumeContent();

		}

		if (response.getStatusCode() != HttpStatus.SC_OK) {

			throw new AutomationException(
					"Failed to update adapter heartbeat at "
							+ updateUrl.toString() + ". "
							+ response.getStatusCode() + ": "
							+ response.getMessage());

		}

	}

	/**
	 * Upload a File to the Automation Service Provider using the
	 * rqm_auto:uploadAttachmentUrl property in the Automation Request.
	 *
	 * Before calling this method the adapter needs to be logged into the
	 * Automation Service Provider.
	 *
	 * @param file
	 * @param request
	 * @return
	 * @throws AutomationException
	 * @throws URISyntaxException
	 * @throws IOException
	 * @throws OAuthException
	 */
	public URI uploadAttachment(File file, AutomationRequest request)
			throws AutomationException, URISyntaxException, IOException,
			OAuthException {

		if (client == null) {

			throw new AutomationException(
					"The adapter has not logged into the server.");

		}

		assertNotCanceled(request);

		URI attachmentUploadUrl = (URI) request.getExtendedProperties().get(
				PROPERTY_RQM_UPLOAD_ATTACHMENT_URL);

		String fileName = file.getName();

		byte[] bytes = FileUtils.readFileToByteArray(file);

		OutPart outPart = new OutPart();
		outPart.setContentType("application/octet-stream; name=" + fileName);
		outPart.addHeader("Content-Transfer-Encoding", "binary");
		outPart.addHeader("Content-Disposition", "form-data; name=\""
				+ fileName + "\"; filename=\"" + fileName + "\"");
		outPart.addHeader("Content-Length", String.valueOf(bytes.length));
		outPart.setBody(bytes);

		String boundary = "---------------------------"
				+ UUID.randomUUID().toString();

		BufferedOutMultiPart requestEntity = new BufferedOutMultiPart();
		requestEntity.addPart(outPart);
		requestEntity.setBoundary(boundary);

		ClientResponse response;

		synchronized (client) {

			response = client.createResource(attachmentUploadUrl.toString(),
					requestEntity, "multipart/form-data; boundary=" + boundary);

			response.consumeContent();

		}

		if (response.getStatusCode() != HttpStatus.SC_CREATED) {

			throw new AutomationException("Failed to upload attachment at "
					+ attachmentUploadUrl.toString() + ". "
					+ response.getStatusCode() + ": " + response.getMessage());

		}

		String location = response.getHeaders().getFirst(HttpHeaders.LOCATION);

		return URI.create(location);

	}

	/**
	 * Send the current progress (percentage) of the Automation Request. If the
	 * provided value is greater than 99 then it is adjusted down to 99 since
	 * the progress should not reach 100 until the
	 * {@link IAutomationRequestHandler} has finished processing the request and
	 * returned control back to this Adapter.
	 *
	 * Before calling this method the adapter needs to be logged in and
	 * registered with the automation service provider.
	 *
	 * @param i
	 *            An integer less than 100
	 * @param request
	 * @throws URISyntaxException
	 * @throws AutomationException
	 * @throws OAuthException
	 * @throws IOException
	 */
	public void sendProgressForRequest(int i, AutomationRequest request)
			throws URISyntaxException, AutomationException, IOException,
			OAuthException {

		if (client == null) {

			throw new AutomationException(
					"Adapter is not logged into the Automation Service Provider.");

		}

		if (getAbout() == null) {

			throw new AutomationException(
					"Adapter is not registered with the Automation Service Provider");

		}

		assertNotCanceled(request);

		// don't allow progress to surpass 99 until the request has completed
		if (i > 99) {
			i = 99;
		}

		AutomationRequest requestCopy = new AutomationRequest(request.getAbout());

		requestCopy.getExtendedProperties().put(PROPERTY_RQM_PROGRESS,
				new Integer(i));

		URI updateUri = appendOslcProperties(requestCopy.getAbout(),
				"rqm_auto:progress");

		ClientResponse response;

		synchronized (client) {

			response = client.updateResource(updateUri.toString(), requestCopy,
					OslcMediaType.APPLICATION_RDF_XML);

			response.consumeContent();

		}

		if (response.getStatusCode() != HttpStatus.SC_OK) {

			throw new AutomationException(
					"Failed to update AutomationResult at "
							+ updateUri.toString() + ". "
							+ response.getStatusCode() + ": "
							+ response.getMessage());

		}

	}

	/**
	 * Send a status for the Automation Request. Before calling this method the
	 * adapter needs to be logged in and registered with the automation service
	 * provider.
	 *
	 * @param statusResponse
	 * @param request
	 * @throws URISyntaxException
	 * @throws AutomationException
	 * @throws OAuthException
	 * @throws IOException
	 */
	public void sendStatusForRequest(StatusResponse statusResponse,
			AutomationRequest request) throws AutomationException, IOException,
			OAuthException, URISyntaxException {

		if (client == null) {

			throw new AutomationException(
					"Adapter is not logged into the Automation Service Provider.");

		}

		if (getAbout() == null) {

			throw new AutomationException(
					"Adapter is not registered with the Automation Service Provider");

		}

		assertNotCanceled(request);

		AutomationRequest requestCopy = new AutomationRequest(request.getAbout());

		requestCopy.getExtendedProperties().put(PROPERTY_RQM_STATUS_RESPONSE,
				statusResponse);

		URI updateUri = appendOslcProperties(requestCopy.getAbout(),
				"rqm_auto:statusResponse");

		ClientResponse response;

		synchronized (client) {

			response = client.updateResource(updateUri.toString(), requestCopy,
					OslcMediaType.APPLICATION_RDF_XML);

			response.consumeContent();

		}

		if (response.getStatusCode() != HttpStatus.SC_OK) {

			throw new AutomationException(
					"Failed to update AutomationResult at "
							+ updateUri.toString() + ". "
							+ response.getStatusCode() + ": "
							+ response.getMessage());

		}

	}

	/**
	 * Send a message update to the Automation Request. Before calling this
	 * method the adapter needs to be logged in and registered with the
	 * automation service provider.
	 *
	 * @param message
	 * @param request
	 * @throws URISyntaxException
	 * @throws AutomationException
	 * @throws OAuthException
	 * @throws IOException
	 */
	public void sendMessageForRequest(Message message, AutomationRequest request)
			throws AutomationException, IOException, OAuthException,
			URISyntaxException {

		if (client == null) {

			throw new AutomationException(
					"Adapter is not logged into the Automation Service Provider.");

		}

		if (getAbout() == null) {

			throw new AutomationException(
					"Adapter is not registered with the Automation Service Provider");

		}

		assertNotCanceled(request);

		AutomationRequest requestCopy = new AutomationRequest(request.getAbout());

		requestCopy.getExtendedProperties().put(PROPERTY_RQM_MESSAGE,
				message);

		URI updateUri = appendOslcProperties(requestCopy.getAbout(),
				"rqm_auto:message");

		ClientResponse response;

		synchronized (client) {

			response = client.updateResource(updateUri.toString(), requestCopy,
					OslcMediaType.APPLICATION_RDF_XML);

			response.consumeContent();

		}

		if (response.getStatusCode() != HttpStatus.SC_OK) {

			throw new AutomationException(
					"Failed to update AutomationResult at "
							+ updateUri.toString() + ". "
							+ response.getStatusCode() + ": "
							+ response.getMessage());

		}

	}


	/**
	 * Cancel an Automation Request by updating its desiredState property to
	 * Canceled.
	 *
	 * Before calling this method the adapter needs to be logged into the
	 * Automation Service Provider.
	 *
	 * @param request
	 * @throws AutomationException
	 * @see http://open-services.net/wiki/automation/OSLC-Automation-Specification-Version-2.0/#Canceling-the-execution-of-an-automation
	 */
	public void cancel(AutomationRequest request) throws AutomationException {

		if (client == null) {

			throw new AutomationException(
					"The adapter has not logged into the server.");

		}

		request.setDesiredState(URI.create(AutomationConstants.STATE_CANCELED));

		// Some automation providers require the client to set the state to canceled
		request.setStates(new URI[] { URI
				.create(AutomationConstants.STATE_CANCELED) });

		URI updateUri = appendOslcProperties(request.getAbout(),
				"oslc_auto:state");

		ClientResponse response;

		synchronized (client) {

			response = client.updateResource(updateUri.toString(), request,
					OslcMediaType.APPLICATION_RDF_XML);

			response.consumeContent();

		}

		if (response.getStatusCode() != HttpStatus.SC_OK) {

			throw new AutomationException(
					"Failed to update AutomationResult at "
							+ updateUri.toString() + ". "
							+ response.getStatusCode() + ": "
							+ response.getMessage());

		}
	}

	/**
	 * Checks to see if the AutomationRequest's current state at the Service
	 * Provider indicates that the request has been canceled.
	 *
	 * @param request
	 * @throws AutomationRequestCanceledException
	 *   if the Request has been canceled.
	 * @throws IOException
	 * @throws OAuthException
	 * @throws URISyntaxException
	 */
	private void assertNotCanceled(AutomationRequest request)
			throws AutomationRequestCanceledException, IOException,
			OAuthException, URISyntaxException {

		URI selectUri = appendOslcProperties(request.getAbout(),
				"oslc_auto:state");

		AutomationRequest requestAtServiceProvider;

		synchronized (client) {

			requestAtServiceProvider = client.getResource(
					selectUri.toString(), OslcMediaType.APPLICATION_RDF_XML)
					.getEntity(AutomationRequest.class);

		}

		// oslc_auto:state is defined as one-or-many in the specification
		URI stateUri = requestAtServiceProvider.getStates()[0];

		if (URI.create(AutomationConstants.STATE_CANCELED).equals(stateUri)) {

			throw new AutomationRequestCanceledException(request);

		}

	}

	/**
	 * A runnable which can be used by a Thread created by the program
	 * controlling the adapter.
	 *
	 * The runnable will send heart beats to the Automation Service Provider at
	 * the polling interval.
	 *
	 * Stopping the adapter will also stop this runnable.
	 *
	 * @see AutomationAdapter#stop()
	 *
	 */
	public class HeartbeatRunnable implements Runnable {

		public HeartbeatRunnable() {
		}

		public void run() {
			try {
				while (true) {

					if (isStopped)
						break;

					sendHeartbeat();

					Thread.sleep(1000L * pollingInterval);

				}
			} catch (Exception e) {

				throw new RuntimeException(e);

			}
		}
	}

}
