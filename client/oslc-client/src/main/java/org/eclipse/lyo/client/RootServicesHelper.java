/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: EPL-1.0 OR BSD-3-Clause
 */
package org.eclipse.lyo.client;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.StatusLine;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.DefaultRedirectStrategy;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.eclipse.lyo.client.exception.ResourceNotFoundException;
import org.eclipse.lyo.client.exception.RootServicesException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

/**
 * Helper class to assist in retrieval of attributes from the IBM Rational
 * Jazz rootservices document
 *
 * This class is not currently thread safe.
 */
public class RootServicesHelper {
	private String baseUrl;
	private String rootServicesUrl;
	private String catalogDomain;
	private String catalogNamespace;
	private String catalogProperty;
	private String catalogUrl;
	private Model rdfModel;

	//OAuth URLs
	String authorizationRealm;
	String requestTokenUrl;
	String authorizationTokenUrl;
	String accessTokenUrl;
	String requestConsumerKeyUrl;
	String consumerApprovalUrl;

	public static final String JFS_NAMESPACE = "http://jazz.net/xmlns/prod/jazz/jfs/1.0/";
	public static final String JD_NAMESPACE = "http://jazz.net/xmlns/prod/jazz/discovery/1.0/";

	private final static Logger logger = LoggerFactory.getLogger(RootServicesHelper.class);

	/**
	 * Initialize Jazz rootservices-related URLs such as the catalog location and OAuth URLs
	 *
	 * rootservices is unprotected and access does not require authentication
	 *
	 * @param url - base URL of the Jazz server, no including /rootservices.  Example:  https://example.com:9443/ccm
	 * @param catalogDomain - Namespace of the OSLC domain to find the catalog for.  Example:  OSLCConstants.OSLC_CM
	 * @throws RootServicesException
	 */
	public RootServicesHelper (String url, String catalogDomain, OslcClient client) throws RootServicesException {
		this.baseUrl = url;
		this.rootServicesUrl = UriBuilder.fromUri(this.baseUrl).path("rootservices").build().toString();
		logger.debug(String.format("Fetching rootservices document at URL <%s>", this.rootServicesUrl));
		this.catalogDomain = catalogDomain;
		logger.debug(String.format("Using catalog domain <%s>", this.catalogDomain));

		if (this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_CM) ||
		    this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_CM_V2)) {

			this.catalogNamespace = OSLCConstants.OSLC_CM;
			this.catalogProperty  = RootServicesConstants.CM_ROOTSERVICES_CATALOG_PROP;

		} else if (this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_QM) ||
			       this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_QM_V2)) {

			this.catalogNamespace = OSLCConstants.OSLC_QM;
			this.catalogProperty =  RootServicesConstants.QM_ROOTSERVICES_CATALOG_PROP;

		} else if (this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_RM) ||
			       this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_RM_V2)) {

			this.catalogNamespace = OSLCConstants.OSLC_RM;
			this.catalogProperty =  RootServicesConstants.RM_ROOTSERVICES_CATALOG_PROP;

		} else if (this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_AM_V2)) {

			this.catalogNamespace = OSLCConstants.OSLC_AM_V2;
			this.catalogProperty =  RootServicesConstants.AM_ROOTSERVICES_CATALOG_PROP;

		}
		else if (this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_AUTO)) {

			this.catalogNamespace = OSLCConstants.OSLC_AUTO;
			this.catalogProperty =  RootServicesConstants.AUTO_ROOTSERVICES_CATALOG_PROP;

		}
		else {
			logger.error("Jazz rootservices only supports CM, RM, QM, and Automation catalogs");
		}

		processRootServices(client);
	}

	/**
	 * Get the OSLC Catalog URL
	 *
	 * @return the catalog URL
	 */
	public String getCatalogUrl()
	{
		return catalogUrl;
	}

	private void processRootServices(OslcClient rootServicesClient) throws RootServicesException
	{
		try {
			Response response = rootServicesClient.getResource(rootServicesUrl,OSLCConstants.CT_RDF);
			InputStream is = response.readEntity(InputStream.class);
			rdfModel = ModelFactory.createDefaultModel();
			rdfModel.read(is,rootServicesUrl);
            is.close();

			//get the catalog URL
			this.catalogUrl = getRootServicesProperty(rdfModel, this.catalogNamespace, this.catalogProperty);

			//get the OAuth URLs
			this.requestTokenUrl = getRootServicesProperty(rdfModel, JFS_NAMESPACE, RootServicesConstants.OAUTH_REQUEST_TOKEN_URL);
			this.authorizationTokenUrl = getRootServicesProperty(rdfModel, JFS_NAMESPACE, RootServicesConstants.OAUTH_USER_AUTH_URL);
			this.accessTokenUrl = getRootServicesProperty(rdfModel, JFS_NAMESPACE, RootServicesConstants.OAUTH_ACCESS_TOKEN_URL);
			try { // Following field is optional, try to get it, if not found ignore exception because it will use the default
				this.authorizationRealm = getRootServicesProperty(rdfModel, JFS_NAMESPACE, RootServicesConstants.OAUTH_REALM_NAME);
			} catch (ResourceNotFoundException e) {
				logger.debug(String.format("OAuth authorization realm not found in rootservices <%s>", rootServicesUrl));
			}

			try {
				this.requestConsumerKeyUrl = getRootServicesProperty(rdfModel, JFS_NAMESPACE, RootServicesConstants.OAUTH_REQUEST_CONSUMER_KEY_URL);
			} catch (ResourceNotFoundException e) {
				logger.debug(String.format("OAuth request consumer key URL not found in rootservices <%s>", rootServicesUrl));
			}

			try {
				this.consumerApprovalUrl = getRootServicesProperty(rdfModel, JFS_NAMESPACE, RootServicesConstants.OAUTH_APPROVAL_MODULE_URL);
			} catch (ResourceNotFoundException e) {
				logger.debug(String.format("OAuth approval module URL not found in rootservices <%s>", rootServicesUrl));
			}
		} catch (Exception e) {
			throw new RootServicesException(this.baseUrl, e);
		}


    }

	private String getRootServicesProperty(Model rdfModel, String namespace, String predicate) throws ResourceNotFoundException {
		String returnVal = null;

		Property prop = rdfModel.createProperty(namespace, predicate);
		Statement stmt = rdfModel.getProperty((Resource) null, prop);
		if (stmt != null && stmt.getObject() != null)
			returnVal = stmt.getObject().toString();
		if (returnVal == null)
		{
			throw new ResourceNotFoundException(baseUrl, namespace + predicate);
		}
		return returnVal;
	}

	/**
	 * Returns the underlying RDF model for the rootservices document. This
	 * allows clients to read other service URLs not directly supported by this
	 * class.
	 *
	 * @return the RDF model
	 */
	public Model getRdfModel() {
		return rdfModel;
	}

	public String getAuthorizationRealm() {
	    return authorizationRealm;
    }

    public String getRequestTokenUrl() {
        return requestTokenUrl;
    }

    public String getAuthorizationTokenUrl() {
        return authorizationTokenUrl;
    }

    public String getAccessTokenUrl() {
        return accessTokenUrl;
    }

	/**
	 * Gets the URL for requesting an OAuth consumer key.
	 *
	 * @return the request consumer key URL
	 * @see <a href="https://jazz.net/wiki/bin/view/Main/RootServicesSpecAddendum2">RootServicesSpecAddendum2</a>
	 */
	public String getRequestConsumerKeyUrl() {
		return requestConsumerKeyUrl;
	}

	/**
	 * Gets the URL for approving an OAuth consumer
	 *
	 * @return the approval URL
	 * @see <a href="https://jazz.net/wiki/bin/view/Main/RootServicesSpecAddendum2">RootServicesSpecAddendum2</a>
	 */
	public String getConsumerApprovalUrl() {
		return consumerApprovalUrl;
	}

   public String getConsumerApprovalUrl(String consumerKey) {
       return UriBuilder.fromUri(consumerApprovalUrl)
               .queryParam("key", consumerKey)
               .build().toString();
    }

    public String requestConsumerKey(String consumerName, String consumerSecret) throws ClientProtocolException, IOException {
        String  postData = "{\"trusted\":true, \"secretType\":\"string\", \"name\":\"" + consumerName + "\", \"secret\":\"" + consumerSecret + "\"}";
        HttpResponse response = null;
        StringEntity postDataEntity = new StringEntity(postData);

        HttpClient client = HttpClientBuilder.create()
            .setRedirectStrategy(new DefaultRedirectStrategy()) // Lax strategy has problems with HTTPS upgrade
            .build();
        HttpPost request = new HttpPost(requestConsumerKeyUrl);
        request.addHeader("Content-Type", MediaType.APPLICATION_JSON);
        request.addHeader("Accept", MediaType.APPLICATION_JSON);
        request.setEntity(postDataEntity);
        response = client.execute(request);
        HttpEntity entity = response.getEntity();
        final StatusLine statusLine = response.getStatusLine();
        if (statusLine.getStatusCode() > 399) {
            throw new IllegalStateException(String.format("Server reported an error: %s %s",
                statusLine.getStatusCode(), statusLine.getReasonPhrase()));
        } else if (statusLine.getStatusCode() > 299) {
            final String newLocation = response.getFirstHeader("Location").getValue();
            if(requestConsumerKeyUrl.equals(newLocation)) {
                throw new IllegalStateException("Redirect loop detected while trying to request consumer key");
            }
            requestConsumerKeyUrl = newLocation; // TODO: 2020-11-19 refactor to an argument
            logger.debug("Following the redirect for consumer key to {}", requestConsumerKeyUrl);
            return requestConsumerKey(consumerName, consumerSecret);
        }
        InputStream content = entity.getContent();
        if (!response.getFirstHeader("Content-Type").getValue().toLowerCase().contains("json")) {
            // trying to be liberal with all possible JSON content types
            throw new IllegalStateException("Server returned something else than JSON in the response");
        }
        ObjectMapper mapper = new ObjectMapper();
        Map<String, Object> jsonData = new HashMap<String, Object>();
        jsonData = mapper.readValue(content, Map.class);
        String consumerKey = (String) jsonData.get("key");
        logger.debug("Consumer should redirect user to this approval URL, to approve the OAuth consumer: " + getConsumerApprovalUrl(consumerKey));
        return consumerKey;
    }

}
