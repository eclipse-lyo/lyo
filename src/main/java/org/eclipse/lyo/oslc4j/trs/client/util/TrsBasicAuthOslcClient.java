/*
 * Copyright (c) 2016-2018   KTH Royal Institute of Technology.
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
 * Omar Kacimi         -  Initial implementation
 * Andrew Berezovskyi  -  Lyo contribution updates
 */
package org.eclipse.lyo.oslc4j.trs.client.util;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;
import javax.ws.rs.core.Response;
import net.oauth.OAuthException;
import org.apache.commons.codec.binary.Base64;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.wink.client.ClientResponse;
import org.eclipse.lyo.client.oslc.jazz.JazzFormAuthClient;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.trs.client.exceptions.TrsEndpointConfigException;
import org.eclipse.lyo.oslc4j.trs.client.exceptions.TrsEndpointErrorExpection;
import org.eclipse.lyo.oslc4j.trs.client.exceptions.TrsEndpointException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The http client instance shared between all TRS Task handlers in the
 * consumer. Provides methods allowing retrieval OSLC resources from an OSLC
 * Provider using basic authentication and the return of a requested content
 * type from the server response. Also supports the return of an rdf Jena model
 * in case a model is requested
 *
 * @author Omar
 */
public class TrsBasicAuthOslcClient extends JazzFormAuthClient {

    private static Logger log = LoggerFactory.getLogger(TrsBasicAuthOslcClient.class);

    public TrsBasicAuthOslcClient() {
        super();
    }

    /**
     * call the getResourceUsingBaseAuth to retirve the response for the
     * requested url and return a jena rdf model or an object of the requested
     * class if possible
     *
     * @param url      requested resource's url
     * @param objClass requested content type
     * @param userName usename for basic authentication
     * @param pwd      password for basic authentication
     */
    public Object fetchResourceUsingBaseAuth(String url, Class<?> objClass, String userName,
            String pwd)
            throws IOException, OAuthException, URISyntaxException, TrsEndpointException {
        ClientResponse clResp = fetchResourceUsingBasicAuth(url, userName, pwd);

        final Response.Status.Family httpCodeType = clResp.getStatusType().getFamily();
        if (httpCodeType.equals(Response.Status.Family.CLIENT_ERROR)) {
            throw new TrsEndpointConfigException(clResp);
        } else if (httpCodeType.equals(Response.Status.Family.SERVER_ERROR)) {
            throw new TrsEndpointErrorExpection(clResp);
        }
        // TODO Andrew@2018-02-28: split into 2 methods: unmarshalling a resource and for a model
        if (AbstractResource.class.isAssignableFrom(objClass)) {
            Object objToRet;
            log.debug(
                    "Getting entity of type '{}' for server response for resource '{}'",
                    objClass.getName(),
                    url);
            objToRet = clResp.getEntity(objClass);
            log.debug(
                    "Getting entity of type '{}' for server response for resource '{}'",
                    objClass.getName(),
                    url);
            clResp.consumeContent();
            log.trace("Finished consuming content from server response");
            return objToRet;
        } else if (Model.class.isAssignableFrom(objClass)) {
            log.debug("Getting jena model for server response for resource: " + url);
            return extractModelFromResponse(url, clResp);
        }

        throw new IllegalStateException("The resources could not be fetched");
    }

    /**
     * using bastic authentication if possible retrieve the requested resource
     * from the given url and return the retrieved http response
     *
     * @param url      url of the requested resoruce
     * @param userName username for basic authentication if application
     * @param pwd      pwd for basic authentication if applicable
     *
     * @return the http response from the server
     */
    public ClientResponse fetchResourceUsingBasicAuth(String url, String userName, String pwd)
            throws OAuthException, IOException, URISyntaxException {
        log.debug("sending GET request to retrieve: " + url + " using basic credentials, user: "
                + userName);
        ClientResponse response = null;
        Map<String, String> requestHeaders = new HashMap<String, String>();
//        requestHeaders.put("Accept", "application/rdf+xml, application/xml;q=0.6, text/xml;
// q=0.6");

        if (userName != null && pwd != null && !userName.isEmpty() && !pwd.isEmpty()) {
            String authenticationHeaderVal = userName + ":" + pwd;
            String authentificationHeaderName = "Authorization";

            byte[] authenticationHeaderValEncodedBytes = Base64.encodeBase64(authenticationHeaderVal
                    .getBytes());

            String authenticationHeaderValEncoded = new String(authenticationHeaderValEncodedBytes);
            String fullAuthenticationHeaderVal = "Basic " + authenticationHeaderValEncoded;

            requestHeaders.put(authentificationHeaderName, fullAuthenticationHeaderVal);
        }

        response = super.getResource(url, requestHeaders);
        log.debug("result of GET request for: " + url + " retrieved");
        return response;
    }

    /**
     * Extract and return a Jena model from the response if possible
     *
     * @param clientResponse response object from which the rdf model is read
     *
     * @throws IOException thrown while reading from the response
     */
    public static Model extractModelFromResponse(String absoluteUrl,
            final ClientResponse clientResponse) throws IOException {

        if (clientResponse == null) {
            log.debug("The server response for url: " + absoluteUrl + " is null. Returning null");
            return null;
        }

        final String responseAsString = clientResponse.getEntity(String.class);
        log.trace("Response for {}:\n{}\n", absoluteUrl, responseAsString);

        if (responseAsString == null) {
            log.debug("The server response for url: " + absoluteUrl + " is null. Returning null");
            return null;
        }

        log.trace("Found entity of type String in the sever response for url: " + absoluteUrl + ". Creating a Jena Model and returning it.");

        final Model rdFModel = ModelFactory.createDefaultModel();

        try (InputStream is = new ByteArrayInputStream(responseAsString.getBytes())) {
            rdFModel.read(is, null);
        }

        log.trace("successful created Jena model from response for url: ." + absoluteUrl + " . "
                + "Returning the response");

        if (!rdFModel.isEmpty()) {
            log.debug("model extracted from response for url: ." + absoluteUrl + " has size: " +
                    rdFModel
                    .size() + " " + "statement");
        }

        return rdFModel;
    }

}
