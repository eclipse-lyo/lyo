/*-
 * Copyright (c) 2016-2017   KTH Royal Institute of Technology.
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
package org.eclipse.lyo.oslc4j.trs.client.httpclient;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;
import net.oauth.OAuthException;
import org.apache.commons.codec.binary.Base64;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.wink.client.ClientResponse;
import org.eclipse.lyo.client.oslc.jazz.JazzFormAuthClient;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
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
 *
 */
public class TRSHttpClient extends JazzFormAuthClient {

    static Logger logger = LoggerFactory.getLogger(TRSHttpClient.class);

    public TRSHttpClient() {
        super();
    }

    /**
     * call the getResourceUsingBaseAuth to retirve the response for the
     * requested url and return a jena rdf model or an object of the requested
     * class if possible
     *
     * @param url
     *            requested resource's url
     * @param objClass
     *            requested content type
     * @param userName
     *            usename for basic authentication
     * @param pwd
     *            password for basic authentication
     * @return
     * @throws IOException
     * @throws OAuthException
     * @throws URISyntaxException
     */
    public Object fetchResourceUsingBaseAuth(String url, Class<?> objClass, String userName, String pwd)
            throws IOException, OAuthException, URISyntaxException {
        ClientResponse clResp = fetchResourceUsingBasicAuth(url, userName, pwd);

        if (AbstractResource.class.isAssignableFrom(objClass)) {
            Object objToRet = null;
            logger.debug("getting entity of type: " + objClass.getName() + " for server reponse for resource: " + url);
            objToRet = clResp.getEntity(objClass);
            logger.debug("finished getting entity of type: " + objClass.getName() + " for server reponse for resource: "
                    + url);
            clResp.consumeContent();
            logger.debug("finished consuming content from server response");
            return objToRet;
        }

        else if (Model.class.isAssignableFrom(objClass)) {
            logger.debug("getting jena model for server reponse for resource: " + url);
            return extractModelFromResponse(url, clResp);
        }

        return null;
    }

    /**
     * using bastic authentication if possible retrieve the requested resource
     * from the given url and return the retrieved http response
     *
     * @param url
     *            url of the requested resoruce
     * @param userName
     *            username for basic authentication if application
     * @param pwd
     *            pwd for basic authentication if applicable
     * @return the http response from the server
     */
    public ClientResponse fetchResourceUsingBasicAuth(String url, String userName, String pwd)
            throws IOException, OAuthException, URISyntaxException {
        logger.debug("sending GET request to retrieve: " + url + " using basic credentials, user: " + userName);
        ClientResponse response = null;
        Map<String, String> requestHeaders = new HashMap<String, String>();
//        requestHeaders.put("Accept", "application/rdf+xml, application/xml;q=0.6, text/xml;q=0.6");

        if (userName != null && pwd != null && !userName.isEmpty() && !pwd.isEmpty()) {
            String authenticationHeaderVal = userName + ":" + pwd;
            String authentificationHeaderName = "Authorization";

            byte[] authenticationHeaderValEncodedBytes = Base64.encodeBase64(authenticationHeaderVal.getBytes());

            String authenticationHeaderValEncoded = new String(authenticationHeaderValEncodedBytes);
            String fullAuthenticationHeaderVal = "Basic " + authenticationHeaderValEncoded;



            requestHeaders.put(authentificationHeaderName, fullAuthenticationHeaderVal);
        }

        response = super.getResource(url, requestHeaders);
        logger.debug("result of GET request for: " + url + " retrieved");
        return response;
    }

    /**
     * Extract and return a Jena model from the response if possible
     *
     * @param clientResponse
     *            response object from which the rdf model is read
     * @return
     * @throws IOException
     *             thrown while reading from the response
     */
    public static Model extractModelFromResponse(String absoluteUrl, final ClientResponse clientResponse)
            throws IOException {

        if (clientResponse == null) {
            logger.debug("The server response for url: " + absoluteUrl + " is null. Returning null");
            return null;
        }

        final String responseAsString = clientResponse.getEntity(String.class);

        if (responseAsString == null) {
            logger.debug("The server response for url: " + absoluteUrl + " is null. Returning null");
            return null;
        }

        logger.debug("Found entity of type String in the sever response for url: " + absoluteUrl
                + ". Creating a Jena Model and returning it.");

        final Model rdFModel = ModelFactory.createDefaultModel();

        try (java.io.InputStream is = new java.io.ByteArrayInputStream(
                responseAsString.getBytes())) {
            rdFModel.read(is, null);
        } catch (Exception e) {
            logger.error(
                    "Error while retrieving the Jena model from response for url: ." + absoluteUrl,
                    e
            );
            clientResponse.consumeContent();
            return null;
        }

        logger.debug(
                "successful created Jena model from response for url: ." + absoluteUrl + " . Returning the response");

        if (rdFModel != null && !rdFModel.isEmpty()) {
            logger.debug("model extracted from response for url: ." + absoluteUrl + " has size: " + rdFModel.size()
            + " statement");
        }

        return rdFModel;
    }

}
