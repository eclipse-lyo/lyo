/**
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
package org.eclipse.lyo.oslc4j.trs.client.handlers;

import java.io.IOException;
import java.net.URISyntaxException;
import javax.ws.rs.core.Response;
import org.eclipse.lyo.oslc4j.client.OslcClient;
import org.eclipse.lyo.oslc4j.trs.client.exceptions.TrsEndpointConfigException;
import org.eclipse.lyo.oslc4j.trs.client.exceptions.TrsEndpointErrorExpection;
import org.eclipse.lyo.oslc4j.trs.client.util.ClientUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A generic class containing the information necessary for any thread class to
 * do TRS information for example to send sparql queries to the triplestore, to
 * communicate with a TRS provider etc..
 *
 * @author Omar
 *
 */
public abstract class TRSTaskHandler implements Runnable {

    final static Logger log = LoggerFactory.getLogger(TRSTaskHandler.class);
    /**
     * instance of the http client used by this TRS Task handler to communicate
     * with the TRS providers
     */
    protected OslcClient oslcClient;
    /**
     * http sparql endpoints of the triplestore used to store the data
     */
    protected    String sparqlUpdateService;
    protected    String sparqlQueryService;
    /**
     * http sparql endpoints of the triplestore used to store the data and to
     * query it by a task handler
     */
    protected    String sparql_baseAuth_userName;
    protected    String sparql_baseAuth_pwd;
    /**
     * http sparql endpoints of the triplestore used to store the data
     */
    protected    String baseAuth_userName;
    protected    String baseAuth_pwd;
    /**
     * this is used for logging purposes. Whenever the run method of the
     * Runnable is executed a check is done to see whether the name of the
     * current runnable is the same as this variable in order to know which bit
     * of code is executed in which thread in the logging output
     */
    protected    String threadName;

    /**
     * retrieve the distant resource using basic authentication if necessary
     * from the given url param and try and return the requested content type
     *
     * @param url
     *            the requestes resource url
     * @param objClass
     *            the required content type
     * @return an instance of the required content type
     * @throws IOException
     */
    protected Object fetchTRSRemoteResource(String url, Class<?> objClass)
            throws IOException, URISyntaxException {
        final Response response = oslcClient.getResource(url);
        final Object resource;
        try {
            resource = ClientUtil.extractResourceFromResponse(response, objClass);
            response.close();
            return resource;
        } catch (TrsEndpointConfigException e) {
            log.error("TRS Provider configuration is wrong: ", e);
        } catch (TrsEndpointErrorExpection e) {
            log.warn("The TRS endpoint {} is unreachable", url);
            log.debug("TRS endpoint exception", e);
        }
        // TODO Andrew@2019-04-18: convert return type into Optional<Object>
        throw new IllegalStateException();
    }

    public TRSTaskHandler(OslcClient oslcClient, String sparqlUpdateService, String sparqlQueryService,
            String sparql_baseAuth_userName, String sparql_baseAuth_pwd, String baseAuth_userName,
            String baseAuth_pwd) {
        super();
        this.oslcClient = oslcClient;
        this.sparqlUpdateService = sparqlUpdateService;
        this.sparqlQueryService = sparqlQueryService;
        this.sparql_baseAuth_userName = sparql_baseAuth_userName;
        this.sparql_baseAuth_pwd = sparql_baseAuth_pwd;
        this.baseAuth_userName = baseAuth_userName;
        this.baseAuth_pwd = baseAuth_pwd;
    }

    /**
     * Method to be overridden by the implementing class providing the task
     * behaviour
     */
    protected abstract void processTRSTask();

    @Override
    public void run() {
        processTRSTask();
    }
}
