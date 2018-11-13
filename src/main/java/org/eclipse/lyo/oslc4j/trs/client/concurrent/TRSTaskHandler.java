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
package org.eclipse.lyo.oslc4j.trs.client.concurrent;

import java.io.IOException;
import java.net.URISyntaxException;
import net.oauth.OAuthException;
import org.apache.log4j.Logger;
import org.eclipse.lyo.oslc4j.trs.client.httpclient.TRSHttpClient;

/**
 * A generic class containing the information necessary for any thread class to
 * do TRS information for example to send sparql queries to the triplestore, to
 * communicate with a TRS provider etc..
 *
 * @author Omar
 *
 */
public abstract class TRSTaskHandler implements Runnable {

    final static Logger logger = Logger.getLogger(TRSTaskHandler.class);
    /**
     * instance of the http client used by this TRS Task handler to communicate
     * with the TRS providers
     */
    protected TRSHttpClient oslcClient;
    /**
     * http sparql endpoints of the triplestore used to store the data
     */
    protected String sparqlUpdateService;
    protected String sparqlQueryService;
    /**
     * http sparql endpoints of the triplestore used to store the data and to
     * query it by a task handler
     */
    protected String sparql_baseAuth_userName;
    protected String sparql_baseAuth_pwd;
    /**
     * http sparql endpoints of the triplestore used to store the data
     */
    protected String baseAuth_userName;
    protected String baseAuth_pwd;
    /**
     * this is used for logging purposes. Whenever the run method of the
     * Runnable is executed a check is done to see whether the name of the
     * current runnable is the same as this variable in order to know which bit
     * of code is executed in which thread in the logging output
     */
    protected String threadName;

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
     * @throws OAuthException
     * @throws URISyntaxException
     */
    protected Object fetchTRSRemoteResource(String url, Class<?> objClass)
            throws IOException, OAuthException, URISyntaxException {
        return oslcClient.fetchResourceUsingBaseAuth(url, objClass, baseAuth_userName, baseAuth_pwd);
    }

    public TRSTaskHandler(TRSHttpClient oslcClient, String sparqlQueryService, String sparqlUpdateService,
            String baseAuth_userName, String baseAuth_pwd) {
        super();
        this.oslcClient = oslcClient;
        this.sparqlUpdateService = sparqlUpdateService;
        this.sparqlQueryService = sparqlQueryService;
        this.baseAuth_userName = baseAuth_userName;
        this.baseAuth_pwd = baseAuth_pwd;
    }

    public TRSTaskHandler(TRSHttpClient oslcClient, String sparqlUpdateService, String sparqlQueryService,
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
    protected void processTRSTask() {
        Thread currentThread = Thread.currentThread();
        if (!currentThread.getName().equals(threadName)) {
            currentThread.setName(threadName);
        }
    }

    @Override
    public void run() {
        processTRSTask();
    }

    public String getSparql_baseAuth_userName() {
        return sparql_baseAuth_userName;
    }

    public void setSparql_baseAuth_userName(String sparql_baseAuth_userName) {
        this.sparql_baseAuth_userName = sparql_baseAuth_userName;
    }

    public String getSparql_baseAuth_pwd() {
        return sparql_baseAuth_pwd;
    }

    public void setSparql_baseAuth_pwd(String sparql_baseAuth_pwd) {
        this.sparql_baseAuth_pwd = sparql_baseAuth_pwd;
    }

}
