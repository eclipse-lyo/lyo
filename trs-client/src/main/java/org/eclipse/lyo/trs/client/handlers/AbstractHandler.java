/*
 * Copyright (c) 2016 KTH Royal Institute of Technology and others
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */

package org.eclipse.lyo.trs.client.handlers;

import javax.ws.rs.core.Response;
import org.eclipse.lyo.oslc4j.client.OslcClient;
import org.eclipse.lyo.oslc4j.core.exception.LyoModelException;
import org.eclipse.lyo.trs.client.exceptions.RepresentationRetrievalException;
import org.eclipse.lyo.trs.client.exceptions.TrsEndpointConfigException;
import org.eclipse.lyo.trs.client.exceptions.TrsEndpointErrorExpection;
import org.eclipse.lyo.trs.client.util.ClientUtil;
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
public abstract class AbstractHandler implements Runnable {

    private final static Logger log = LoggerFactory.getLogger(AbstractHandler.class);
    /**
     * instance of the http client used by this TRS Task handler to communicate
     * with the TRS providers
     */
    private final OslcClient oslcClient;

    /**
     * retrieve the distant resource using basic authentication if necessary
     * from the given url param and try and return the requested content type
     *
     * @param url
     *            the requestes resource url
     * @param objClass
     *            the required content type
     * @return an instance of the required content type
     */
    Object fetchTRSRemoteResource(String url, Class<?> objClass)
            throws RepresentationRetrievalException {
        final Response response = oslcClient.getResource(url);
        final Object resource;
        try {
            resource = ClientUtil.extractResourceFromResponse(response, objClass);
            response.close();
            return resource;
        } catch (TrsEndpointConfigException e) {
            log.error("TRS Provider configuration is wrong: ", e);
            throw new RepresentationRetrievalException(e);
        } catch (TrsEndpointErrorExpection e) {
            log.warn("The TRS endpoint {} is unreachable", url);
            log.debug("TRS endpoint exception", e);
            throw new RepresentationRetrievalException(e);
        } catch (LyoModelException e) {
            log.warn("Failed to create a Jena Model from the response");
            throw new RepresentationRetrievalException(e);
        }
    }

    AbstractHandler(OslcClient oslcClient) {
        super();
        this.oslcClient = oslcClient;
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
