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
package org.eclipse.lyo.oslc4j.trs.consumer.TRSProvider.handler;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import org.apache.jena.rdf.model.Model;
import org.apache.log4j.Logger;
import org.eclipse.lyo.oslc4j.trs.consumer.concurrent.TRSTaskHandler;
import org.eclipse.lyo.oslc4j.trs.consumer.httpclient.TRSHttpClient;
import org.eclipse.lyo.oslc4j.trs.consumer.sparql.SparqlUtil;

import net.oauth.OAuthException;

/**
 * This class is the thread handling the creation of sparql updates handling the
 * processing of a base member
 *
 * @author Omar
 *
 */
public class BaseMemberHandler extends TRSTaskHandler {

    final static Logger logger = Logger.getLogger(BaseMemberHandler.class);
    /*
     * the uri of the base member to be processed
     */
    String baseMemberUri;
    /*
     * a list of sparql updates passed as an argument to this thread so that it
     * adds the sparql update it generate to it
     */
    List<String> queries;
    /*
     * the size of the rdf representation of the rdf member
     */
    AtomicLong modelSize;

    public BaseMemberHandler(TRSHttpClient oslcClient, String sparqlQueryService, String sparqlUpdateService,
            String baseAuth_userName, String baseAuth_pwd, String baseMemberUri, List<String> queries,
            AtomicLong modelSize) {
        super(oslcClient, sparqlQueryService, sparqlUpdateService, baseAuth_userName, baseAuth_pwd);
        this.baseMemberUri = baseMemberUri;
        threadName = "Base Member: " + baseMemberUri + " addition handler thread";
        this.queries = queries;
        this.modelSize = modelSize;
    }

    /**
     * create the necessary sparql update for processing the base member
     *
     * @throws IOException
     * @throws OAuthException
     * @throws URISyntaxException
     */
    private void processBaseMemberAddition() throws IOException, OAuthException, URISyntaxException {

        logger.debug("processing base member " + baseMemberUri + " addition.  Creating necessary sparql update query ");
        StringBuilder query = new StringBuilder();
        Model graphToUpload = (Model) fetchTRSRemoteResource(baseMemberUri, Model.class);
        if (graphToUpload != null) {
            modelSize.set(modelSize.get() + graphToUpload.size());
            String graphCreationQuery = SparqlUtil.createGraphQuery(baseMemberUri);
            String addTriplesToGraphQuery = SparqlUtil.addTriplesToGraphQuery(baseMemberUri, graphToUpload);

            query.append(graphCreationQuery);
            query.append("; \n");
            query.append(addTriplesToGraphQuery);
            queries.add(query.toString());

            logger.debug("finished processing  base member " + baseMemberUri + " addition ");
        } else {
            logger.error("could not retrieve representation of member resource with uri: " + baseMemberUri);

        }
    }

    @Override
    protected void processTRSTask() {
        try {
            super.processTRSTask();
            processBaseMemberAddition();
        } catch (IOException | URISyntaxException | OAuthException e) {
            logger.error(e);
        }
    }

}
