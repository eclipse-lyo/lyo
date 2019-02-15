/*
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
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;
import net.oauth.OAuthException;
import org.apache.jena.rdf.model.Model;
import org.eclipse.lyo.oslc4j.trs.client.util.SparqlUtil;
import org.eclipse.lyo.oslc4j.trs.client.util.TrsBasicAuthOslcClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class is the thread handling the creation of sparql updates handling the
 * processing of a base member
 *
 * @author Omar
 */
public class BaseMemberHandler extends TRSTaskHandler {

    final static Logger logger = LoggerFactory.getLogger(BaseMemberHandler.class);
    /*
     * the uri of the base member to be processed
     */ String baseMemberUri;
    /*
     * a list of sparql updates passed as an argument to this thread so that it
     * adds the sparql update it generate to it
     */ List<String> queries;
    /*
     * the size of the rdf representation of the rdf member
     */ AtomicLong modelSize;

    public BaseMemberHandler(TrsBasicAuthOslcClient oslcClient, String sparqlQueryService,
            String sparqlUpdateService, String baseAuth_userName, String baseAuth_pwd,
            String baseMemberUri, List<String> queries, AtomicLong modelSize) {
        // TODO Andrew@2018-03-01: use a common SPARQL interface
        super(oslcClient,
                sparqlQueryService,
                sparqlUpdateService,
                null,
                null,
                baseAuth_userName,
                baseAuth_pwd
        );
        this.baseMemberUri = baseMemberUri;
        threadName = "Base Member: " + baseMemberUri + " addition handler thread";
        this.queries = queries;
        this.modelSize = modelSize;
    }

    @Override
    protected void processTRSTask() {
        try {
            processBaseMemberAddition();
        } catch (IOException | OAuthException | URISyntaxException e) {
            logger.error("Error processing TRS task", e);
        }
    }

    /**
     * create the necessary sparql update for processing the base member
     */
    private void processBaseMemberAddition()
            throws IOException, OAuthException, URISyntaxException {

        logger.debug("processing base member " + baseMemberUri + " addition.  Creating necessary " +
                "" + "" + "sparql update query ");
        StringBuilder query = new StringBuilder();
        Model graphToUpload = (Model) fetchTRSRemoteResource(baseMemberUri, Model.class);
        if (graphToUpload != null) {
            modelSize.set(modelSize.get() + graphToUpload.size());
            String graphCreationQuery = SparqlUtil.createGraphQuery(baseMemberUri);
            String addTriplesToGraphQuery = SparqlUtil.addTriplesToGraphQuery(baseMemberUri,
                    graphToUpload
            );

            query.append(graphCreationQuery);
            query.append("; \n");
            query.append(addTriplesToGraphQuery);
            queries.add(query.toString());

            logger.debug("finished processing  base member " + baseMemberUri + " addition ");
        } else {
            logger.error("could not retrieve representation of member resource with uri: " +
                    baseMemberUri);

        }
    }

}
