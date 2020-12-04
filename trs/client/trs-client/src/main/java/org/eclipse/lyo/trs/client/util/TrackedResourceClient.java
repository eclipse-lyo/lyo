/*
 * Copyright (c) 2019 KTH Royal Institute of Technology and others
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */

package org.eclipse.lyo.trs.client.util;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import javax.ws.rs.core.Response;
import org.apache.jena.rdf.model.Model;
import org.eclipse.lyo.client.OslcClient;
import org.eclipse.lyo.core.trs.Base;
import org.eclipse.lyo.core.trs.ChangeLog;
import org.eclipse.lyo.core.trs.Page;
import org.eclipse.lyo.core.trs.TrackedResourceSet;
import org.eclipse.lyo.oslc4j.core.exception.LyoModelException;
import org.eclipse.lyo.trs.client.exceptions.RepresentationRetrievalException;
import org.eclipse.lyo.trs.client.exceptions.TrsEndpointConfigException;
import org.eclipse.lyo.trs.client.exceptions.TrsEndpointErrorException;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TrackedResourceClient implements ITrackedResourceClient {
    private final static Logger log = LoggerFactory.getLogger(TrackedResourceClient.class);
    private final OslcClient oslcClient;

    public TrackedResourceClient(final OslcClient oslcClient) {this.oslcClient = oslcClient;}

    @NotNull
    @Override
    public Model fetchTRSRemoteResource(final URI uri) throws RepresentationRetrievalException {
        final Response response = oslcClient.getResource(uri.toString());
        final Model resource;
        try {
            // TODO Andrew@2019-07-15: JHM typed method use
            // TODO Andrew@2019-07-15: switch to extractModel
            resource = (Model) ClientUtil.extractResourceFromResponse(response, Model.class);
            response.close();
            if(resource != null) {
                return resource;
            } else {
                throw new RepresentationRetrievalException("Empty model was retrieved");
            }
        } catch (TrsEndpointConfigException e) {
            log.error("Bad request", e);
            throw new RepresentationRetrievalException(e);
        } catch (TrsEndpointErrorException e) {
            log.warn("Failed to fetch {}", uri);
            log.debug("Server error", e);
            throw new RepresentationRetrievalException(e);
        } catch (LyoModelException e) {
            log.debug("Error reading Jena Model from the response");
            throw new RepresentationRetrievalException(e);
        }
    }


    /**
     * Return a list of base objects corresponding to the pages of the base
     * after requesting them from the base url. The base url is retrieved from
     * the trs object passes as a parameter.
     *
     * @param updatedTrs the trs object retrieved after retrieving it using the trs uri
     *
     * @return the pages of the base of this trs provider
     */
    @Override
    public List<Base> updateBases(TrackedResourceSet updatedTrs)
            throws LyoModelException, RepresentationRetrievalException {
        List<Base> bases = new ArrayList<>();
        URI firstBasePageUri = updatedTrs.getBase();
        Base currentBase = fetchRemoteBase(firstBasePageUri);
        Page nextPage = currentBase.getNextPage();
        bases.add(currentBase);
        while (nextPage != null) {
            URI currentPageUri = nextPage.getNextPage();
            if (ProviderUtil.isNilUri(currentPageUri)) {
                break;
            }
            currentBase = fetchRemoteBase(currentPageUri);
            bases.add(currentBase);
            nextPage = currentBase.getNextPage();
        }
        return bases;
    }

    @Override
    public TrackedResourceSet extractRemoteTrs(URI trsUri)
            throws LyoModelException, RepresentationRetrievalException {
        Model rdfModel = fetchTRSRemoteResource(trsUri);
        return ClientUtil.extractTrsFromRdfModel(rdfModel);
    }

    @Override
    public ChangeLog fetchRemoteChangeLog(URI changeLogURl)
            throws IllegalArgumentException, SecurityException, LyoModelException,
            RepresentationRetrievalException {
        Model rdfModel = fetchTRSRemoteResource(changeLogURl);
        return ClientUtil.extractChangeLogFromRdfModel(rdfModel);
    }

    @Override
    public Base fetchRemoteBase(URI baseUrl)
            throws LyoModelException, RepresentationRetrievalException {
        final Model rdFModel = fetchTRSRemoteResource(baseUrl);
        return ClientUtil.extractBaseFromRdfModel(rdFModel);
    }
}
