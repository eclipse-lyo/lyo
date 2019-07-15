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

package org.eclipse.lyo.trs.client.handlers;

import org.apache.jena.rdf.model.Model;
import org.eclipse.lyo.core.trs.ChangeEvent;
import org.eclipse.lyo.core.trs.Deletion;
import org.eclipse.lyo.trs.client.model.BaseMember;
import org.eclipse.lyo.trs.client.model.ChangeEventMessageTR;
import org.eclipse.lyo.trs.client.util.SparqlUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SparqlDirectHandler implements IProviderEventHandler {

    private final static Logger log = LoggerFactory.getLogger(SparqlDirectHandler.class);
    private final String sparqlUpdateService;

    public SparqlDirectHandler(
            final String sparqlUpdateService) {this.sparqlUpdateService = sparqlUpdateService;}

    @Override
    public void finishCycle() {

    }

    @Override
    public void handleBaseMember(final BaseMember baseMember) {
        SparqlUtil.createGraph(baseMember.getUri().toString(), sparqlUpdateService);
        SparqlUtil.addTriplesToNamedGraph(baseMember.getModel(), baseMember.getUri().toString(),
                sparqlUpdateService);
    }

    @Override
    public void handleChangeEvent(final ChangeEventMessageTR eventMessageTR) {
        final ChangeEvent changeEvent = eventMessageTR.getChangeEvent();
        final Model trsResourceModel = eventMessageTR.getTrackedResourceModel();
        if (changeEvent instanceof Deletion) {
            SparqlUtil.processChangeEvent(changeEvent, null, sparqlUpdateService);
        } else {
            if (trsResourceModel != null) {
                SparqlUtil.processChangeEvent(changeEvent, trsResourceModel, sparqlUpdateService);
            }
        }

    }

    @Override
    public void rebase() {

    }
}
