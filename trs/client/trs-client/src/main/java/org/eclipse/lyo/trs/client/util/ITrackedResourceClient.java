/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
 *
 * See the NOTICE file(s) distributed with this work for additional
 * information regarding copyright ownership.
 *
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0, or the Eclipse Distribution License 1.0
 * which is available at http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause
 */

package org.eclipse.lyo.trs.client.util;

import org.apache.jena.rdf.model.Model;
import org.eclipse.lyo.core.trs.Base;
import org.eclipse.lyo.core.trs.ChangeLog;
import org.eclipse.lyo.core.trs.TrackedResourceSet;
import org.eclipse.lyo.trs.client.exceptions.RepresentationRetrievalException;

import java.net.URI;
import java.util.List;

public interface ITrackedResourceClient {
    Model fetchTRSRemoteResource(URI resource) throws RepresentationRetrievalException;

    List<Base> updateBases(TrackedResourceSet updatedTrs);

    /**
     * retieve the trs from the trs provider using the trs uri attribute and
     * return a trs pojo accordingly
     *
     * @return trs pojo
     */
    TrackedResourceSet extractRemoteTrs(URI trsUri);

    /**
     * Retrieve the change log from the trs provider using the changeLogURI
     * argument return a change log pojo accordingly
     *
     * @param changeLogURl url of the change log
     *
     * @return change log pojo
     */
    ChangeLog fetchRemoteChangeLog(URI changeLogURl);

    /**
     * Retrieve the base from the trs provider using the baseURI argument
     * return a base pojo accordingly
     *
     * @param baseUrl url of the base
     *
     * @return base pojo
     */
    Base fetchRemoteBase(URI baseUrl);
}
