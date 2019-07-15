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
import java.net.URISyntaxException;
import java.util.List;
import org.apache.jena.rdf.model.Model;
import org.eclipse.lyo.core.trs.Base;
import org.eclipse.lyo.core.trs.ChangeLog;
import org.eclipse.lyo.core.trs.TrackedResourceSet;
import org.eclipse.lyo.oslc4j.core.exception.LyoModelException;
import org.eclipse.lyo.trs.client.exceptions.RepresentationRetrievalException;
import org.jetbrains.annotations.NotNull;

public interface ITrackedResourceClient {
    @NotNull
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
