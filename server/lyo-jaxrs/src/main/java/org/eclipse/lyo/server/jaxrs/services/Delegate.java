/*
 * Copyright (c) 2021 Contributors to the Eclipse Foundation
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
package org.eclipse.lyo.server.jaxrs.services;

import java.util.List;
import java.util.Optional;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.Response.ResponseBuilder;
import javax.ws.rs.core.UriInfo;
import javax.xml.namespace.QName;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.eclipse.lyo.client.OSLCConstants;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Compact;
import org.eclipse.lyo.server.jaxrs.repository.RepositoryOperationException;

public interface Delegate<RT extends AbstractResource, IDT extends ResourceId<RT>> {
    // TODO use another namespace or raise an issue in the OP
    QName OSLC_ETAG = new QName(OSLCConstants.OSLC_V2, "etag");

    ImmutablePair<ResponseBuilder, Optional<RT>> getResource(IDT id, Class<RT> clazz);

    // TODO @andrew think about wrapping Response.ResponseBuilder in a LyoResponse
    // or providing a factory method
    ImmutablePair<ResponseBuilder, Optional<Compact>> getResourceCompact(IDT id, Class<RT> clazz);

    ResponseBuilder deleteResource(IDT id);

    ImmutablePair<ResponseBuilder, Optional<RT>> putResource(RT aResource, IDT id, String etag, Class<RT> klass);

    ImmutablePair<ResponseBuilder, Optional<RT>> createResourceForCreationFactory(RT aResource, Class<RT> klass);

    ImmutablePair<ResponseBuilder, Optional<RT>> createResourceForDelegatedUI(RT aResource, Class<RT> klass);

    ResponseBuilder queryResources(HttpServletRequest httpServletRequest, UriInfo uriInfo,
            String where, String prefix, int page, int pageSize);

    List<RT> find(String terms) throws RepositoryOperationException;

    @Deprecated
    Optional<RT> fetchResource(IDT id) throws RepositoryOperationException;
}
