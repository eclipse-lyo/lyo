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

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.server.jaxrs.repository.RepositoryConnectionException;
import org.eclipse.lyo.server.jaxrs.repository.RepositoryOperationException;

public interface Delegate<RT extends AbstractResource,IBT extends ResourceId<RT>> {
    ResponseBuilder getResource(Class<RT> clazz, IBT id);
    
    // TODO @andrew think about wrapping Response.ResponseBuilder in a LyoResponse
    // or providing a factory method
    ResponseBuilder getResourceCompact(Class<RT> clazz, IBT id);

    ResponseBuilder deleteResource(IBT id);

    ResponseBuilder putResource(Class<RT> klass, IBT id, RT aResource, String etag);

    ImmutablePair<ResponseBuilder, RT> createResource(Class<RT> klass, RT aResource);
    
    ImmutablePair<ResponseBuilder, RT> createResourceJson(Class<RT> klass, RT aResource);
    
    ResponseBuilder queryResources(HttpServletRequest httpServletRequest, UriInfo uriInfo, 
            String where, String prefix, int page, int pageSize);

    List<RT> find(String terms) throws RepositoryOperationException;

    Optional<RT> fetchResource(IBT id) throws RepositoryOperationException;
}
