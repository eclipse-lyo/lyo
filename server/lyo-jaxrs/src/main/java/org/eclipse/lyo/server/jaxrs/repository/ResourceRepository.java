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
package org.eclipse.lyo.server.jaxrs.repository;

import org.eclipse.lyo.oslc4j.core.model.AbstractResource;

import java.net.URI;
import java.util.List;
import java.util.Optional;

public interface ResourceRepository<R extends AbstractResource> {
    Optional<R> getResource(URI id) throws RepositoryConnectionException;
    
    boolean deleteResource(URI id);
  
    R update(URI uri, R updatedResource, Class<R> klass) throws RepositoryConnectionException, RepositoryOperationException;
  
    List<R> queryResources(String oslcWhere, String oslcPrefixes, int page, int pageSize);

    String getETag(R resource);
}
