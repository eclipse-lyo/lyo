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
package org.eclipse.lyo.server.jaxrs.repository.impl;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import org.eclipse.lyo.core.query.ComparisonTerm;
import org.eclipse.lyo.core.query.ComparisonTerm.Operator;
import org.eclipse.lyo.core.query.ParseException;
import org.eclipse.lyo.core.query.QueryUtils;
import org.eclipse.lyo.core.query.SimpleTerm;
import org.eclipse.lyo.core.query.SimpleTerm.Type;
import org.eclipse.lyo.core.query.WhereClause;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.server.jaxrs.repository.RepositoryOperationException;
import org.eclipse.lyo.server.jaxrs.repository.ResourceRepository;
import org.eclipse.lyo.server.jaxrs.services.ResourceId;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.jsonldjava.shaded.com.google.common.base.Strings;

public class InMemResourceRepositoryImpl<R extends AbstractResource, IBT extends ResourceId<R>> implements ResourceRepository<R, IBT> {

    private final static Logger LOG = LoggerFactory.getLogger(InMemResourceRepositoryImpl.class);

    protected final Map<URI, R> resources = new HashMap<>();
    protected final Map<URI, String> etags = new HashMap<>();

    @Override
    public Optional<R> getResource(IBT id) {
        R r = resources.get(id.toUri());
        if(r == null) {
            return Optional.empty();
        } else {
            return Optional.of(r);
        }
    }

    @Override
    public boolean deleteResource(IBT id) {
        return resources.remove(id.toUri()) != null;
    }

    @Override
    public R update(R updatedResource, IBT id, Class<R> klass) {
        URI uri = id.toUri();
        if(updatedResource == null || uri == null) {
            throw new NullPointerException("Resource or Id URI cannot be null");
        }
        if(!uri.equals(updatedResource.getAbout())) {
            if (updatedResource.getAbout() != null) {
                LOG.info("Resource URI {} != {} (Id)", updatedResource.getAbout(), uri);
            }
            updatedResource.setAbout(uri);
        }
        R aResource = resources.put(uri, updatedResource);
        String etag = UUID.randomUUID().toString();
        etags.put(updatedResource.getAbout(), etag);
        return aResource;
    }

    @Override
    public List<R> queryResources(String oslcWhere, String oslcPrefixes, int page, int pageSize) {
        try {
            if(!Strings.isNullOrEmpty(oslcWhere)) {
                Map<String, String> prefixes = new HashMap<>();
                if (!Strings.isNullOrEmpty(oslcPrefixes)) {
                    prefixes = QueryUtils.parsePrefixes(oslcPrefixes);

                }
                WhereClause whereClause = QueryUtils.parseWhere(oslcWhere, prefixes);
                for (SimpleTerm simpleTerm : whereClause.children()) {
                    if(simpleTerm.type() == Type.COMPARISON) {
                        ComparisonTerm comparison = (ComparisonTerm) simpleTerm;
                        if(comparison.operator() == Operator.EQUALS) {
                            System.out.printf("   -> %s == %s", comparison.property(), comparison.operand());
                        }
                        // TODO add a method to IResource to get any property to avoid reflection
                    }
                }

            }
            // TODO sort properly by some property by default
            ArrayList<R> arrayList = new ArrayList<>(resources.values());
            return arrayList.subList(page * pageSize, Math.min((page + 1)* pageSize, arrayList.size()));
        } catch (ParseException e) {
            throw new IllegalArgumentException(e);
        }
    }

    @Override
    public String getETag(R resource) {
        return etags.get(resource.getAbout());
    }

    @Override
    public R createResource(R aResource, IBT id, Class<R> klass) {
        if(aResource == null) {
            throw new NullPointerException("Resource cannot be null");
        }
        aResource.setAbout(id.toUri());
        R oldResource = resources.put(aResource.getAbout(), aResource);
        if(oldResource != null) {
            LOG.warn("Overwriting an existing resource with a createResource() call");
        }
        String etag = UUID.randomUUID().toString();
        etags.put(aResource.getAbout(), etag);
        return aResource;
    }
}
