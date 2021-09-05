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

import java.util.function.Function;
import java.util.function.Supplier;

import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.server.jaxrs.repository.RepositoryOperationException;
import org.eclipse.lyo.server.jaxrs.services.ResourceId;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DummyInMemResourceRepository<RT extends AbstractResource, IDT extends ResourceId<RT>> extends InMemResourceRepositoryImpl<RT, IDT> {
    private final static Logger LOG = LoggerFactory.getLogger(DummyInMemResourceRepository.class);

    public DummyInMemResourceRepository(Supplier<RT> s, int n, Class<RT> klass) {
        for (int i = 0; i < n; i++) {
            RT resource = s.get();
            this.createResource(resource, klass);
        }
    }
}
