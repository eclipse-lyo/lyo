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

import java.util.function.Supplier;

import org.eclipse.lyo.oslc4j.core.model.AbstractResource;

public class DummyInMemResourceRepository<R extends AbstractResource> extends InMemResourceRepositoryImpl<R> {
    public DummyInMemResourceRepository(Supplier<R> s, int n, Class<R> klass) {
        for (int i = 0; i < n; i++) {
            R resource = s.get();
            this.update(resource.getAbout(), resource, klass);
        }
    }
}
