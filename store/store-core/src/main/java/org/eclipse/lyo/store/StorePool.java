package org.eclipse.lyo.store;

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

import java.net.URI;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import org.eclipse.lyo.store.Store;
import org.eclipse.lyo.store.StoreFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class StorePool {

    private URI defaultNamedGraphUri;
    private BlockingQueue<Store> storePool;
    private static final Logger log = LoggerFactory.getLogger(StorePool.class);


    public StorePool (int poolSize, URI defaultNamedGraphUri, URI sparqlQueryEndpoint, URI sparqlUpdateEndpoint, String userName, String password) {
        this.defaultNamedGraphUri = defaultNamedGraphUri;
        this.storePool = new ArrayBlockingQueue<>(poolSize);
        for (int i = 0; i < poolSize; i++) {
            Store s = null;
            if( userName != null && password != null ){
                s = StoreFactory.sparql(sparqlQueryEndpoint.toString(), sparqlUpdateEndpoint.toString(), userName, password);
            }else{
                s = StoreFactory.sparql(sparqlQueryEndpoint.toString(), sparqlUpdateEndpoint.toString());
            }
            storePool.add(s);
        }
    }

    public URI getDefaultNamedGraphUri() {
        return defaultNamedGraphUri;
    }

    public Store getStore() {
        try {
            return storePool.take();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();  // set interrupt flag
            log.error("Failed to get a store from the pool", e);
            return null;
        }
    }

    public void releaseStore(Store store) {
        try {
            storePool.put(store);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();  // set interrupt flag
            log.error("Failed to get a store from the pool", e);
        }
    }
}
