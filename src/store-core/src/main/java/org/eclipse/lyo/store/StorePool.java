package org.eclipse.lyo.store;

/*-
 * #%L
 * Contributors:
 *     Jad El-khoury - initial implementation
 * %%
 * Copyright (C) 2020 KTH Royal Institute of Technology
 * %%
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * #L%
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
        this.storePool = new ArrayBlockingQueue<Store>(poolSize);
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
