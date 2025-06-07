package org.eclipse.lyo.store;

/*
 * Copyright (c) 2022 Contributors to the Eclipse Foundation
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
import static org.assertj.core.api.Assertions.assertThat;

import java.net.URI;
import java.util.List;
import java.util.UUID;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class StorePoolTest {
    private final Logger log = LoggerFactory.getLogger(StorePoolTest.class);

    @Test
    void StoreInMem_TwoSize_BothCanBeUsed()
            throws StoreAccessException, ModelUnmarshallingException {
        StorePool pool = new StorePool(2, StorePool.DEFAULT_GRAPH_JENA);

        Store store1 = pool.getStore();
        Store store2 = pool.getStore();

        Thread thread1 =
                new Thread(
                        () -> {
                            try {
                                insertResources(500, store1);
                            } catch (StoreAccessException e) {
                                log.error("Store error (Thread 1)", e);
                            }
                        });
        Thread thread2 =
                new Thread(
                        () -> {
                            try {
                                insertResources(500, store1);
                            } catch (StoreAccessException e) {
                                log.error("Store error (Thread 2)", e);
                            }
                        });
        thread1.start();
        thread2.start();
        try {
            thread1.join();
            thread2.join();
        } catch (InterruptedException e) {
            log.error("One of the threads did not terminate cleanly");
        }
        assertThat(store2).isNotNull();
        List<ServiceProvider> resources =
                store2.getResources(StorePool.DEFAULT_GRAPH_JENA, ServiceProvider.class);
        assertThat(resources).hasSize(1000);
    }

    private void insertResources(int i, Store store) throws StoreAccessException {
        for (; i > 0; i--) {
            ServiceProvider resource = new ServiceProvider();
            resource.setAbout(URI.create("urn:lyo:" + UUID.randomUUID().toString()));
            resource.setTitle("Resource " + i);
            store.appendResource(StorePool.DEFAULT_GRAPH_JENA, resource);
        }
    }
}
