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
package org.eclipse.lyo.server.oauth.core.consumer;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import net.oauth.OAuth;
import net.oauth.OAuthException;
import net.oauth.OAuthMessage;

/**
 * Manages the list of OAuth consumers.
 *
 * @author Samuel Padgett
 */
public abstract class AbstractConsumerStore implements ConsumerStore {
    private Map<String, LyoOAuthConsumer> consumerMap =
            Collections.synchronizedMap(new HashMap<>());

    public AbstractConsumerStore() {}

    public void addAll(Collection<LyoOAuthConsumer> consumers) {
        for (LyoOAuthConsumer consumer : consumers) {
            consumerMap.put(consumer.consumerKey, consumer);
        }
    }

    public Collection<LyoOAuthConsumer> getAllConsumers() {
        return consumerMap.values();
    }

    public LyoOAuthConsumer getConsumer(OAuthMessage requestMessage)
            throws OAuthException, IOException {
        requestMessage.requireParameters(OAuth.OAUTH_CONSUMER_KEY);
        return getConsumer(requestMessage.getConsumerKey());
    }

    public LyoOAuthConsumer getConsumer(String consumerKey) {
        return consumerMap.get(consumerKey);
    }

    public abstract void closeConsumerStore();

    protected LyoOAuthConsumer add(LyoOAuthConsumer consumer) {
        return consumerMap.put(consumer.consumerKey, consumer);
    }

    protected LyoOAuthConsumer remove(String consumerKey) {
        return consumerMap.remove(consumerKey);
    }
}
