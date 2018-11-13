/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
 *
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the Eclipse Public License v1.0
 *  and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *  
 *  The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 *  and the Eclipse Distribution License is available at
 *  http://www.eclipse.org/org/documents/edl-v10.php.
 *  
 *  Contributors:
 *  
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
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
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
public abstract class AbstractConsumerStore implements ConsumerStore {
	private Map<String, LyoOAuthConsumer> consumerMap = Collections
			.synchronizedMap(new HashMap<String, LyoOAuthConsumer>());
	
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
