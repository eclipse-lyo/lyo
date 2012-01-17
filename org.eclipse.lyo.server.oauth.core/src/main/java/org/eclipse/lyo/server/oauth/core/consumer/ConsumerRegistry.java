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
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import net.oauth.OAuth;
import net.oauth.OAuthMessage;
import net.oauth.OAuthProblemException;

/**
 * Manages the list of OAuth consumers.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
public class ConsumerRegistry {
	private Map<String, LyoOAuthConsumer> consumerMap = Collections
			.synchronizedMap(new HashMap<String, LyoOAuthConsumer>());
	private static ConsumerRegistry instance = new ConsumerRegistry();
	
	public static ConsumerRegistry getInstance() {
		return instance;
	}
	
	private ConsumerRegistry() {}

	public void init(ConsumerStore consumerStore) throws IOException {
		for (LyoOAuthConsumer consumer : consumerStore.load()) {
			consumerMap.put(consumer.consumerKey, consumer);
		}
	}
	
	public LyoOAuthConsumer getConsumer(OAuthMessage requestMessage)
			throws OAuthProblemException, IOException {
		requestMessage.requireParameters(OAuth.OAUTH_CONSUMER_KEY);
		return getConsumer(requestMessage.getConsumerKey());
	}
	
	public LyoOAuthConsumer getConsumer(String consumerKey) {
		return consumerMap.get(consumerKey);
	}
	
//	public LyoOAuthConsumer addConsumer(LyoOAuthConsumer consumer) {
//		return consumerMap.put(consumer.consumerKey, consumer);
//	}
//	
//	public LyoOAuthConsumer removeConsumer(LyoOAuthConsumer consumer) {
//		return consumerMap.remove(consumer);
//	}
}
