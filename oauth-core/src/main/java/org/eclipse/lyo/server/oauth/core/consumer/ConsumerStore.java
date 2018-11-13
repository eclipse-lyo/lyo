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

import net.oauth.OAuthException;
import net.oauth.OAuthMessage;

/**
 * Manages persistence of OAuth consumers.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
public interface ConsumerStore {
	/**
	 * Gets the consumer for this message.
	 * 
	 * @param requestMessage
	 *            the OAuth message
	 * 
	 * @return the consumer
	 * 
	 * @throws OAuthException
	 *             if the OAuth message does not contain a consumer key
	 * @throws IOException
	 *             on errors reading the message
	 * @throws ConsumerStoreException
	 *             on other errors
	 */
	public LyoOAuthConsumer getConsumer(OAuthMessage requestMessage)
			throws OAuthException, IOException, ConsumerStoreException;

	/**
	 * Gets all OAuth consumers.
	 * 
	 * @return all OAuth consumers, provisional and approved
	 * 
	 * @throws ConsumerStoreException
	 *             on errors
	 */
	public Collection<LyoOAuthConsumer> getAllConsumers()
			throws ConsumerStoreException;
	
	/**
	 * Gets the consumer for a key.
	 * 
	 * @param consumerKey
	 *            the consumer key
	 * @return the consumer or null if there is no consumer for this key
	 * 
	 * @throws ConsumerStoreException
	 *             on errors
	 */
	public LyoOAuthConsumer getConsumer(String consumerKey)
			throws ConsumerStoreException;

	/**
	 * Adds a new consumer.
	 * 
	 * @param consumer the consumer
	 * @return the previous consumer associated with this key or null if there wasn't one
	 * 
	 * @throws ConsumerStoreException
	 */
	public LyoOAuthConsumer addConsumer(LyoOAuthConsumer consumer)
			throws ConsumerStoreException;

	/**
	 * Removes a consumer.
	 * 
	 * @param key
	 *            the consumer key
	 * @return the removed consumer or null if it wasn't previously in the store
	 * 
	 * @throws ConsumerStoreException
	 *             on errors
	 */
	public LyoOAuthConsumer removeConsumer(String consumerKey)
			throws ConsumerStoreException;

	/**
	 * Updates a consumer.
	 * 
	 * @param consumer
	 *            the consumer
	 * @return the same consumer or null if it wasn't previously in the store
	 * 
	 * @throws ConsumerStoreException
	 *             on errors
	 */
	public LyoOAuthConsumer updateConsumer(LyoOAuthConsumer consumer)
			throws ConsumerStoreException;
	
	/**
	 * Closes the consumer store.  It is unavailable for further use.
	 * 
	 */
	public void closeConsumerStore();
}
