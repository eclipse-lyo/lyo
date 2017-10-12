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
 *     Samuel Padgett  - initial API and implementation
 *     Michael Fiedler - updates to use Jena TDB instead of Derby 
 *******************************************************************************/
package org.eclipse.lyo.server.oauth.consumerstore;


import org.eclipse.lyo.server.oauth.core.consumer.AbstractConsumerStore;
import org.eclipse.lyo.server.oauth.core.consumer.ConsumerStoreException;
import org.eclipse.lyo.server.oauth.core.consumer.LyoOAuthConsumer;

import org.apache.jena.tdb.TDBFactory;
import org.apache.jena.query.Dataset;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ResIterator;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.shared.Command;
import org.apache.jena.shared.JenaException;
import org.apache.jena.shared.Lock;
import org.apache.jena.shared.PropertyNotFoundException;
import org.apache.jena.vocabulary.RDF;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A simple RDF consumer store backed by a Jena TDB native datastore
 **/

public class RdfConsumerStore extends AbstractConsumerStore {
	protected final static String LYO_OAUTH_NAMESPACE = "http://eclipse.org/lyo/server/oauth#";
	protected final static String CONSUMER_RESOURCE = LYO_OAUTH_NAMESPACE
			+ "Consumer";
	protected final static String CALLBACK_URL = LYO_OAUTH_NAMESPACE
			+ "callback";
	protected final static String CONSUMER_NAME = LYO_OAUTH_NAMESPACE
			+ "consumerName";
	protected final static String CONSUMER_KEY = LYO_OAUTH_NAMESPACE
			+ "consumerKey";
	protected final static String CONSUMER_SECRET = LYO_OAUTH_NAMESPACE
			+ "consumerSecret";
	protected final static String PROVISIONAL = LYO_OAUTH_NAMESPACE
			+ "provisional";
	protected final static String TRUSTED = LYO_OAUTH_NAMESPACE + "trusted";

	protected final static String DB = "RDFStore"; // database type

	private final static Logger log = LoggerFactory.getLogger(RdfConsumerStore.class);

	private Model model;
	private Dataset dataset;

	public RdfConsumerStore() throws ConsumerStoreException,
			ClassNotFoundException {
		createDataset();
		if (this.dataset != null) {
			createModel(this.dataset);
			loadConsumers();
		}
	}
	
	public RdfConsumerStore(Dataset dataset) throws ConsumerStoreException {
		this.dataset = dataset;
		createModel(this.dataset);
		loadConsumers();
	}

	public RdfConsumerStore(Model model) throws ConsumerStoreException {
		createDataset();
		if (this.dataset != null) {
			this.model = model;
			dataset.setDefaultModel(this.model);
			loadConsumers();
		}
	}
	
	protected void createDataset() {
		try {
			this.dataset = TDBFactory.createDataset(DB);
		} catch (Exception e) {
			log.error("Could not create dataset for OAuth consumer store.", e);
		}
	}

	protected void createModel(Dataset dataset) {
		try {
			this.model = dataset.getDefaultModel();
		} catch (Exception e) {
			log.error("Could not create model for OAuth consumer store.", e);
		}
	}

	protected void loadConsumers() throws ConsumerStoreException {
		try {
			model.enterCriticalSection(Lock.READ);
			ResIterator i = model.listResourcesWithProperty(RDF.type,
					model.createResource(CONSUMER_RESOURCE));
			while (i.hasNext()) {
				Resource consumerResource = i.next();
				try {
					add(fromResource(consumerResource));
				} catch (PropertyNotFoundException e) {
					// The resource is missing some properties.
					// Not good, but other consumer resources might
					// be OK, so continue. (Log the error, though.)
					log.error(
							"Could not load consumer "
									+ consumerResource.getProperty(model
											.createProperty(CONSUMER_NAME))
									+ " ("
									+ consumerResource.getProperty(model
											.createProperty(CONSUMER_KEY))
									+ ")", e);
				} catch (JenaException e) {
					// Some other runtime exception occurred.
					throw new ConsumerStoreException(e);
				}
			}
		} finally {
			model.leaveCriticalSection();
		}
	}

	@Override
	public LyoOAuthConsumer addConsumer(final LyoOAuthConsumer consumer)
			throws ConsumerStoreException {
		if (model == null) {
			throw new ConsumerStoreException("Consumer store not initialized.");
		}
		
		try {
			model.enterCriticalSection(Lock.WRITE);
			model.executeInTransaction(new Command() {
				@Override
				public Object execute() {
					removeProperties(consumer);
					toResource(consumer);
					
					return consumer;
				}
			});
						
			return add(consumer);
		} catch (JenaException e) {
			throw new ConsumerStoreException(e);
		} finally {
			model.leaveCriticalSection();
		}
	}

	@Override
	public LyoOAuthConsumer removeConsumer(final String consumerKey)
			throws ConsumerStoreException {
		if (model == null) {
			throw new ConsumerStoreException("Consumer store not initialized.");
		}
		
		try {
			model.enterCriticalSection(Lock.WRITE);
			model.executeInTransaction(new Command() {
				@Override
				public Object execute() {
					removeProperties(consumerKey);
					
					return consumerKey;
				}
			});
	
			return remove(consumerKey);
		} catch (JenaException e) {
			throw new ConsumerStoreException(e);
		} finally {
			model.leaveCriticalSection();
		}
	}

	@Override
	public LyoOAuthConsumer updateConsumer(LyoOAuthConsumer consumer)
			throws ConsumerStoreException {
		// addConsumer() also works for update.
		return addConsumer(consumer);
	}
	
	@Override
	public void closeConsumerStore() {
		if (this.model != null) {
			model.close();
		}
		if (this.dataset != null) {
			dataset.close();
		}
	}

	/**
	 * Removes any properties previously associated with the consumer.
	 * 
	 * @param consumerKey
	 *            the consumer key
	 */
	protected void removeProperties(String consumerKey) {
		ResIterator i = model.listResourcesWithProperty(
				model.createProperty(CONSUMER_KEY),
				model.createLiteral(consumerKey));
		while (i.hasNext()) {
			i.next().removeProperties();
		}
	}
	
	/**
	 * Removes any properties previously associated with the consumer.
	 * 
	 * @param consumer the consumer
	 */
	protected void removeProperties(LyoOAuthConsumer consumer) {
		removeProperties(consumer.consumerKey);
	}

	protected Resource toResource(LyoOAuthConsumer consumer) {
		Resource resource = model.createResource();
		resource.addProperty(RDF.type, model.createResource(CONSUMER_RESOURCE));
		resource.addProperty(model.createProperty(CONSUMER_NAME),
				consumer.getName());
		resource.addProperty(model.createProperty(CONSUMER_KEY),
				consumer.consumerKey);
		resource.addProperty(model.createProperty(CONSUMER_SECRET),
				consumer.consumerSecret);
		resource.addProperty(model.createProperty(PROVISIONAL),
				(consumer.isProvisional()) ? "true" : "false");
		resource.addProperty(model.createProperty(TRUSTED),
				(consumer.isTrusted()) ? "true" : "false");

		return resource;
	}

	protected LyoOAuthConsumer fromResource(Resource resource) {
		String key = resource.getRequiredProperty(
				model.createProperty(CONSUMER_KEY)).getString();
		String secret = resource.getRequiredProperty(
				model.createProperty(CONSUMER_SECRET)).getString();
		LyoOAuthConsumer consumer = new LyoOAuthConsumer(key, secret);
		consumer.setName(resource.getRequiredProperty(
				model.createProperty(CONSUMER_NAME)).getString());

		String provisional = resource.getProperty(
				model.createProperty(PROVISIONAL)).getString();
		consumer.setProvisional("true".equals(provisional));

		String trusted = resource.getProperty(model.createProperty(TRUSTED))
				.getString();
		consumer.setTrusted("true".equals(trusted));

		return consumer;
	}
}
