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
 *     Sam Padgett - initial API and implementation
 *     Michael Fiedler - updates to use file persistence
 *******************************************************************************/
package org.eclipse.lyo.server.oauth.consumerstore;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.sql.SQLException;

import org.apache.commons.codec.binary.Base64;

import org.eclipse.lyo.server.oauth.core.consumer.AbstractConsumerStore;
import org.eclipse.lyo.server.oauth.core.consumer.ConsumerStoreException;
import org.eclipse.lyo.server.oauth.core.consumer.LyoOAuthConsumer;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.ResIterator;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.shared.JenaException;
import com.hp.hpl.jena.shared.PropertyNotFoundException;
import com.hp.hpl.jena.util.FileManager;
import com.hp.hpl.jena.util.FileUtils;
import com.hp.hpl.jena.vocabulary.RDF;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A simple RDF consumer store backed by an XML file on the filesystem.
 * 
 * NOTE: The shared consumer secret is stored as Base64 and is only obfuscated, not encrypted.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
public class FileSystemConsumerStore extends AbstractConsumerStore {
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

	private final static Logger log = LoggerFactory.getLogger(FileSystemConsumerStore.class);

	private Model model;
	private String oauthStore;

	
	public FileSystemConsumerStore(String oauthStoreRoot) throws SQLException, ConsumerStoreException,
			ClassNotFoundException {
		this.oauthStore = oauthStoreRoot;
		createModel();
		loadConsumers();
	}
	
	public FileSystemConsumerStore(Model model, String oauthStoreRoot) throws ConsumerStoreException {
		this.oauthStore = oauthStoreRoot;
		this.model = model;
		loadConsumers();
	}

	protected void createModel() {
		try {
			model = FileManager.get().loadModel(this.oauthStore);
		} catch (Exception e) {
			model = ModelFactory.createDefaultModel();
		}
	}

	protected void writeModel() throws FileNotFoundException {
		Model writeModel = ModelFactory.createDefaultModel();
		writeModel.add(this.model);
		OutputStream os = new FileOutputStream(oauthStore);
		writeModel.write(os, FileUtils.langXMLAbbrev);
	}
	
	protected synchronized void loadConsumers() throws ConsumerStoreException {
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
			} catch (UnsupportedEncodingException ue) {
				throw new ConsumerStoreException(ue);
			} catch (JenaException e) {
				// Some other runtime exception occurred.
				throw new ConsumerStoreException(e);
			}
		}

	}

	@Override
	public synchronized LyoOAuthConsumer addConsumer(final LyoOAuthConsumer consumer)
			throws ConsumerStoreException {
		if (model == null) {
			throw new ConsumerStoreException("Consumer store not initialized.");
		}
		try {

			removeProperties(consumer);
			toResource(consumer);
			
			LyoOAuthConsumer retConsumer = add(consumer);			
			writeModel();			
			return retConsumer;
			
		} catch (UnsupportedEncodingException ue) {
			throw new ConsumerStoreException(ue);	
		} catch (FileNotFoundException fe) {
			throw new ConsumerStoreException(fe);
		} 
	}

	@Override
	public synchronized LyoOAuthConsumer removeConsumer(final String consumerKey)
			throws ConsumerStoreException {
		if (model == null) {
			throw new ConsumerStoreException("Consumer store not initialized.");
		}
		
		try {
			
			removeProperties(consumerKey);
			LyoOAuthConsumer retConsumer = remove(consumerKey);

			writeModel();
			return retConsumer;
			
		} catch (FileNotFoundException fe) {
			throw new ConsumerStoreException(fe);
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
		try {
			writeModel();
		} catch (Exception e) {
			log.error("Error finalizing model to disk");
		}
		
		this.model.close();
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

	protected Resource toResource(LyoOAuthConsumer consumer) throws UnsupportedEncodingException {
		Resource resource = model.createResource();
		resource.addProperty(RDF.type, model.createResource(CONSUMER_RESOURCE));
		resource.addProperty(model.createProperty(CONSUMER_NAME),
				consumer.getName());
		resource.addProperty(model.createProperty(CONSUMER_KEY),
				consumer.consumerKey);
		
		String encodedSecret = new String(Base64.encodeBase64(consumer.consumerSecret.getBytes("UTF8")),"UTF8");
		resource.addProperty(model.createProperty(CONSUMER_SECRET),
				encodedSecret);
		
		resource.addProperty(model.createProperty(PROVISIONAL),
				(consumer.isProvisional()) ? "true" : "false");
		resource.addProperty(model.createProperty(TRUSTED),
				(consumer.isTrusted()) ? "true" : "false");

		return resource;
	}

	protected LyoOAuthConsumer fromResource(Resource resource) throws UnsupportedEncodingException {
		String key = resource.getRequiredProperty(
				model.createProperty(CONSUMER_KEY)).getString();
		
		String encodedSecret = resource.getRequiredProperty(
				model.createProperty(CONSUMER_SECRET)).getString();
		String secret = new String(Base64.decodeBase64(encodedSecret.getBytes("UTF8")),"UTF8");
		
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
