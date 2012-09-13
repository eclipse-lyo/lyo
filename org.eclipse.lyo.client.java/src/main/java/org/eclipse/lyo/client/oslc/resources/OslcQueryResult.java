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
 *     Sean Kennedy     - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.resources;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;

import org.apache.wink.client.ClientResponse;
import org.eclipse.lyo.client.oslc.OSLCConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.ResIterator;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Selector;
import com.hp.hpl.jena.rdf.model.SimpleSelector;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;


/**
 * The results of an OSLC query. If the query was paged, subsequent pages can be retrieved using the Iterator interface.
 * 
 * This class is not currently thread safe.
 */
public class OslcQueryResult implements Iterator<OslcQueryResult> {

	private final OslcQuery query;

	private final ClientResponse response;
	
	private final int pageNumber;
	
	private Model rdfModel;
	
	private Resource infoResource, membersResource; 
	
	private String nextPageUrl;
	
	private boolean rdfInitialized = false;
	
	public OslcQueryResult(OslcQuery query, ClientResponse response) {
		this.query = query;
		this.response = response;
		
		this.pageNumber = 1;
		
		
	}
	
	private OslcQueryResult(OslcQueryResult prev) {
		this.query = new OslcQuery(prev);
		this.response = this.query.getResponse();
		
		this.pageNumber = prev.pageNumber + 1;
		
	}
	
	private synchronized void initializeRdf() {
		if (!rdfInitialized) {
			rdfInitialized = true;
			rdfModel = ModelFactory.createDefaultModel();
			rdfModel.read(response.getEntity(InputStream.class), query.getCapabilityUrl());
			
			//Find a resource with rdf:type of oslc:ResourceInfo
			Property rdfType = rdfModel.createProperty(OslcConstants.RDF_NAMESPACE, "type");
			Property responseInfo = rdfModel.createProperty(OslcConstants.OSLC_CORE_NAMESPACE, "ResponseInfo");
			ResIterator iter = rdfModel.listResourcesWithProperty(rdfType, responseInfo);
			
			//There should only be one - take the first
			infoResource = null;
			while (iter.hasNext()) {
				infoResource = iter.next();
				break;
			}
			membersResource = rdfModel.getResource(query.getCapabilityUrl());
		}
	}
	
	String getNextPageUrl() {
		initializeRdf();
		if (nextPageUrl == null && infoResource != null) {
			Property predicate = rdfModel.getProperty(OslcConstants.OSLC_CORE_NAMESPACE, "nextPage");
			Selector select = new SimpleSelector(infoResource, predicate, (RDFNode) null);
			StmtIterator iter = rdfModel.listStatements(select);
			if (iter.hasNext()) {
				Statement nextPage = iter.next();
				nextPageUrl = nextPage.getResource().getURI();
			} else {
				nextPageUrl = "";
			}
		}
		return nextPageUrl;
	}
		
	/**
	 * @return whether there is another page of results after this
	 */
	public boolean hasNext() {
		return (!"".equals(getNextPageUrl()));
	}

	/**
	 * @return the next page of results
	 * @throws NoSuchElementException if there is no next page
	 */
	public OslcQueryResult next() {
		return new OslcQueryResult(this);
	}

	/**
	 * @throws UnsupportedOperationException always
	 */
	public void remove() {
		throw new UnsupportedOperationException();
	}
	
	public OslcQuery getQuery() {
		return query;
	}
	
	/**
	 * Get the raw Wink client response to a query.  
	 * 
	 * NOTE:  Using this method and consuming the response will make other methods
	 * which examine the response unavailable (Examples:  getMemberUrls(), next() and hasNext()).
	 * When this method is invoked, the consumer is responsible for OSLC page processing
	 * 
	 * @return
	 */
	public ClientResponse getRawResponse() {
		return response;
	}

	/**
	 * Return the subject URLs of the query response.  The URLs are the location of all artifacts 
	 * which satisfy the query conditions.
	 * 
	 * NOTE:  Using this method consumes the query response and makes other methods
	 * which examine the response unavailable (Example: getRawResponse(). 
	 * @return
	 */
	public String[] getMembersUrls() {
		initializeRdf();
		ArrayList<String> membersUrls = new ArrayList<String>();
		Selector select = new SimpleSelector(membersResource, (Property) null, (RDFNode) null);
		StmtIterator iter = rdfModel.listStatements(select);
		while (iter.hasNext()) {
			Statement member = iter.next();
			try {
				membersUrls.add(member.getResource().getURI());
			} catch (Throwable t) {
				//FIXME
				System.err.println("Member was not a resource");
			}
		}
		return membersUrls.toArray(new String[membersUrls.size()]);
	}
}
