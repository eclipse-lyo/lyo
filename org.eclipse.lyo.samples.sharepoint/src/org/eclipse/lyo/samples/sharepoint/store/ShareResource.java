/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *  
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 *     Keith Wells             - initial API and implementation
 *     Sam Padgett           - initial API and Implementation
 *     Jim Conallen           - initial API and implementation
 *
 *******************************************************************************/
package org.eclipse.lyo.samples.sharepoint.store;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.lyo.samples.sharepoint.core.IConstants;
import org.eclipse.lyo.samples.sharepoint.store.ShareValue.ShareValueType;


public class ShareResource {
	
	protected String uri;
	protected String resourceContext;
	protected List<ShareStatement> statements = new ArrayList<ShareStatement>();
	protected Map<String,ShareResource> inlinedResources = new HashMap<String,ShareResource>();
	protected ShareResource parentResource;
	
	public ShareResource(String uri) throws ShareServerException {
		this.uri = uri;
		this.resourceContext = uri;
	}

	public ShareResource(String uri, String resourceContext) throws ShareServerException {
		this.uri = uri;
		this.resourceContext = resourceContext;
	}

	public ShareResource(String uri, List<ShareStatement> statements) {
		this.uri = uri;
		this.resourceContext = uri;
		this.addStatements(statements);
	}
	
	public ShareResource(String uri, String resourceContext, List<ShareStatement> statements) {
		this.uri = uri;
		this.resourceContext = resourceContext;
		this.addStatements(statements);
	}
	
	public String getUri(){
		return uri;
	}
	
	/**
	 *  
	 * @param uri
	 * @throws ShareServerException
	 */
	public void setUri(String uri) throws ShareServerException {
		// need to re-index everything
		if( uri == null || uri.isEmpty() ) throw new ShareServerException("Resource URI must be valid or blank node");
		
		try{
			if( this.parentResource != null ) {
				// need to update dad
				this.parentResource.inlinedResources.remove(this.uri);
				this.parentResource.inlinedResources.put(uri, this);
			} else {
				// this is also a resource context change 
				this.resourceContext = uri;
				List<ShareStatement> allStatements = this.getStatements();
				for (ShareStatement rioStatement : allStatements) {
					rioStatement.setContext(uri);
				}
			}
			
			// update immediate properties
			for( ShareStatement statement : statements ) {
				statement.setSubject(uri);
				ShareValue obj = statement.getObject();
				if(  ( obj.getType() == ShareValueType.BLANK_NODE || obj.getType() == ShareValueType.URI) && 
						uri.equals(obj.stringValue()) ) {
					ShareValue updatedObj = null;
					if( uri.startsWith("http://") || uri.startsWith("https://") ) {
						updatedObj = new ShareValue(ShareValueType.URI, uri);
					} else {
						updatedObj = new ShareValue(ShareValueType.BLANK_NODE, uri);
					}
					statement.setObject(updatedObj);
				}
			}
			// now recurse into inlined resources, and update objects
			for(ShareResource rioResource : this.inlinedResources.values() ) {
				rioResource.reindexProperties(this.uri, uri);
			}
			
			
			// finally update this uri
			this.uri = uri;
			
		}catch( UnrecognizedValueTypeException e) {
			throw new ShareServerException(e);
		}
		
	}

	private void reindexProperties(String oldUri, String updatedUri) throws ShareServerException {
		try{
			
			for( ShareStatement statement : statements ) {
				ShareValue obj = statement.getObject();
				if( ( obj.getType() == ShareValueType.BLANK_NODE || obj.getType() == ShareValueType.URI) 
						&& oldUri.equals(obj.stringValue()) ) {
					ShareValue updatedObj = null;
					if( updatedUri.startsWith("http://") || updatedUri.startsWith("https://") ) {
						updatedObj = new ShareValue(ShareValueType.URI, updatedUri);
					} else {
						updatedObj = new ShareValue(ShareValueType.BLANK_NODE, updatedUri);
					}
					statement.setObject(updatedObj);
				}
			}
			
			for(ShareResource resource : this.inlinedResources.values() ) {
				resource.reindexProperties(oldUri, updatedUri);
			}
			
		}catch( UnrecognizedValueTypeException e) {
			throw new ShareServerException(e);
		}

	}
	
	public String getResourceContext(){
		return resourceContext;
	}
	
	public void setResourceContext(String resourceContext){
		// update all statements!
	}
	
	public void addStatements(List<ShareStatement> statements ) {
		this.statements.addAll(statements);
	}

	public void addStatement(ShareStatement rioStatement) {
		this.statements.add(rioStatement);
	}

	public List<ShareStatement> getStatements(){
		List<ShareStatement> allStatements = new ArrayList<ShareStatement>(this.statements);
		for (ShareResource inlinedResource : this.inlinedResources.values()) {
			allStatements.addAll( inlinedResource.getStatements());
		}
		return Collections.unmodifiableList(allStatements);
	}
	
	public List<ShareStatement> getStatements(String subject, String predicate, ShareValue value) {
		ArrayList<ShareStatement> filteredStatements = new ArrayList<ShareStatement>();
		for (ShareStatement rioStatement : this.getStatements()) { // be sure to use all child statements too
			if( subject != null && !subject.equals(rioStatement.getSubject() ) ) continue;
			if( predicate != null && !predicate.equals(rioStatement.getPredicate() ) ) continue;
			if( value != null ) {
				if( value.getType() == rioStatement.getObject().getType() ) {
					if( !value.toString().equals(rioStatement.getObject().toString()) ) continue;
				} else {
					continue;
				}
			}
			filteredStatements.add(rioStatement);
		}
		
		return filteredStatements;
		
	}

	public ShareStatement getFirstStatement(String subject, String predicate, ShareValue value) {
		for (ShareStatement rioStatement : statements) {
			if( subject != null && !subject.equals(rioStatement.getSubject() ) ) continue;
			if( predicate != null && !predicate.equals(rioStatement.getPredicate() ) ) continue;
			if( value != null ) {
				if( value.getType() == rioStatement.getObject().getType() ) {
					if( !value.toString().equals(rioStatement.getObject().toString()) ) continue;
				} else {
					continue;
				}
			}
			return rioStatement;
		}
		return null;
	}
	
	public ShareValue getFirstPropertyValue(String predicateUri) {
		ShareStatement statement = getFirstStatement(uri, predicateUri, null);
		if( statement != null ) {
			return statement.getObject();
		}
		return null;
	}


	public void addRdfType(String rdfTypeUri) throws ShareServerException {
		addProperty(IConstants.RDF_TYPE, ShareValueType.URI, rdfTypeUri, false);
	}
	
	public boolean isRdfType(String rdfTypeUri) throws ShareServerException {
		List<ShareStatement> rdftypes = this.getStatements(uri, IConstants.RDF_TYPE, null);
		for (ShareStatement rioStatement : rdftypes) {
			ShareValue obj = rioStatement.getObject();
			if( rdfTypeUri.equals(obj.stringValue())) return true;
		}
		return false;
	}

	private void insertInlinedResource( String propertyUri, ShareResource rioResource ) throws ShareServerException {
		try{
			String bn = null;
			if( this.uri.startsWith("_:") ) { 
				bn = uri + '_' + inlinedResources.size();
			} else {
				bn = "_:bn" +  + inlinedResources.size();
			}
			ShareValue blankNode = ShareValue.createBlankNodeValue(bn);
			ShareStatement statement = new ShareStatement(uri, propertyUri, blankNode, uri);
			statements.add(statement);
			rioResource.setUri(bn);
			rioResource.setResourceContext(this.resourceContext);
			inlinedResources.put(blankNode.stringValue(),rioResource);
			rioResource.parentResource = this;
		} catch ( Exception e ) {
			throw new ShareServerException(e);
		}
	}

	
	
	public ShareResource createInlinedResource( String propertyUri, String typeUri ) throws ShareServerException {
		try{
			String bn = null;
			if( this.uri.startsWith("_:") ) { 
				bn = uri + '_' + inlinedResources.size();
			} else {
				bn = "_:bn" +  + inlinedResources.size();
			}
			ShareValue blankNode = ShareValue.createBlankNodeValue(bn);
			ShareStatement statement = new ShareStatement(uri, propertyUri, blankNode, uri);
			statements.add(statement);
			ShareResource resource = new ShareResource(blankNode.stringValue(), uri);
			inlinedResources.put(blankNode.stringValue(),resource);
			if( typeUri != null ) {
				ShareValue val = ShareValue.createUriValue(typeUri);
				resource.createProperty(IConstants.RDF_TYPE, val, true);
			}
			resource.parentResource = this;
			return resource;
		} catch ( Exception e ) {
			throw new ShareServerException(e);
		}
	}

	public ShareResource getFirstInlinedResource( String propertyUri ) throws ShareServerException {
		ShareStatement statement = getFirstStatement(uri, propertyUri, null);
		if( statement != null ) {
			ShareValue obj = statement.getObject();
			if( obj.getType() == ShareValueType.BLANK_NODE ) {
				return getInlinedResource(obj.toString());
			}
		}
		return null;
	}
	
	@SuppressWarnings("unchecked")
	public <T> List<T> getInlinedResources( String propertyUri ) throws ShareServerException {
		List<T> inlinedResources = new ArrayList<T>();
		List<ShareStatement> inlinedStatements = getStatements(uri, propertyUri, null);
		for (ShareStatement rioStatement : inlinedStatements) {
			ShareValue obj = rioStatement.getObject();
			inlinedResources.add((T) getInlinedResource(obj.toString()));
		}
		return inlinedResources;
	}

	
	public ShareResource getInlinedResource( String bnodeID ) throws ShareServerException {
		return this.inlinedResources.get(bnodeID);
	}
	
	public void removeInlinedResource(String bnodeId) throws ShareServerException {
		this.inlinedResources.remove(bnodeId);
	} 
	
	
	/**
	 * change owner of the resource to this one 
	 * @param propertyUri
	 * @param rioResource
	 * @param typeUri
	 * @throws ShareServerException 
	 */
	public void setInlinedResource(String propertyUri, ShareResource rioResource) throws ShareServerException {
		ShareResource oldResource = this.getFirstInlinedResource(propertyUri);
		if(oldResource != null ) {
			// remove it
			String bnode = oldResource.getUri();
			this.removeInlinedResource(bnode);
		} 
		insertInlinedResource(propertyUri, rioResource);
	}

	// String
	
	public void setStringProperty( String propertyUri, String value) throws ShareServerException {
		try {
			ShareValue lyoVal = ShareValue.createStringValue(value);
			this.createProperty(propertyUri, lyoVal, true);
		} catch (Exception e) {
			throw new ShareServerException(e);
		}
	}
	
	public void addStringProperty( String propertyUri, String value) throws ShareServerException {
		try {
			ShareValue lyoVal = ShareValue.createStringValue(value);
			this.createProperty(propertyUri, lyoVal, false);
		} catch (Exception e) {
			throw new ShareServerException(e);
		}
	}

	public boolean hasStringProperty(String propertyUri ) {
		return hasPropertyOfType(propertyUri, ShareValueType.STRING);
	}
	
	public String getFirstStringProperty(String propertyUri ) {
		List<ShareStatement> strStatements = this.getStatements(this.uri, propertyUri, null);
		for (ShareStatement rioStatement : strStatements) {
			ShareValue val = rioStatement.getObject();
			if( val.getType().equals(ShareValueType.STRING)) {
				return val.stringValue();
			}
		}
		return null;
	}
	
	public Collection<String> getStringProperties(String propertyUri ) {
		List<String> propertyValues = new ArrayList<String>(); 
		List<ShareStatement> strStatements = this.getStatements(this.uri, propertyUri, null);
		for (ShareStatement rioStatement : strStatements) {
			ShareValue val = rioStatement.getObject();
			if( val.getType().equals(ShareValueType.STRING)) {
				propertyValues.add(val.stringValue());
			}
		}
		return propertyValues;
	}

	// Integer
	
	public void setIntegerProperty( String propertyUri, int value) throws ShareServerException {
		try {
			ShareValue lyoVal = ShareValue.createIntegerValue(value);
			this.createProperty(propertyUri, lyoVal, true);
		} catch (Exception e) {
			throw new ShareServerException(e);
		}
	}

	public void addIntegerProperty( String propertyUri, int value) throws ShareServerException {
		try {
			ShareValue lyoVal = ShareValue.createIntegerValue(value);
			this.createProperty(propertyUri, lyoVal, false);
		} catch (Exception e) {
			throw new ShareServerException(e);
		}
	}
	
	public boolean hasIntegerProperty(String propertyUri ) {
		return hasPropertyOfType(propertyUri, ShareValueType.INTEGER);
	}
	
	public Integer getFirstIntegerProperty(String propertyUri ) throws ShareServerException {
		List<ShareStatement> strStatements = this.getStatements(this.uri, propertyUri, null);
		for (ShareStatement rioStatement : strStatements) {
			ShareValue val = rioStatement.getObject();
			if( val.getType().equals(ShareValueType.INTEGER)) {
				try {
					return val.intValue();
				} catch (IncompatibleValueException e) {
					throw new ShareServerException(e);
				}
			}
		}
		return null;
	}
	
	public Collection<Integer> getIntegerProperties(String propertyUri ) {
		List<Integer> propertyValues = new ArrayList<Integer>(); 
		List<ShareStatement> strStatements = this.getStatements(this.uri, propertyUri, null);
		for (ShareStatement rioStatement : strStatements) {
			ShareValue val = rioStatement.getObject();
			if( val.getType().equals(ShareValueType.INTEGER)) {
				try {
					propertyValues.add(val.intValue());
				} catch (IncompatibleValueException e) {
					// log this?  don't abort since the client probably interested only in the ints that are working
				}
			}
		}
		return propertyValues;
	}
	
	// Decimal
	
	public void setDecimalProperty( String propertyUri, double value) throws ShareServerException {
		try {
			ShareValue lyoVal = ShareValue.createDecimalValue(value);
			this.createProperty(propertyUri, lyoVal, true);
		} catch (Exception e) {
			throw new ShareServerException(e);
		}
	}

	public void addDecimalProperty( String propertyUri, double value) throws ShareServerException {
		try {
			ShareValue lyoVal = ShareValue.createDecimalValue(value);
			this.createProperty(propertyUri, lyoVal, false);
		} catch (Exception e) {
			throw new ShareServerException(e);
		}
	}

	public boolean hasDecimalProperty(String propertyUri ) {
		return hasPropertyOfType(propertyUri, ShareValueType.DECIMAL);
	}
	
	public Double getFirstDecimalProperty(String propertyUri ) throws ShareServerException {
		List<ShareStatement> strStatements = this.getStatements(this.uri, propertyUri, null);
		for (ShareStatement rioStatement : strStatements) {
			ShareValue val = rioStatement.getObject();
			if( val.getType().equals(ShareValueType.DECIMAL)) {
				try {
					return val.doubleValue();
				} catch (IncompatibleValueException e) {
					throw new ShareServerException(e);
				}
			}
		}
		return null;
	}
	
	public Collection<Double> getDecimalProperties(String propertyUri ) {
		List<Double> propertyValues = new ArrayList<Double>(); 
		List<ShareStatement> strStatements = this.getStatements(this.uri, propertyUri, null);
		for (ShareStatement rioStatement : strStatements) {
			ShareValue val = rioStatement.getObject();
			if( val.getType().equals(ShareValueType.DECIMAL)) {
				try {
					propertyValues.add(val.doubleValue());
				} catch (IncompatibleValueException e) {
					// log this?  don't abort since the client probably interested only in the ints that are working
				}
			}
		}
		return propertyValues;
	}

	// Date
	
	public void setDateProperty( String propertyUri, Date value) throws ShareServerException {
		try {
			ShareValue lyoVal = ShareValue.createCalendarValue(value);
			this.createProperty(propertyUri, lyoVal, true);
		} catch (Exception e) {
			throw new ShareServerException(e);
		}
	}
	
	public void addDateProperty( String propertyUri, Date value) throws ShareServerException {
		try {
			ShareValue lyoVal = ShareValue.createCalendarValue(value);
			this.createProperty(propertyUri, lyoVal, false);
		} catch (Exception e) {
			throw new ShareServerException(e);
		}
	}

	public boolean hasDateProperty(String propertyUri ) {
		return hasPropertyOfType(propertyUri, ShareValueType.CALENDAR);
	}
	
	public Date getFirstDateProperty(String propertyUri ) throws ShareServerException {
		List<ShareStatement> strStatements = this.getStatements(this.uri, propertyUri, null);
		for (ShareStatement rioStatement : strStatements) {
			ShareValue val = rioStatement.getObject();
			if( val.getType().equals(ShareValueType.CALENDAR)) {
				try {
					return val.dateValue();
				} catch (IncompatibleValueException e) {
					throw new ShareServerException(e);
				}
			}
		}
		return null;
	}
	
	public Collection<Date> getDateProperties(String propertyUri ) {
		List<Date> propertyValues = new ArrayList<Date>(); 
		List<ShareStatement> strStatements = this.getStatements(this.uri, propertyUri, null);
		for (ShareStatement rioStatement : strStatements) {
			ShareValue val = rioStatement.getObject();
			if( val.getType().equals(ShareValueType.CALENDAR)) {
				try {
					propertyValues.add(val.dateValue());
				} catch (IncompatibleValueException e) {
					// log this?  don't abort since the client probably interested only in the ints that are working
				}
			}
		}
		return propertyValues;
	}

	// URI 
	
	public void setUriProperty( String propertyUri, String value) throws ShareServerException {
		try {
			ShareValue lyoVal = ShareValue.createUriValue(value);
			this.createProperty(propertyUri, lyoVal, true);
		} catch (Exception e) {
			throw new ShareServerException(e);
		}
	}

	public void addUriProperty( String propertyUri, String value) throws ShareServerException {
		try {
			ShareValue lyoVal = ShareValue.createUriValue(value);
			this.createProperty(propertyUri, lyoVal, false);
		} catch (Exception e) {
			throw new ShareServerException(e);
		}
	}

	public void removeUriProperty( String propertyUri, String uri) throws ShareServerException {
		try {
			ShareValue value = ShareValue.createUriValue(uri);
			ShareStatement stmt = this.getFirstStatement(getUri(), propertyUri, value);
			if( stmt != null ) {
				statements.remove(stmt);
			}
		} catch (Exception e) {
			throw new ShareServerException(e);
		}
	}

	public boolean hasUriProperty(String propertyUri ) {
		return hasPropertyOfType(propertyUri, ShareValueType.URI);
	}
	
	public String getFirstUriProperty(String propertyUri ) {
		List<ShareStatement> strStatements = this.getStatements(this.uri, propertyUri, null);
		for (ShareStatement rioStatement : strStatements) {
			ShareValue val = rioStatement.getObject();
			if( val.getType().equals(ShareValueType.URI)) {
				return val.stringValue();
			}
		}
		return null;
	}
	
	public List<String> getUriProperties(String propertyUri ) {
		List<String> propertyValues = new ArrayList<String>(); 
		List<ShareStatement> strStatements = this.getStatements(this.uri, propertyUri, null);
		for (ShareStatement rioStatement : strStatements) {
			ShareValue val = rioStatement.getObject();
			if( val.getType().equals(ShareValueType.URI)) {
				propertyValues.add(val.stringValue());
			}
		}
		return propertyValues;
	}

	// Boolean
	
	public void setBooleanProperty( String propertyUri, boolean value) throws ShareServerException {
		try {
			ShareValue lyoVal = ShareValue.createBooleanValue(value);
			this.createProperty(propertyUri, lyoVal, true);
		} catch (Exception e) {
			throw new ShareServerException(e);
		}
	}

	public void addBooleanProperty( String propertyUri, boolean value) throws ShareServerException {
		try {
			ShareValue lyoVal = ShareValue.createBooleanValue(value);
			this.createProperty(propertyUri, lyoVal, false);
		} catch (Exception e) {
			throw new ShareServerException(e);
		}
	}

	public boolean hasBooleanProperty(String propertyUri ) {
		return hasPropertyOfType(propertyUri, ShareValueType.BOOLEAN);
	}
	
	public boolean getFirstBooleanProperty(String propertyUri, boolean defaultValue ) {
		List<ShareStatement> strStatements = this.getStatements(this.uri, propertyUri, null);
		for (ShareStatement rioStatement : strStatements) {
			ShareValue val = rioStatement.getObject();
			if( val.getType().equals(ShareValueType.BOOLEAN)) {
				try {
					return val.booleanValue();
				} catch (IncompatibleValueException e) {
					// log this?  it should never happen
				}
			}
		}
		return defaultValue;
	}
	
	public Collection<Boolean> getBooleanProperties(String propertyUri ) {
		List<Boolean> propertyValues = new ArrayList<Boolean>(); 
		List<ShareStatement> strStatements = this.getStatements(this.uri, propertyUri, null);
		for (ShareStatement rioStatement : strStatements) {
			ShareValue val = rioStatement.getObject();
			if( val.getType().equals(ShareValueType.BOOLEAN)) {
				try {
					propertyValues.add(val.booleanValue());
				} catch (IncompatibleValueException e) {
					// log this?  it should never happen
				}
			}
		}
		return propertyValues;
	}


	// Common property methods
	
	protected void addProperty(String propertyUri, ShareValueType valType, Object value, boolean exclusive) 
			throws ShareServerException {
		createProperty(this.uri, propertyUri, valType, value, exclusive);
	}

	protected void createProperty(String subjectUri, String propertyUri, ShareValueType valType, Object value, boolean exclusive)
			throws ShareServerException {
		ShareValue rioValue = null;
		try {
			rioValue = new ShareValue(valType, value);
		} catch (UnrecognizedValueTypeException e) {
			// should NEVER happen
			throw new ShareServerException(e);
		}
		createProperty(subjectUri, propertyUri, rioValue, exclusive);
	}

	protected void createProperty(String propertyUri, ShareValue value, boolean exclusive) throws ShareServerException {
		createProperty(this.uri, propertyUri, value, exclusive);
	}

	protected void createProperty(String subjectUri, String propertyUri, ShareValue value, boolean exclusive) throws ShareServerException {
		List<ShareStatement> statements = this.getStatements(subjectUri, propertyUri, null);
		if ((exclusive || value == null)) {
			for (ShareStatement rioStatement : statements) {
				this.statements.remove(rioStatement);
			}
		}

		if (value == null || value.getValue() == null)
			return; // our work is finished

		ShareStatement statement = new ShareStatement(subjectUri, propertyUri, value, resourceContext);
		this.statements.add(statement);
	}
	
	public boolean hasPropertyOfType( String propertyUri, ShareValueType type) {
		List<ShareStatement> strStatements = this.getStatements(this.uri, propertyUri, null);
		for (ShareStatement rioStatement : strStatements) {
			ShareValue val = rioStatement.getObject();
			if( val.getType().equals(type)) {
				return true;
			}
		}
		return false;
	}
	

	
//
//	 Ordered list methods (rdf sequences)
//	
	
	public void appendToSeq(String propertyUri, ShareValue value) throws ShareServerException {
		if (value == null)
			return; // our work is finished
		String propName = extractLastSegment(propertyUri);
		ShareStatement statement = null;
		List<ShareStatement> statements = this.getStatements(null, propertyUri,
				null);
		if (statements.size() == 0) {
			// create it
			try {
				ShareValue bnVal = new ShareValue(ShareValueType.BLANK_NODE, propName);
				statement = new ShareStatement(uri, propertyUri, bnVal, resourceContext);
				this.addStatement(statement);
				ShareValue rdfSeqVal = new ShareValue(ShareValueType.URI,
						IConstants.RDF_SEQ);
				ShareStatement collTypeStatement = new ShareStatement(propName,
						IConstants.RDF_TYPE, rdfSeqVal, resourceContext);
				this.addStatement(collTypeStatement);
			} catch (Exception e) {
				throw new ShareServerException(e);
			}
		} else {
			statement = statements.get(0);
		}

		// now that we have the blank node
		String bNodeId = statement.getObject().stringValue();
		// get all other elements in this collection
		statements = this.getStatements(bNodeId, null, null);
		int count = statements.size() - 1; // -1 to subtract the rdf:type from the list

		statement = new ShareStatement(bNodeId, IConstants.RDF_NAMESPACE + "_" + (count + 1), value, uri);
		this.statements.add(statement);
	}

	public List<ShareValue> getSeq(String propertyUri) throws ShareServerException {
		List<ShareValue> seqValues = new ArrayList<ShareValue>();
		String propName = extractLastSegment(propertyUri);
		List<ShareStatement> statements = this
				.getStatements(propName, null, null);

		// TODO: sort them by predicate
		for (ShareStatement rioStatement : statements) {
			ShareValue obj = rioStatement.getObject();
			String pred = rioStatement.getPredicate();
			if (pred.startsWith("http://www.w3.org/1999/02/22-rdf-syntax-ns#_")) { 
				seqValues.add(obj);
			}
		}
		return seqValues;
	}

	public void removeSeq(String propertyUri) throws ShareServerException {
		List<ShareStatement> statements = this.getStatements(null, propertyUri,
				null);
		for (ShareStatement rioStatement : statements) {
			List<ShareStatement> childStatements = this.getStatements(
					rioStatement.getObject().stringValue(), null, null);
			this.statements.removeAll(childStatements);
		}
		this.statements.removeAll(statements);
	}

	protected String extractLastSegment(String uri) {
		int pos = uri.lastIndexOf('#');
		if( pos > 0 ) {
			return uri.substring(pos + 1);
		}
		pos = uri.lastIndexOf('/');
		return uri.substring(pos + 1);
	}
	
	public String dumpNTriples(){
		StringBuffer sb = new StringBuffer();
		List<ShareStatement> allStatements = this.getStatements();
		for (ShareStatement rioStatement : allStatements) {
			sb.append( rioStatement.toString() + " .\n");
		}
		
		return sb.toString();
	}

}
