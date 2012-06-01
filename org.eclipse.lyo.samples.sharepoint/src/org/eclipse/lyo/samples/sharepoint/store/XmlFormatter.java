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
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;


import org.eclipse.lyo.samples.sharepoint.core.IConstants;
import org.eclipse.lyo.samples.sharepoint.store.ShareValue.ShareValueType;
import org.eclipse.lyo.samples.sharepoint.util.XmlUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class XmlFormatter {
	
	private ShareResource resource = null;
	private Map<String,String> namespacePrefixes = new HashMap<String,String>();
	private Map<String,Element> otherStatementIds = new HashMap<String,Element>();
	private Map<String,Element> idElm = new HashMap<String,Element>();
	private Map<String,Element> blankNodes = new HashMap<String,Element>();
	private Document document;
	private static final String RDF_LI_PREFIX = "http://www.w3.org/1999/02/22-rdf-syntax-ns#_";
	private static int RDF_LI_PREFIX_LEN = RDF_LI_PREFIX.length(); 
	
	static public String formatResource(ShareResource resource, String rdfType ) throws ShareServerException {
		XmlFormatter formatter = new XmlFormatter();
		return formatter.format(resource, rdfType);
	}
	
	public void addNamespacePrefix(String ns, String prefix) {
		this.namespacePrefixes.put(ns, prefix);
	}
	
	public String format(ShareResource resource, String rdfType ) throws ShareServerException {
		this.resource = resource;

		if( rdfType != null && !resource.isRdfType(rdfType) ) {
			throw new ShareServerException("Cannot format resource (" + resource.getUri() + ")to XML with type " + rdfType);
		}
		
		initPrefixes();
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setValidating(true);
		factory.setNamespaceAware(true);
		try{
			DocumentBuilder builder = factory.newDocumentBuilder();
			document = builder.newDocument();
			Element rdf = document.createElementNS(IConstants.RDF_NAMESPACE, IConstants.RDF_TYPE_PTERM_RDF);
			document.appendChild(rdf);
			rdf.setAttribute(IConstants.XML_BASE, resource.getUri());
			
			String[] nlp;
			if( rdfType == null  ){
				nlp = new String[]{ IConstants.RDF_NAMESPACE, IConstants.RDF_TYPE_TERM_DESCRIPTION, IConstants.RDF_PREFIX };	
			} else {
				nlp = this.extractNsLocal(rdfType);
			}
			
			
			Element resRoot = document.createElementNS(nlp[0], nlp[2] + ':' + nlp[1] );
			rdf.appendChild(resRoot);
			resRoot.setAttributeNS(IConstants.RDF_NAMESPACE, IConstants.RDF_PTERM_ABOUT, resource.getUri());

			List<ShareStatement> statements = resource.getStatements();
			
			// find all reified statements in a first pass of statements.  Looking for statement with type rdf:statement 
			// these indicate a link with an anchor, so we should deal with it specially
			Iterator<ShareStatement> i = statements.iterator();
			while( i.hasNext() ) {
				// rdfTypePredicate, rdfStatementPredicate, false, oslcResource.getResource());
				ShareStatement statement = i.next();
				String predicate = statement.getPredicate();
				ShareValue object = statement.getObject();
				
				if( IConstants.RDF_TYPE_STATEMENT.equals(object.stringValue()) && IConstants.RDF_TYPE.equals(predicate) ) {
					createOtherDescription(statement, resource, rdf);
				}
				
				if( object.isBlankNode() ) {
					// find what type this is pointing to.  If none use rdf:Description
					Element elm = null;
					List<ShareStatement> stmts = resource.getStatements(object.stringValue(), IConstants.RDF_TYPE, null);
					if( stmts.isEmpty() ) {
						elm = document.createElementNS(IConstants.RDF_NAMESPACE, IConstants.RDF_TYPE_PTERM_DESCRIPTION);
					} else {
						String typeUri = stmts.get(0).getObject().stringValue(); 
						nlp = this.extractNsLocal(typeUri);
						elm = document.createElementNS(nlp[0], nlp[2] + ':' + nlp[1]);
					}
					
					this.blankNodes.put(statement.getObject().stringValue(), elm);
				}
			}
			
			//now get the rest of the statements
			for (ShareStatement statement : statements) {
				String subject = statement.getSubject();
				String predicate = statement.getPredicate();
				
				if( subject.equals(resource.getUri()) ) {
					// normal first level property
					String nsLocal[] = extractNsLocal(predicate);
					String ns = nsLocal[0]; 
					String local = nsLocal[1]; 
					String prefix = nsLocal[2];
					
					Element elm = document.createElementNS(ns, prefix + ':' + local);
					resRoot.appendChild(elm);
					ShareValue value = statement.getObject();
					setValue(elm, value);
					
					String predObj = extractPredObj(statement);
					Element descrElm = this.otherStatementIds.get(predObj);
					if( descrElm != null ) {
						String id = descrElm.getAttributeNS(IConstants.RDF_NAMESPACE, IConstants.RDF_TERM_ABOUT);
						if( id.length()>1 ) { 
							elm.setAttributeNS(IConstants.RDF_NAMESPACE, IConstants.RDF_TYPE_PTERM_ID, id.substring(1));
						}
					}
				} else if( statement.isBNode() ) {
					Element elm = this.blankNodes.get(subject);
					if( elm != null && !predicate.toString().equals(IConstants.RDF_TYPE) ) {
						Element elm2 = document.createElementNS(IConstants.RDF_NAMESPACE, IConstants.RDF_PTERM_LI);
						elm.appendChild(elm2);
						ShareValue value = statement.getObject();
						setValue(elm2, value);
					}
				} else {
					if( !isReifiedStatementProperty(predicate) ) {
						// now put it in the right description
						String strSubj = subject.toString();
						String id = extractUrlFragment(strSubj);
						Element descrElm = this.idElm.get(id);
						if( descrElm != null ) {
							String[] nsLocal = extractNsLocal(predicate);
							String ns = nsLocal[0];
							String local = nsLocal[1];
							String prefix = nsLocal[2];
							Element elm = document.createElementNS(ns, prefix + ':' + local);
							ShareValue value = statement.getObject();
							setValue(elm, value);
							descrElm.appendChild(elm);
						}
					}
				}
			}
			
			// set namespaces and prefixes
			setNamespaces(rdf);
			return XmlUtils.prettyPrint(document);
			
		} catch( Exception e ) {
			throw new ShareServerException(e);
		}
	}
	
	private Element constructInlinedElement(String blankNodeSubject) throws ShareServerException {
		String type; 
		List<ShareStatement> stmts = resource.getStatements(blankNodeSubject, IConstants.RDF_TYPE, null);
		if( stmts.isEmpty() ) {
			type = IConstants.RDF_TYPE_DESCRIPTION;
		} else {
			type = stmts.get(0).getObject().stringValue();
		}
		
		String[] nlp = this.extractNsLocal(type);
		Element inlinedElm = document.createElementNS(nlp[0], nlp[2] + ':' + nlp[1]);
		
		stmts = resource.getStatements(blankNodeSubject, null, null);

		if( IConstants.RDF_TYPE_SEQ.equals(type) || IConstants.RDF_TYPE_SEQ.equals(type) ) {
			// we need to order all the children
			Collections.sort(stmts, new Comparator<ShareStatement>(){
				@Override
				public int compare(ShareStatement s1, ShareStatement s2) {
					int i1 = 0;
					int i2 = 0;
					String p1 = s1.getPredicate();
					if( p1.startsWith(RDF_LI_PREFIX) ) {
						try{
							i1 = Integer.parseInt(p1.substring(RDF_LI_PREFIX_LEN) );
						}catch( NumberFormatException e ){}
					}  
					String p2 = s2.getPredicate();
					if( p2.startsWith(RDF_LI_PREFIX) ) {
						try{
							i2 = Integer.parseInt(p2.substring(RDF_LI_PREFIX_LEN) );
						}catch( NumberFormatException e ){}
					}  
					return i1-i2;
				}});
		} 
		for (ShareStatement statement : stmts) {
			String predicate = statement.getPredicate(); 
			if( !IConstants.RDF_TYPE.equals(predicate) ) {
				nlp = extractNsLocal(predicate);
				if( IConstants.RDF_TYPE_SEQ.equals(type) || IConstants.RDF_TYPE_SEQ.equals(type) ) {
					Element childElm = document.createElementNS(IConstants.RDF_NAMESPACE, IConstants.RDF_PTERM_LI );
					setValue(childElm, statement.getObject());
					inlinedElm.appendChild(childElm);
				} else {
					Element childElm = document.createElementNS(nlp[0], nlp[2] + ':' + nlp[1]);
					setValue(childElm, statement.getObject());
					inlinedElm.appendChild(childElm);
				}
			}
		}
		return inlinedElm;
	}
	
	/**
	 * 
	 * For the given URI a string array is returned with the namespace of the URI in the first
	 * spot, the local value in the second and the namespace prefx to use in the third.
	 * @param uri
	 * @return
	 */
	private String[] extractNsLocal(String uri) {
		int pos = uri.lastIndexOf('#');
		if( pos < 0 ) {
			pos = uri.lastIndexOf('/');
		}
		String namespace = uri.substring(0,pos+1);
		String local = uri.substring(pos+1);
		String prefix = this.getPrefix(namespace);
		return new String[] { namespace, local, prefix };
	}

	private void createOtherDescription(ShareStatement statementStatement, ShareResource resource, Element rdf) throws ShareServerException{
		Document doc = rdf.getOwnerDocument();

		// get subject and use to get all the other statement in the reification
		String subject = statementStatement.getSubject();
		List<ShareStatement> preds = resource.getStatements(subject, IConstants.RDF_PREDICATE, null);
		if( preds.size() == 0 ) return; // nothing to do, not a full reification, maybe we could log this.
		ShareStatement stPred = preds.get(0);
		
		List<ShareStatement> objs = resource.getStatements(subject, IConstants.RDF_OBJECT, null);
		if( objs.size() == 0 ) return; // nothing to do, not a full reification, maybe we could log this.
		ShareStatement stObj = objs.get(0);

		Element otherDescrElm = doc.createElementNS(IConstants.RDF_NAMESPACE, IConstants.RDF_TYPE_PTERM_DESCRIPTION);
		rdf.appendChild(otherDescrElm);
		String id = extractUrlFragment(subject);
		otherDescrElm.setAttributeNS(IConstants.RDF_NAMESPACE, IConstants.RDF_PTERM_ABOUT, '#' + id);
				
		String predObjStr = concatPredObj(stPred.getObject().stringValue(), stObj.getObject().stringValue() );
		otherStatementIds.put(predObjStr, otherDescrElm);
		idElm.put(id, otherDescrElm);

	}
	
	private String extractUrlFragment(String uri) {
		int pos = uri.lastIndexOf('#');
		if( pos > 0 ) {
			return uri.substring(pos+1);
		}
		return null;
	}
	
	private String concatPredObj(String p, String o ) {
		return '[' + p + ',' + o + ']';
	}
	private String extractPredObj(ShareStatement statement){
		String p = statement.getPredicate();
		String o = statement.getObject().stringValue();
		return concatPredObj(p, o);
	}
	
	private void setValue( Element elm, ShareValue value ) throws ShareServerException {
		
		if( value.getType() == ShareValueType.BLANK_NODE ) {
			Element inlinedElm = this.constructInlinedElement(value.stringValue());
			elm.appendChild(inlinedElm);
		} else if( value.getType() == ShareValueType.URI ) {
			elm.setAttributeNS(IConstants.RDF_NAMESPACE, IConstants.RDF_PTERM_RESOURCE, value.stringValue());
		} else if( value.getType() == ShareValueType.STRING ) {
			elm.setTextContent( value.stringValue());
		} else {
			String datatype = value.rdfDataType();
			if( datatype != null ) {
				elm.setAttributeNS(IConstants.RDF_NAMESPACE, IConstants.RDF_PTERM_DATATYPE, datatype);
			}
			elm.setTextContent( value.stringValue());
		}
	}
	
	private String getPrefix(String namespace) {
		String prefix = namespacePrefixes.get(namespace);
		if( prefix == null ) {
			// then find the next available prefix
			int i=0;
			String pre = "pr" + i; //$NON-NLS-1$
			Collection<String> prefixes = namespacePrefixes.values();
			while( prefixes.contains(pre) ) {
				i++;
				pre = "pr" + i; //$NON-NLS-1$
			}
			namespacePrefixes.put(namespace, pre);
			prefix = pre;
		}
		return prefix;
	}
	
	private void setNamespaces(Element root) {
		Set<String> namespaces = namespacePrefixes.keySet();
		for (String namespace : namespaces) {
			root.setAttribute( IConstants.XMLNS + ':' + namespacePrefixes.get(namespace), namespace);
		}
	}
	
	private void initPrefixes() {
		namespacePrefixes.put(IConstants.RDF_NAMESPACE, IConstants.RDF_PREFIX);
		namespacePrefixes.put(IConstants.OSLC_NAMESPACE, IConstants.OSLC_PREFIX);
		namespacePrefixes.put(IConstants.DCTERMS_NAMESPACE, IConstants.DCTERMS_PREFIX);
		namespacePrefixes.put(IConstants.SHARE_NAMESPACE, IConstants.SHARE_PREFIX);
	}

	static private List<String> reifiedStatementProperties = null;

	static {
		reifiedStatementProperties = new ArrayList<String>();
		reifiedStatementProperties.add(IConstants.RDF_STATEMENT);
		reifiedStatementProperties.add(IConstants.RDF_SUBJECT);
		reifiedStatementProperties.add(IConstants.RDF_PREDICATE);
		reifiedStatementProperties.add(IConstants.RDF_OBJECT);
		reifiedStatementProperties.add(IConstants.RDF_TYPE);
	}
	
	static public  boolean isReifiedStatementProperty(String uri) {
		return reifiedStatementProperties.contains(uri);
	}
	

}
