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

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.lyo.samples.sharepoint.store.ShareValue.ShareValueType;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;
import org.openrdf.repository.sail.SailRepository;
import org.openrdf.rio.RDFWriter;
import org.openrdf.rio.rdfxml.RDFXMLWriter;
import org.openrdf.rio.rdfxml.util.RDFXMLPrettyWriter;
import org.openrdf.sail.memory.MemoryStore;

public class RdfXmlFormatter {
	
	private Map<String,String> namespacePrefixes = new HashMap<String,String>();
	private SailRepository repository;
	
	public RdfXmlFormatter() throws ShareServerException  {
		try{
			this.repository = new SailRepository(new MemoryStore());
			repository.initialize();
		} catch( Exception e ) {
			throw new ShareServerException(e);
		}
	}
	
	public class SharePrettyRdfXmlWriter extends RDFXMLPrettyWriter {

		public SharePrettyRdfXmlWriter(OutputStream out, Map<String,String> prefixes) {
			super(out);
			
			Set<String> keys= prefixes.keySet();
			for (String ns : keys) {
				String pr = prefixes.get(ns);
				this.setNamespace(pr, ns);
			}

			Map<String, String> storePrefixes = ShareStore.getPredefinedNamespaceMappings();
			keys = storePrefixes.keySet();
			for (String ns : keys) {
				String pr = storePrefixes.get(ns);
				this.setNamespace(pr, ns);
			}
			if( prefixes != null ) {
			}
		}
	}
	


	/*
	 * Resource
	 */
	static public String formatResource(ShareResource resource ) throws ShareServerException {
		RdfXmlFormatter formatter = new RdfXmlFormatter();
		return formatter.format(resource);
	}

	public void addNamespacePrefix(String ns, String prefix) {
		this.namespacePrefixes.put(ns, prefix);
	}
	
	public String formatPretty(ShareResource resource ) throws ShareServerException {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		RDFWriter rdfWriter = new SharePrettyRdfXmlWriter(out, this.namespacePrefixes);
		return _format(resource, rdfWriter, out);
	}	
	
	public String format(ShareResource resource ) throws ShareServerException {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		RDFXMLWriter rdfWriter = new RDFXMLWriter(out);
		return _format(resource, rdfWriter, out);
	}	
	
	public String _format(ShareResource resource, RDFWriter rdfWriter, ByteArrayOutputStream out ) throws ShareServerException {
		String prettyRdfXml = null;
		RepositoryConnection conn = null;
		HashMap<String, Resource> bnMap = new HashMap<String,Resource>();
		// make sure there are some statements with this context
		try {
			conn = repository.getConnection();
			ValueFactory vf = conn.getValueFactory();
			rdfWriter.startRDF();
			List<ShareStatement> rioStatements = resource.getStatements();
			
			// first pass get and organize blank nodes
			for (ShareStatement rioStatement : rioStatements) {
				String rioSubject = rioStatement.getSubject();
				if( rioStatement.isBNode() ) {
					if( bnMap.get(rioSubject) == null ) {
						bnMap.put(rioSubject, vf.createBNode());
					}
				}
			}
			
			for (ShareStatement rioStatement : rioStatements) {
				URI pred = vf.createURI(rioStatement.getPredicate());
				Resource subj = null;
				String rioSubject = rioStatement.getSubject();
				if( rioStatement.isBNode() ) {
					subj = bnMap.get(rioSubject);
				} else {
					subj = vf.createURI(rioSubject);
				}
				Value val = null;
				ShareValue rioVal = rioStatement.getObject();
				if( rioVal.getType() == ShareValueType.URI ) {
					val = vf.createURI(rioVal.stringValue());
				} else if( rioVal.getType() == ShareValueType.BLANK_NODE ) {
					val = bnMap.get(rioVal.toString()); 
				} else if( rioVal.getType() == ShareValueType.BOOLEAN ) {
					val = vf.createLiteral(rioVal.booleanValue());
				} else if( rioVal.getType() == ShareValueType.CALENDAR ) {
					val = vf.createLiteral(rioVal.xmlGregorianCalendarValue());
				} else if( rioVal.getType() == ShareValueType.DECIMAL ) {
					val = vf.createLiteral(rioVal.doubleValue());
				} else if( rioVal.getType() == ShareValueType.INTEGER ) {
					val = vf.createLiteral(rioVal.intValue());
				} else {
					val = vf.createLiteral(rioVal.stringValue());
				}
				
				Statement statement = vf.createStatement(subj, pred, val);
				System.out.println( statement );
				rdfWriter.handleStatement(statement);
			}
			rdfWriter.endRDF();
			prettyRdfXml = out.toString("UTF-8");
		} catch (Exception e) {
			throw new ShareServerException(e);
		} finally {
			if( conn != null ) {
				try {
					conn.close();
				} catch (RepositoryException e) {
					throw new ShareServerException(e);
				}
			}
		}
		return prettyRdfXml;
	}
	

}
