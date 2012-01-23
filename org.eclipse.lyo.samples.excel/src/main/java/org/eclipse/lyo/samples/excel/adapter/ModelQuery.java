/*******************************************************************************
 * Copyright (c) 2011 IBM Corporation.
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
 *     Masaki Wakao 
 *     Yoshio Horiuchi 
 *     Kohji Ohsawa 
 *******************************************************************************/
package org.eclipse.lyo.samples.excel.adapter;

import java.io.OutputStream;
import java.util.Iterator;

import org.eclipse.lyo.rio.query.OslcWhereHelper.OslcWhereParseException;
import org.eclipse.lyo.rio.query.SimpleQueryBuilder;
import org.eclipse.lyo.samples.excel.adapter.common.ResourceSet;
import org.eclipse.lyo.samples.excel.common.ICmConstants;

import com.hp.hpl.jena.query.Query;
import com.hp.hpl.jena.query.QueryExecution;
import com.hp.hpl.jena.query.QueryExecutionFactory;
import com.hp.hpl.jena.query.QueryFactory;
import com.hp.hpl.jena.query.QuerySolution;
import com.hp.hpl.jena.query.ResultSet;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.RDFWriter;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.sparql.vocabulary.FOAF;
import com.hp.hpl.jena.vocabulary.DC;

public class ModelQuery {
	class ResourceSetImpl implements ResourceSet {
		private Model model;
		public ResourceSetImpl(Model model) {
			this.model = model;
		}
		public void outputAsXML(OutputStream out) {
			RDFWriter w = model.getWriter("RDF/XML-ABBREV");
			w.setProperty("showXMLDeclaration", "true");
			w.write(model, out, "");
		}
	}
	
	public ResourceSet query(String uri, ModelManager modelManager, String prefix, String select, String where, String orderBy, String searchTerms) {
		ModelGroup modelGroup = modelManager.getModelGroup(uri);
		Model model = modelGroup.getModel();
		System.out.println("original model :");
		printModel(model);

		if (prefix == null || prefix.length() == 0) {
			prefix = "dcterms=<" + DC.getURI()+ ">,foaf=<" + FOAF.getURI() + ">";
		}
		SimpleQueryBuilder queryBuilder = new SimpleQueryBuilder();
		
		try {
			queryBuilder.parsePrefix(prefix);
			queryBuilder.addPrefix(ICmConstants.OSLC_CM_NAMESPACE, "oslc_cm");
			queryBuilder.parseSelect(select);
			queryBuilder.parseWhere("uri", where); //$NON-NLS-1$
			queryBuilder.parseSelect(orderBy);
			queryBuilder.parseSearchTerms(searchTerms);
		} catch (OslcWhereParseException e) {
			e.printStackTrace();
		}
		
		String sparql = queryBuilder.getQueryString(ICmConstants.OSLC_CM_CHANGEREQUEST);
		System.out.println("query:");
		System.out.println(sparql);
		
		Query query = QueryFactory.create(sparql);
		QueryExecution qexec = QueryExecutionFactory.create(query, model);
		ResultSet result = qexec.execSelect();
		
//		Model retModel = ResultSetFormatter.toModel(result);
//		qexec.close();
		
		Model retModel = ModelFactory.createDefaultModel();
		retModel.setNsPrefix("oslc_cm", ICmConstants.OSLC_CM_NAMESPACE);
		retModel.setNsPrefix("foaf", FOAF.NS);
		retModel.setNsPrefix("dcterms", DC.getURI());
		
		try {
			while (result.hasNext()) {
				QuerySolution qs = result.next();
				Resource r = (Resource) qs.get("uri");
				System.out.println(r);
				Resource o = model.getResource(r.getURI());
				StmtIterator si = o.listProperties();
				while (si.hasNext()) {
					Statement s = si.nextStatement();
					System.out.println("stmt: " + s);
					retModel.add(s);
				}
			}
		} finally {
			qexec.close();
		}
		System.out.println("return model:");
		printModel(retModel);
		
		return new ResourceSetImpl(retModel);
	}
	
	public ResourceSet getResource(ModelManager modelManager, String resourceUri) {
		Iterator<ModelGroup> iterator = modelManager.getModelGroups().iterator();
		while (iterator.hasNext()) {
			ModelGroup modelGroup = iterator.next();
			Model model = modelGroup.getModel();
			Resource r = model.getResource(resourceUri);
			if (r != null) {
				Model retModel = ModelFactory.createDefaultModel();
				retModel.setNsPrefix("oslc_cm", ICmConstants.OSLC_CM_NAMESPACE);
				retModel.setNsPrefix("foaf", FOAF.NS);
				retModel.setNsPrefix("dcterms", DC.getURI());
				Resource o = retModel.createResource(r.getURI());
				System.out.println(o);
				StmtIterator si = r.listProperties();
				while (si.hasNext()) {
					Statement s = si.nextStatement();
					System.out.println("stmt: " + s);
					retModel.add(s);
				}
				return new ResourceSetImpl(retModel);
			}
		}
		return null;
	}
	
	private void printModel(Model model) {
		try {
			RDFWriter w = model.getWriter("RDF/XML-ABBREV");
			w.setProperty("showXMLDeclaration", "true");
			w.write(model, System.out, "");
		} catch (Exception e){
			e.printStackTrace();
		}
	}

	public ResultSet executeSparql(String uri, ModelManager modelManager, String queryExp) {
		ModelGroup modelGroup = modelManager.getModelGroup(uri);
		Model model = modelGroup.getModel();
		
		Query query = QueryFactory.create(queryExp);
        QueryExecution qexec = QueryExecutionFactory.create(query, model);
        
        return qexec.execSelect();
	}
}
