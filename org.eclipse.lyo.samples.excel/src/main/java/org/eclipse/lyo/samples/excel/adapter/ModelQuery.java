/*******************************************************************************
 * Copyright (c) 2011,2013 IBM Corporation.
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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.lyo.core.query.NestedProperty;
import org.eclipse.lyo.core.query.PName;
import org.eclipse.lyo.core.query.ParseException;
import org.eclipse.lyo.core.query.Property;
import org.eclipse.lyo.core.query.QueryUtils;
import org.eclipse.lyo.core.query.SelectClause;
import org.eclipse.lyo.rio.query.OslcWhereHelper.OslcWhereParseException;
import org.eclipse.lyo.rio.query.SimpleQueryBuilder;
import org.eclipse.lyo.samples.excel.adapter.common.ResourceSet;
import org.eclipse.lyo.samples.excel.common.ConfigSingleton;
import org.eclipse.lyo.samples.excel.services.common.ServiceProviderService;

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
import com.hp.hpl.jena.vocabulary.RDFS;

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
	class QueryResultResourceSet implements ResourceSet {
		private String requestUri;
		private List<Model> list = new ArrayList<Model>();
		public void addRequestUri(String requestUri) {
			this.requestUri = requestUri;
		}
		public void addModel(Model model) {
			list.add(model);
		}
		private byte[] getModelByteArray(Model model) {
			ByteArrayOutputStream os = new ByteArrayOutputStream();
			RDFWriter w = model.getWriter("RDF/XML-ABBREV");
			w.setProperty("showXMLDeclaration", "true");
			w.write(model, os, "");
			return os.toByteArray();
		}
		private int[] findResourcePos(byte[] b) {
			int start = 0;
			boolean openTag = false;
			for (int i = 0; i < b.length; i++) {
				if (openTag && b[i] == '>') {
					start = i + 1;
					break;
				}
				if (b[i] == 'R' &&
					(i + 1 < b.length && b[i + 1] == 'D') &&
					(i + 2 < b.length && b[i + 2] == 'F')) {
					openTag = true;
				}
			}
			int end = b.length - 1;
			boolean closeTag = false;
			for (int i = b.length - 1; i >= 0; i--) {
				if (closeTag && b[i] == '<') {
					end = i - 1;
					break;
				}
				if (b[i] == 'F' &&
					(i - 1 >= 0 && b[i - 1] == 'D') &&
					(i - 2 >= 0 && b[i - 2] == 'R')) {
					closeTag = true;
				}
			}
			if (start < end) {
				return new int[] { start, end };
			} else { 
				return new int[] { 0, b.length - 1 };
			}
		}
		public void outputAsXML(OutputStream out) {
			byte[] close = new byte[0];
			for (int i = 0; i < list.size(); i++) {
				Model model = list.get(i);
				try {
					byte[] b = getModelByteArray(model); // one model, one resource
					int[] pos = findResourcePos(b);
					int start;
					int end;
					// write open tag
					if (i == 0) {
						start = 0;
						end = pos[0] - 1;
						out.write(b, start, end - start + 1);
						out.write("\n".getBytes());
						out.write(new String("<rdf:Description rdf:about=\"" + requestUri + "\">\n").getBytes());
					}
					// write resource contents
					out.write(new String("<!-- " + i + " -->").getBytes());
					start = pos[0];
					end = pos[1];
					out.write(new String("<rdfs:member>").getBytes());
					out.write(b, start, end - start + 1);
					out.write(new String("</rdfs:member>").getBytes());
					// store close tag to a buffer, and append it later
					if (i == 0) {
						start = pos[1] + 1;
						end = b.length - 1;
						ByteArrayOutputStream os = new ByteArrayOutputStream();
						os.write(b, start, end - start + 1);
						close = os.toByteArray();
					}
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
			try {
				// write close tag
				if(list.isEmpty()){
					out.write(new String("<rdf:Description xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'>\n").getBytes());
				}
				out.write(new String("</rdf:Description>\n").getBytes());
				out.write(close);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
	
	public ResourceSet query(String requestUri, String uri, ModelManager modelManager, String prefix, String select, String where, String orderBy, String searchTerms) {
		ModelGroup modelGroup = modelManager.getModelGroup(uri);
		Model model = modelGroup.getModel();
		System.out.println("original model :");
		printModel(model);

		SimpleQueryBuilder queryBuilder = new SimpleQueryBuilder();
		
		try {
			queryBuilder.parsePrefix(prefix);
			for (Entry<String, String> e : ConfigSingleton.getInstance().getNsPrefixes().entrySet()) {
				queryBuilder.addPrefix(e.getValue(), e.getKey());
			}
			queryBuilder.parseSelect(select);
			if (where != null && !where.isEmpty()) {
				queryBuilder.parseWhere("uri", where); //$NON-NLS-1$
			}
			String targetResourceType = getQueryResourceType(requestUri);
			if (targetResourceType != null) {
				queryBuilder.appendWhere("?uri a <" + targetResourceType + "> .");
			} else {
				queryBuilder.appendWhere("?uri a ?type .");
			}
			queryBuilder.parseSelect(orderBy);
			queryBuilder.parseSearchTerms(searchTerms);
		} catch (OslcWhereParseException e) {
			e.printStackTrace();
		}
		
		Map<String, String> prefixMap = null;
		SelectClause selectClause = null;
		try {
			if (select != null) {
				prefixMap = QueryUtils.parsePrefixes(prefix);
				prefixMap.putAll(ConfigSingleton.getInstance().getNsPrefixes());
				selectClause = QueryUtils.parseSelect(select, prefixMap);
			}
		} catch (ParseException e) {
		}
		
		String sparql = queryBuilder.getQueryString(null);
		System.out.println("query:");
		System.out.println(sparql);
		
		Query query = QueryFactory.create(sparql);
		QueryExecution qexec = QueryExecutionFactory.create(query, model);
		ResultSet result = qexec.execSelect();
		
		QueryResultResourceSet rs = new QueryResultResourceSet();
		rs.addRequestUri(requestUri);
		try {
			while (result.hasNext()) {
				Model retModel = ModelFactory.createDefaultModel();
				retModel.setNsPrefixes(ConfigSingleton.getInstance().getNsPrefixes());
				retModel.setNsPrefix("rdfs", RDFS.getURI());
				QuerySolution qs = result.next();
				Resource r = (Resource) qs.get("uri");
				System.out.println(r);
				Resource o = model.getResource(r.getURI());
				StmtIterator si = o.listProperties();
				while (si.hasNext()) {
					Statement s = si.nextStatement();
					System.out.println("stmt: " + s);
					retModel.add(s);
					//
					if (selectClause != null) {
						ArrayList<String> nameList = new ArrayList<String>();
						nameList.add(s.getPredicate().getURI());
						if (isNested(nameList, selectClause, prefixMap)) {
							setStatements(retModel, s.getObject().asResource(), nameList, selectClause, prefixMap);
						}
					}
				}
				rs.addModel(retModel);
			}
		} finally {
			qexec.close();
		}

		return rs;
	}

	private void setStatements(Model model, Resource resource, List<String> nameList, SelectClause selectClause, Map<String, String> prefixMap) {
		System.out.println(nameList + " is nested property...");
		StmtIterator si = resource.listProperties();
		while (si.hasNext()) {
			Statement s = si.nextStatement();
			System.out.println("stmt: " + s);
			model.add(s);
			//
			ArrayList<String> nextNameList = new ArrayList<String>(nameList);
			nextNameList.add(s.getPredicate().getURI());
			if (isNested(nextNameList, selectClause, prefixMap)) {
				setStatements(model, s.getObject().asResource(), nextNameList, selectClause, prefixMap);
			}
		}
	}
	
    private boolean isNested(List<String> nameList, SelectClause sel, Map<String, String> prefixMap) {
    	List<Property> children = sel.children();
    	for (int i = 0; i < children.size(); i++) {
    		Property child = children.get(i);
    		if (isNested(nameList, child, prefixMap)) {
    			return true;
    		}
    	}
    	return false;
    }
    private boolean isNested(List<String> nameList, Property p, Map<String, String> prefixMap) {
    	if (nameList.isEmpty()) {
    		return false;
    	}
    	if (p instanceof NestedProperty) {
    		String name = nameList.get(0);
    		if (p.isWildcard() || equalPName(p.identifier(), name, prefixMap)) {
    			if (nameList.size() == 1) {
    				return true;
    			}
    			ArrayList<String> nextNameList = new ArrayList<String>();
    			for (int i = 1; i < nameList.size(); i++) {
    				nextNameList.add(nameList.get(i));
    			}
    		    List<Property> children = ((NestedProperty)p).children();
    		    for (int i = 0; i < children.size(); i++) {
    		    	Property child = children.get(i);
    		    	if (isNested(nextNameList, child, prefixMap)) {
    		    		return true;
    		    	}
    		    }
    		}
    	}
    	return false;
    }
    private String getNameUri(String name, Map<String, String> prefixMap) {
    	Set<String> keys = prefixMap.keySet();
    	Iterator<String> ite = keys.iterator();
    	String prefix = null;
    	while (ite.hasNext()) {
    		String key = ite.next();
    		if (name.startsWith(key + ":")) {
    			prefix = key;
    		}
    	}
    	if (prefix != null) {
    		String uri = prefixMap.get(prefix);
    		return name.replaceFirst(prefix + ":", uri);
    	}
    	return name;
    }
    private boolean equalPName(PName pname, String uri, Map<String, String> prefixMap) {
    	String pnameUri = getNameUri(pname.toString(), prefixMap);
    	return pnameUri.equals(uri);
    }

	/**
	 *  should be sync with {@link ServiceProviderService#buildServicesResource(String, String)}
	 */
	private String getQueryResourceType(String uri){
		Map<String, String> queryCapabilities = ConfigSingleton.getInstance().getQueryCapabilities();
		for(String uriLastSegment: queryCapabilities.keySet()){
			if(uri.endsWith(uriLastSegment)){
				return queryCapabilities.get(uriLastSegment);
			}
		}
		return null;
	}
	
	public ResourceSet getResource(ModelManager modelManager, String resourceUri) {
		Iterator<ModelGroup> iterator = modelManager.getModelGroups().iterator();
		while (iterator.hasNext()) {
			ModelGroup modelGroup = iterator.next();
			Model model = modelGroup.getModel();
			Resource r = model.getResource(resourceUri);
			if (r != null) {
				Model retModel = ModelFactory.createDefaultModel();
				retModel.setNsPrefixes(ConfigSingleton.getInstance().getNsPrefixes());
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
