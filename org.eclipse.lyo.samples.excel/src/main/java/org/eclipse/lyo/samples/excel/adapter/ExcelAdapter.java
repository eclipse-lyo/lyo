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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.lyo.samples.excel.adapter.common.ResourceAdapter;
import org.eclipse.lyo.samples.excel.adapter.common.ResourceSet;

import com.hp.hpl.jena.query.ResultSet;


public class ExcelAdapter implements ResourceAdapter {
	private ModelManager modelManager = new ModelManager();
	
	public void setBaseUri(String uri) {
		modelManager.setBaseUri(uri);
	}
	public void setRepositoryLocation(String location) {
		modelManager.setRepositoryLocation(location);
	}
	public void loadRepository() {
		modelManager.scanRepository();
	}
	@Override
	public List<String> getContexts() {
		List<String> uriList = new ArrayList<String>();
		Collection<ModelGroup> groups = modelManager.getModelGroups();
		for(ModelGroup group: groups){
			uriList.add(group.getName());
		}
		return uriList;
	}
	
	private String getUri(String context) {
		Collection<ModelGroup> groups = modelManager.getModelGroups();
		for(ModelGroup g: groups){
			if(context == null || context.equals(g.getName())){
				return g.getUri();
			}
		}
		return null;
	}
	
	public ResourceSet query(String uri, String context, String prefix, String select, String where, String orderBy, String searchTerms) {
		modelManager.scanRepository();
		
		return new ModelQuery().query(uri, getUri(context), modelManager, prefix, select, where, orderBy, searchTerms);
	}
	
	public ResourceSet getResource(String resourceUri) {
		modelManager.scanRepository();
		
		return new ModelQuery().getResource(modelManager, resourceUri);
	}
	@Override
	public ResultSet executeSparql(String context, String queryExp) {
		modelManager.scanRepository();
		
		return new ModelQuery().executeSparql(getUri(context), modelManager, queryExp);
	}
	@Override
	public String getDefaultExcelAbsolutePath() {
		modelManager.scanRepository();
		return modelManager.getDefaultExcelAbsolutePath();
	}
}
