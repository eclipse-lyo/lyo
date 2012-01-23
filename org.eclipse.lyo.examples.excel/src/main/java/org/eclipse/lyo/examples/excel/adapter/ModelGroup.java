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
package org.eclipse.lyo.examples.excel.adapter;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.lyo.examples.excel.adapter.dao.PropertyMappingConfig;
import org.eclipse.lyo.examples.excel.common.ICmConstants;


import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.sparql.vocabulary.FOAF;
import com.hp.hpl.jena.vocabulary.DC;

public class ModelGroup {
	private String pathName;
	private String name;
	private String uri;
	
	private PropertyMappingConfig propertyMappingConfig = null;
	private long propertyMappingConfigLastModified;
	
	private Map<String, ModelContainer> modelMap = new HashMap<String, ModelContainer>();
	
	private Model model;
	private long modelLastModified;
	
	public String getPathName() {
		return pathName;
	}
	public void setPathName(String pathName) {
		this.pathName = pathName;
	}
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public String getUri() {
		return uri;
	}
	public void setUri(String uri) {
		this.uri = uri;
	}
	
	public long getPropertyMappingConfigLastModified() {
		return propertyMappingConfigLastModified;
	}
	public void setPropertyMappingConfigLastModified(long propertyMappingConfigLastModified) {
		this.propertyMappingConfigLastModified = propertyMappingConfigLastModified;
	}
	public void loadPropertyMappingConfig(String filename) {
		propertyMappingConfig = new PropertyMappingConfig(filename);
	}
	public PropertyMappingConfig getPropertyMappingConfig() {
		return propertyMappingConfig;
	}
	
	public Map<String, ModelContainer> getModelMap() {
		return modelMap;
	}
	
	public Model getModel() {
		boolean merge = false;
		if (model == null) {
			model = ModelFactory.createDefaultModel();
			model.setNsPrefix("oslc_cm", ICmConstants.OSLC_CM_NAMESPACE);
			model.setNsPrefix("foaf", FOAF.NS);
			model.setNsPrefix("dcterms", DC.getURI());
			merge = true;
		} else {
			Iterator<ModelContainer> i = modelMap.values().iterator();
			while (i.hasNext()) {
				ModelContainer container = i.next();
				if (modelLastModified < container.getLastModified()) {
					merge = true;
					break;
				}
			}
		}
		if (merge) {
			model.removeAll();
			long lastModified = 0;
			Iterator<ModelContainer> i = modelMap.values().iterator();
			while (i.hasNext()) {
				ModelContainer container = i.next();
				if (lastModified < container.getLastModified()) {
					lastModified = container.getLastModified();
				}
				model.add(container.getModel());
			}
			modelLastModified = lastModified;
		}
		return model;
	}
}
