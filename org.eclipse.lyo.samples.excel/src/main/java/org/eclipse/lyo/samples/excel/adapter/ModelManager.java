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

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.lyo.samples.excel.adapter.dao.ExcelDao;
import org.eclipse.lyo.samples.excel.adapter.dao.ExcelDaoFactory;
import org.eclipse.lyo.samples.excel.common.ConfigSingleton;

import com.hp.hpl.jena.rdf.model.Model;

public class ModelManager {
	private static String repositoryLocationDefault;
	private static String modelGroupNameDefault;
	private static String mapperFileNameDefault;
	private static String generatedExcelFileNameDefault;
	private static String baseUriDefault;
	
	static {
		ConfigSingleton singleton = ConfigSingleton.getInstance();

		repositoryLocationDefault = singleton.getRepositoryLocationDefault();
		modelGroupNameDefault = singleton.getModelGroupNameDefault();
		mapperFileNameDefault = singleton.getMapperFileNameDefault();
		baseUriDefault = singleton.getBaseUriDefault();
			
	}
	private String repositoryLocation = repositoryLocationDefault;
	private String mapperFileName = mapperFileNameDefault;
	private String baseUri = baseUriDefault;

	private ExcelDao dao = ExcelDaoFactory.createDefaultReader();

	private Map<String, ModelGroup> modelGroupMap = new HashMap<String, ModelGroup>();
	
	public void setRepositoryLocation(String repositryLocation) {
		this.repositoryLocation = repositryLocation;
	}
	public void setMapperFileName(String mapperFile) {
		this.mapperFileName = mapperFile;
	}
	public void setBaseUri(String baseUri) {
		if(!baseUri.endsWith("/")){
			baseUri += "/";
		}
		this.baseUri = baseUri;
	}
	public void scanRepository() {
		File d = new File(repositoryLocation);
		if (!d.exists() || !d.isDirectory()) {
			return;
		}
		List<String> list = new ArrayList<String>();
		File[] files = d.listFiles();
		for (int i = 0; i < files.length; i++) {
			File f = files[i];
			if (f.isDirectory()) {
				String name = f.getName();
				String ename = name;
				try {
					ename = URLEncoder.encode(name, "UTF-8");
				} catch (UnsupportedEncodingException e) {
				}
				String uri = baseUri + ename + "/";
				ModelGroup modelGroup = modelGroupMap.get(uri);
				if (modelGroup == null) {
					modelGroup = new ModelGroup();
					modelGroup.setPathName(f.getAbsolutePath());
					modelGroup.setName(name);
					modelGroup.setUri(uri);
					modelGroupMap.put(uri, modelGroup);
				}
				loadModelGroup(f, modelGroup);
				list.add(uri);
			}
		}
		
		// clean up
		List<String> obsoleteList = new ArrayList<String>();
		Set<String> set = modelGroupMap.keySet();
		Iterator<String> iter = set.iterator();
		while (iter.hasNext()) {
			String key = iter.next();
			if (!list.contains(key)) {
				obsoleteList.add(key);
			}
		}
		if (!obsoleteList.isEmpty()) {
			for (int i = 0; i < obsoleteList.size(); i++) {
				String key = obsoleteList.get(i);
				modelGroupMap.remove(key);
			}
		}
	}

	public Collection<ModelGroup> getModelGroups() {
		return modelGroupMap.values();
	}
	public ModelGroup getModelGroup(String uri) {
		return modelGroupMap.get(uri);
	}

	private void loadModelGroup(File dir, ModelGroup modelGroup) {
		File mapperFile = null;
		List<File> fileList = new ArrayList<File>();
		File[] files = dir.listFiles();
		for (int i = 0; i < files.length; i++) {
			File f = files[i];
			String fileName = f.getName();
			if (fileName.equals(mapperFileName)) {
				mapperFile = f;
			} else {
				fileList.add(f);
			}
		}

		if (modelGroup.getMapperTableLastModified() <= mapperFile.lastModified()) {
			// reload mapper xls file
			modelGroup.setMapperTableLastModified(mapperFile.lastModified());
			modelGroup.loadMapperTable(mapperFile.getAbsolutePath());
			// clear map, and reload all
			modelGroup.getModelMap().clear();
		}

		List<String> list = new ArrayList<String>();
		for (int i = 0; i < fileList.size(); i++) {
			File f = fileList.get(i);
			String fileName = f.getName();
			String name = null;
			String suffix = null;
			int dot = fileName.lastIndexOf('.');
			if (dot != -1) {
				name = fileName.substring(0, dot);
				suffix = fileName.substring(dot + 1);
			}
			if (name != null && suffix != null && suffix.equalsIgnoreCase("xls")) {
				boolean reload = true;
				String ename = name;
				try {
					ename = URLEncoder.encode(name, "UTF-8");
				} catch (UnsupportedEncodingException e) {
					e.printStackTrace();
				}
				String relationshipUri = modelGroup.getUri() + ename + "/";
				String pathName = f.getAbsolutePath();
				ModelContainer container = modelGroup.getModelMap().get(relationshipUri);
				if (container == null) {
					container = new ModelContainer();
					container.setPathName(pathName);
					container.setName(name);
					container.setRelationshipUri(relationshipUri);
					container.setLastModified(f.lastModified());
					modelGroup.getModelMap().put(relationshipUri, container);
				} else if (f.lastModified() <= container.getLastModified()) {
					reload = false;
				}
				if (reload) {
					dao.setRelationshipUri(relationshipUri);
					dao.setMapperTable(modelGroup.getMapperTable());
					Model model = dao.parseFile(pathName);
					if (container.getModel() != null) {
						container.getModel().close();
					}
					container.setModel(model);
				}
				list.add(relationshipUri);
			}
		}
		List<String> obsoleteList = new ArrayList<String>();
		Iterator<String> iter = modelGroup.getModelMap().keySet().iterator();
		while (iter.hasNext()) {
			String key = iter.next();
			if (!list.contains(key)) {
				obsoleteList.add(key);
			}
		}
		if (!obsoleteList.isEmpty()) {
			for (int i = 0; i < obsoleteList.size(); i++) {
				String key = obsoleteList.get(i);
				modelGroup.getModelMap().remove(key);
			}
		}
	}
	
	public String getDefaultExcelAbsolutePath(){
		ModelGroup modelGroup = getModelGroup(baseUri + modelGroupNameDefault);
		String pathName = modelGroup.getPathName();
		
		return pathName + "\\" + generatedExcelFileNameDefault;
	}	
}
