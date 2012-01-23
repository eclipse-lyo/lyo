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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Random;
import java.util.Set;

import org.eclipse.lyo.examples.excel.adapter.dao.ExcelDao;
import org.eclipse.lyo.examples.excel.adapter.dao.ExcelDaoFactory;
import org.eclipse.lyo.examples.excel.adapter.dao.PropertyMappingInfo;
import org.eclipse.lyo.examples.excel.adapter.dao.ResourceFactory;
import org.eclipse.lyo.examples.excel.changerequest.ChangeRequestDto;
import org.eclipse.lyo.examples.excel.common.ICmConstants;
import org.eclipse.lyo.rio.util.RandomTextGenerator;

import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.sparql.vocabulary.FOAF;
import com.hp.hpl.jena.vocabulary.DC;
import com.hp.hpl.jena.vocabulary.RDF;

public class ModelManager {
	private static String repositoryLocationDefault;
	private static String modelGroupNameDefault;
	private static String mapperFileNameDefault;
	private static String generatedExcelFileNameDefault;
	private static String baseUriDefault;
	
	static {
		Properties prop = new Properties();
		try {
			prop.loadFromXML(new FileInputStream("config.xml"));
			repositoryLocationDefault = prop.getProperty("repositoryLocation");
			modelGroupNameDefault = prop.getProperty("modelGroupName");
			mapperFileNameDefault = prop.getProperty("mapperFileName");
			generatedExcelFileNameDefault = prop.getProperty("generatedExcelFileName");
			baseUriDefault = prop.getProperty("baseUri");
			
			System.out.println("default repositoryLocation : " + repositoryLocationDefault);
			System.out.println("default modelGroupName : " + modelGroupNameDefault);
			System.out.println("default mapperFileName : " + mapperFileNameDefault);
			System.out.println("default generatedExcelFileName : " + generatedExcelFileNameDefault);
			System.out.println("default baseUri : " + baseUriDefault);
		} catch (Exception e) {
		}
	}
	private String repositoryLocation = repositoryLocationDefault;
	private String mapperFileName = mapperFileNameDefault;
	private String baseUri = baseUriDefault;

	private ExcelDao dao = ExcelDaoFactory.createDefaultReader();

	private Map<String, ModelGroup> modelGroupMap = new HashMap<String, ModelGroup>();
	
	class ResourceFactoryImpl implements ResourceFactory {
		private Map<String, String> typeToRelationshipUri = new HashMap<String, String>();
		public void setRelationshipUri(String type, String relationshipUri) {
			typeToRelationshipUri.put(type, relationshipUri);
		}
		public Resource createResource(Model model, String resourceType, String value, String valueType) {
			String relationshipUri = typeToRelationshipUri.get(resourceType);
			if (relationshipUri == null) {
				return null;
			}
			Resource resource = null;
			try {
				resource = model.createResource(relationshipUri + URLEncoder.encode(value, "UTF-8"));
				resource.addProperty(RDF.type, model.createResource(resourceType));
				resource.addProperty(model.createProperty(valueType), value);
			} catch (UnsupportedEncodingException e) {
				e.printStackTrace();
			}
			return resource;
		}
	}
	
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
				String uri = baseUri + name + "/";
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

		if (modelGroup.getPropertyMappingConfigLastModified() <= mapperFile.lastModified()) {
			// reload mapper xml file
			modelGroup.loadPropertyMappingConfig(mapperFile.getAbsolutePath());
			modelGroup.setPropertyMappingConfigLastModified(mapperFile.lastModified());
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
				String relationshipUri = modelGroup.getUri() + name + "/";
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
					ResourceFactoryImpl resourceFactory = new ResourceFactoryImpl();
					resourceFactory.setRelationshipUri(ICmConstants.OSLC_CM_CHANGEREQUEST, relationshipUri);
					resourceFactory.setRelationshipUri(FOAF.Person.getURI(), modelGroup.getUri() + "users/");
					dao.setRelationshipUri(relationshipUri);
					dao.setResourceFactory(resourceFactory);
					dao.setPropertyMappingConfig(modelGroup.getPropertyMappingConfig());
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
	
	public void addChangeRequest(ChangeRequestDto dto){
		ModelGroup modelGroup = getModelGroup(baseUri + modelGroupNameDefault);
		String pathName = modelGroup.getPathName();
		
		ResourceFactoryImpl resourceFactory = new ResourceFactoryImpl();
		resourceFactory.setRelationshipUri(ICmConstants.OSLC_CM_CHANGEREQUEST, modelGroup.getUri());
		resourceFactory.setRelationshipUri(FOAF.Person.getURI(), modelGroup.getUri() + "users/");
		dao.setRelationshipUri(modelGroup.getUri());
		dao.setResourceFactory(resourceFactory);
		dao.setPropertyMappingConfig(modelGroup.getPropertyMappingConfig());
		
		List<PropertyMappingInfo> mappingInfo = modelGroup.getPropertyMappingConfig().listPropertyInfo(null); //try default mapping
				
		Model model = ModelFactory.createDefaultModel();
		model.setNsPrefix("oslc_cm", ICmConstants.OSLC_CM_NAMESPACE);
		model.setNsPrefix("foaf", FOAF.NS);
		
		try{
			Resource changeRequest = null;
			for (int j = 0; j < mappingInfo.size(); j++) {
				PropertyMappingInfo info = mappingInfo.get(j);
				if (info.getResourceName().equalsIgnoreCase(ICmConstants.OSLC_CM_CHANGEREQUEST) && 
					info.getPropertyName().equalsIgnoreCase(DC.identifier.getURI())) {
					//identifier
					String value = Integer.toString(dao.getNewId(pathName + "\\" + generatedExcelFileNameDefault) + 1);
					if (value != null) {
						changeRequest = resourceFactory.createResource(model, info.getResourceName(), value, info.getPropertyName());
						break;
					}
				}
			}
			for (int j = 0; j < mappingInfo.size(); j++) {
				PropertyMappingInfo info = mappingInfo.get(j);
				Property property = model.createProperty(info.getPropertyName());
				
				if (info.getPropertyType().equalsIgnoreCase("http://open-services.net/ns/core#Resource") && 
						info.getPropertyName().equalsIgnoreCase("http://purl.org/dc/elements/1.1/contributor")) { // TODO add to cm constant def
					//contributor
					String value = "_UNKNOWN_USER_";
					PropertyMappingInfo detailInfo = info.getDetailInfo();
					String resourceClass = detailInfo.getResourceName();
					Resource resource = resourceFactory.createResource(model, resourceClass, value, detailInfo.getPropertyName());
					changeRequest.addProperty(property, resource);
//				}else if (info.getPropertyType().equalsIgnoreCase("http://open-services.net/ns/core#Resource") && 
//						info.getPropertyName().equalsIgnoreCase("http://open-services.net/ns/cm#relatedChangeRequest")) { // TODO add to cm constant def
//					//relatedChangeRequest
//					String value = String.valueOf(1); //TODO must change to refer existing resource
//					PropertyMappingInfo detailInfo = info.getDetailInfo();
//					String resourceClass = detailInfo.getResourceName();
//					Resource resource = resourceFactory.createResource(model, resourceClass, value, detailInfo.getPropertyName());
//					changeRequest.addProperty(property, resource);
				} else if (info.getPropertyName().equalsIgnoreCase("http://purl.org/dc/elements/1.1/title")) { // TODO add to cm constant def
					//title
					Literal literal = model.createLiteral(dto.getTitle());
					changeRequest.addLiteral(property, literal);
				} else if (info.getPropertyName().equalsIgnoreCase("http://purl.org/dc/elements/1.1/description")) { // TODO add to cm constant def
					//description
					Literal literal = model.createLiteral(dto.getDescription());
					changeRequest.addLiteral(property, literal);
				} else if (info.getPropertyName().equalsIgnoreCase("http://purl.org/dc/elements/1.1/created")) { // TODO add to cm constant def
					//created
					Calendar calendar = Calendar.getInstance();
					Date nowTime = calendar.getTime();  
					Literal literal = model.createLiteral(nowTime.toString());
					changeRequest.addLiteral(property, literal);
				} else if (info.getPropertyName().equalsIgnoreCase("http://open-services.net/ns/cm#status")) { // TODO add to cm constant def
					//status
					Literal literal = model.createLiteral(dto.getStatus());
					changeRequest.addLiteral(property, literal);
				}
			}
		
			dao.write(pathName + "\\" + generatedExcelFileNameDefault, model, true);
		
//		try {
//			RDFWriter w = model.getWriter("RDF/XML-ABBREV");
//			w.setProperty("showXMLDeclaration", "true");
//			w.write(model, System.out, "");
//		} catch (Exception e){
//			e.printStackTrace();
//		}
			
		}catch(IOException e){
			e.printStackTrace();
		}
	}
	
	
	
	public void generateDefaultContents(int count) {
		ModelGroup modelGroup = getModelGroup(baseUri + modelGroupNameDefault);
		String pathName = modelGroup.getPathName();
		
		ResourceFactoryImpl resourceFactory = new ResourceFactoryImpl();
		resourceFactory.setRelationshipUri(ICmConstants.OSLC_CM_CHANGEREQUEST, modelGroup.getUri());
		resourceFactory.setRelationshipUri(FOAF.Person.getURI(), modelGroup.getUri() + "users/");
		dao.setRelationshipUri(modelGroup.getUri());
		dao.setResourceFactory(resourceFactory);
		dao.setPropertyMappingConfig(modelGroup.getPropertyMappingConfig());
		
		List<PropertyMappingInfo> mappingInfo = modelGroup.getPropertyMappingConfig().listPropertyInfo(null); //try default mapping
				
		Model model = ModelFactory.createDefaultModel();
		model.setNsPrefix("oslc_cm", ICmConstants.OSLC_CM_NAMESPACE);
		model.setNsPrefix("foaf", FOAF.NS);
		
		Random rnd = new Random(System.currentTimeMillis());
		
		try{
			RandomTextGenerator gen = new RandomTextGenerator();
			
			for(int i=1;i<=count;i++) {
				Resource changeRequest = null;
				for (int j = 0; j < mappingInfo.size(); j++) {
					PropertyMappingInfo info = mappingInfo.get(j);
					if (info.getResourceName().equalsIgnoreCase(ICmConstants.OSLC_CM_CHANGEREQUEST) && 
						info.getPropertyName().equalsIgnoreCase(DC.identifier.getURI())) {
						//identifier
						String value = String.valueOf(i);
						if (value != null) {
							changeRequest = resourceFactory.createResource(model, info.getResourceName(), value, info.getPropertyName());
							break;
						}
					}
				}
				if (changeRequest == null) {
					continue;
				}
				for (int j = 0; j < mappingInfo.size(); j++) {
					PropertyMappingInfo info = mappingInfo.get(j);
					Property property = model.createProperty(info.getPropertyName());
					
					if (info.getPropertyType().equalsIgnoreCase("http://open-services.net/ns/core#Resource") && 
							info.getPropertyName().equalsIgnoreCase("http://purl.org/dc/elements/1.1/contributor")) { // TODO add to cm constant def
						//contributor
						String value = "_UNKNOWN_USER_";
						PropertyMappingInfo detailInfo = info.getDetailInfo();
						String resourceClass = detailInfo.getResourceName();
						Resource resource = resourceFactory.createResource(model, resourceClass, value, detailInfo.getPropertyName());
						changeRequest.addProperty(property, resource);
					}else if (info.getPropertyType().equalsIgnoreCase("http://open-services.net/ns/core#Resource") && 
							info.getPropertyName().equalsIgnoreCase("http://open-services.net/ns/cm#relatedChangeRequest")) { // TODO add to cm constant def
						//relatedChangeRequest
						String value = String.valueOf(i);
						PropertyMappingInfo detailInfo = info.getDetailInfo();
						String resourceClass = detailInfo.getResourceName();
						Resource resource = resourceFactory.createResource(model, resourceClass, value, detailInfo.getPropertyName());
						changeRequest.addProperty(property, resource);
					} else if (info.getPropertyName().equalsIgnoreCase("http://purl.org/dc/elements/1.1/title")) { // TODO add to cm constant def
						//title
						int titleLen = rnd.nextInt(3) + 2;
						String title = gen.generateText(titleLen);
						Literal literal = model.createLiteral(title);
						changeRequest.addLiteral(property, literal);
					} else if (info.getPropertyName().equalsIgnoreCase("http://purl.org/dc/elements/1.1/description")) { // TODO add to cm constant def
						//description
						int descriptionLen = rnd.nextInt(50) + 50;
						String description = gen.generateText(descriptionLen);
						Literal literal = model.createLiteral(description);
						changeRequest.addLiteral(property, literal);
					} else if (info.getPropertyName().equalsIgnoreCase("http://purl.org/dc/elements/1.1/created")) { // TODO add to cm constant def
						//created
						Calendar calendar = Calendar.getInstance();
						Date nowTime = calendar.getTime();  
						Literal literal = model.createLiteral(nowTime.toString());
						changeRequest.addLiteral(property, literal);
					} else if (info.getPropertyName().equalsIgnoreCase("http://open-services.net/ns/cm#status")) { // TODO add to cm constant def
						//status
						int statusIndex = rnd.nextInt(4);
						Literal literal = model.createLiteral(ICmConstants.OSLC_CM_STATUS_VALUES[statusIndex]);
						changeRequest.addLiteral(property, literal);
					}
				}
			}
			
			dao.write(pathName + "\\" + generatedExcelFileNameDefault, model, false);
		
//		try {
//			RDFWriter w = model.getWriter("RDF/XML-ABBREV");
//			w.setProperty("showXMLDeclaration", "true");
//			w.write(model, System.out, "");
//		} catch (Exception e){
//			e.printStackTrace();
//		}
			
		}catch(IOException e){
			e.printStackTrace();
		}
	}
}
