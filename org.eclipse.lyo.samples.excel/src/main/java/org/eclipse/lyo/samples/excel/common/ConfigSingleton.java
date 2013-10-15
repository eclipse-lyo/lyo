/*******************************************************************************
 * Copyright (c) 2013 IBM Corporation.
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
 *******************************************************************************/
package org.eclipse.lyo.samples.excel.common;

import java.io.FileInputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.eclipse.lyo.rio.core.IConstants;

public class ConfigSingleton {
	
	private static ConfigSingleton theInstance = new ConfigSingleton();
	
	private String repositoryLocationDefault;
	private String modelGroupNameDefault;
	private String mapperFileNameDefault;
	private String baseUriDefault;
	private Map<String, String> nsPrefixes = new HashMap<String, String>();
	private Map<String, String> backlinks = new HashMap<String, String>();
	private String publisherTitleDefault;
	private String publisherIdentifierDefault;
	private String publisherIconDefault;
	private String serviceDomainDefault;
	private Map<String, String> queryCapabilities = new HashMap<String, String>();

	private ConfigSingleton() {
		Properties prop = new Properties();
		try {
			prop.loadFromXML(new FileInputStream("config.xml"));
			repositoryLocationDefault = prop.getProperty("repositoryLocation");
			modelGroupNameDefault = prop.getProperty("modelGroupName");
			mapperFileNameDefault = prop.getProperty("mapperFileName");
			baseUriDefault = prop.getProperty("baseUri");
			publisherTitleDefault = prop.getProperty("publisherTitle");
			publisherIdentifierDefault = prop.getProperty("publisherIdentifier");
			publisherIconDefault = prop.getProperty("publisherIcon");
			serviceDomainDefault = prop.getProperty("serviceDomain");
			
			System.out.println("default repositoryLocation : " + repositoryLocationDefault);
			System.out.println("default modelGroupName : " + modelGroupNameDefault);
			System.out.println("default mapperFileName : " + mapperFileNameDefault);
			System.out.println("default baseUri : " + baseUriDefault);
			System.out.println("default publisherTitle : " + publisherTitleDefault);
			System.out.println("default publisherIdentifier : " + publisherIdentifierDefault);
			System.out.println("default publisherIcon : " + publisherIconDefault);
			System.out.println("default serviceDomain : " + serviceDomainDefault);

			// prefix to namespace map
			nsPrefixes.put(IConstants.DCTERMS_PREFIX, IConstants.DCTERMS_NAMESPACE);
			for (int i = 1; ; i++) {
				String value = prop.getProperty("namespace" + i);
				if (value == null) {
					break;
				}
				String[] tokens = value.split(",");
				if (tokens.length > 1) {
					nsPrefixes.put(tokens[0], tokens[1]);
					System.out.println("namespace prefix = " + tokens[0] + ", uri = " + tokens[1]);
				}
			}
			
			// backlink map
			for (int i = 1; ; i++) {
				String value = prop.getProperty("backlink" + i);
				if (value == null) {
					break;
				}
				String[] tokens = value.split(",");
				if (tokens.length > 1) {
					backlinks.put(tokens[0], tokens[1]);
					System.out.println("backlink " + tokens[0] + ", " + tokens[1]);
				}
			}

			// query capabilities map
			for (int i = 1; ; i++) {
				String value = prop.getProperty("queryCapability" + i);
				if (value == null) {
					break;
				}
				String[] tokens = value.split(",");
				if (tokens.length > 1) {
					queryCapabilities.put(tokens[0], tokens[1]);
					System.out.println("queryCapability " + tokens[0] + ", " + tokens[1]);
				}
			}
		} catch (Exception e) {
		}
	}

	public static ConfigSingleton getInstance() {
		return theInstance;
	}
	
	/*
	 * Model related properties
	 */
	public String getRepositoryLocationDefault() {
		return repositoryLocationDefault;
	}
	public String getModelGroupNameDefault() {
		return modelGroupNameDefault;
	}
	public String getMapperFileNameDefault() {
		return mapperFileNameDefault;
	}
	public String getBaseUriDefault() {
		return baseUriDefault;
	}
	public Map<String, String> getBacklinks() {
		return backlinks;
	}
	/*
	 * Namespace related properties
	 */
	public Map<String, String> getNsPrefixes() {
		return nsPrefixes;
	}
	/*
	 * Catalog related properties
	 */
	public String getPublisherTitle() {
		return publisherTitleDefault;
	}
	public String getPublisherIdentifier() {
		return publisherIdentifierDefault;
	}
	public String getPublisherIcon() {
		return publisherIconDefault;
	}
	public String getServiceDomainDefault() {
		return serviceDomainDefault;
	}
	public Map<String, String> getQueryCapabilities() {
		return queryCapabilities;
	}
}
