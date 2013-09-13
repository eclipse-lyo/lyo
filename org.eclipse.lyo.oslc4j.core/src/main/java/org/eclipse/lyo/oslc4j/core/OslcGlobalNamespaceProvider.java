/*******************************************************************************
 * Copyright (c) 2013 IBM Corporation.
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
 *     Daniel Figueiredo Caetano       - custom namespace provider
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.lyo.oslc4j.core.annotation.OslcSchema;

/**
 * Defines the global namespace prefix mappings.
 * The global namespace mappings do not take precedence 
 * over any other namespace mappings definition, which means 
 * that in case of conflict the {@link OslcSchema} will 
 * override the prefix. 
 * 
 * This class is a singleton instance, that can be obtained 
 * calling {@link #getInstance()}, since it works with 
 * any request even if there are no annotation mapping.
 * 
 * @author Daniel Figueiredo Caetano
 *
 */
public class OslcGlobalNamespaceProvider {

	private static OslcGlobalNamespaceProvider instance;
	
	private Map<String, String> prefixDefinitionMap;
	
	/**
	 * Private construct for singleton pattern.
	 */
	private OslcGlobalNamespaceProvider()
	{
		this.prefixDefinitionMap = new HashMap<String, String>();
	}
	
	/**
	 * Gets the unique instance of this class.
	 * @return singleton class instance.
	 */
	public static OslcGlobalNamespaceProvider getInstance() 
	{
		if(null == instance) {
			synchronized (OslcGlobalNamespaceProvider.class) {
				if(null == instance) {
					instance = new OslcGlobalNamespaceProvider();
				}
			}
		}
		return instance;
	}

	/**
	 * Gets the Global namespace mappings, these mappings are 
	 * applied to all operations, even without the annotation 
	 * mappings.
	 * 
	 * key 	 - prefix
	 * value - namespace
	 * 
	 * @return empty hash map instance if there are no global
	 *  namespace mappings.
	 */
	public Map<String, String> getPrefixDefinitionMap() 
	{
		return prefixDefinitionMap;
	}

	/**
	 * Sets the global prefix definition map with the given map.
	 * Note that this operation overrides the current map.
	 * 
	 * @param prefixDefinitionMap that will replace the current.
	 */
	public void setPrefixDefinitionMap(Map<String, String> prefixDefinitionMap) 
	{
		if(null == prefixDefinitionMap) 
		{
			this.prefixDefinitionMap.clear();
		} 
		else 
		{
			this.prefixDefinitionMap = prefixDefinitionMap;
		}
	}

}