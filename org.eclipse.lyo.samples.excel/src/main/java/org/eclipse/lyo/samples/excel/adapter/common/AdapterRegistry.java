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
package org.eclipse.lyo.samples.excel.adapter.common;

import java.util.HashMap;
import java.util.Map;

public class AdapterRegistry {
	private static Map<String, ResourceAdapter> _registry = new HashMap<String, ResourceAdapter>();

	public static void addAdapter(String key, ResourceAdapter adapter){
		_registry.put(key, adapter);
	}
	
	public static ResourceAdapter getAdapter(String baseUrl){
		ResourceAdapter adapter = _registry.get(baseUrl);
		if(adapter == null){
			adapter = DefaultAdapterFactory.createAdapter();
			adapter.setBaseUri(baseUrl);
			addAdapter(baseUrl, adapter);
		}
		return adapter;
	}
}
