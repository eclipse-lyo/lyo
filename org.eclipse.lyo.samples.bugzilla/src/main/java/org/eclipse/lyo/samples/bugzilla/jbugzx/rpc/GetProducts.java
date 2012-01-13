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
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.samples.bugzilla.jbugzx.rpc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.lyo.samples.bugzilla.jbugzx.base.Product;

import com.j2bugzilla.base.BugzillaMethod;

public class GetProducts implements BugzillaMethod {
	
	/**
	 * The method Bugzilla will execute via XML-RPC
	 */
	private static final String METHOD_NAME = "Product.get";
	
	private Map<Object, Object> params = new HashMap<Object, Object>();
	private Map<Object, Object> hash = new HashMap<Object, Object>();
	
	public GetProducts(Integer[] ids) {
		params.put("ids", ids);
	}
	
	@SuppressWarnings("unchecked")
	public List<Product> getProducts() {
		List<Product> products = new ArrayList<Product>();
		if (hash.containsKey("products")) {
			Object[] values = (Object[])hash.get("products");
			for (Object hashmap : values) {
				products.add(new Product((Map<String, Object>)hashmap));				
			}
		}
		return products;
	}
	
	@Override
	public void setResultMap(Map<Object, Object> hash) {
		this.hash = hash;
	}

	@Override
	public Map<Object, Object> getParameterMap() {
		return params;
	}

	@Override
	public String getMethodName() {
		return METHOD_NAME;
	}

}
