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

import java.util.HashMap;
import java.util.Map;

import com.j2bugzilla.base.BugzillaMethod;

public class GetLegalValues implements BugzillaMethod {
	
	/**
	 * The method Bugzilla will execute via XML-RPC
	 */
	private static final String METHOD_NAME = "Bug.legal_values";
	
	private Map<Object, Object> params = new HashMap<Object, Object>();
	private Map<Object, Object> hash = new HashMap<Object, Object>();
	
	public GetLegalValues(String field, int productId) {
		params.put("field", field);
		if (productId != -1) params.put("product_id", productId);
	}
	
	public String[] getValues() {
		if (hash.containsKey("values")) {
			Object[] values = (Object[])hash.get("values"); 
			String[] strings = new String[values.length];
			System.arraycopy(values, 0, strings, 0, values.length);
			return strings;
		} else {
			return new String[0];
		}		
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
