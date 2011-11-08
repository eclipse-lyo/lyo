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
package org.eclipse.lyo.samples.bugzilla.jbugzx.base;

import java.util.Map;

public class Product {
	private Map<String, Object> internalState;

	public Product(Map<String, Object> state) {
		internalState = state;
	}
	
	public Map<String, Object> getInternalState() {
		return internalState;
	}
	
	public int getId() {
		return (Integer)internalState.get("id");
	}
	
	public String getName() {
		return (String)internalState.get("name");
	}
}
