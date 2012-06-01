/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
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
 *     Keith Wells             - initial API and implementation
 *     Sam Padgett           - initial API and Implementation
 *     Jim Conallen           - initial API and implementation
 *
 *******************************************************************************/
package org.eclipse.lyo.samples.sharepoint;

import java.util.Map;

public class Library {
	private Map<String, Object> internalState;

	public Library(Map<String, Object> state) {
		internalState = state;
	}
	
	public Map<String, Object> getInternalState() {
		return internalState;
	}
	
	public String getUri() {
		return (String)internalState.get("uri");
	}
	
	public String getName() {
		return (String)internalState.get("name");
	}
}
