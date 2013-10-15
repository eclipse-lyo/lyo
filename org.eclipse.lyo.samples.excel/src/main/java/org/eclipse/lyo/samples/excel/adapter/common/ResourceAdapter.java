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
package org.eclipse.lyo.samples.excel.adapter.common;

import java.util.List;

import com.hp.hpl.jena.query.ResultSet;

public interface ResourceAdapter {
	void setBaseUri(String uri);
	void loadRepository();
	ResourceSet query(String uri, String context, String prefix, String select, String where, String orderBy, String searchTerms);
	ResourceSet getResource(String resourceUri);
	List<String> getContexts();
	ResultSet executeSparql(String context, String queryExp);
	String getDefaultExcelAbsolutePath();
}
