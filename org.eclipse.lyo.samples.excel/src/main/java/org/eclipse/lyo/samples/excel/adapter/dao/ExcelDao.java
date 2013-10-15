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
package org.eclipse.lyo.samples.excel.adapter.dao;

import org.eclipse.lyo.samples.excel.adapter.MapperTable;

import com.hp.hpl.jena.rdf.model.Model;

public interface ExcelDao {
	Model parseFile(String fileName);
	void setRelationshipUri(String relationshipUri);
	void setMapperTable(MapperTable mapperTable);
	int getNewId(String fileName);
}
