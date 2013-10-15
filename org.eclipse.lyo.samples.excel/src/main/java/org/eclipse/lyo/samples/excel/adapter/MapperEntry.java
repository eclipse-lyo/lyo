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
package org.eclipse.lyo.samples.excel.adapter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MapperEntry {
	public class Property {
		private String name;
		private String type;
		private String column;
		private String reference;
		
		public Property(String name) {
			this.name = name;
		}
		public void setType(String type) {
			this.type = type;
		}
		public void setColumn(String column) {
			this.column = column;
		}
		public void setReference(String reference) {
			this.reference = reference;
		}
		public String getName() {
			return name;
		}
		public String getType() {
			return type;
		}
		public String getColumn() {
			return column;
		}
		public String getReference() {
			return reference;
		}
	}
	private String name;
	private String type;
	private String line;
	private String uri;
	private Map<String, MapperEntry.Property> propMap = new HashMap<String, MapperEntry.Property>();
	private List<String> propNameList = new ArrayList<String>();
	
	public MapperEntry(String name) {
		this.name = name;
	}
	public void setType(String type) {
		this.type = type;
	}
	public void setLine(String line) {
		this.line = line;
	}
	public void setUri(String uri) {
		this.uri = uri;
	}
	public String getName() {
		return name;
	}
	public String getType() {
		return type;
	}
	public String getLine() {
		return line;
	}
	public String getUri() {
		return uri;
	}
	
	public MapperEntry.Property addProperty(String name) {
		MapperEntry.Property prop = new MapperEntry.Property(name);
		propMap.put(name, prop);
		propNameList.add(name);
		return prop;
	}
	public List<String> getPropertyNameList() {
		return propNameList;
	}
	public MapperEntry.Property getProperty(String name) {
		return propMap.get(name);
	}
}
