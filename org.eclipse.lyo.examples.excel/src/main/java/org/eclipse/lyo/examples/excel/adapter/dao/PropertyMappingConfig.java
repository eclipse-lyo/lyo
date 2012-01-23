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
package org.eclipse.lyo.examples.excel.adapter.dao;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class PropertyMappingConfig {
	
	private final static String ELEMENT_MAPPING = "mapping";
	private final static String ELEMENT_PROPERTY = "property";
	private final static String ATTR_FOR = "for";
	private final static String ATTR_INDEX = "index";
	private final static String ATTR_TYPE = "type";
	private final static String ATTR_NAME = "name";
	private final static String ATTR_OF = "of";
	private final static String ATTR_FORMAT_STRING = "formatString";
	
	private Document doc;
	
	private Map<String, Node> fileToMappingNode = new HashMap<String, Node>();
	
	public PropertyMappingConfig(String defFile) {
		try {
			DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
			DocumentBuilder db = dbf.newDocumentBuilder();
			doc = db.parse(defFile);
			parse();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	private void parse() {
		NodeList nodeList = doc.getChildNodes();
		for (int i = 0; i < nodeList.getLength(); i++) {
			Node node = nodeList.item(i);
			if (node.getNodeName().equals(ELEMENT_MAPPING)) {
				String forValue = ((Element)node).getAttribute(ATTR_FOR);
				forValue.trim();
				if (forValue.length() == 0) {
					forValue = null; // default
				}
				fileToMappingNode.put(forValue, node);
			}
		}
	}
	
	public List<PropertyMappingInfo> listPropertyInfo(String forValue) {
		List<PropertyMappingInfo> list = new ArrayList<PropertyMappingInfo>();
		Node mappingNode = fileToMappingNode.get(forValue);
		if (mappingNode == null && forValue != null) {
			mappingNode = fileToMappingNode.get(null); // try default
		}
		if (mappingNode != null) {
			NodeList nodeList = mappingNode.getChildNodes();
			for (int i = 0; i < nodeList.getLength(); i++) {
				Node node = nodeList.item(i);
				PropertyMappingInfo info = getPropertyInfo(node);
				if (info != null) {
					list.add(info);
				}
			}
		}
		return list;
	}
	
	class MappingInfoImpl implements PropertyMappingInfo {
		private Element element;
		public MappingInfoImpl(Element element) {
			this.element = element;
		}
		public int getIndex() {
			try {
				String v = element.getAttribute(ATTR_INDEX);
				return Integer.parseInt(v);
			} catch (NumberFormatException e) {
			}
			return -1;
		}
		public String getPropertyType() {
			return element.getAttribute(ATTR_TYPE);
		}
		public String getPropertyName() {
			return element.getAttribute(ATTR_NAME);
		}
		public String getResourceName() {
			return element.getAttribute(ATTR_OF);
		}
		public String getFormatString() {
			return element.getAttribute(ATTR_FORMAT_STRING);
		}
		public PropertyMappingInfo getDetailInfo() {
			NodeList nodeList = element.getChildNodes();
			Node node = null;
			for (int i = 0; i < nodeList.getLength(); i++) {
				node = nodeList.item(i);
				if (node != null && node.getNodeType() == Node.ELEMENT_NODE && 
					node.getNodeName().equals(ELEMENT_PROPERTY)) {
					break;
				}
				node = null;
			}
			if (node != null) {
				return new MappingInfoImpl((Element)node);
			}
			return null;
		}
		
	}
	private PropertyMappingInfo getPropertyInfo(Node node) {
		if (node != null && node.getNodeType() == Node.ELEMENT_NODE && node.getNodeName().equals(ELEMENT_PROPERTY)) {
			return new MappingInfoImpl((Element)node);
		}
		return null;
	}
}
