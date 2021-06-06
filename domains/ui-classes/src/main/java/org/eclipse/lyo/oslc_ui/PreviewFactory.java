/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
 *
 * See the NOTICE file(s) distributed with this work for additional
 * information regarding copyright ownership.
 *
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0, or the Eclipse Distribution License 1.0
 * which is available at http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause
 */
package org.eclipse.lyo.oslc_ui;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.ValueType;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

public class PreviewFactory {

	public static Preview getPreview(final AbstractResource aResource, List<String> getterMethodNames, boolean showPropertyHeadingsAsLinks) throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException {
			ArrayList<Property> previewItems = new ArrayList<Property>();
			Method getterMethod;
			for (String getterMethodName : getterMethodNames) {
				getterMethod = aResource.getClass().getMethod(getterMethodName);
				boolean multiple = getterMethod.getAnnotation(OslcOccurs.class).value().equals(Occurs.ZeroOrMany) || getterMethod.getAnnotation(OslcOccurs.class).value().equals(Occurs.OneOrMany);
				boolean showPropertyValueAsLink = getterMethod.getAnnotation(OslcValueType.class).value().equals(ValueType.Resource);
				PropertyDefintion key;
				if (showPropertyHeadingsAsLinks) {
					key = constructPropertyDefintion(getterMethod.getAnnotation(OslcPropertyDefinition.class).value(), getterMethod.getAnnotation(OslcName.class).value());
				}
				else {
					key = constructPropertyDefintion(getterMethod.getAnnotation(OslcName.class).value());
				}
				PropertyValue value;
				if (showPropertyValueAsLink) {
					if (multiple) {
						Collection<Link> links = (Collection<Link>) getterMethod.invoke(aResource);
						List<org.eclipse.lyo.oslc_ui.Link> l = new ArrayList<org.eclipse.lyo.oslc_ui.Link>();
						for(Link link : links) {
							if (StringUtils.isBlank(link.getLabel())) {
								l.add(constructLink(link.getValue().toString(), link.getValue().toString()));
							}
							else {
								l.add(constructLink(link.getValue().toString(), link.getLabel()));
							}
						}
						value = constructPropertyValue(PropertyDefintion.RepresentationType.LINK, multiple, l);
					}
					else {
						value = constructPropertyValue(PropertyDefintion.RepresentationType.LINK, multiple, getterMethod.invoke(aResource));
					}
				}
				else {
					value = constructPropertyValue(PropertyDefintion.RepresentationType.TEXT, multiple, getterMethod.invoke(aResource));
				}
				previewItems.add(constructProperty(key, value));
			}
			Preview oslcPreviewDataSet = new Preview();
			oslcPreviewDataSet.setProperties(previewItems);
			return oslcPreviewDataSet; 
	}

	public static String getPreviewAsJsonString(final AbstractResource aResource, List<String> getterMethodNames, boolean showPropertyHeadingsAsLinks) throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException, JsonProcessingException {
		Preview preview = PreviewFactory.getPreview(aResource, getterMethodNames, showPropertyHeadingsAsLinks);
		ObjectMapper mapper = new ObjectMapper();
		String previewAsString = mapper.writeValueAsString(preview);
		return previewAsString;
	}

	
	private static Property constructProperty(PropertyDefintion propertyDefintion, PropertyValue propertyValue) {
		Property item = new Property();
		item.setPropertyDefintion(propertyDefintion);
		item.setPropertyValue(propertyValue);
		return item; 
	}

    private static PropertyValue constructPropertyValue(PropertyDefintion.RepresentationType representationType, Boolean representAsList, Object data) {
		PropertyValue value = new PropertyValue();
		value.setRepresentationType(representationType);
		value.setRepresentAsList(representAsList);
		value.setData(data);
		return value;
	}
	
	private static PropertyDefintion getPropertyDefintion(PropertyDefintion.RepresentationType representationType, Object data) {
		PropertyDefintion key = new PropertyDefintion();
		key.setRepresentationType(representationType);
		key.setData(data);
		return key; 
	}
	private static PropertyDefintion constructPropertyDefintion(String dataAsString) {
    	return getPropertyDefintion(PropertyDefintion.RepresentationType.TEXT, dataAsString);
	}
	private static PropertyDefintion constructPropertyDefintion(String linkUri, String linkTitle) {
    	return getPropertyDefintion(PropertyDefintion.RepresentationType.LINK, constructLink(linkUri, linkTitle));
	}
	
    private static org.eclipse.lyo.oslc_ui.Link constructLink(String link, String title) {
    	org.eclipse.lyo.oslc_ui.Link l = new org.eclipse.lyo.oslc_ui.Link();
		l.setLink(link);
		l.setTitle(title);
		return l;
	}
}
