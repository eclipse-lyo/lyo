/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
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
 *     Samuel Padgett - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.model;

import java.util.Map;

import javax.xml.namespace.QName;

/**
 * A resource that can hold unknown properties and content. If a setter is not
 * found for a property when reading a resource, it is added as an extended
 * property. These extended properties are preserved when writing the resource
 * back out, for instance on a PUT request. In OSLC, clients MUST preserve
 * unknown content when performing updates of resources.
 * 
 * @see <a href="http://open-services.net/bin/view/Main/OslcCoreSpecification?sortcol=table;up=#Unknown_properties_and_content">OSLC Core 2.0: Unknown properties and content</a>
 */
public interface IExtendedResource extends IResource
{	
	/**
	 * Sets extended properties not defined in the bean.
	 * 
	 * @param properties
	 *            a map of properties where the key is the predicate qualified
	 *            name and the value is the object of the statement. Values are
	 *            collections if there are multiple statements for a predicate.
	 */
	public void setExtendedProperties(Map<QName, Object> properties);
	
	/**
	 * Gets back the list of extended properties not defined in this bean.
	 * 
	 * @return the extended properties, a map of properties where the key is the
	 *         predicate qualified name and the value is the object of the
	 *         statement
	 */
	public Map<QName, Object> getExtendedProperties();
}
