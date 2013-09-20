/*******************************************************************************
 * Copyright (c) 2013 IBM Corporation.
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
 *     Daniel Figueiredo Caetano - initial implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.test.customnamespace;

import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;

/**
 * Resource shape to be used by test classes.
 * 
 * @author Daniel Figueiredo Caetano
 *
 */
@OslcName("CustomNamespace")
@OslcNamespace(CustomNamespaceResource.CUSTOM_NAMESPACE)
@OslcResourceShape(title = "Test Resource", describes = CustomNamespaceResource.CUSTOM_RESOURCE_TYPE)
public class CustomNamespaceResource extends AbstractResource {

	public final static String CUSTOM_NAMESPACE = "http://customnamespace.oslc.com/ns#";
	public final static String CUSTOM_RESOURCE_TYPE = CUSTOM_NAMESPACE + "Test";
	
}