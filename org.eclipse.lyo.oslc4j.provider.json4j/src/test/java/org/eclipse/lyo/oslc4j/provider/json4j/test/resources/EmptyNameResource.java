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
 *	   Fabio Negrello - initial implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.provider.json4j.test.resources;

import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;

@OslcName("")
@OslcNamespace(EmptyNameResource.TEST_NAMESPACE)
@OslcResourceShape(title = "Test Resource", describes = EmptyNameResource.TEST_RESOURCE_TYPE)
public class EmptyNameResource extends AbstractResource {
	public final static String TEST_NAMESPACE = "http://example.com/ns#";
	public final static String TEST_RESOURCE_TYPE = TEST_NAMESPACE + "Test";
}
