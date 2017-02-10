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
 *	   Samuel Padgett - initial implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.provider.jena.test.resources;

import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;

@OslcName("Test")
@OslcNamespace(TestResource.TEST_NAMESPACE)
@OslcResourceShape(title = "Test Resource", describes = TestResource.TEST_RESOURCE_TYPE)
public class TestResource extends AbstractResource {
	public final static String TEST_NAMESPACE = "http://example.com/ns#";
	public final static String TEST_RESOURCE_TYPE = TEST_NAMESPACE + "Test";
}
