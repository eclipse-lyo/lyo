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
 *	   Daniel Figueiredo Caetano - initial implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.test.customnamespace;

/**
 * Common constants used to the test the global and custom namespace.
 *	
 * @author Daniel Figueiredo Caetano
 *
 */
public interface CustomNamespaceConstants {

	/*
	 * Test prefixes
	 */
	String TEST1_PREFIX	 = "test1";
	String TEST2_PREFIX	 = "test2";
	String CUSTOM_PREFIX = "custom";
	String GLOBAL_PREFIX = "global";

	/*
	 * Test namespaces 
	 */
	String TEST1_URL  = "http://test1.oslc4j.com#";
	String TEST2_URL  = "http://test2.oslc4j.com#";
	String CUSTOM_URL = "http://custom.oslc4j.com#";
	String GLOBAL_URL = "http://global.oslc4j.com#";
	
}