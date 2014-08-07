/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
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
 *	   Russell Boykin		- initial API and implementation
 *	   Alberto Giammaria	- initial API and implementation
 *	   Chris Peters			- initial API and implementation
 *	   Gianluca Bernardini	- initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.test;

public interface Constants
{
	public static String TEST_DOMAIN		   = "http://ibm.com/ns/test#";
	public static String TEST_NAMESPACE		   = "http://ibm.com/ns/test#";
	public static String TEST_NAMESPACE_PREFIX = "oslc_test";

	public static String TYPE_NESTED = TEST_NAMESPACE + "Nested";
	public static String TYPE_TEST	 = TEST_NAMESPACE + "Test";

	public static String PATH_NESTED = "nested";
	public static String PATH_TEST	 = "test";

	public static String USAGE_COLLECTION				 = TEST_DOMAIN + "collection";
	public static String USAGE_ERROR					 = TEST_DOMAIN + "error";
	public static String USAGE_MESSAGE_BODY_WRITER_ERROR = TEST_DOMAIN + "messageBodyWriterError";
}
