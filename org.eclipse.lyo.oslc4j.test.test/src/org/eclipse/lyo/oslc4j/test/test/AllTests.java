/*******************************************************************************
 * Copyright (c) 2012, 2013 IBM Corporation.
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
package org.eclipse.lyo.oslc4j.test.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests
{
	public static Test suite()
	{
		final TestSuite suite = new TestSuite(AllTests.class.getName());

		suite.addTestSuite(TestCoreResourceShapes.class);
		suite.addTestSuite(TestServiceProviderCatalog.class);
		suite.addTestSuite(TestTypesRdfXml.class);
		suite.addTestSuite(TestTypesXml.class);
		suite.addTestSuite(TestTypesTextXml.class);
		suite.addTestSuite(TestTypesJson.class);
		suite.addTestSuite(TestTypesTextTurtle.class);
		suite.addTestSuite(TestInvalid.class);
		suite.addTestSuite(TestInvalidTypesRdfXml.class);
		suite.addTestSuite(TestInvalidTypesJson.class);
		suite.addTestSuite(TestRelativeUriRdfXml.class);
		suite.addTestSuite(TestLiteralXml.class);

		return suite;
	}
}