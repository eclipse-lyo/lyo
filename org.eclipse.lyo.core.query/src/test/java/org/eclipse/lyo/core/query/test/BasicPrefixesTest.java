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
 *    Steve Pitschke - initial API and implementation
 *    Samuel Padgett - convert to JUnit tests
 *******************************************************************************/
package org.eclipse.lyo.core.query.test;

import org.eclipse.lyo.core.query.ParseException;
import org.eclipse.lyo.core.query.QueryUtils;
import org.junit.Test;

/**
 * Basic tests of oslc.searchTerms clause parsing
 */
public class BasicPrefixesTest
{
	@Test
    public void testPrefixes() throws ParseException
    {
		QueryUtils.parsePrefixes("qm=<http://qm.example.com/ns>," +
				"olsc=<http://open-services.net/ns/core#>," +
				"xs=<http://www.w3.org/2001/XMLSchema>");
    }
	
	@Test(expected=ParseException.class)
	public void testBadPrefixes() throws ParseException
	{
		QueryUtils.parsePrefixes("qm=<http://qm.example.com/ns>,XXX>");
	}
}
