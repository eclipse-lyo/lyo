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

import static org.junit.Assert.*;

import java.util.Map;

import org.eclipse.lyo.core.query.ParseException;
import org.eclipse.lyo.core.query.QueryUtils;
import org.eclipse.lyo.core.query.WhereClause;
import org.junit.Test;

/**
 * Basic tests of oslc.where clause parsing
 */
public class BasicWhereTest
{
	final static String PREFIXES = "qm=<http://qm.example.com/ns>," +
			"olsc=<http://open-services.net/ns/core#>," +
			"xs=<http://www.w3.org/2001/XMLSchema>";

	@Test
	public void testWhere() throws ParseException
	{

		String[] expressions = {
				"qm:testcase=<http://example.com/tests/31459>",
				"qm:duration>=10.4",
				"oslc:create!=\"Bob\" and qm:verified!=true",
				"qm:state in [\"Done\",\"Open\"]",
				"oslc:verified_by{oslc:owner=\"Steve\" and qm:duration=-47.0} and oslc:description=\"very hairy expression\"",
				"qm:submitted<\"2011-10-10T07:00:00Z\"^^xs:dateTime",
				"oslc:label>\"The End\"@en-US"
		};

		Map<String, String> prefixMap =
				QueryUtils.parsePrefixes(PREFIXES);

		for (String expression : expressions) {
			WhereClause whereClause = (WhereClause)
					QueryUtils.parseWhere(expression, prefixMap);

			System.out.println(whereClause);
		}
	}

	@Test(expected=ParseException.class)
	public void testBadWhere() throws ParseException
	{
		Map<String, String> prefixMap = null;
		try {
			prefixMap = QueryUtils.parsePrefixes(PREFIXES);
		} catch (ParseException e) {
			fail("Couldn't parse prefixes");
		}

		QueryUtils.parseSelect("XXX", prefixMap);
	}
}
