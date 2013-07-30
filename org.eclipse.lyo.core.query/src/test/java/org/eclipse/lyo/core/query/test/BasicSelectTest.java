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
import org.eclipse.lyo.core.query.SelectClause;
import org.junit.Test;

/**
 * Basic tests of oslc.select clause parsing
 */
public class BasicSelectTest
{
	static final String PREFIXES = "qm=<http://qm.example.com/ns>," +
			"olsc=<http://open-services.net/ns/core#>";

	@Test
	public void testSelect() throws ParseException
	{
		String[] expressions = {
				"*{*}",
				"qm:testcase",
				"*",
				"oslc:create,qm:verified",
				"qm:state{oslc:verified_by{oslc:owner,qm:duration}}",
				"qm:submitted{*}",
				"qm:testcase,*",
				"*,qm:state{*}"
		};

		Map<String, String> prefixMap =
				QueryUtils.parsePrefixes(PREFIXES);

		for (String expression : expressions) {
			SelectClause selectClause =
					QueryUtils.parseSelect(expression, prefixMap);

			System.out.println(selectClause.toString());
		}
	}

	@Test(expected=ParseException.class)
	public void testBadSelect() throws ParseException
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
