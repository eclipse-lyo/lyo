/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
 *
 * See the NOTICE file(s) distributed with this work for additional
 * information regarding copyright ownership.
 *
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0, or the Eclipse Distribution License 1.0
 * which is available at http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Simple
 */
package org.eclipse.lyo.core.query.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.List;
import java.util.Map;

import org.eclipse.lyo.core.query.ComparisonTerm;
import org.eclipse.lyo.core.query.PName;
import org.eclipse.lyo.core.query.ParseException;
import org.eclipse.lyo.core.query.QueryUtils;
import org.eclipse.lyo.core.query.SimpleTerm;
import org.eclipse.lyo.core.query.UriRefValue;
import org.eclipse.lyo.core.query.Value;
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
	
	@Test
	public void testUriRef() throws ParseException {
		Map<String, String> prefixMap = QueryUtils.parsePrefixes(PREFIXES);
		WhereClause where = QueryUtils.parseWhere(
				"qm:testCase=<http://example.org/tests/24>", prefixMap);
		
		List<SimpleTerm> children = where.children();
		assertEquals("Where clause should only have one term", 1, children.size());
		
		SimpleTerm simpleTerm = children.get(0);
		PName prop = simpleTerm.property();
		assertEquals(prop.namespace + prop.local, "http://qm.example.com/nstestCase");
		assertTrue(simpleTerm instanceof ComparisonTerm);
	
		ComparisonTerm comparison = (ComparisonTerm) simpleTerm;
		assertEquals(comparison.operator(), ComparisonTerm.Operator.EQUALS);
		
		Value v = comparison.operand();
		assertTrue(v instanceof UriRefValue);
		
		UriRefValue uriRef = (UriRefValue) v;
		assertEquals("http://example.org/tests/24", uriRef.value());
	}
}
