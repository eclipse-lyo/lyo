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

import static org.junit.Assert.*;

import java.util.Map;

import org.eclipse.lyo.core.query.OrderByClause;
import org.eclipse.lyo.core.query.ParseException;
import org.eclipse.lyo.core.query.QueryUtils;
import org.junit.Test;

/**
 * Basic tests of oslc.orderBy clause parsing
 */
public class BasicOrderByTest
{
	static final String prefixes = "qm=<http://qm.example.com/ns>," +
			"olsc=<http://open-services.net/ns/core#>";

	@Test
	public void testOrderBy() throws ParseException
	{
		String[] expressions = {
				"+gm:priority",
				"+gm:priority,-oscl:name",
				"gm:tested_by{+oslc:description}"
		};

		Map<String, String> prefixMap = QueryUtils.parsePrefixes(prefixes);

		for (String expression : expressions) {
			OrderByClause orderByClause =
					QueryUtils.parseOrderBy(expression, prefixMap);
			System.out.println(orderByClause);
		}
	}

	@Test(expected=ParseException.class)
	public void testBadOrderBy() throws ParseException {
		Map<String, String> prefixMap = null;
		try {
			prefixMap = QueryUtils.parsePrefixes(prefixes);
		} catch (ParseException e) {
			fail("Couldn't parse prefixes");
		}
		QueryUtils.parseOrderBy("?qm:blah", prefixMap);
	}
}
