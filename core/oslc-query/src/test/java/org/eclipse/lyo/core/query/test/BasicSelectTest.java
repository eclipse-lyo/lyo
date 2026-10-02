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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause
 */
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
			"oslc=<http://open-services.net/ns/core#>";

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

	@Test(expected=ParseException.class)
	public void testUnknownPrefix() throws ParseException
	{
		Map<String, String> prefixMap = QueryUtils.parsePrefixes(
				"qm=<http://qm.example.com/ns/>");

		QueryUtils.parseSelect("unknown:property", prefixMap);
	}

	@Test(expected=ParseException.class)
	public void testUnknownDefaultPrefix() throws ParseException
	{
		Map<String, String> prefixMap = QueryUtils.parsePrefixes(
				"qm=<http://qm.example.com/ns/>");

		QueryUtils.parseSelect(":property", prefixMap);
	}

	@Test(expected=ParseException.class)
	public void testMalformedSelect() throws ParseException
	{
		Map<String, String> prefixMap = QueryUtils.parsePrefixes(PREFIXES);

		QueryUtils.parseSelect("*Open\",\"In Progress\",\"Done\"", prefixMap);
	}

	@Test
	public void testDuplicatePropertiesDoNotThrow() throws ParseException
	{
		Map<String, String> prefixMap = QueryUtils.parsePrefixes(PREFIXES);

		Map<String, Object> result = QueryUtils.invertSelectedProperties(
				QueryUtils.parseSelect("qm:property,qm:property", prefixMap));

		assertEquals(1, result.size());
	}

	@Test
	public void testDuplicateNestedPropertiesAreMerged() throws ParseException
	{
		Map<String, String> prefixMap = QueryUtils.parsePrefixes(
				"qm=<http://qm.example.com/ns/>," +
				"dcterms=<http://purl.org/dc/terms/>," +
				"oslc=<http://open-services.net/ns/core#>");

		Map<String, Object> result = QueryUtils.invertSelectedProperties(
				QueryUtils.parseSelect(
						"qm:property{dcterms:title},qm:property{oslc:shortTitle}",
						prefixMap));

		assertEquals(1, result.size());
		@SuppressWarnings("unchecked")
		Map<String, Object> nested = (Map<String, Object>)
				result.get("http://qm.example.com/ns/property");
		assertTrue(nested.containsKey("http://purl.org/dc/terms/title"));
		assertTrue(nested.containsKey("http://open-services.net/ns/core#shortTitle"));
	}

	@Test
	public void testNestedWildcardWithSiblingPropertyDoesNotThrow() throws ParseException
	{
		Map<String, String> prefixMap = QueryUtils.parsePrefixes(
				"qm=<http://qm.example.com/ns/>," +
				"dcterms=<http://purl.org/dc/terms/>");

		Map<String, Object> result = QueryUtils.invertSelectedProperties(
				QueryUtils.parseSelect("qm:property,*{dcterms:title}", prefixMap));

		assertEquals(1, result.size());
		assertTrue(result.containsKey("http://qm.example.com/ns/property"));
	}
}
