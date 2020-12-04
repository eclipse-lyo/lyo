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
package org.eclipse.lyo.client.test;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;

import java.io.InputStream;
import java.net.MalformedURLException;

import org.apache.wink.client.ClientResponse;
import org.eclipse.lyo.client.oslc.OslcClient;
import org.eclipse.lyo.client.oslc.resources.OslcQuery;
import org.eclipse.lyo.client.oslc.resources.OslcQueryParameters;
import org.eclipse.lyo.client.oslc.resources.OslcQueryResult;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

/**
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
public class OslcQueryResultTest {
	@Before
	public void clearPublicURISystemProperty() throws MalformedURLException {
		System.clearProperty(OslcQueryResult.SELECT_ANY_MEMBER);
	}

	@Test
	public void testEmpty() {
		 ClientResponse mockedResponse = mockClientResponse("/emptyQuery.rdf");

		 OslcQueryParameters params = new OslcQueryParameters();
		 params.setWhere("dceterms:identifier=3");
		 OslcQuery query = new OslcQuery(new OslcClient(), "http://example.com/provider/query", params);
		 OslcQueryResult result = new OslcQueryResult(query, mockedResponse);
		 assertEquals(0, result.getMembersUrls().length);
	}

	@Test
	public void testNoParameters() {
		 ClientResponse mockedResponse = mockClientResponse("/noParamQuery.rdf");

		 OslcQuery query = new OslcQuery(new OslcClient(), "http://example.com/provider/query");
		 OslcQueryResult result = new OslcQueryResult(query, mockedResponse);
		 assertEquals(2, result.getMembersUrls().length);
	}

	@Test
	public void testQuery() {
		 ClientResponse mockedResponse = mockClientResponse("/queryResponse.rdf");

		 OslcQueryParameters params = new OslcQueryParameters();
		 params.setWhere("ex:product=\"Product A\"");
		 OslcQuery query = new OslcQuery(new OslcClient(), "http://example.com/provider/query", params);
		 OslcQueryResult result = new OslcQueryResult(query, mockedResponse);
		 assertEquals(2, result.getMembersUrls().length);
	}

	@Test
	public void testBlogQuery() {
		 ClientResponse mockedResponse = mockClientResponse("/blogQuery.rdf");

		 OslcQueryParameters params = new OslcQueryParameters();
		 params.setSelect("dcterms:title");
		 OslcQuery query = new OslcQuery(new OslcClient(), "http://example.com/query");
		 OslcQueryResult result = new OslcQueryResult(query, mockedResponse);
		 result.setMemberProperty("http://open-services.net/ns/bogus/blogs#comment");
		 assertEquals(5, result.getMembersUrls().length);
	}

	@Test
	public void testAnyMember() {
		System.setProperty(OslcQueryResult.SELECT_ANY_MEMBER, "true");
		 ClientResponse mockedResponse = mockClientResponse("/blogQuery.rdf");

		 OslcQueryParameters params = new OslcQueryParameters();
		 params.setSelect("dcterms:title");
		 OslcQuery query = new OslcQuery(new OslcClient(), "http://example.com/query");
		 OslcQueryResult result = new OslcQueryResult(query, mockedResponse);
		 assertEquals(5, result.getMembersUrls().length);
	}

	private ClientResponse mockClientResponse(String file) {
		final InputStream is = OslcQueryResultTest.class.getResourceAsStream(file);
		ClientResponse mockedResponse = Mockito.mock(ClientResponse.class);
		when(mockedResponse.getEntity(InputStream.class)).thenReturn(is);

		return mockedResponse;
    }

}
