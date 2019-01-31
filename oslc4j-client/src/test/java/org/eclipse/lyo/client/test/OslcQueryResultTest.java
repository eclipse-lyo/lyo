/*******************************************************************************
 * Copyright (c) 2013, 2014 IBM Corporation and others.
 *
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the Eclipse Public License v1.0
 *  and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 *  The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 *  and the Eclipse Distribution License is available at
 *  http://www.eclipse.org/org/documents/edl-v10.php.
 *
 *  Contributors:
 *
 *     Samuel Padgett - initial API and implementation
 *     Samuel Padgett - test member property on call to OslcQueryResult.getNext()
 *     Samuel Padgett - remove previous test as it causes problems in our Hudson builds
 *******************************************************************************/
package org.eclipse.lyo.client.test;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;

import java.io.InputStream;
import java.net.MalformedURLException;


import javax.ws.rs.core.Response;
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
		 Response mockedResponse = mockClientResponse("/emptyQuery.rdf");

		 OslcQueryParameters params = new OslcQueryParameters();
		 params.setWhere("dceterms:identifier=3");
		 OslcQuery query = new OslcQuery(new OslcClient(), "http://example.com/provider/query", params);
		 OslcQueryResult result = new OslcQueryResult(query, mockedResponse);
		 assertEquals(0, result.getMembersUrls().length);
	}

	@Test
	public void testNoParameters() {
		 Response mockedResponse = mockClientResponse("/noParamQuery.rdf");

		 OslcQuery query = new OslcQuery(new OslcClient(), "http://example.com/provider/query");
		 OslcQueryResult result = new OslcQueryResult(query, mockedResponse);
		 assertEquals(2, result.getMembersUrls().length);
	}

	@Test
	public void testQuery() {
		 Response mockedResponse = mockClientResponse("/queryResponse.rdf");

		 OslcQueryParameters params = new OslcQueryParameters();
		 params.setWhere("ex:product=\"Product A\"");
		 OslcQuery query = new OslcQuery(new OslcClient(), "http://example.com/provider/query", params);
		 OslcQueryResult result = new OslcQueryResult(query, mockedResponse);
		 assertEquals(2, result.getMembersUrls().length);
	}

	@Test
	public void testBlogQuery() {
		 Response mockedResponse = mockClientResponse("/blogQuery.rdf");

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
		 Response mockedResponse = mockClientResponse("/blogQuery.rdf");

		 OslcQueryParameters params = new OslcQueryParameters();
		 params.setSelect("dcterms:title");
		 OslcQuery query = new OslcQuery(new OslcClient(), "http://example.com/query");
		 OslcQueryResult result = new OslcQueryResult(query, mockedResponse);
		 assertEquals(5, result.getMembersUrls().length);
	}

	private Response mockClientResponse(String file) {
		final InputStream is = OslcQueryResultTest.class.getResourceAsStream(file);
		Response mockedResponse = Mockito.mock(Response.class);
		when(mockedResponse.readEntity(InputStream.class)).thenReturn(is);

		return mockedResponse;
    }

}
