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
package org.eclipse.lyo.oslc4j.test.test;

import java.io.StringBufferInputStream;
import java.net.URI;

import junit.framework.TestCase;

import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.provider.jena.OslcRdfXmlProvider;
import org.eclipse.lyo.oslc4j.test.Constants;

@SuppressWarnings("deprecation")
public class TestRelativeUriRdfXml
	   extends TestCase
{
	private static final String URI_BASE	 = "http://www.ibm.com";
	private static final String URI_RELATIVE = "relative";

	private static final String RELATIVE_URI_RDF_XML = "<?xml version=\"1.0\"?>\n" +
													   "<rdf:RDF xmlns:rdf=\"" + OslcConstants.RDF_NAMESPACE + "\" " +
																"xmlns:oslc_test=\"" + Constants.TEST_NAMESPACE + "\" " +
																"xml:base=\"" + URI_BASE + "\">\n" +
													   "	<oslc_test:RelativeUri>\n" +
													   "		<oslc_test:uri rdf:resource=\"" + URI_RELATIVE + "\"/>\n" +
													   "	</oslc_test:RelativeUri>\n" +
													   "</rdf:RDF>";

	public void testRelativeUriRdfXml()
		   throws Exception
	{
		final StringBufferInputStream stringBufferInputStream = new StringBufferInputStream(RELATIVE_URI_RDF_XML);

		final OslcRdfXmlProvider oslcRdfXmlProvider = new OslcRdfXmlProvider();

		@SuppressWarnings({
			"unchecked",
			"rawtypes"
		})
		final Object object = oslcRdfXmlProvider.readFrom((Class) RelativeUri.class,
														  null,
														  null,
														  null,
														  null,
														  stringBufferInputStream);

		assertTrue(object instanceof RelativeUri);

		final RelativeUri relativeUri = (RelativeUri) object;

		assertEquals(new URI(URI_BASE + "/" + URI_RELATIVE),
					 relativeUri.getUri());
	}
}
