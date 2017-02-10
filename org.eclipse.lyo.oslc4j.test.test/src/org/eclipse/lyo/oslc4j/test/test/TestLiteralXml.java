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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

import junit.framework.TestCase;

import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.core.model.Publisher;
import org.eclipse.lyo.oslc4j.provider.jena.OslcRdfXmlProvider;

public class TestLiteralXml
	   extends TestCase
{
	private static final String XMLLITERAL = "rdf:datatype=\"" + OslcConstants.RDF_NAMESPACE + "XMLLiteral\"";
	
	public void testLiteralXml()
		   throws Exception
	{
		final Publisher publisher = new Publisher("<b>Publisher1/b>", // Intentionally not well-formed XML
												  "publisher1");
		
		final ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
		
		final OslcRdfXmlProvider oslcRdfXmlProvider = new OslcRdfXmlProvider();
		
		oslcRdfXmlProvider.writeTo(publisher, 
								   Publisher.class, 
								   Publisher.class,
								   null, 
								   OslcMediaType.APPLICATION_XML_TYPE, 
								   null,
								   byteArrayOutputStream);	 

		final String string = byteArrayOutputStream.toString();
//		  System.out.println(string);
		
		assertTrue(string.indexOf(XMLLITERAL) >= 0);
		
		final ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(byteArrayOutputStream.toByteArray());
		
		@SuppressWarnings({
			"unchecked",
			"rawtypes"
		})
		final Object object = oslcRdfXmlProvider.readFrom((Class) Publisher.class,
														  Publisher.class,
														  null,
														  OslcMediaType.APPLICATION_XML_TYPE,
														  null,
														  byteArrayInputStream);
		
		assertTrue(object instanceof Publisher);
		
		assertEquals(publisher.getTitle(), ((Publisher) object).getTitle());
	}
}