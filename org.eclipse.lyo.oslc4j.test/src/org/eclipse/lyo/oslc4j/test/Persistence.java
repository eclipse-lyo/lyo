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
package org.eclipse.lyo.oslc4j.test;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.TreeMap;

import javax.xml.datatype.DatatypeConfigurationException;

import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.RDFWriter;
import org.apache.jena.util.FileUtils;

public class Persistence
{
	private final static TreeMap<Long, Test> TESTS_MAP = new TreeMap<Long, Test>();

	private static long MAX_IDENTIFIER;

	private Persistence()
	{
		super();
	}

	public static void load(final String uriString)
		   throws DatatypeConfigurationException,
				  FileNotFoundException,
				  IllegalAccessException,
				  IllegalArgumentException,
				  InstantiationException,
				  InvocationTargetException,
				  OslcCoreApplicationException,
				  URISyntaxException,
				  SecurityException,
				  NoSuchMethodException
	{
		final String fileName = createFileName(uriString);

		final File file = new File(fileName);

		if ((file.exists()) &&
			(file.isFile()) &&
			(file.canRead()))
		{
			final Model model = ModelFactory.createDefaultModel();

			model.read(new FileInputStream(file),
					   null,
					   FileUtils.langXMLAbbrev);

			final Object[] resources = JenaModelHelper.fromJenaModel(model,
																	 Test.class);

			synchronized(TESTS_MAP)
			{
				TESTS_MAP.clear();

				if (resources != null)
				{
					for (final Object resource : resources)
					{
						if (resource instanceof Test)
						{
							final Test test = (Test) resource;

							final String identifier = test.getIdentifier();

							final long longIdentifier = Long.parseLong(identifier);

							MAX_IDENTIFIER = Math.max(longIdentifier, MAX_IDENTIFIER);

							TESTS_MAP.put(Long.valueOf(longIdentifier),
										  test);
						}
					}
				}
			}
		}
	}

	public static void save(final String uriString)
		   throws URISyntaxException,
				  OslcCoreApplicationException,
				  IllegalArgumentException,
				  DatatypeConfigurationException,
				  IllegalAccessException,
				  InvocationTargetException,
				  FileNotFoundException
	{
		final String fileName = createFileName(uriString);

		final Test[] tests = getTests();

		final Model model = JenaModelHelper.createJenaModel(tests);

		if (model != null)
		{
			final RDFWriter writer = model.getWriter(FileUtils.langXMLAbbrev);

			writer.setProperty("showXmlDeclaration", "true");

			writer.write(model, new FileOutputStream(fileName), null);
		}
	}

	private static String createFileName(final String uriString)
			throws URISyntaxException
	{
		final URI uri = new URI(uriString);

		final String host = uri.getHost();
		final int	 port = uri.getPort();
		final String path = uri.getPath();

		final String tmpDir = System.getProperty("java.io.tmpdir");

		final String fileName = tmpDir + "/" + host + "_" + port + path.replace('/', '_').replace('\\', '_') + ".xml";

		return fileName;
	}

	public static long getNextIdentifier()
	{
		synchronized(TESTS_MAP)
		{
			return ++MAX_IDENTIFIER;
		}
	}

	public static Test[] getTests()
	{
		synchronized (TESTS_MAP)
		{
			return TESTS_MAP.values().toArray(new Test[TESTS_MAP.size()]);
		}
	}

	public static Test getTest(final String identifier)
	{
		synchronized (TESTS_MAP)
		{
			return TESTS_MAP.get(Long.valueOf(identifier));
		}
	}

	public static void addTest(final Test test)
	{
		synchronized (TESTS_MAP)
		{
			TESTS_MAP.put(Long.valueOf(test.getIdentifier()),
						  test);
		}
	}

	public static Test updateTest(final String identifier,
								  final Test   test)
	{
		final Long longIdentifier = Long.valueOf(identifier);

		synchronized (TESTS_MAP)
		{
			final Test existingTest = TESTS_MAP.get(longIdentifier);

			if (existingTest != null)
			{
				TESTS_MAP.put(longIdentifier,
							  test);

				return test;
			}
		}

		return null;
	}

	public static Test deleteTest(final String identifier)
	{
		synchronized (TESTS_MAP)
		{
			return TESTS_MAP.remove(Long.valueOf(identifier));
		}
	}
}
