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
package org.eclipse.lyo.oslc4j.stockquote;

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
	private final static TreeMap<String, StockQuote> STOCK_QUOTES_MAP = new TreeMap<String, StockQuote>();

	private Persistence()
	{
		super();
	}

	public static boolean load(final String uriString)
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
																	 StockQuote.class);

			synchronized(STOCK_QUOTES_MAP)
			{
				STOCK_QUOTES_MAP.clear();

				if (resources != null)
				{
					for (final Object resource : resources)
					{
						if (resource instanceof StockQuote)
						{
							final StockQuote stockQuote = (StockQuote) resource;

							final String identifier = stockQuote.getIdentifier();

							STOCK_QUOTES_MAP.put(identifier,
												 stockQuote);
						}
					}
				}
			}

			return true;
		}

		return false;
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

		final StockQuote[] stockQuotes = getStockQuotes();

		final Model model = JenaModelHelper.createJenaModel(stockQuotes);

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

	public static StockQuote[] getStockQuotes()
	{
		synchronized (STOCK_QUOTES_MAP)
		{
			return STOCK_QUOTES_MAP.values().toArray(new StockQuote[STOCK_QUOTES_MAP.size()]);
		}
	}

	public static StockQuote getStockQuote(final String identifier)
	{
		synchronized (STOCK_QUOTES_MAP)
		{
			return STOCK_QUOTES_MAP.get(identifier);
		}
	}

	public static void addStockQuote(final StockQuote stockQuote)
	{
		synchronized (STOCK_QUOTES_MAP)
		{
			STOCK_QUOTES_MAP.put(stockQuote.getIdentifier(),
								 stockQuote);
		}
	}

	public static StockQuote deleteStockQuote(final String identifier)
	{
		synchronized (STOCK_QUOTES_MAP)
		{
			return STOCK_QUOTES_MAP.remove(identifier);
		}
	}
}
