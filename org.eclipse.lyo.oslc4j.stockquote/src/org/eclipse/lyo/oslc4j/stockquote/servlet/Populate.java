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
package org.eclipse.lyo.oslc4j.stockquote.servlet;

import java.net.URI;
import java.net.URISyntaxException;

import org.eclipse.lyo.oslc4j.stockquote.Exchange;
import org.eclipse.lyo.oslc4j.stockquote.Persistence;
import org.eclipse.lyo.oslc4j.stockquote.StockQuote;
import org.eclipse.lyo.oslc4j.stockquote.Utilities;

final class Populate
{
	private final String basePath;
	private final URI	 serviceProviderURI;

	public Populate(final String  basePath,
					final URI	  serviceProviderURI)
	{
		super();

		this.basePath			= basePath;
		this.serviceProviderURI = serviceProviderURI;
	}

	public void fixup()
	{
		final StockQuote[] stockQuotes = Persistence.getStockQuotes();

		for (final StockQuote stockQuote : stockQuotes)
		{
			stockQuote.setServiceProvider(serviceProviderURI);
		}
	}

	public void populate()
		   throws URISyntaxException
	{
		persistStockQuote(createStockQuote(Exchange.NASDAQ, "AAPL"));
		persistStockQuote(createStockQuote(Exchange.NYSE,	"F"));
		persistStockQuote(createStockQuote(Exchange.NASDAQ, "GOOG"));
		persistStockQuote(createStockQuote(Exchange.NYSE,	"IBM"));
		persistStockQuote(createStockQuote(Exchange.NASDAQ, "NFLX"));
		persistStockQuote(createStockQuote(Exchange.NYSE,	"UPS"));
		persistStockQuote(createStockQuote(Exchange.NYSE,	"WMT"));
		persistStockQuote(createStockQuote(Exchange.NYSE,	"XOM"));
	}

	private static StockQuote createStockQuote(final Exchange exchange,
											   final String	  symbol)
	{
		final StockQuote stockQuote = new StockQuote();

		stockQuote.setExchange(exchange.toString());
		stockQuote.setSymbol(symbol);

		return stockQuote;
	}

	private void persistStockQuote(final StockQuote stockQuote)
			throws URISyntaxException
	{
		final String identifier = Utilities.createStockQuoteIdentifier(stockQuote);

		final URI about = new URI(basePath + "/stockQuotes/" + identifier);

		stockQuote.setAbout(about);
		stockQuote.setIdentifier(identifier);
		stockQuote.setServiceProvider(serviceProviderURI);

		Persistence.addStockQuote(stockQuote);
	}
}
