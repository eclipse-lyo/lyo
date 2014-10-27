/*******************************************************************************
 * Copyright (c) 2012, 2014 IBM Corporation.
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
 *	   Samuel Padgett		- remove missing dialog
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.stockquote.resources;

import java.net.URI;
import java.net.URISyntaxException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.apache.wink.client.ClientConfig;
import org.apache.wink.client.ClientResponse;
import org.apache.wink.client.Resource;
import org.apache.wink.client.RestClient;
import org.apache.wink.json4j.JSONArray;
import org.apache.wink.json4j.JSONException;
import org.apache.wink.json4j.JSONObject;
import org.eclipse.lyo.oslc4j.core.annotation.OslcCreationFactory;
import org.eclipse.lyo.oslc4j.core.annotation.OslcQueryCapability;
import org.eclipse.lyo.oslc4j.core.annotation.OslcService;
import org.eclipse.lyo.oslc4j.core.model.Compact;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.stockquote.Constants;
import org.eclipse.lyo.oslc4j.stockquote.Persistence;
import org.eclipse.lyo.oslc4j.stockquote.StockQuote;
import org.eclipse.lyo.oslc4j.stockquote.Utilities;
import org.eclipse.lyo.oslc4j.stockquote.servlet.ServiceProviderSingleton;

@OslcService(Constants.STOCK_QUOTE_DOMAIN)
@Path("stockQuotes")
public class StockQuoteResource
{
	private static final Logger logger = Logger.getLogger(StockQuoteResource.class.getName());

	public StockQuoteResource()
	{
		super();
	}

	@OslcQueryCapability
	(
		title = "Stock Quote Query Capability",
		label = "Stock Quote Catalog Query",
		resourceShape = OslcConstants.PATH_RESOURCE_SHAPES + "/" + Constants.PATH_STOCK_QUOTE,
		resourceTypes = {Constants.TYPE_STOCK_QUOTE},
		usages = {OslcConstants.OSLC_USAGE_DEFAULT}
	)
	@GET
	@Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_XML, OslcMediaType.TEXT_XML, OslcMediaType.APPLICATION_JSON})
	public StockQuote[] getStockQuotes()
		   throws JSONException
	{
		final StockQuote[] stockQuotes = Persistence.getStockQuotes();

		retrieveStockQuoteInformation(stockQuotes);

		for (final StockQuote stockQuote : stockQuotes)
		{
			stockQuote.setServiceProvider(ServiceProviderSingleton.getServiceProviderURI());
		}

		return stockQuotes;
	}

	@GET
	@Path("{stockQuoteId}")
	@Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_XML, OslcMediaType.TEXT_XML, OslcMediaType.APPLICATION_JSON})
	public StockQuote getStockQuote(@PathParam("stockQuoteId") final String stockQuoteId)
		   throws JSONException
	{
		final StockQuote stockQuote = Persistence.getStockQuote(stockQuoteId);

		if (stockQuote != null)
		{
			retrieveStockQuoteInformation(stockQuote);

			stockQuote.setServiceProvider(ServiceProviderSingleton.getServiceProviderURI());

			return stockQuote;
		}

		throw new WebApplicationException(Status.NOT_FOUND);
	}

	@GET
	@Path("{stockQuoteId}")
	@Produces({OslcMediaType.APPLICATION_X_OSLC_COMPACT_XML, OslcMediaType.APPLICATION_X_OSLC_COMPACT_JSON})
	public Compact getCompact(@PathParam("stockQuoteId") final String stockQuoteId)
	{
		final StockQuote stockQuote = Persistence.getStockQuote(stockQuoteId);

		if (stockQuote != null)
		{
			final Compact compact = new Compact();

			compact.setAbout(stockQuote.getAbout());
			compact.setShortTitle(stockQuote.getTitle());
			compact.setTitle(stockQuote.getTitle());

			// TODO - Need icon for stock quote compact

			return compact;
		}

		throw new WebApplicationException(Status.NOT_FOUND);
	}

	@OslcCreationFactory
	(
		 title = "Stock Quote Creation Factory",
		 label = "Stock Quote Creation",
		 resourceShapes = {OslcConstants.PATH_RESOURCE_SHAPES + "/" + Constants.PATH_STOCK_QUOTE},
		 resourceTypes = {Constants.TYPE_STOCK_QUOTE},
		 usages = {OslcConstants.OSLC_USAGE_DEFAULT}
	)
	@POST
	@Consumes({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_XML, OslcMediaType.TEXT_XML, OslcMediaType.APPLICATION_JSON})
	@Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_XML, OslcMediaType.TEXT_XML, OslcMediaType.APPLICATION_JSON})
	public Response addStockQuote(@Context final HttpServletRequest httpServletRequest,
										   final StockQuote			stockQuote)
		   throws URISyntaxException
	{
		final String identifier = Utilities.createStockQuoteIdentifier(stockQuote);

		final URI about = new URI(httpServletRequest.getScheme(),
								  null,
								  httpServletRequest.getServerName(),
								  httpServletRequest.getServerPort(),
								  httpServletRequest.getContextPath() + "/stockQuotes/" + identifier,
								  null,
								  null);

		stockQuote.setAbout(about);
		stockQuote.setIdentifier(identifier);
		stockQuote.setServiceProvider(ServiceProviderSingleton.getServiceProviderURI());

		Persistence.addStockQuote(stockQuote);

		return Response.created(about).entity(stockQuote).build();
	}

	@DELETE
	@Path("{stockQuoteId}")
	public Response deleteStockQuote(@PathParam("stockQuoteId") final String stockQuoteId)
	{
		final StockQuote stockQuote = Persistence.deleteStockQuote(stockQuoteId);

		if (stockQuote != null)
		{
			return Response.noContent().build();
		}

		throw new WebApplicationException(Status.NOT_FOUND);
	}

	private static void retrieveStockQuoteInformation(final StockQuote ... stockQuotes)
			throws JSONException
	{
		// We will use the google stock api
		// Example:	 http://www.google.com/finance/info?infotype=infoquoteall&q=IBM,Goog

		final Map<String, StockQuote> map = new HashMap<String, StockQuote>();

		String uri = "http://www.google.com/finance/info?infotype=infoquoteall&q=";

		boolean first = true;
		for (final StockQuote stockQuote : stockQuotes)
		{
			final String requestedExchange = stockQuote.getExchange();
			final String requestedSymbol   = stockQuote.getSymbol();

			final String googleKey = requestedExchange + ":" + requestedSymbol;

			if (first)
			{
				first = false;
			}
			else
			{
				uri += ",";
			}

			uri += googleKey;

			map.put(stockQuote.getIdentifier(),
					stockQuote);
		}

		final ClientConfig clientConfig = new ClientConfig();

		final RestClient restClient = new RestClient(clientConfig);

		final Resource resource = restClient.resource(uri);

		final ClientResponse clientResponse = resource.accept(MediaType.APPLICATION_JSON).get();

		if (HttpServletResponse.SC_OK == clientResponse.getStatusCode())
		{
			final String result = clientResponse.getEntity(String.class);

			// We have to strip off leading // in Google response

			int indexOf = result.indexOf('[');

			if (indexOf < 0)
			{
				indexOf = 0;
			}

			final JSONArray jsonArray = new JSONArray(result.substring(indexOf));

			// Dec 9, 11:34AM EST
			final DateFormat dateFormat = new SimpleDateFormat("MMM dd, hh:mma z");

			for (final Object object : jsonArray)
			{
				if (object instanceof JSONObject)
				{
					final JSONObject jsonObject = (JSONObject) object;

					final Object exchange = jsonObject.opt("e");
					final Object ticker	  = jsonObject.opt("t");

					if (!(exchange instanceof String))
					{
						logger.warning("Exchange not String.  Value is " + String.valueOf(exchange));
					}
					else if (!(ticker instanceof String))
					{
						logger.warning("Ticker not String.	Value is " + String.valueOf(ticker));
					}
					else
					{
						final String mapKey = Utilities.createStockQuoteIdentifier(exchange.toString(),
																				   ticker.toString());

						final StockQuote stockQuote = map.get(mapKey);

						if (stockQuote == null)
						{
							logger.warning("Stock quote resource not found for exchange " + exchange + ", symbol " + ticker);
						}
						else
						{
							final Object change		   = jsonObject.opt("c");
							final Object changePercent = jsonObject.opt("cp");
							final Object hi			   = jsonObject.opt("hi");
							final Object hi52		   = jsonObject.opt("hi52");
							final Object last		   = jsonObject.opt("l");
							final Object lo			   = jsonObject.opt("lo");
							final Object lo52		   = jsonObject.opt("lo52");
							final Object lastTrade	   = jsonObject.opt("lt");
							final Object name		   = jsonObject.opt("name");
							final Object open		   = jsonObject.opt("op");

							stockQuote.setChangePrice(toFloat(change));
							stockQuote.setChangePricePercentage(toFloat(changePercent));
							stockQuote.setHighPrice(toFloat(hi));
							stockQuote.setHigh52WeekPrice(toFloat(hi52));
							stockQuote.setLastTradedPrice(toFloat(last));
							stockQuote.setLastTradedDate(toDate(dateFormat, lastTrade));
							stockQuote.setLowPrice(toFloat(lo));
							stockQuote.setLow52WeekPrice(toFloat(lo52));
							stockQuote.setTitle(toString(name));
							stockQuote.setOpenPrice(toFloat(open));
						}
					}
				}
			}
		}
	}

	private static String toString(final Object object)
	{
		if (object != null)
		{
			return object.toString();
		}

		return null;
	}

	private static Float toFloat(final Object object)
	{
		if (object != null)
		{
			try
			{
				return Float.valueOf(object.toString());
			}
			catch (final NumberFormatException exception)
			{
				logger.warning("Unable to parse '" + object + "' as float");
			}
		}

		return null;

	}

	private static Date toDate(final DateFormat dateFormat,
							   final Object		object)
	{
		if (object != null)
		{
			try
			{
				return dateFormat.parse(object.toString());
			}
			catch (final ParseException exception)
			{
				logger.warning("Unable to parse '" + object + "' as date");
			}
		}

		return null;
	}
}
