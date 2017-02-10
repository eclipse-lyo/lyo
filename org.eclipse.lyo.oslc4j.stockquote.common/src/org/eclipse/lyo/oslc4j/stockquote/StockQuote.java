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

import java.net.URI;
import java.util.Date;

import org.eclipse.lyo.oslc4j.core.annotation.OslcAllowedValue;
import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.ValueType;

@OslcNamespace(Constants.STOCK_QUOTE_NAMESPACE)
@OslcResourceShape(title = "Stock Quote Resource Shape", describes = Constants.TYPE_STOCK_QUOTE)
public final class StockQuote
	   extends AbstractResource
{
	private Float	 changePrice;
	private Float	 changePricePercentage;
	private Exchange exchange;
	private Float	 high52WeekPrice;
	private Float	 highPrice;
	private String	 identifier;
	private Date	 lastTradedDate;
	private Float	 lastTradedPrice;
	private Float	 low52WeekPrice;
	private Float	 lowPrice;
	private Float	 openPrice;
	private URI		 serviceProvider;
	private String	 symbol;
	private String	 title;

	public StockQuote()
	{
		super();
	}

	@OslcDescription("Change in traded price for the stock.")
	@OslcPropertyDefinition(Constants.STOCK_QUOTE_NAMESPACE + "changePrice")
	@OslcReadOnly
	@OslcTitle("Change in Price")
	public Float getChangePrice()
	{
		return changePrice;
	}

	@OslcDescription("Percentage change in traded price for the stock.")
	@OslcPropertyDefinition(Constants.STOCK_QUOTE_NAMESPACE + "changePricePercentage")
	@OslcReadOnly
	@OslcTitle("Change in Price Percentage")
	public Float getChangePricePercentage()
	{
		return changePricePercentage;
	}

	@OslcAllowedValue({"NYSE", "NASDAQ"})
	@OslcDescription("The stock exchange.  Possible values are NYSE and NASDAQ.")
	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.STOCK_QUOTE_NAMESPACE + "exchange")
	@OslcTitle("Exchange")
	public String getExchange()
	{
		return exchange.toString();
	}

	@OslcDescription("High 52 week traded price for the stock.")
	@OslcPropertyDefinition(Constants.STOCK_QUOTE_NAMESPACE + "high52WeekPrice")
	@OslcReadOnly
	@OslcTitle("High 52 Week Price")
	public Float getHigh52WeekPrice()
	{
		return high52WeekPrice;
	}

	@OslcDescription("High traded price for the stock.")
	@OslcPropertyDefinition(Constants.STOCK_QUOTE_NAMESPACE + "highPrice")
	@OslcReadOnly
	@OslcTitle("High Price")
	public Float getHighPrice()
	{
		return highPrice;
	}

	@OslcDescription("A unique identifier for a resource. Assigned by the service provider when a resource is created. Not intended for end-user display.")
	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "identifier")
	@OslcReadOnly
	@OslcTitle("Identifier")
	public String getIdentifier()
	{
		return identifier;
	}

	@OslcDescription("Last traded date for the stock.")
	@OslcPropertyDefinition(Constants.STOCK_QUOTE_NAMESPACE + "lastTradedDate")
	@OslcReadOnly
	@OslcTitle("Last Traded Date")
	public Date getLastTradedDate()
	{
		return lastTradedDate;
	}

	@OslcDescription("Last traded price for the stock.")
	@OslcPropertyDefinition(Constants.STOCK_QUOTE_NAMESPACE + "lastTradedPrice")
	@OslcReadOnly
	@OslcTitle("Last Traded Price")
	public Float getLastTradedPrice()
	{
		return lastTradedPrice;
	}

	@OslcDescription("Low 52 week traded price for the stock.")
	@OslcPropertyDefinition(Constants.STOCK_QUOTE_NAMESPACE + "low52WeekPrice")
	@OslcReadOnly
	@OslcTitle("Low 52 Week Price")
	public Float getLow52WeekPrice()
	{
		return low52WeekPrice;
	}

	@OslcDescription("Low traded price for the stock.")
	@OslcPropertyDefinition(Constants.STOCK_QUOTE_NAMESPACE + "lowPrice")
	@OslcReadOnly
	@OslcTitle("Low Price")
	public Float getLowPrice()
	{
		return lowPrice;
	}

	@OslcDescription("Open traded price for the stock.")
	@OslcPropertyDefinition(Constants.STOCK_QUOTE_NAMESPACE + "openPrice")
	@OslcReadOnly
	@OslcTitle("Open Price")
	public Float getOpenPrice()
	{
		return openPrice;
	}

	@OslcDescription("The scope of a resource is a URI for the resource's OSLC Service Provider.")
	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "serviceProvider")
	@OslcRange(OslcConstants.TYPE_SERVICE_PROVIDER)
	@OslcReadOnly
	@OslcTitle("Service Provider")
	public URI getServiceProvider()
	{
		return serviceProvider;
	}

	@OslcDescription("The stock symbol.")
	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.STOCK_QUOTE_NAMESPACE + "symbol")
	@OslcTitle("Symbol")
	public String getSymbol()
	{
		return symbol;
	}

	@OslcDescription("Title (reference: Dublin Core) or often a single line summary of the resource represented as rich text in XHTML content.")
	@OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "title")
	@OslcTitle("Title")
	@OslcValueType(ValueType.XMLLiteral)
	public String getTitle()
	{
		return title;
	}

	public void setChangePrice(final Float changePrice)
	{
		this.changePrice = changePrice;
	}

	public void setChangePricePercentage(final Float changePricePercentage)
	{
		this.changePricePercentage = changePricePercentage;
	}

	public void setExchange(final String exchange)
	{
		this.exchange = Exchange.valueOf(exchange);
	}

	public void setHigh52WeekPrice(final Float high52WeekPrice)
	{
		this.high52WeekPrice = high52WeekPrice;
	}

	public void setHighPrice(final Float highPrice)
	{
		this.highPrice = highPrice;
	}

	public void setIdentifier(final String identifier)
	{
		this.identifier = identifier;
	}

	public void setLastTradedDate(final Date lastTradedDate)
	{
		this.lastTradedDate = lastTradedDate;
	}

	public void setLastTradedPrice(final Float lastTradedPrice)
	{
		this.lastTradedPrice = lastTradedPrice;
	}

	public void setLow52WeekPrice(final Float low52WeekPrice)
	{
		this.low52WeekPrice = low52WeekPrice;
	}

	public void setLowPrice(final Float lowPrice)
	{
		this.lowPrice = lowPrice;
	}

	public void setOpenPrice(final Float openPrice)
	{
		this.openPrice = openPrice;
	}

	public void setServiceProvider(final URI serviceProvider)
	{
		this.serviceProvider = serviceProvider;
	}

	public void setSymbol(final String symbol)
	{
		this.symbol = symbol;
	}

	public void setTitle(final String title)
	{
		this.title = title;
	}
}
