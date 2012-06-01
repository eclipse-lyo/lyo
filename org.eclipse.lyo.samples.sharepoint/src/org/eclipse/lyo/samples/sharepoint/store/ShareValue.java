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
 *     Keith Wells             - initial API and implementation
 *     Sam Padgett           - initial API and Implementation
 *     Jim Conallen           - initial API and implementation
 *
 *******************************************************************************/
package org.eclipse.lyo.samples.sharepoint.store;

import java.util.Date;
import java.util.GregorianCalendar;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import org.eclipse.lyo.samples.sharepoint.util.StringUtils;


public class ShareValue {
	private Object value = null;
	private ShareValueType type = null;

	public ShareValue(ShareValueType type, Object value)
			throws UnrecognizedValueTypeException {
		this.type = type;
		this.value = value;
	}

	/* factory methods */

	static public ShareValue createCalendarValue(Date date)
			throws UnrecognizedValueTypeException,
			DatatypeConfigurationException {
		GregorianCalendar c = new GregorianCalendar();
		c.setTime(date);
		XMLGregorianCalendar gc = DatatypeFactory.newInstance()
				.newXMLGregorianCalendar(c);
		return new ShareValue(ShareValueType.CALENDAR, gc);
	}

	static public ShareValue createUriValue(String uri)
			throws UnrecognizedValueTypeException,
			DatatypeConfigurationException {
		return new ShareValue(ShareValueType.URI, uri);
	}

	static public ShareValue createStringValue(String str)
			throws UnrecognizedValueTypeException,
			DatatypeConfigurationException {
		return new ShareValue(ShareValueType.STRING, str);
	}

	static public ShareValue createBooleanValue(boolean b)
			throws UnrecognizedValueTypeException,
			DatatypeConfigurationException {
		return new ShareValue(ShareValueType.BOOLEAN, new Boolean(b));
	}

	static public ShareValue createBooleanValue(Boolean b)
			throws UnrecognizedValueTypeException,
			DatatypeConfigurationException {
		return new ShareValue(ShareValueType.BOOLEAN, b);
	}

	static public ShareValue createDecimalValue(double d)
			throws UnrecognizedValueTypeException,
			DatatypeConfigurationException {
		return new ShareValue(ShareValueType.DECIMAL, new Double(d));
	}

	static public ShareValue createDecimalValue(Double d)
			throws UnrecognizedValueTypeException,
			DatatypeConfigurationException {
		return new ShareValue(ShareValueType.DECIMAL, d);
	}

	static public ShareValue createDecimalValue(float f)
			throws UnrecognizedValueTypeException,
			DatatypeConfigurationException {
		return new ShareValue(ShareValueType.DECIMAL, new Double(f));
	}

	static public ShareValue createDecimalValue(Float f)
			throws UnrecognizedValueTypeException,
			DatatypeConfigurationException {
		return new ShareValue(ShareValueType.DECIMAL, f);
	}

	static public ShareValue createIntegerValue(int i)
			throws UnrecognizedValueTypeException,
			DatatypeConfigurationException {
		return new ShareValue(ShareValueType.INTEGER, new Integer(i));
	}

	static public ShareValue createIntegerValue(Integer i)
			throws UnrecognizedValueTypeException,
			DatatypeConfigurationException {
		return new ShareValue(ShareValueType.INTEGER, i);
	}

	static public ShareValue createBlankNodeValue(String bn)
			throws UnrecognizedValueTypeException,
			DatatypeConfigurationException {
		return new ShareValue(ShareValueType.BLANK_NODE, bn);
	}

	/* Instance Methods */

	public Object getValue() {
		return value;
	}

	public ShareValueType getType() {
		return type;
	}

	public String rdfDataType() {
		return type.uri();
	}
	
	public boolean isBlankNode(){
		return type == ShareValueType.BLANK_NODE;
	}

	public String stringValue() {
		if (this.type == ShareValueType.CALENDAR) {
			Date date = ((XMLGregorianCalendar) this.value)
					.toGregorianCalendar().getTime();
			return StringUtils.xsdDateTime(date);
		}
		return this.value.toString();
	}

	public boolean booleanValue() throws IncompatibleValueException {
		if (type != ShareValueType.BOOLEAN)
			throw new IncompatibleValueException();
		return ((Boolean) value).booleanValue();
	}

	public int intValue() throws IncompatibleValueException {
		if (type != ShareValueType.INTEGER)
			throw new IncompatibleValueException();
		return ((Integer) value).intValue();
	}

	public float longValue() throws IncompatibleValueException {
		if (type != ShareValueType.INTEGER)
			throw new IncompatibleValueException();
		return ((Integer) value).longValue();
	}

	public double doubleValue() throws IncompatibleValueException {
		if (type != ShareValueType.DECIMAL)
			throw new IncompatibleValueException();
		return ((Double) value).doubleValue();
	}

	public float floatValue() throws IncompatibleValueException {
		if (type != ShareValueType.DECIMAL)
			throw new IncompatibleValueException();
		return ((Double) value).floatValue();
	}

	public Date dateValue() throws IncompatibleValueException {
		if (type != ShareValueType.CALENDAR)
			throw new IncompatibleValueException();
		return ((XMLGregorianCalendar) value).toGregorianCalendar().getTime();
	}

	public GregorianCalendar gregorianCalendarValue()
			throws IncompatibleValueException {
		if (type != ShareValueType.CALENDAR)
			throw new IncompatibleValueException();
		return ((XMLGregorianCalendar) value).toGregorianCalendar();
	}

	public XMLGregorianCalendar xmlGregorianCalendarValue()
			throws IncompatibleValueException {
		if (type != ShareValueType.CALENDAR)
			throw new IncompatibleValueException();
		return (XMLGregorianCalendar) value;
	}

	public java.net.URI uriValue() throws IncompatibleValueException {
		if (type == ShareValueType.URI || type == ShareValueType.BLANK_NODE)
			return (java.net.URI) value;
		throw new IncompatibleValueException();
	}

	@Override
	public String toString() {
		try{
			switch( this.type ) {
			case BLANK_NODE: {
				return this.value.toString();
			}
			case URI: {
				return '<' + this.value.toString() + '>';
			}
			case DECIMAL:
			case BOOLEAN:
			case XMLLiteral:
			case INTEGER: {
				return '"' + this.stringValue() + "\"^^<" + this.type.uri + ">";
			}
			case CALENDAR: {
				return '"' + StringUtils.xsdDateTime(this.dateValue()) + "\"^^<" + this.type.uri + ">";
			}
			case STRING: {
				return '"' + this.value.toString() + '"';
			}
			}
		} catch( Exception e ) {
			// log it
			e.printStackTrace();
		}
		return this.value.toString() + '@' + this.type.toString();
	}

	public static enum ShareValueType {
		BOOLEAN("http://www.w3.org/2001/XMLSchema#boolean"), CALENDAR(
				"http://www.w3.org/2001/XMLSchema#dateTime"), DECIMAL(
				"http://www.w3.org/2001/XMLSchema#decimal"), INTEGER(
				"http://www.w3.org/2001/XMLSchema#int"), STRING(
				"http://www.w3.org/2001/XMLSchema#string"), XMLLiteral(
				"http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral"), URI(
				"http://www.w3.org/2001/XMLSchema#anyURI"), BLANK_NODE("");

		private String uri;

		ShareValueType(String uri) {
			this.uri = uri;
		}

		public String uri() {
			return uri;
		}
	}

}
