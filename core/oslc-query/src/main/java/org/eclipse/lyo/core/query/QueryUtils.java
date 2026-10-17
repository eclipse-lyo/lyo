package org.eclipse.lyo.core.query;
/*
 * Copyright (c) 2022 Contributors to the Eclipse Foundation
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

import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.Token;
import org.antlr.runtime.tree.CommonErrorNode;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.Tree;
import org.eclipse.lyo.core.query.impl.CompoundTermInvocationHandler;
import org.eclipse.lyo.core.query.impl.PropertiesInvocationHandler;
import org.eclipse.lyo.core.query.impl.SortTermsInvocationHandler;
import org.eclipse.lyo.oslc4j.core.OSLC4JConstants;
import org.eclipse.lyo.oslc4j.core.NestedWildcardProperties;
import org.eclipse.lyo.oslc4j.core.SingletonWildcardProperties;

/**
 * Utility methods for parsing various OSLC HTTP query
 * parameter clauses; e.g. {@code oslc.where}
 */
public class QueryUtils
{
	/**
	 * A property list that selects all properties
	 */
	static public final Properties WILDCARD_PROPERTY_LIST = (Properties)
		Proxy.newProxyInstance(Properties.class.getClassLoader(),
				new Class<?>[] { Properties.class },
				new PropertiesInvocationHandler());

	/**
	 * Parse a oslc.prefix clause into a map between prefixes
	 * and corresponding URIs
	 *
	 * <p><b>Note</b>: {@link Object#toString()} of result has been overridden to
	 * return input expression.
	 *
	 * @param prefixExpression the oslc.prefix expression
	 *
	 * @return the prefix map
	 *
	 * @throws ParseException
	 */
	public static Map<String, String>
	parsePrefixes(
		String prefixExpression
	) throws ParseException
	{
		if (prefixExpression == null) {
			return new HashMap<>();
		}

		OslcPrefixParser parser = new OslcPrefixParser(prefixExpression);

		try {

			CommonTree rawTree = parser.oslc_prefixes().getTree();

			checkErrors(parser.getErrors());

            PrefixMap prefixMap =
				new PrefixMap(rawTree.getChildCount());

			for (int index = 0; index < rawTree.getChildCount(); index++) {

				Tree rawPrefix = rawTree.getChild(index);

				if (rawPrefix.getType() == Token.INVALID_TOKEN_TYPE) {
					throw ((CommonErrorNode)rawPrefix).trappedException;
				}

				String pn = rawPrefix.getChild(0).getText();
				String uri = rawPrefix.getChild(1).getText();

				uri = uri.substring(1, uri.length() - 1);

				prefixMap.put(pn, uri);
			}

			return prefixMap;

		} catch (RecognitionException e) {
			throw new ParseException(e);
		}
	}

	/**
	 * Parse a oslc.where expression
	 *
	 * @param whereExpression contents of an oslc.where HTTP query
	 * parameter
	 * @param prefixMap map between XML namespace prefixes and
	 * associated URLs
	 *
	 * @return the parsed where clause
	 *
	 * @throws ParseException
	 */
	public static WhereClause
	parseWhere(
		String whereExpression,
		Map<String, String> prefixMap
	) throws ParseException
	{
		OslcWhereParser parser = new OslcWhereParser(whereExpression);

		try {

			OslcWhereParser.oslc_where_return resultTree =
				parser.oslc_where();

			checkErrors(parser.getErrors());

            CommonTree rawTree = resultTree.getTree();
			Tree child = rawTree.getChild(0);

			if (child.getType() == Token.INVALID_TOKEN_TYPE) {
				throw ((CommonErrorNode)child).trappedException;
			}

			return (WhereClause)
				Proxy.newProxyInstance(CompoundTerm.class.getClassLoader(),
						new Class<?>[] { CompoundTerm.class, WhereClause.class },
						new CompoundTermInvocationHandler(
								rawTree, true, prefixMap));

		} catch (RecognitionException e) {
			throw new ParseException(e);
		}
	}

	/**
	 * Parse a oslc.select expression
	 *
	 * @param selectExpression contents of an oslc.select HTTP query
	 * parameter
	 * @param prefixMap map between XML namespace prefixes and
	 * associated URLs
	 *
	 * @return the parsed select clause
	 *
	 * @throws ParseException
	 */
	public static SelectClause
	parseSelect(
		String selectExpression,
		Map<String, String> prefixMap
	) throws ParseException
	{
		OslcSelectParser parser = new OslcSelectParser(selectExpression);

		try {

			OslcSelectParser.oslc_select_return resultTree =
				parser.oslc_select();

			checkErrors(parser.getErrors());

            CommonTree rawTree = resultTree.getTree();

			if (rawTree.getType() == Token.INVALID_TOKEN_TYPE) {
				throw ((CommonErrorNode)rawTree).trappedException;
			}

			return (SelectClause)
				Proxy.newProxyInstance(SelectClause.class.getClassLoader(),
						new Class<?>[] { SelectClause.class, Properties.class },
						new PropertiesInvocationHandler(
                            resultTree.getTree(),
								prefixMap));

		} catch (RecognitionException e) {
			throw new ParseException(e);
		}
	}

	/**
	 * Parse a oslc.properties expression
	 *
	 * @param propertiesExpression contents of an oslc.properties HTTP query
	 * parameter
	 * @param prefixMap map between XML namespace prefixes and
	 * associated URLs
	 *
	 * @return the parsed properties clause
	 *
	 * @throws ParseException
	 */
	public static PropertiesClause
	parseProperties(
		String propertiesExpression,
		Map<String, String> prefixMap
	) throws ParseException
	{
		OslcSelectParser parser = new OslcSelectParser(propertiesExpression);

		try {

			OslcSelectParser.oslc_select_return resultTree =
				parser.oslc_select();

			checkErrors(parser.getErrors());

            CommonTree rawTree = resultTree.getTree();

			if (rawTree.getType() == Token.INVALID_TOKEN_TYPE) {
				throw ((CommonErrorNode)rawTree).trappedException;
			}

			return (PropertiesClause)
				Proxy.newProxyInstance(PropertiesClause.class.getClassLoader(),
						new Class<?>[] { PropertiesClause.class, Properties.class },
						new PropertiesInvocationHandler(
                            resultTree.getTree(),
								prefixMap));

		} catch (RecognitionException e) {
			throw new ParseException(e);
		}
	}

	/**
	 * Parse a oslc.orderBy expression
	 *
	 * @param orderByExpression contents of an oslc.orderBy HTTP query
	 * parameter
	 * @param prefixMap map between XML namespace prefixes and
	 * associated URLs
	 *
	 * @return the parsed order by clause
	 *
	 * @throws ParseException
	 */
	public static OrderByClause
	parseOrderBy(
		String orderByExpression,
		Map<String, String> prefixMap
	) throws ParseException
	{
		OslcOrderByParser parser = new OslcOrderByParser(orderByExpression);

		try {

			OslcOrderByParser.oslc_order_by_return resultTree =
				parser.oslc_order_by();

			checkErrors(parser.getErrors());

            CommonTree rawTree = resultTree.getTree();
			Tree child = rawTree.getChild(0);

			if (child.getType() == Token.INVALID_TOKEN_TYPE) {
				throw ((CommonErrorNode)child).trappedException;
			}

			return (OrderByClause)
					Proxy.newProxyInstance(OrderByClause.class.getClassLoader(),
							new Class<?>[] { OrderByClause.class, SortTerms.class },
							new SortTermsInvocationHandler(rawTree, prefixMap));

		} catch (RecognitionException e) {
			throw new ParseException(e);
		}
	}

	/**
	 * Create a map representation of the {@link Properties} returned
	 * from parsing {@code oslc.properties} or {@code oslc.select} URL query
	 * parameters suitable for generating a property result from an
	 * HTTP GET request.<p>
	 *
	 * The map keys are the property names; i.e. the local name of the
	 * property concatenated to the XML namespace of the property.	The
	 * values of the map are:<p>
	 *
	 * <ul>
	 * <li> Wildcard, if all
	 * properties at this level are to be output.  No recursion
	 * below this level is to be done.</li>
	 * <li> {@link org.eclipse.lyo.oslc4j.core.OSLC4JConstants#OSL4J_PROPERTY_SINGLETON OSLC4JConstants.OSL4J_PROPERTY_SINGLETON} - if only
	 * the named property is to be output, without recursion</li>
	 * <li> a nested property list to recurse through</li>
	 * </ul>
	 *
	 * @param properties
	 *
	 * @return the property map
	 */
	public static Map<String, Object>
	invertSelectedProperties(final Properties properties)
	{
		List<Property> children = properties.children();
		Map<String, Object> result = new HashMap<>(children.size());

		for (Property property : children) {

			PName pname;
			String propertyName = null;

			if (! property.isWildcard()) {
				pname = property.identifier();
				propertyName = pname.namespace + pname.local;
			}

			switch (property.type()) {
			case IDENTIFIER:
				if (property.isWildcard()) {

					if (result instanceof SingletonWildcardProperties) {
						break;
					}

					if (result instanceof NestedWildcardProperties) {
						result = new BothWildcardPropertiesImpl(
								(NestedWildcardPropertiesImpl)result);
					} else {
						result = new SingletonWildcardPropertiesImpl();
					}

					break;

				} else {

					if (result instanceof SingletonWildcardProperties) {
						break;
					}
				}

				result.put(propertyName,
						   OSLC4JConstants.OSL4J_PROPERTY_SINGLETON);

				break;

			case NESTED_PROPERTY:
				if (property.isWildcard()) {

					if (! (result instanceof NestedWildcardProperties)) {
						if (result instanceof SingletonWildcardProperties) {
							result = new BothWildcardPropertiesImpl();
						} else {
							result = new NestedWildcardPropertiesImpl(result);
						}

						((NestedWildcardPropertiesImpl)result).commonNestedProperties =
							invertSelectedProperties((NestedProperty)property);

				   } else {
						mergePropertyMaps(
							((NestedWildcardProperties)result).commonNestedProperties(),
							invertSelectedProperties((NestedProperty)property));
				   }

					break;
				}

				result.put(propertyName,
						   invertSelectedProperties(
								   (NestedProperty)property));

				break;
			}
		}

		if (! (result instanceof NestedWildcardProperties)) {
			return result;
		}

		Map<String, Object> commonNestedProperties =
			((NestedWildcardProperties)result).commonNestedProperties();

		for (Map.Entry<String, Object> propertyMapping : result.entrySet()) {

			@SuppressWarnings("unchecked")
			Map<String, Object> nestedProperties =
				(Map<String, Object>)propertyMapping.getValue();

			if (nestedProperties == OSLC4JConstants.OSL4J_PROPERTY_SINGLETON) {
				result.put(propertyMapping.getKey(), commonNestedProperties);
			} else {
				mergePropertyMaps(nestedProperties, commonNestedProperties);
			}
		}

		return result;
	}

	/**
	 * Parse an {@code oslc.searchTerms} expression
	 *
	 * <p><b>Note</b>: {@link Object#toString()} of result has been overridden to
	 * return input expression.
	 *
	 * @param searchTermsExpression contents of an {@code oslc.searchTerms} HTTP query
	 * parameter
	 *
	 * @return the parsed search terms clause
	 *
	 * @throws ParseException
	 */
	public static SearchTermsClause
	parseSearchTerms(
		String searchTermsExpression
	) throws ParseException
	{
		OslcSearchTermsParser parser = new OslcSearchTermsParser(searchTermsExpression);

		try {

			OslcSearchTermsParser.oslc_search_terms_return resultTree =
				parser.oslc_search_terms();

			checkErrors(parser.getErrors());

            CommonTree rawTree = resultTree.getTree();
			Tree child = rawTree.getChild(0);

			if (child.getType() == Token.INVALID_TOKEN_TYPE) {
				throw ((CommonErrorNode)child).trappedException;
			}

			StringList stringList = new StringList(rawTree.getChildCount());

			for (int index = 0; index < rawTree.getChildCount(); index++) {

				Tree string = rawTree.getChild(index);

				String rawString = string.getText();

				stringList.add(rawString.substring(1, rawString.length()-1));
			}

			return stringList;

		} catch (RecognitionException e) {
			throw new ParseException(e);
		}
	}

	/**
	 * Implementation of a {@link SearchTermsClause} interface
	 */
	private static class StringList extends ArrayList<String>
		implements SearchTermsClause
	{
		public
		StringList(int size)
		{
			super(size);
		}

		public String
		toString()
		{
			StringBuilder buffer = new StringBuilder();
			boolean first = true;

			for (String string : this) {

				if (first) {
					first = false;
				} else {
					buffer.append(',');
				}

				buffer.append('"');
				buffer.append(string);
				buffer.append('"');
			}

			return buffer.toString();
		}

		private static final long serialVersionUID = 1943909246265711359L;
	}

	/**
	 * Implementation of a {@code Map<String, String> prefixMap}
	 */
	private static class PrefixMap extends HashMap<String, String>
	{
		public
		PrefixMap(int size)
		{
			super(size);
		}

		public String
		toString()
		{
			StringBuilder buffer = new StringBuilder();
			Iterator<String> keys = this.keySet().iterator();
			boolean first = true;

			while (keys.hasNext()) {

				if (first) {
					first = false;
				} else {
					buffer.append(',');
				}

				String key = keys.next();

				buffer.append(key);
				buffer.append('=');
				buffer.append('<');
				buffer.append(this.get(key));
				buffer.append('>');
			}

			return buffer.toString();
		}

		private static final long serialVersionUID = 1943909246265711359L;
	}
	/**
	 * Implementation of {@link SingletonWildcardProperties}
	 */
	private static class SingletonWildcardPropertiesImpl
		extends HashMap<String, Object>
		implements SingletonWildcardProperties
	{
		public
		SingletonWildcardPropertiesImpl()
		{
			super(0);
		}

		private static final long serialVersionUID = -5490896670186283412L;
	}

	/**
	 * Implementation of {@link NestedWildcardProperties}
	 */
	private static class NestedWildcardPropertiesImpl
		extends HashMap<String, Object>
		implements NestedWildcardProperties
	{
		public
		NestedWildcardPropertiesImpl(Map<String, Object> accumulated)
		{
			super(accumulated);
		}

		protected
		NestedWildcardPropertiesImpl()
		{
			super(0);
		}

		public Map<String, Object>
		commonNestedProperties()
		{
			return commonNestedProperties;
		}

		protected Map<String, Object> commonNestedProperties =
            new HashMap<>();
		private static final long serialVersionUID = -938983371894966574L;
	}

	/**
	 * Implementation of both {@link SingletonWildcardProperties} and
	 * {@link NestedWildcardProperties}
	 */
	private static class BothWildcardPropertiesImpl
		extends NestedWildcardPropertiesImpl
		implements SingletonWildcardProperties
	{
		public
		BothWildcardPropertiesImpl()
		{
		}

		public
		BothWildcardPropertiesImpl(NestedWildcardPropertiesImpl accumulated)
		{
			this();

			commonNestedProperties = accumulated.commonNestedProperties();
		}

		private static final long serialVersionUID = 654939845613307220L;
	}

	/**
	 * Merge into {@code lhs} properties those of {@code rhs} property
	 * map, merging any common, nested property maps.
	 *
	 * @param lhs target of property map merge
	 * @param rhs source of property map merge
	 */
	private static void
	mergePropertyMaps(
		Map<String, Object> lhs,
		Map<String, Object> rhs
	)
	{

        for (String propertyName : rhs.keySet()) {

            @SuppressWarnings("unchecked")
            Map<String, Object> lhsNestedProperties =
                (Map<String, Object>) lhs.get(propertyName);
            @SuppressWarnings("unchecked")
            Map<String, Object> rhsNestedProperties =
                (Map<String, Object>) rhs.get(propertyName);

            if (lhsNestedProperties == rhsNestedProperties) {
                continue;
            }

            if (lhsNestedProperties == null ||
                lhsNestedProperties == OSLC4JConstants.OSL4J_PROPERTY_SINGLETON) {

                lhs.put(propertyName, rhsNestedProperties);

                continue;
            }

            mergePropertyMaps(lhsNestedProperties, rhsNestedProperties);
        }
	}

	/**
	 * Check list of errors from parsing some expression, generating
	 * {@link ParseException} if there are any.
	 *
	 * @param errors list of errors, hopefully empty
	 *
	 * @throws ParseException
	 */
	private static void
	checkErrors(List<String> errors) throws ParseException
	{
		if (errors.isEmpty()) {
			return;
		}

		StringBuilder buffer = new StringBuilder();
		boolean first = true;

		for (String error : errors) {

			if (first) {
				first = false;
			} else {
				buffer.append('\n');
			}

			buffer.append(error);
		}

		throw new ParseException(buffer.toString());
	}
}
