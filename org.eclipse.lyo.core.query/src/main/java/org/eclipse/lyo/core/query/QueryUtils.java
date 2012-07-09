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
 *    Steve Pitschke - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.core.query;

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
import org.eclipse.lyo.core.query.impl.PropertyListInvocationHandler;
import org.eclipse.lyo.core.query.impl.SortTermsInvocationHandler;
import org.eclipse.lyo.core.query.impl.WildcardInvocationHandler;

/**
 * Utility methods for parsing various OSLC HTTP query
 * parameter clauses; e.g. oslc.where
 */
public class QueryUtils
{
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
        OslcPrefixParser parser = new OslcPrefixParser(prefixExpression);
        
        try {
            
            CommonTree rawTree =
                (CommonTree)parser.oslc_prefixes().getTree();            
            @SuppressWarnings("unchecked")
            List<CommonTree> rawPrefixes = rawTree.getChildren();
            PrefixMap prefixMap =
                new PrefixMap(rawPrefixes.size());
            
            for (CommonTree rawPrefix : rawPrefixes) {
                
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
            CommonTree rawTree = (CommonTree)resultTree.getTree();
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
            CommonTree rawTree = (CommonTree)resultTree.getTree();
            
            if (rawTree.getType() == Token.INVALID_TOKEN_TYPE) {
                throw ((CommonErrorNode)rawTree).trappedException;
            }            
            
            if (rawTree.getType() == OslcSelectParser.PROPERTIES) {
                return (SelectClause)
                    Proxy.newProxyInstance(SelectClause.class.getClassLoader(), 
                            new Class<?>[] { SelectClause.class, PropertyList.class },
                            new PropertyListInvocationHandler(
                                    (CommonTree)resultTree.getTree(),
                                    prefixMap));
            } else {
                return (SelectClause)
                    Proxy.newProxyInstance(SelectClause.class.getClassLoader(), 
                            new Class<?>[] { SelectClause.class, Wildcard.class },
                            new WildcardInvocationHandler());
            }            
            
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
            CommonTree rawTree = (CommonTree)resultTree.getTree();
            
            if (rawTree.getType() == Token.INVALID_TOKEN_TYPE) {
                throw ((CommonErrorNode)rawTree).trappedException;
            }
            
            if (rawTree.getType() == OslcSelectParser.PROPERTIES) {
                return (PropertiesClause)
                    Proxy.newProxyInstance(SelectClause.class.getClassLoader(), 
                            new Class<?>[] { PropertiesClause.class, PropertyList.class },
                            new PropertyListInvocationHandler(
                                    (CommonTree)resultTree.getTree(),
                                    prefixMap));
            } else {
                return (PropertiesClause)
                    Proxy.newProxyInstance(SelectClause.class.getClassLoader(), 
                            new Class<?>[] { PropertiesClause.class, Wildcard.class },
                            new WildcardInvocationHandler());
            }            
            
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
            CommonTree rawTree = (CommonTree)resultTree.getTree();
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
     * Parse a oslc.searchTerms expression
     * 
     * <p><b>Note</b>: {@link Object#toString()} of result has been overridden to
     * return input expression.
     *  
     * @param searchTermsExpression contents of an oslc.searchTerms HTTP query
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
            CommonTree rawTree = (CommonTree)resultTree.getTree();
            Tree child = rawTree.getChild(0);
            
            if (child.getType() == Token.INVALID_TOKEN_TYPE) {
                throw ((CommonErrorNode)child).trappedException;
            }
            
            @SuppressWarnings("unchecked")
            List<CommonTree> rawList = rawTree.getChildren();
            StringList stringList = new StringList(rawList.size());
            
            for (CommonTree string : rawList) {
                
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
    static class StringList extends ArrayList<String>
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
            StringBuffer buffer = new StringBuffer();
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
     * Implementation of a Map<String, String> prefixMap
     */
    static class PrefixMap extends HashMap<String, String>
    {
        public
        PrefixMap(int size)
        {
            super(size);
        }
        
        public String
        toString()
        {
            StringBuffer buffer = new StringBuffer();
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
}
