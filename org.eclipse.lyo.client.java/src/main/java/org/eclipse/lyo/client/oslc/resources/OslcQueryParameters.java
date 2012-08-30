/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
 *
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the Eclipse Public License v1.0
 *  and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *  
 *  The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 *  and the Eclipse Distribution License is available at
 *  http://www.eclipse.org/org/documents/edl-v10.php.
 *  
 *  Contributors:
 *  
 *     Michael Fiedler     - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.resources;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * A container for OSLC Query parameters which can be associated with an OslcQuery object.
 * 
 * @see OslcQuery
 *
 */
public class OslcQueryParameters {
	
	private String where;
	private String select;
	private String searchTerms;
	private String orderBy;
	private String prefix;
	
	private static final Logger logger = Logger.getLogger(OslcQuery.class.getName());

	public OslcQueryParameters()
	{
		
	}
	
	/**
	 * Initialize an OSLC Parameter using the supplied terms
	 * 
	 * @param where
	 * @param select
	 * @param searchTerms
	 * @param orderBy
	 * @param prefix
	 */
	public OslcQueryParameters (String where, String select, String searchTerms, String orderBy, String prefix) {
		this.where       = where;
		this.select      = select;
		this.searchTerms = searchTerms;
		this.orderBy     = orderBy;
		this.prefix      = prefix;
	}
	/**
	 * 
	 * @return
	 */
	public String getWhere() {
		return where;
	}

	/**
	 * 
	 * @param where
	 */
	public void setWhere(String where) {
		this.where = encodeQueryParams(where);
	}

	/**
	 * 
	 * @return
	 */
	public String getSelect() {
		return select;
	}

	/**
	 * 
	 * @param select
	 */
	public void setSelect(String select) {
		this.select = encodeQueryParams(select);
	}

	/**
	 * 
	 * @return
	 */
	public String getSearchTerms() {
		return searchTerms;
	}

	/**
	 * 
	 * @param searchTerms
	 */
	public void setSearchTerms(String searchTerms) {
		this.searchTerms = encodeQueryParams(searchTerms);
	}

	/**
	 * 
	 * @return
	 */
	public String getOrderBy() {
		return orderBy;
	}

	/**
	 * 
	 * @param orderBy
	 */
	public void setOrderBy(String orderBy) {
		this.orderBy = encodeQueryParams(orderBy);
	}

	/**
	 * 
	 * @return
	 */
	public String getPrefix() {
		return prefix;
	}

	/**
	 * 
	 * @param prefix
	 */
	public void setPrefix(String prefix) {
		this.prefix = encodeQueryParams(prefix);
	}
	
	private String encodeQueryParams(String oslcQueryParam) {
		String encodedQueryParms = null;
		try {
			encodedQueryParms = URLEncoder.encode(oslcQueryParam, "UTF-8");
		} catch (UnsupportedEncodingException e) {
			//Should not occur
			logger.log(Level.SEVERE, "Could not UTF-8 encode query parameters: " + oslcQueryParam, e);
		} 
		
		return encodedQueryParms;
	}
}
