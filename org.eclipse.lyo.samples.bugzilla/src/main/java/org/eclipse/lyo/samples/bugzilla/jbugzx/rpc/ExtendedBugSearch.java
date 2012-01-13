/*******************************************************************************
 * Copyright (c) 2011 IBM Corporation.
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
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.samples.bugzilla.jbugzx.rpc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.j2bugzilla.base.Bug;
import com.j2bugzilla.base.BugFactory;
import com.j2bugzilla.rpc.BugSearch;

/**
 * Adds additional search limiters to those provided by j2bugzilla and fixes a
 * bug reading the version property.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
public class ExtendedBugSearch extends BugSearch {
	private Map<Object, Object> hash = new HashMap<Object, Object>();
	
	public enum ExtendedSearchLimiter {
		/**
		 * The maximum number of bugs to return.
		 */
		LIMIT("limit"),
		
		/**
		 * An offset into bugs returned by search.
		 */
		OFFSET("offset");

		private final String name;

		/**
		 * Creates a new {@link ExtendedSearchLimiter} with the designated name
		 * 
		 * @param name
		 *            The name Bugzilla expects for this search limiter
		 */
		ExtendedSearchLimiter(String name) {
			this.name = name;
		}

		/**
		 * Get the name Bugzilla expects for this search limiter
		 * 
		 * @return A <code>String</code> representing the search limiter
		 */
		String getName() {
			return this.name;
		}
	}

	public ExtendedBugSearch(BugSearch.SearchLimiter limit, String query) {
		super(limit, query);
	}

	public ExtendedBugSearch(ExtendedSearchLimiter limit, String query) {
		// We have to add a bogus search limiter and remove it since
		// the only consutrctor for the parent class takes a
		// BugSearch.SearchLimiter. Instead of subclassing BugSearch,
		// we should submit a patch back to j2bugzilla that adds
		// additional limiters.
		super(BugSearch.SearchLimiter.STATUS, "bogus");
		getParameterMap().clear();
		addQueryParam(limit, query);
	}

	/**
	 * Add an additional search limit to the {@Link BugSearch}
	 * 
	 * @param limit
	 *            What dimension to search {@link Bug}s by in the Bugzilla
	 *            installation
	 * @param query
	 *            What to match fields against
	 */
	public void addQueryParam(ExtendedSearchLimiter limit, Object query) {
		getParameterMap().put(limit.getName(), query);
	}
	
	/**
	 * Returns the {@link Bug}s found by the query as a <code>List</code>
	 * @return a {@link List} of {@link Bug}s that match the query and limit
	 */
	public List<Bug> getSearchResults() {
		List<Bug> results = new ArrayList<Bug>();
		/*
		 * The following is messy, but necessary due to how the returned XML document nests
		 * Maps.
		 */	
		if(hash.containsKey("bugs")) {
			Object[] bugs = (Object[])hash.get("bugs");
			for(Object o : bugs) {
				@SuppressWarnings("unchecked")
				Map<String, Object> bugMap = (HashMap<String, Object>)o;
				// Work around a bug in j2bugzilla where version isn't correctly recognized.
				Map<?, ?> internals = (Map<?, ?>) bugMap.get("internals");
				bugMap.put("version", internals.get("version"));
				results.add(new BugFactory().createBug(bugMap));
			}
		}
		return results;
	}
	
	@Override
	public void setResultMap(Map<Object, Object> hash) {
		this.hash = hash;
	}
}
