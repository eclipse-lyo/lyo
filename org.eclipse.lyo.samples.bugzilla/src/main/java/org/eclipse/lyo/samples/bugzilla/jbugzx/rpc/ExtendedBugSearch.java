/*******************************************************************************
 * Copyright (c) 2011, 2012 IBM Corporation.
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

import com.j2bugzilla.base.Bug;
import com.j2bugzilla.rpc.BugSearch;

/**
 * Adds additional search limiters to those provided by j2bugzilla and fixes a
 * bug reading the version property.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
public class ExtendedBugSearch extends BugSearch {
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
}
