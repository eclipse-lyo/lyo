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

import java.util.HashMap;
import java.util.Map;

import com.j2bugzilla.base.Bug;
import com.j2bugzilla.base.BugFactory;
import com.j2bugzilla.rpc.GetBug;

/**
 * Allows users to retrieve a specific bug for which the ID is already known.
 * Extends {@link GetBug} to work around problems with specific Bugzilla
 * versions.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
public class ExtendedGetBug extends GetBug {
	private Map<Object, Object> hash = new HashMap<Object, Object>();

	/**
	 * Creates a new {@link ExtendedGetBug} object to retrieve the {@code Bug}
	 * specified by the ID parameter
	 * 
	 * @param id
	 *            An {@code int} representing the ID of an existing bug in the
	 *            installation connected to
	 */
	public ExtendedGetBug(int id) {
		super(id);
	}

	@Override
	public void setResultMap(Map<Object, Object> hash) {
		this.hash = hash;
		super.setResultMap(hash);
	}

	/**
	 * Retrieves the {@link jbugz.base.Bug} corresponding to the given ID
	 * 
	 * @return A {@code Bug} matching the ID, or null if the returned hash does
	 *         not contain a match
	 */
	public Bug getBug() {
		Object[] bugs = (Object[]) hash.get("bugs");
		if (bugs != null) {
			for (Object o : bugs) {
				@SuppressWarnings("unchecked")
				Map<String, Object> bugMap = (HashMap<String, Object>) o;
				// Work around a bug in j2bugzilla where version isn't correctly
				// recognized.
				Map<?, ?> internals = (Map<?, ?>) bugMap.get("internals");
				bugMap.put("version", internals.get("version"));
				return new BugFactory().createBug(bugMap);
			}
		}

		return null;
	}
}
