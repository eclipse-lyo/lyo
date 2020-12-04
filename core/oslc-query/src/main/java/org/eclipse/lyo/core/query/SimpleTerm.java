/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
 *
 * See the NOTICE file(s) distributed with this work for additional
 * information regarding copyright ownership.
 *
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0, or the Eclipse Distribution License 1.0
 * which is available at http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Simple
 */

package org.eclipse.lyo.core.query;

/**
 * simple term from oslc.where clause
 */
public interface SimpleTerm
{
	enum Type
	{
		COMPARISON,
		IN_TERM,
		NESTED,
		TOP_LEVEL;
	}
	
	Type type();
	
	/**
	 * @return type of simple term.	 When {@link CompoundTerm}
	 * return <code>null</code>.
	 */
	PName property();
}
