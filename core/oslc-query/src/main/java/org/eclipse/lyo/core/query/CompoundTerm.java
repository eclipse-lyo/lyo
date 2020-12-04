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

import java.util.List;

/**
 * Compound term from oslc.where clause.<p>
 * 
 * <b>Note:</b> If top-level compound term, {@link SimpleTerm#property()}
 * will return <code>null</code>.
 */
public interface CompoundTerm extends SimpleTerm
{
	/**
	 * @return immutable list of child simple terms
	 */
	List<SimpleTerm> children();
}
