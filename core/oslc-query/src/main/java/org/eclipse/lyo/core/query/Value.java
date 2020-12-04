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
 * Operand from {@link ComparisonTerm} or {@link InTerm}
 * {@link SimpleTerm} from olsc.where clause
 */
public interface Value
{
	enum Type
	{
		URI_REF,
		BOOLEAN,
		DECIMAL,
		STRING,
		TYPED_STRING,
		LANGED_STRING;
	}
	
	Type type();	
}
