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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause
 */
package org.eclipse.lyo.core.query;

import org.antlr.runtime.RecognitionException;

/**
 * Wrapper for {@link RecognitionException} so clients of OLSC query
 * functionality don't need to import ANTLR classes.
 */
public class ParseException extends Exception
{
	ParseException(RecognitionException cause)
	{
		super(cause);
	}
	
	ParseException(String msg)
	{
		super(msg);
	}
	
	private static final long serialVersionUID = 2373494371127406191L;
}
