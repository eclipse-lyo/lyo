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
package org.eclipse.lyo.oslc4j.provider.jena;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.jena.rdf.model.RDFErrorHandler;
import org.apache.jena.shared.JenaException;

public final class ErrorHandler
	   implements RDFErrorHandler
{
	private static final Logger logger = Logger.getLogger(ErrorHandler.class.getName());
	private static final String JENA_RELATIVE_URI_WARNING_ID = "W130";

	public ErrorHandler()
	{
		super();
	}

	@Override
	public void error(final Exception exception)
	{
		handleException(exception);
	}

	@Override
	public void fatalError(final Exception exception)
	{
		handleException(exception);
	}

	@Override
	public void warning(final Exception exception)
	{
		Level level = Level.WARNING;

		//Workaround to avoid flooding the logs with Jena warnings about using
		//relative URIs with no base URI.  Common for reified statements in OSLC
		String msg = exception.getMessage();
		if (msg != null && (msg.contains(JENA_RELATIVE_URI_WARNING_ID)))
		{
			level=Level.FINE;
		}

		logger.log(level,
				"Warning in Jena handling",
				exception);

	}

	private static void handleException(final Exception exception)
	{
		if (exception instanceof RuntimeException)
		{
			throw (RuntimeException) exception;
		}

		throw new JenaException(exception);
	}
}
