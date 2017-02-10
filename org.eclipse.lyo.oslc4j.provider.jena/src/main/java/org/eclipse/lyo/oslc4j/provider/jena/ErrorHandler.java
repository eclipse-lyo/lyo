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
 *	   Russell Boykin		- initial API and implementation
 *	   Alberto Giammaria	- initial API and implementation
 *	   Chris Peters			- initial API and implementation
 *	   Gianluca Bernardini	- initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.provider.jena;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.hp.hpl.jena.rdf.model.RDFErrorHandler;
import com.hp.hpl.jena.shared.JenaException;

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
		if (msg != null && (msg.indexOf(JENA_RELATIVE_URI_WARNING_ID) >= 0))
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
