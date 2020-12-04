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
package org.eclipse.lyo.oslc4j.utils;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.apache.wink.common.internal.http.Accept;
import org.apache.wink.common.internal.providers.header.AcceptHeaderDelegate;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;

/**
 * Utilities for matching media types from an HTTP Accept header. Note it is
 * usually better if using Wink to annotate methods with {@link Produces} and
 * let JAX-RS process the Accept header for you, but in some contexts, it's
 * not possible.
 */
public class AcceptUtil {
	public static final MediaType[] OSLC_ACCEPTABLE = {
		OslcMediaType.APPLICATION_RDF_XML_TYPE,
		OslcMediaType.TEXT_TURTLE_TYPE,
		OslcMediaType.APPLICATION_XML_TYPE,
		OslcMediaType.APPLICATION_JSON_TYPE,
		OslcMediaType.APPLICATION_X_OSLC_COMPACT_XML_TYPE
	};

	/**
	 * Matches requested media types against {@link #OSLC_ACCEPTABLE} media types.
	 * 
	 * @param request
	 *			  the HTTP request
	 * @return A matching media type, or null if none match. If there is no
	 *		   Accept header, returns
	 *		   {@link OslcMediaType#APPLICATION_RDF_XML_TYPE}.
	 */
	public static MediaType matchMediaType(HttpServletRequest request) {
		return matchMediaType(request, OSLC_ACCEPTABLE);
	}

	/**
	 * Matches requested media types against a list of acceptable media types.
	 * 
	 * @param request
	 *			  the HTTP request
	 * @param acceptable
	 *			  a list of acceptable media types in order of preference
	 * @return A matching media type, or null if none match. If there is no
	 *		   Accept header, returns the first media type in {@code acceptable}.
	 */
	public static MediaType matchMediaType(HttpServletRequest request, MediaType[] acceptable) {
		if (acceptable == null || acceptable.length == 0) {
			return null;
		}
		
		String accept = request.getHeader("Accept");
		if (accept == null) {
			return acceptable[0];
		}
		
		AcceptHeaderDelegate delegate = new AcceptHeaderDelegate();
		Accept a = delegate.fromString(accept);
		
		for (MediaType nextRequested : a.getSortedMediaTypes()) {
			for (MediaType nextAcceptable : acceptable) {
				if (nextRequested.isCompatible(nextAcceptable)) {
					return nextAcceptable;
				}
			}
		}
		
		return null;
	}
}
