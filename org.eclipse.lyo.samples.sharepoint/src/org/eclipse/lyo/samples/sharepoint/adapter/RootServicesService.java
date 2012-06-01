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
 *     Keith Wells             - initial API and implementation
 *     Sam Padgett           - initial API and Implementation
 *     Jim Conallen           - initial API and implementation
 *
 *******************************************************************************/
package org.eclipse.lyo.samples.sharepoint.adapter;

import java.io.IOException;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;


/**
 * Jazz Root Services Service, see:
 *	https://jazz.net/wiki/bin/view/Main/RootServicesSpec
 *	https://jazz.net/wiki/bin/view/Main/RootServicesSpecAddendum2
 */
public class RootServicesService extends HttpServlet {    	

	private static final long serialVersionUID = -8125286361811879744L;

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        System.err.println("RootServices.doGET - Accept: " + request.getHeader("Accept"));
        request.setAttribute("baseUri", SharepointInitializer.getBaseUri());

		final RequestDispatcher rd = request.getRequestDispatcher("/sharepoint/rootservices_rdfxml.jsp"); 
		rd.forward(request, response);
		response.flushBuffer();
	}
}
 