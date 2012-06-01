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
import java.io.PrintWriter;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class ConsumerKeyService extends HttpServlet {

	@Override
	public void init(ServletConfig config) throws ServletException {
		super.init(config);
	}

	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response)
			throws IOException, ServletException {

		returnConsumerKey(request, response);
	}

	@Override
	public void doPost(HttpServletRequest request, HttpServletResponse response)
			throws IOException, ServletException {
		returnConsumerKey(request, response);
	}

	private void returnConsumerKey(HttpServletRequest request,
			HttpServletResponse response) throws IOException, ServletException {
		// TODO: Properly handle consumer key requests from JTS
		response.setContentType("application/json");
		PrintWriter out = response.getWriter();
		out.print("{\"key\":\"rtc\"}");
		out.close();
	}

	private static final long serialVersionUID = -7611240899575174568L;

}
