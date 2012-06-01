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
import java.util.List;
import java.util.Map;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.eclipse.lyo.samples.sharepoint.SharepointConnector;
import org.eclipse.lyo.samples.sharepoint.exceptions.ConnectionException;
import org.eclipse.lyo.samples.sharepoint.exceptions.SharepointException;
import org.eclipse.lyo.samples.sharepoint.services.ShareBaseService;
import org.eclipse.lyo.samples.sharepoint.store.ShareValue;
import org.eclipse.lyo.samples.sharepoint.store.UnrecognizedValueTypeException;




public class ResourceListService extends ShareBaseService {
	private static final long serialVersionUID = -8436719131002636593L;

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	@SuppressWarnings("nls")
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		try {
			
			String collection = (String)request.getParameter("collection");
			//List<Map<String, ShareValue>> results = store.query(IConstants.SPARQL, query, IAmConstants.DEFAULT_MAX_RESULTS);
			final SharepointConnector sc = SharepointInitializer.getSharepointConnector();
			List<Map<String, ShareValue>> results = sc.getDocuments(collection);
			
			request.setAttribute("results", results);
			request.setAttribute("collection", collection);
			
			RequestDispatcher rd = request.getRequestDispatcher("/sharepoint/resource_listing.jsp");
			rd.forward(request, response);
			
		} catch (ConnectionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SharepointException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (UnrecognizedValueTypeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}

}
