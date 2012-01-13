/*******************************************************************************
 * Copyright (c) 2011 IBM Corporation.
 *
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the Eclipse Public License v1.0
 *  and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *  
 *  The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 *  and the Eclipse Distribution License is available at
 *  http://www.eclipse.org/org/documents/edl-v10.php.
 *  
 *  Contributors:
 *  
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.eclipse.lyo.samples.bugzilla;

import java.io.IOException;
import java.util.List;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.eclipse.lyo.samples.bugzilla.exception.UnauthroziedException;
import org.eclipse.lyo.samples.bugzilla.jbugzx.rpc.ExtendedBugSearch;
import org.eclipse.lyo.samples.bugzilla.utils.HttpUtils;

import com.j2bugzilla.base.Bug;
import com.j2bugzilla.base.BugzillaConnector;
import com.j2bugzilla.rpc.BugSearch;


/**
 * Servlet implementation class ResourceFactory
 */
public class ChangeRequestSelectorService extends HttpServlet {
	private static final long serialVersionUID = 6114613293834136389L;

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) 
		throws ServletException, IOException {
		doGet(request, response);
	}

	protected void doGet(HttpServletRequest request, HttpServletResponse response) 
		throws ServletException, IOException {

		int productId = Integer.parseInt(request.getParameter("productId"));
		String terms = request.getParameter("terms");
		
		request.setAttribute("productId", productId);
		request.setAttribute("terms", terms);
        request.setAttribute("bugzillaUri", BugzillaInitializer.getBugzillaUri());

		if (terms != null ) {
			sendFilteredBugsReponse(productId, terms, request, response);

		} else {
			try {	
                RequestDispatcher rd = request.getRequestDispatcher("/cm/changerequest_selector.jsp"); 
	    		rd.forward(request, response);
				
			} catch (Exception e) {
				throw new ServletException(e);
			}
		}
	}
	
	private void sendFilteredBugsReponse(
		int productId, String terms, HttpServletRequest request, HttpServletResponse response) 
		throws ServletException, IOException {
		try {
			BugzillaConnector bc = BugzillaInitializer.getBugzillaConnector(request);
			ExtendedBugSearch bugSearch = new ExtendedBugSearch(BugSearch.SearchLimiter.SUMMARY, terms);
			bc.executeMethod(bugSearch);
			List<Bug> results = bugSearch.getSearchResults();
			request.setAttribute("results", results);

            RequestDispatcher rd = request.getRequestDispatcher("/cm/changerequest_filtered_json.jsp"); 
    		rd.forward(request, response);

		} catch (UnauthroziedException e) {
			HttpUtils.sendUnauthorizedResponse(response, e);
		} catch (Exception e) {
			throw new ServletException(e);
		}								
	}
}
