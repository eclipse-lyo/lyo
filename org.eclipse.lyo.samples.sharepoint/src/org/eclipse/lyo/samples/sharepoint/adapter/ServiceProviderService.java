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

import org.eclipse.lyo.samples.sharepoint.SharepointConnector;
import org.eclipse.lyo.samples.sharepoint.util.AcceptType;
import org.eclipse.lyo.samples.sharepoint.util.StringUtils;
/*
import jbugz.base.BugzillaConnector;
import jbugz.base.Product;
import jbugz.rpc.GetAccessibleProducts;
import jbugz.rpc.GetProducts;
*/


/**
 * OSLC CM Change Request Service
 */
public class ServiceProviderService extends HttpServlet {    	
	private static final long serialVersionUID = -5280734755943517104L; 

    public ServiceProviderService() {}

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        System.err.println("ServiceProvider.doGET - Accept: " + request.getHeader("Accept"));

		try {
			String collection = request.getParameter("collection");		
			//Integer[] productIds = { productId }; 
			
			/* get Sharepoint document */
			
			final SharepointConnector sc = SharepointInitializer.getSharepointConnector();
			//Library library = sc.getLibrary(collection);
			
			request.setAttribute("collection", collection);
			
		} catch (Exception e) {
			response.sendError(HttpServletResponse.SC_NOT_FOUND);
			return;
		}
		
		try {
			final String dispatchTo;
			String accept = request.getHeader("Accept");
			
			if (AcceptType.willAccept("text/html", request)) {
				dispatchTo = "/sharepoint/serviceprovider_html.jsp";
			} else if (StringUtils.isEmpty(accept) || AcceptType.willAccept("application/rdf+xml", request)
					|| AcceptType.willAccept("application/xml", request)) {	
				dispatchTo = "/sharepoint/serviceprovider_rdfxml.jsp";

			} else {
				response.sendError(HttpServletResponse.SC_UNSUPPORTED_MEDIA_TYPE);
				return;
			}	
			
			/*
			final BugzillaConnector bc = BugzillaInitializer.getBugzillaConnector();			
            request.setAttribute("baseUri", BugzillaInitializer.getBaseUri());
            request.setAttribute("bugzillaUri", BugzillaInitializer.getBugzillaUri());

			GetAccessibleProducts getProductIds = new GetAccessibleProducts();
			bc.executeMethod(getProductIds);
			Integer[] productIds = getProductIds.getIds();

			GetProducts getProducts = new GetProducts(productIds); 
			bc.executeMethod(getProducts);
			List<Product> products = getProducts.getProducts();
			request.setAttribute("products", products);            
            */
            
			final RequestDispatcher rd = request.getRequestDispatcher(dispatchTo); 
			rd.forward(request, response);
			response.flushBuffer();

		} catch (Throwable e) {
			throw new ServletException(e);
		}
	}
	
	@Override
	protected void doPost(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {
		// TODO Auto-generated method stub
		System.err.println("ServiceProvider POST");
		// HACK: Use this same URL (service provider) to handle POST requests to add links (RTC, RQM, RRC, etc)
	}
	
	@Override
	protected void doPut(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {
		// TODO Auto-generated method stub
		System.err.println("ServiceProvider PUT");
	}
	

	
}
 