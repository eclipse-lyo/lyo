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
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import jbugz.base.Bug;
import jbugz.base.BugzillaConnector;
import jbugz.rpc.ReportBug;

import org.eclipse.lyo.samples.bugzilla.jbugzx.base.Product;
import org.eclipse.lyo.samples.bugzilla.jbugzx.rpc.GetLegalValues;
import org.eclipse.lyo.samples.bugzilla.jbugzx.rpc.GetProducts;
import org.eclipse.lyo.samples.bugzilla.utils.StringUtils;


/**
 * GET returns Delegated UI and POST accepts form post of new bug data.
 * Relies on new J2Bugzilla methods: GetProducts, GetAccessibleProducts and GetLegalValues.
 */
public class ChangeRequestCreatorService extends HttpServlet {
	private static final long serialVersionUID = 7466374797163202313L;
	
	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		Product product = null;
		try {
			BugzillaConnector bc = BugzillaInitializer.getBugzillaConnector(request);
			
			int productId = Integer.parseInt(request.getParameter("productId"));
			Integer[] productIds = { productId }; 
			
			GetProducts getProducts = new GetProducts(productIds);
			bc.executeMethod(getProducts);
			List<Product> products = getProducts.getProducts();
			product = products.get(0);
			request.setAttribute("product", product);
			
		} catch (Exception e) {
			response.sendError(HttpServletResponse.SC_NOT_FOUND);
			return;
		}		
		
		try {				
			BugzillaConnector bc = BugzillaInitializer.getBugzillaConnector(request);

			GetLegalValues getComponentValues = 
				new GetLegalValues("component", product.getId());
			bc.executeMethod(getComponentValues);
			List<String> components = Arrays.asList(getComponentValues.getValues());
			request.setAttribute("components", components);
			
			GetLegalValues getOsValues = new GetLegalValues("op_sys", -1);
			bc.executeMethod(getOsValues);
			List<String> operatingSystems = Arrays.asList(getOsValues.getValues());
			request.setAttribute("operatingSystems", operatingSystems);
			
			GetLegalValues getPlatformValues = new GetLegalValues("platform", -1);
			bc.executeMethod(getPlatformValues);
			List<String> platforms = Arrays.asList(getPlatformValues.getValues());
			request.setAttribute("platforms", platforms);
			
			GetLegalValues getVersionValues = new GetLegalValues("version", product.getId());
			bc.executeMethod(getVersionValues);
			List<String> versions = Arrays.asList(getVersionValues.getValues());
			request.setAttribute("versions", versions);
			
	        request.setAttribute("bugzillaUri", BugzillaInitializer.getBugzillaUri());

	        RequestDispatcher rd = request.getRequestDispatcher("/cm/changerequest_creator.jsp");
    		rd.forward(request, response);
			
		} catch (Exception e) {
			throw new ServletException(e);
		}
	}


	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        System.err.println("CRCreator.doPOST - Accept: " + request.getHeader("Accept") + ", query="+request.getQueryString());

		try {	
        
			int productId = Integer.parseInt(request.getParameter("productId"));
			Integer[] productIds = { productId }; 

			String prefill   = request.getParameter("prefill");

			if (StringUtils.isEmpty(prefill) || "true".equals(prefill)) {
				// TODO: Implement form prefill, just return same dialog URL in Location for now
				response.setHeader("Location", URLStrategy.getDelegatedCreationURL(productId));
				response.setStatus(201);
				return;

			}


			BugzillaConnector bc = BugzillaInitializer.getBugzillaConnector(request);				
			GetProducts getProducts = new GetProducts(productIds); 
			bc.executeMethod(getProducts);
			List<Product> products = getProducts.getProducts();

			String summary   = request.getParameter("summary"); 
			String component = request.getParameter("component");
			String version   = request.getParameter("version"); 
			String op_sys    = request.getParameter("op_sys"); 
			String platform  = request.getParameter("platform");

			Map<String, Object> bugState = new HashMap<String, Object>();

			bugState.put("product",   products.get(0).getName());
			bugState.put("component", component);
			bugState.put("summary",   summary);
			bugState.put("version",   version);
			bugState.put("op_sys",    op_sys);
			bugState.put("platform",  platform);

			System.err.println("Bug="+bugState);

			Bug bug = new  Bug(bugState);
			ReportBug reportBug = new ReportBug(bug);			
			bc.executeMethod(reportBug);

			// Send back to the form a small JSON response
			response.setContentType("application/json");
			response.setStatus(201); // Created
			PrintWriter out = response.getWriter();
			out.print("{\"title\": \"" + URLStrategy.getChangeRequestLinkLabel(reportBug.getID(), summary) + "\"," +
					"\"resource\" : \"" + URLStrategy.getChangeRequestURL(reportBug.getID()) + "\"}");
			out.close();
    		
		} catch (Exception e) {
			throw new ServletException(e);
		}
	}


}
