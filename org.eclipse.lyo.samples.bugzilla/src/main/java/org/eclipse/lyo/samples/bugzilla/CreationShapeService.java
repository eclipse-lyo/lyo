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
import java.util.Arrays;
import java.util.List;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import jbugz.base.BugzillaConnector;

import org.eclipse.lyo.samples.bugzilla.jbugzx.base.Product;
import org.eclipse.lyo.samples.bugzilla.jbugzx.rpc.GetAccessibleProducts;
import org.eclipse.lyo.samples.bugzilla.jbugzx.rpc.GetLegalValues;
import org.eclipse.lyo.samples.bugzilla.jbugzx.rpc.GetProducts;
import org.eclipse.lyo.samples.bugzilla.utils.AcceptType;


/**
 * OSLC CM Change Request Service
 */
public class CreationShapeService extends HttpServlet {    	
	private static final long serialVersionUID = -5280734755943517104L; 

    public CreationShapeService() {}

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		try {
			int productId = Integer.parseInt(request.getParameter("productId"));
			Integer[] productIds = { productId }; 
			
			BugzillaConnector bc = BugzillaInitializer.getBugzillaConnector(request);				
			GetProducts getProducts = new GetProducts(productIds); 
			bc.executeMethod(getProducts);
			List<Product> products = getProducts.getProducts();
			request.setAttribute("product", products.get(0));
			
		} catch (Exception e) {
			response.sendError(HttpServletResponse.SC_NOT_FOUND);
			return;
		}
		
		try {			
			final String dispatchTo;
			if (AcceptType.willAccept("application/rdf+xml", request)) {	
				dispatchTo = "/cm/resourceshape_creation_rdfxml.jsp";
				
			} else {
				response.sendError(HttpServletResponse.SC_UNSUPPORTED_MEDIA_TYPE);
				return;
			}	
			
			final BugzillaConnector bc = BugzillaInitializer.getBugzillaConnector(request);			
            request.setAttribute("bugzillaUri", BugzillaInitializer.getBugzillaUri());

			GetAccessibleProducts getProductIds = new GetAccessibleProducts();
			bc.executeMethod(getProductIds);
			Integer[] productIds = getProductIds.getIds();

			GetProducts getProducts = new GetProducts(productIds); 
			bc.executeMethod(getProducts);
			Product product = getProducts.getProducts().get(0);
			request.setAttribute("product", product);  
			
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
			
            response.setHeader("OSLC-Core-Version", "2.0");
			final RequestDispatcher rd = request.getRequestDispatcher(dispatchTo); 
			rd.forward(request, response);
			response.flushBuffer();

		} catch (Throwable e) {
			throw new ServletException(e);
		}
	}	
}
 