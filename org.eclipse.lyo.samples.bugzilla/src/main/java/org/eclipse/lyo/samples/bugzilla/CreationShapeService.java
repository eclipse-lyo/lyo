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

import org.eclipse.lyo.samples.bugzilla.exception.UnauthroziedException;
import org.eclipse.lyo.samples.bugzilla.jbugzx.rpc.GetLegalValues;
import org.eclipse.lyo.samples.bugzilla.utils.AcceptType;
import org.eclipse.lyo.samples.bugzilla.utils.HttpUtils;

import com.j2bugzilla.base.BugzillaConnector;
import com.j2bugzilla.base.Product;
import com.j2bugzilla.rpc.GetProduct;


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

		int productId;
		try {
			productId = Integer.parseInt(request.getParameter("productId"));
			
			BugzillaConnector bc = BugzillaInitializer.getBugzillaConnector(request);				
			GetProduct getProducts = new GetProduct(productId); 
			bc.executeMethod(getProducts);
			Product product = getProducts.getProduct();
			request.setAttribute("product", product);
			
		} catch (UnauthroziedException e) {
			HttpUtils.sendUnauthorizedResponse(response, e);
			return;
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

			GetProduct getProduct = new GetProduct(productId);
			bc.executeMethod(getProduct);
			Product product = getProduct.getProduct();
			request.setAttribute("product", product);  
			
			GetLegalValues getComponentValues = 
				new GetLegalValues("component", product.getID());
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
			
			GetLegalValues getVersionValues = new GetLegalValues("version", product.getID());
			bc.executeMethod(getVersionValues);
			List<String> versions = Arrays.asList(getVersionValues.getValues());
			request.setAttribute("versions", versions);
			
            response.setHeader("OSLC-Core-Version", "2.0");
			final RequestDispatcher rd = request.getRequestDispatcher(dispatchTo); 
			rd.forward(request, response);
			response.flushBuffer();

		} catch (UnauthroziedException e) {
			HttpUtils.sendUnauthorizedResponse(response, e);
		} catch (Throwable e) {
			throw new ServletException(e);
		}
	}	
}
 