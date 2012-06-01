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

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.eclipse.lyo.samples.sharepoint.Library;
import org.eclipse.lyo.samples.sharepoint.SharepointConnector;
import org.eclipse.lyo.samples.sharepoint.util.AcceptType;

//import jbugz.base.Product;
//import jbugz.rpc.GetAccessibleProducts;
//import jbugz.rpc.GetProducts;

/**
 * OSLC CM Change Request Service
 */
public class ServiceProviderCatalogService extends HttpServlet {

    private static final long serialVersionUID = -5280734755943517104L;

    public ServiceProviderCatalogService() {
    }

    /**
     * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
     */
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        try {
            System.err.println("Catalog Service.doGET Accept: " + request.getHeader("Accept"));
            final String dispatchTo;
            if (AcceptType.willAccept("text/html", request) && SharepointInitializer.isProvideHtml()) {
                dispatchTo = "/sharepoint/serviceprovidercatalog_html.jsp";

            } else if (AcceptType.willAccept("application/rdf+xml", request) ||
            		AcceptType.willAccept("application/xml", request)) {
                dispatchTo = "/sharepoint/serviceprovidercatalog_rdfxml.jsp";

            } else {
                response.sendError(HttpServletResponse.SC_UNSUPPORTED_MEDIA_TYPE);
                return;
            }

            final SharepointConnector sc = SharepointInitializer.getSharepointConnector();
            request.setAttribute("sharepointUri", SharepointInitializer.getSharepointUri());

            List<Library> libraries = sc.getLibraries();
            request.setAttribute("libraries", libraries);

            response.setHeader("OSLC-Core-Version", "2.0");
            final RequestDispatcher rd = request.getRequestDispatcher(dispatchTo);
            rd.forward(request, response);
            response.flushBuffer();

        } catch (Throwable e) {
            throw new ServletException(e);
        }
    }
}
