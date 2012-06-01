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
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.eclipse.lyo.samples.sharepoint.SharepointConnector;
import org.eclipse.lyo.samples.sharepoint.core.IConstants;
import org.eclipse.lyo.samples.sharepoint.exceptions.ConnectionException;
import org.eclipse.lyo.samples.sharepoint.exceptions.ShareServiceException;
import org.eclipse.lyo.samples.sharepoint.exceptions.SharepointException;
import org.eclipse.lyo.samples.sharepoint.services.ShareBaseService;
import org.eclipse.lyo.samples.sharepoint.store.OslcResource;
import org.eclipse.lyo.samples.sharepoint.store.ShareServerException;
import org.eclipse.lyo.samples.sharepoint.store.UnrecognizedValueTypeException;



/**
 * Servlet implementation class ResourceCompact
 */
public class ResourceCompactService extends ShareBaseService {
	private static final long serialVersionUID = 5776266053922927743L;

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		String uri = request.getParameter("uri"); //$NON-NLS-1$
		String previewType = request.getParameter("type"); //$NON-NLS-1$
		OslcResource resource = null;
		try {
			final SharepointConnector sc = SharepointInitializer.getSharepointConnector();
			resource = sc.getDocumentProperties(uri);
		} catch (ShareServerException e) {
			throw new ShareServiceException(e);
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
		if( resource == null ) {
			throw new ShareServiceException(IConstants.SC_NOT_FOUND, "Resource not found");
		}
		request.setAttribute("resource", resource); //$NON-NLS-1$
		String preview = null;
		if( "large".equals(previewType) ) { //$NON-NLS-1$
			preview = "/sharepoint/resource_largePreview.jsp"; //$NON-NLS-1$
		} else {
			preview = "/sharepoint/resource_smallPreview.jsp"; //$NON-NLS-1$
		}
		RequestDispatcher rd = request.getRequestDispatcher(preview);
		rd.forward(request, response);
		
	}

}
