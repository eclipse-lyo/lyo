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
package org.eclipse.lyo.samples.sharepoint.web;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Date;
import java.util.List;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.eclipse.lyo.samples.sharepoint.adapter.SharepointInitializer;
import org.eclipse.lyo.samples.sharepoint.adapter.SharepointResource;
import org.eclipse.lyo.samples.sharepoint.adapter.URLStrategy;
import org.eclipse.lyo.samples.sharepoint.common.IAmConstants;
import org.eclipse.lyo.samples.sharepoint.core.IConstants;
import org.eclipse.lyo.samples.sharepoint.exceptions.ShareServiceException;
import org.eclipse.lyo.samples.sharepoint.services.ShareBaseService;



public class ResourceWebService extends ShareBaseService {
	
	private static final long serialVersionUID = 6031384602038217153L;


/*	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {
		RequestDispatcher rd = req.getRequestDispatcher("/cm/changerequest_creator.jsp"); //$NON-NLS-1$
		rd.forward(req, resp);
	}*/


	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		String uri = request.getParameter("uri"); //$NON-NLS-1$
		String eTag = request.getParameter("eTag"); //$NON-NLS-1$
		String title = request.getParameter("title"); //$NON-NLS-1$
		
		String openType = request.getParameter("Open");
		
		
/*		try{
			RioStore store = getStore();
			Resource resource = null;
			if( uri == null ) {
				uri = store.nextAvailableUri(IAmConstants.SERVICE_RESOURCE);
				resource = new Resource(uri);
			} else {
				OslcResource soriResource = store.getOslcResource(uri);
				if( !eTag.equals(soriResource.getETag()) ){
					throw new RioServiceException(IConstants.SC_CONFLICT, "ETag mismatch");
				}
				resource = new Resource(uri, soriResource.getStatements());
			}
			
			if( saveType.equals("SaveRdf")) {
				String rdfxml = request.getParameter("rdfxml"); 
				ByteArrayInputStream content = new ByteArrayInputStream(rdfxml.getBytes());
				
				// cache the created and creator
				Date created = resource.getCreated();
				String creator = resource.getCreator();
				
				OslcResource updatedResource = new OslcResource(resource.getUri());
				List<RioStatement> statements = store.parse(resource.getUri(), content, IConstants.CT_RDF_XML);
				updatedResource.addStatements(statements);
				updatedResource.setCreated(created);
				updatedResource.setCreator(creator);
				String userId = request.getRemoteUser();
				String userUri = this.getUserUri(userId);
				store.update(updatedResource, userUri);

			} else {
				String title = request.getParameter("title"); //$NON-NLS-1$
				String description = request.getParameter("description"); //$NON-NLS-1$

				resource.setTitle(title);
				resource.setDescription(description);

				String userUri = getUserUri(request.getRemoteUser());
				store.update(resource, userUri);
				
			}*/
			
			

		try {
			
			//redirect to the actual sharepoint document: http://www.abcdef.com:20001/Empire/DeathStar Architecture.doc
			if (uri.startsWith(URLStrategy.getResourceBaseURL())) {
			   String tempStr = uri.substring(URLStrategy.getResourceBaseURL().length()+1);
			   String[] tokens = tempStr.split("/");	
				
			   //System.out.println("Entered ResourceWebService: uri is " + "uri");
			   //System.out.println("Entered ResourceWebService: title is " + "title");
			   //System.out.println("Entered ResourceWebService: sharepoint doc is "  +SharepointInitializer.getSharepointNormalUri() + "/" + tokens[0] + "/" + title);
			
			   response.sendRedirect(SharepointInitializer.getSharepointNormalUri() + "/" + tokens[0] + "/" + title);
			}
			
		} catch( Exception e) {
			throw new ShareServiceException(e);
		}
	}


}
