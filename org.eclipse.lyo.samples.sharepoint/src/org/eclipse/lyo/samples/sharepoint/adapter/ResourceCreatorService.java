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
import java.io.InputStream;
import java.util.Iterator;
import java.util.List;


import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.FileItemFactory;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.eclipse.lyo.samples.sharepoint.SharepointConnector;
import org.eclipse.lyo.samples.sharepoint.core.IConstants;
import org.eclipse.lyo.samples.sharepoint.exceptions.ShareServiceException;
import org.eclipse.lyo.samples.sharepoint.services.ShareBaseService;




/**
 * Servlet implementation class ResourceFactory
 */
public class ResourceCreatorService extends ShareBaseService {
	private static final long serialVersionUID = 7466374797163202313L;
	
	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {
		String collection = req.getParameter("collection");
		req.setAttribute("collection", collection);
		RequestDispatcher rd = req.getRequestDispatcher("/sharepoint/resource_creator.jsp"); //$NON-NLS-1$
		rd.forward(req, resp);
	}


	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		String title = request.getParameter("title"); //$NON-NLS-1$
		String description = request.getParameter("description"); //$NON-NLS-1$
		String filename = request.getParameter("file");
		String collection = request.getParameter("collection"); //"Empire";
		
		try {			
			boolean isFileUpload = ServletFileUpload.isMultipartContent(request);
			String contentType = request.getContentType();
			
			if( !isFileUpload && !IConstants.CT_RDF_XML.equals(contentType) ) {
				throw new ShareServiceException(IConstants.SC_UNSUPPORTED_MEDIA_TYPE);
			}
			
			InputStream content = request.getInputStream();
			
			if( isFileUpload ) {
				// being uploaded from a web page
				try{
					FileItemFactory factory = new DiskFileItemFactory();
					ServletFileUpload upload = new ServletFileUpload(factory);
					@SuppressWarnings("unchecked")
					List<FileItem> items = upload.parseRequest(request);
					
					// find the first (and only) file resource in the post
					Iterator<FileItem> iter = items.iterator();
					while (iter.hasNext()) {
					    FileItem item = iter.next();
					    if (item.isFormField()) {
					        // this is a form field, maybe we can accept a title or descr?
					    } else {
					    	
					    	final SharepointConnector sc = SharepointInitializer.getSharepointConnector();
					    	int rc = sc.createDocument(response, collection,item);
					    	
					    	//String content = compactDocument(resource);
							//System.out.println(content);
							//response.setContentType(IConstants.CT_OSLC_COMPACT);
							//response.setContentLength(content.getBytes().length);
							//response.setStatus(IConstants.SC_OK);
							//response.getWriter().write(content);
					    	
					    	//Get Response?
					    	//jsonResults.append( "\n] \n}" ); //$NON-NLS-1$
							//response.setContentType(IConstants.CT_XML);
					        //response.getWriter().write(is.toString()); 
						    //response.setStatus(IConstants.SC_OK);
					    	
						 
					    	// all that may need to be done is to send a POST
					    	// to Sharepoint, with stream content, Slug set and content_type
					    	// slug = /Empire/name.doc
					    	
					    	//response.setHeader(IConstants.HDR_SLUG, "/Empire" + filename);
					    	//response.setHeader(IConstants.HDR_CONTENT_TYPE, contentType);
					    	
					    	//response.sendRedirect(response.encodeRedirectURL(SharepointInitializer.getSharepointUri() + "/Empire"));
					    	
					    	//RequestDispatcher rd = request.getRequestDispatcher(SharepointInitializer.getSharepointUri() + "/Empire");
					    	//rd.forward(request, response);
					    	
							//response.setStatus(IConstants.SC_CREATED);
							//response.setHeader(IConstants.HDR_LOCATION, uri);
							//RequestDispatcher rd = request.getRequestDispatcher("/resource/Empire/5"); //$NON-NLS-1$
							//rd.forward(request, response);
					    }
					}
					
				} catch( Exception e ) {
					throw new ShareServiceException(e);
				}
			}
			
		
			
		} catch( Exception e) {
			throw new ShareServiceException(e);
		}
	}


}
