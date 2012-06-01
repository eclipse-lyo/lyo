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
package org.eclipse.lyo.samples.sharepoint.services;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Enumeration;
import java.util.List;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;



import org.eclipse.lyo.samples.sharepoint.core.IConstants;
import org.eclipse.lyo.samples.sharepoint.l10n.Messages;
import org.eclipse.lyo.samples.sharepoint.services.ShareServiceException;
import org.eclipse.lyo.samples.sharepoint.store.OslcResource;
import org.eclipse.lyo.samples.sharepoint.store.ShareServerException;
import org.eclipse.lyo.samples.sharepoint.store.ShareStore;
import org.eclipse.lyo.samples.sharepoint.util.StringUtils;
import org.openrdf.rio.RDFFormat;

public abstract class ShareBaseService extends HttpServlet {

	private static final long serialVersionUID = -8870677366709073238L;
	
	protected ShareStore getStore() throws ShareServiceException {
		try {
			return ShareStore.getStore();
		} catch (ShareServerException e) {
			throw new ShareServiceException(e);
		}
	}

	
	public String getBaseUrl() throws ShareServiceException{
		return getStore().getUriBase();
	}
	
	public String getUserUri(String userId) throws ShareServiceException {
		if( userId == null ) {
			userId = IConstants.SHARE_UNKNOWN_USER_ID;
		}
		return this.getBaseUrl() + '/' + IConstants.SERVICE_USER + '/' + userId;
	}
	
	@Override
	public void service(ServletRequest req, ServletResponse res)
			throws ServletException, IOException {

		HttpServletRequest	request;
		HttpServletResponse	response;
		
		try {
		    request = (HttpServletRequest) req;
		    response = (HttpServletResponse) res;
		} catch (ClassCastException e) {
		    throw new ServletException(Messages.getString("ShareBaseService.NonHttpRequest")); //$NON-NLS-1$
		}		
				
		try{
			service(request, response);
			response.setHeader(IConstants.HDR_OSLC_VERSION, IConstants.OSLC_VERSION);
		} catch( ShareServiceException ex ) {
			// return with an OSLC message style
			response.setStatus(ex.getStatus());
			response.setHeader(IConstants.HDR_OSLC_VERSION, IConstants.OSLC_VERSION);
			response.setContentType(IConstants.CT_RDF_XML);
			ShareError error = new ShareError(IConstants.SC_INTERNAL_ERROR, ex.getMessage() );
			response.getWriter().write(error.toString());
		} catch( Exception ex ) {
			// return with an OSLC message style
			response.setStatus(IConstants.SC_INTERNAL_ERROR);
			response.setHeader(IConstants.HDR_OSLC_VERSION, IConstants.OSLC_VERSION);
			response.setContentType(IConstants.CT_RDF_XML);
			StringWriter sw = new StringWriter();
		    PrintWriter pw = new PrintWriter(sw);
		    ex.printStackTrace(pw);
			ShareError error = new ShareError(IConstants.SC_INTERNAL_ERROR, sw.toString() );
			response.getWriter().write(error.toString());
		}
	}

	protected void checkConditionalHeaders(HttpServletRequest request, OslcResource resource) throws ShareServiceException {
		String ifMatch = request.getHeader(IConstants.HDR_IF_MATCH);
		String ifUnmodifiedSince = request.getHeader(IConstants.HDR_IF_UNMODIFIED_SINCE);
		if( ifMatch == null && ifUnmodifiedSince == null ) {
			throw new ShareServiceException(IConstants.SC_BAD, 
					Messages.getString("Resource.ConditionalHeaderRequired"));  //$NON-NLS-1$
		}
		try{
			if( ifMatch != null ) {
				String eTag = resource.getETag();
				if( !ifMatch.equals("*") && !ifMatch.equals(eTag) ) {
					throw new ShareServiceException(IConstants.SC_PRECONDITION_FAILED, "ETag mismatch");
				}
			}
			if( ifUnmodifiedSince != null ) {
				Date unmodifiedSince = StringUtils.rfc2822(ifUnmodifiedSince);
				Date modified = resource.getModified();
				if( unmodifiedSince.after(modified) ) {
					throw new ShareServiceException(IConstants.SC_PRECONDITION_FAILED, "Last modified after supplied If-Unmodified-Since value");
				}
			}
		} catch (ParseException e) {
			throw new ShareServiceException(IConstants.SC_PRECONDITION_FAILED, "Invalid If-Unmodified-Since Header");
		} catch (Exception e ) {
			throw new ShareServiceException(e);
		}

	}


	protected boolean willAccept(String contentType, HttpServletRequest req) {
		@SuppressWarnings("rawtypes")
		Enumeration acceptHds = req.getHeaders(IConstants.HDR_ACCEPT);
		while(acceptHds.hasMoreElements()) {
			String acceptHdr = (String) acceptHds.nextElement();
			// split up the individual content types
			String[] hdrs = acceptHdr.split(",");
			for (String acctHdrVal : hdrs) {
				AcceptType acceptType = new AcceptType( acctHdrVal);
				if( acceptType.equals(contentType) ) return true;
			}
		}
		return false;
	}
	
	protected String acceptContentType(String[] prioritizedContentTypes, HttpServletRequest req) {
		// simplified content negotiation, first get all accepted types, and order by qs
		// then we walk through prioritized content types trying to find a match, in the
		// order that the server prefers
		List<AcceptType> acceptableTypes = new ArrayList<AcceptType>();
		@SuppressWarnings("rawtypes")
		Enumeration acceptHds = req.getHeaders(IConstants.HDR_ACCEPT);
		while(acceptHds.hasMoreElements()) {
			String acceptHdr = (String) acceptHds.nextElement();
			// split up the individual content types
			String[] hdrs = acceptHdr.split(",");
			for (String acctHdrVal : hdrs) {
				acceptableTypes.add( new AcceptType( acctHdrVal) );
			}
			// now sort by weight
			Collections.sort(acceptableTypes);
			
			// now find the most appropriate header
			for (String contentType : prioritizedContentTypes) {
				if( acceptableTypes.contains(new AcceptType(contentType) )  ) {
					return contentType;
				}
			}
		}
		return null;
	}
	
	private class AcceptType implements Comparable<AcceptType> {
		public String type;
		public int weight;
		public AcceptType(String typeExp) {
			int pos = typeExp.indexOf(';'); 
			if( pos > 0 ) {
				this.type = typeExp.substring(0,pos).trim();
				String qualifier = typeExp.substring(pos+1);
				if( qualifier.startsWith("q=") ) {
					try{
						float w = Float.parseFloat(qualifier.substring(2)) * 1000;
						this.weight = (int) w;
					}catch( NumberFormatException e ) {
						this.weight = 1000;
					}
				} else {
					this.weight = 1000;
				}
				
			} else {
				this.type = typeExp;
				this.weight = new Integer(1000);
			}
		}
		@Override
		public int compareTo(AcceptType other) {
			return ((AcceptType)other).weight - weight;
		}
		
		@Override
		public boolean equals(Object obj) {
			if( obj instanceof AcceptType ) {
				AcceptType otherAcceptType = (AcceptType) obj;
				// now we need to sort out wildcards
				String[] otc = otherAcceptType.type.split("/");
				String[] ttc = this.type.split("/");
				boolean seg1 = compareSegment(otc[0], ttc[0]);
				boolean seg2 = compareSegment(otc[1], ttc[1]);
				return seg1 && seg2;
			} else if( obj instanceof String ) {
				String[] otc = ((String)obj).split("/");
				String[] ttc = this.type.split("/");
				boolean seg1 = compareSegment(otc[0], ttc[0]);
				boolean seg2 = compareSegment(otc[1], ttc[1]);
				return seg1 && seg2;
			}
			return false;
		}
		
		public boolean compareSegment(String s1, String s2 ) {
			if( s1.equals("*") || s2.equals("*") ) {
				return true; 
			} 
			return s1.equals(s2);
		}
	}
	

	@SuppressWarnings({ })
	protected RDFFormat acceptFormat(RDFFormat[] prioritizedFormats, HttpServletRequest req) {
		@SuppressWarnings("rawtypes")
		Enumeration acceptHds = req.getHeaders(IConstants.HDR_ACCEPT);
		while(acceptHds.hasMoreElements()) {
			String acceptHdr = (String) acceptHds.nextElement();
			for (RDFFormat format : prioritizedFormats) {
				List<String> mimeTypes = format.getMIMETypes();
				for (String mimetype : mimeTypes) {
					if( acceptHdr.indexOf(mimetype) >= 0 ) {
						return format;
					}
				}
			}
		}
		return null;
	}
	
	protected void reportError( int statusCode, String message, HttpServletRequest req, HttpServletResponse resp ) throws ServletException, IOException{
		req.setAttribute("statusCode", Integer.toString(statusCode)); //$NON-NLS-1$
		req.setAttribute("message", StringUtils.forHtml(message) ); //$NON-NLS-1$
		RequestDispatcher dispatcher = 
			req.getRequestDispatcher("/ori/web/error.jsp"); //$NON-NLS-1$
	    if (dispatcher != null) dispatcher.forward(req, resp);
	}


}
