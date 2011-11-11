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
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import jbugz.base.Bug;
import jbugz.base.BugzillaConnector;
import jbugz.exceptions.BugzillaException;
import jbugz.exceptions.ConnectionException;
import jbugz.exceptions.InvalidDescriptionException;
import jbugz.rpc.CommentBug;

import org.eclipse.lyo.samples.bugzilla.exception.UnauthroziedException;
import org.eclipse.lyo.samples.bugzilla.jbugzx.rpc.ExtendedGetBug;
import org.eclipse.lyo.samples.bugzilla.resources.BugzillaChangeRequest;
import org.eclipse.lyo.samples.bugzilla.resources.Person;
import org.eclipse.lyo.samples.bugzilla.utils.AcceptType;
import org.eclipse.lyo.samples.bugzilla.utils.HttpUtils;
import org.eclipse.lyo.samples.bugzilla.utils.RdfUtils;

import thewebsemantic.RDF2Bean;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;


/**
 * OSLC CM Change Request Service
 */
public class ChangeRequestService extends HttpServlet {    	
	private static final long serialVersionUID = -5280734755943517104L;
	
	private static final Map<String, String> PREFIXES = new HashMap<String, String>();
	static {
		PREFIXES.put("oslc", "http://open-services.net/ns/core#");
		PREFIXES.put("oslc_cm", "http://open-services.net/ns/cm#");
		PREFIXES.put("dcterms", "http://purl.org/dc/terms/");
		PREFIXES.put("foaf", "http://xmlns.com/foaf/0.1/");
		PREFIXES.put("bugz", "http://www.bugzilla.org/rdf#");
	}
	
    public ChangeRequestService() {}

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        System.err.println("CR.doGET - Accept: " + request.getHeader("Accept") + ", query="+request.getQueryString());
		
		int bugId = -1;
		Bug bug = null;
		try {
			bugId = Integer.parseInt(request.getParameter("id"));
			final BugzillaConnector bc = BugzillaInitializer.getBugzillaConnector(request);			
			final ExtendedGetBug getBug = new ExtendedGetBug(bugId);
			bc.executeMethod(getBug);
			bug = getBug.getBug();	
			if (bug == null) {
				response.sendError(HttpServletResponse.SC_NOT_FOUND);
				return;
			}			
		} catch (ConnectionException e) {
			e.printStackTrace();
			response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            return;      
		} catch (BugzillaException e) {
			e.printStackTrace();
			response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            return;      
		} catch (UnauthroziedException e) {
			HttpUtils.sendUnauthorizedResponse(response, e);
			return;
		}
		
		try {
			final String dispatchTo;
			final String preview = request.getParameter("preview");
			
			if ("small".equals(preview)) {
				dispatchTo = "/cm/changerequest_preview_small.jsp";
			
            } else if ("large".equals(preview)) {
				dispatchTo = "/cm/changerequest_preview_large.jsp";
			
            } else 	if (AcceptType.willAccept("application/x-oslc-compact+xml", request)) {	
				dispatchTo = "/cm/changerequest_preview_desc.jsp";
			
            } else if (AcceptType.willAccept("text/html", request) && BugzillaInitializer.isProvideHtml()) {
				response.sendRedirect(BugzillaInitializer.getBugzillaUri() + "/show_bug.cgi?id=" + bugId);
				return;
				
			} else if (AcceptType.willAccept("application/rdf+xml", request)
					|| AcceptType.willAccept("application/xml", request)) {
		        response.setHeader("OSLC-Core-Version", "2.0");
		        response.setHeader("Content-Type", "application/rdf+xml");
				BugzillaChangeRequest changeRequest = BugzillaChangeRequest.fromBug(bug);
				RdfUtils.sendRdfResponse(response, changeRequest, RdfUtils.JENA_LANG_ABBREVIATED_RDF_XML);
				return;
				
			} else if (AcceptType.willAccept("text/turtle", request)) {
		        response.setHeader("OSLC-Core-Version", "2.0");
		        response.setHeader("Content-Type", "text/turtle");
				BugzillaChangeRequest changeRequest = BugzillaChangeRequest.fromBug(bug);
				RdfUtils.sendRdfResponse(response, changeRequest, RdfUtils.JENA_LANG_TURTLE);
				return;
				
			} else {
				response.sendError(HttpServletResponse.SC_UNSUPPORTED_MEDIA_TYPE);
				return;
			}

            request.setAttribute("bugUri", URLStrategy.getChangeRequestURL(bug.getID())); 
            request.setAttribute("bug", bug); 
            request.setAttribute("bugzillaUri", BugzillaInitializer.getBugzillaUri());

            response.setHeader("OSLC-Core-Version", "2.0");
			final RequestDispatcher rd = request.getRequestDispatcher(dispatchTo);  
			rd.forward(request, response);
			response.flushBuffer();

		} catch (Throwable e) {
			throw new ServletException(e);
		}
	}

	protected void doPut(HttpServletRequest request,
			HttpServletResponse response) throws ServletException, IOException {
		System.err.println("CR.doPUT - Accept: " + request.getHeader("Accept")
				+ ", query=" + request.getQueryString());

		if (!request.getContentType().startsWith("application/rdf+xml")
				&& !request.getContentType().startsWith("application/xml")) {
			response.sendError(HttpServletResponse.SC_UNSUPPORTED_MEDIA_TYPE);
			return;
		}

		try {
			BugzillaChangeRequest cr = readChangeRequest(request);
			if (cr == null) {
				response.sendError(HttpServletResponse.SC_NOT_FOUND);
				return;
			}

			updateBug(request, cr);
			response.setStatus(HttpServletResponse.SC_NO_CONTENT);
		} catch (Exception e) {
			throw new ServletException(e);
		}
	}
	
	private void updateBug(HttpServletRequest request, BugzillaChangeRequest cr)
			throws ConnectionException, BugzillaException,
			InvalidDescriptionException, UnauthroziedException {
		BugzillaConnector bc = BugzillaInitializer.getBugzillaConnector(request);
		// No built in field to hold external links. Just add the new link as a comment for now.
		String comment = getLinksComment(cr);
		if (comment.length() != 0) {
			CommentBug bugzillaMethod = new CommentBug(Integer.parseInt(cr.getIdentifier()), comment);
			bc.executeMethod(bugzillaMethod);
		}
	}

	public void addLinkComment(StringBuffer buffer, String linkType,
			Collection<URI> links) {
		if (links != null && !links.isEmpty()) {
			buffer.append(linkType);
			buffer.append(":\n\n");
			for (URI link : links) {
				buffer.append(link.toString());
				buffer.append("\n");
			}
		}
	}

	private String getLinksComment(BugzillaChangeRequest cr) {
		StringBuffer b = new StringBuffer();
		
		addLinkComment(b, "Affected by Defect", cr.getAffectedByDefect());
		addLinkComment(b, "Affects Plan Item", cr.getAffectsPlanItem());
		addLinkComment(b, "Affects Requirement", cr.getAffectsRequirement());
		addLinkComment(b, "Affects Test Result", cr.getAffectsTestResult());
		addLinkComment(b, "Blocks Test Execution Record", cr.getBlocksTestExecutionRecord());
		addLinkComment(b, "Implements Requirement", cr.getImplementsRequirement());
		addLinkComment(b, "Related Change Request", cr.getRelatedChangeRequest());
		addLinkComment(b, "Related Test Execution Record", cr.getRelatedTestExecutionRecord());
		addLinkComment(b, "Related Test Plane", cr.getRelatedTestPlan());
		addLinkComment(b, "Related Test Script", cr.getRelatedTestScript());
		addLinkComment(b, "Tested by Test Case", cr.getTestedByTestCase());
		addLinkComment(b, "Tracks Change Set", cr.getTracksChangeSet());
		addLinkComment(b, "Tracks Requirement", cr.getTracksRequirement());
		
		return b.toString();
	}

	private BugzillaChangeRequest readChangeRequest(HttpServletRequest request)
			throws URISyntaxException, IOException {
		String bugURI = getFullRequestURL(request);
		Model model = ModelFactory.createOntologyModel();
		model.read(request.getInputStream(), bugURI);

		RDF2Bean reader = new RDF2Bean(model);
		reader.bind(BugzillaChangeRequest.class);
		reader.bind(Person.class);
		
		return reader.load(BugzillaChangeRequest.class,
				new URI(bugURI));
	}
	
	private String getFullRequestURL(HttpServletRequest request) {
		StringBuffer url = request.getRequestURL();
		String queryString = request.getQueryString();
		if (queryString != null) {
			url.append("?");
			url.append(queryString);
		}
		
		return url.toString();
	}
}
