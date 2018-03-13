/*-
 * Copyright (c) 2016-2017   KTH Royal Institute of Technology.
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
 * Omar Kacimi         -  Initial implementation
 * Andrew Berezovskyi  -  Lyo contribution updates
 */

package org.eclipse.lyo.oslc4j.trs.server.service;

import java.net.URI;
import java.net.URISyntaxException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriInfo;
import org.eclipse.lyo.core.trs.Base;
import org.eclipse.lyo.core.trs.ChangeLog;
import org.eclipse.lyo.core.trs.Page;
import org.eclipse.lyo.core.trs.TRSConstants;
import org.eclipse.lyo.core.trs.TrackedResourceSet;
import org.eclipse.lyo.oslc4j.core.annotation.OslcService;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.trs.server.ChangeHistories;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The service class for the TRS interface. This class needs to be implemented by an OSLC adapter
 * wishing to implement a TRS interface
 *
 * @version $version-stub$
 * @since 2.3.0
 */
@OslcService(TRSConstants.TRS_NAMESPACE)
@Path("/trs")
public abstract class TrackedResourceSetService {
    private static final Logger log     = LoggerFactory.getLogger(TrackedResourceSetService.class);
    private static final String newline = System.getProperty("line.separator");

    @Context protected HttpServletRequest  httpServletRequest;
    @Context protected HttpServletResponse httpServletResponse;
    @Context protected UriInfo             uriInfo;

    public TrackedResourceSetService() {

    }

    /**
     * The instance of the change histories class used by a trs service class implementing this
     * class. The instance returned is expected to be a singleton of a class implementing the
     * ChangeHistories class
     *
     * @return the instance of the class implementing the change histories class
     */
    protected abstract ChangeHistories getChangeHistories();

    /**
     * The url prefix of the trs url
     *
     * @return the url prefix of the trs url
     */
    protected abstract String getServiceBase();

    /**
     * manage calls for a specific page of the base
     *
     * @param pagenum the requested page of the base
     *
     * @return the requested page of the base
     */
    @GET
    @Path(TRSConstants.TRS_TERM_BASE + "/{page}")
    @Produces({OslcMediaType.TEXT_TURTLE, OslcMediaType.APPLICATION_RDF_XML,
                      OslcMediaType.APPLICATION_XML, OslcMediaType.APPLICATION_JSON})
    public Response getBasePage(@PathParam("page") String pagenum) {
        log.info("received request for base page at url:" + httpServletRequest.getRequestURI() + ""
                         + " . Processing " + "request");
        Base base;
        try {
            base = getChangeHistories().getBaseResource(pagenum, httpServletRequest);
        } catch (URISyntaxException e) {
            throw new IllegalStateException(e);
        }
        if (base == null) {
            throw new WebApplicationException(Status.NOT_FOUND);
        }
        Page nextPage = base.getNextPage();
        if (nextPage == null) {
            throw new WebApplicationException(Status.NOT_FOUND);
        }
        // Due to OSLC4J limitation, not Base but NextPage will be returned.
        // See org.eclipse.lyo.rio.trs.resources.BaseResource.getBasePage(Long)
        log.info("finished processing request for base page at url:"
                         + httpServletRequest.getRequestURI() + " .");
        log.info("the returned base at url" + httpServletRequest.getRequestURI() + " contains :"
                         + base.getMembers().size() + " members.");
        return Response.ok(nextPage).header("Link", linkHeaderValue(base)).build();
    }

    /**
     * Returns the requested page of the change log
     *
     * @param pagenum the page number of the wanted page
     *
     * @return the requested page of the change log
     */
    @GET
    @Path(TRSConstants.TRS_TERM_CHANGE_LOG + "/{page}")
    @Produces({OslcMediaType.TEXT_TURTLE, OslcMediaType.APPLICATION_RDF_XML,
                      OslcMediaType.APPLICATION_XML, OslcMediaType.APPLICATION_JSON})
    public ChangeLog getChangeLogPage(@PathParam("page") String pagenum) {
        log.info("received request for changeLog page at url:" + httpServletRequest.getRequestURI()
                         + " . Processing " + "request");

        ChangeLog changeLog;
        try {
            changeLog = getChangeHistories().getChangeLog(pagenum, httpServletRequest);
        } catch (URISyntaxException e) {
            throw new WebApplicationException(e);
        }
        if (changeLog == null) {
            throw new WebApplicationException(Response.Status.NOT_FOUND);
        }
        log.info("finished processing request for changeLog page at url:"
                         + httpServletRequest.getRequestURI() + " .");
        log.info("the returned change with url" + httpServletRequest.getRequestURI() + " log "
                         + "contains :" + changeLog.getChange().size() + " changes.");
        return changeLog;
    }

    /**
     * the method managing calls asking for the tracked resource set object.
     *
     * @return the tracked resource set representation
     */
    @GET
    @Produces({OslcMediaType.TEXT_TURTLE, OslcMediaType.APPLICATION_RDF_XML,
                      OslcMediaType.APPLICATION_XML, OslcMediaType.APPLICATION_JSON})
    public TrackedResourceSet getTrackedResourceSet() throws URISyntaxException {
        log.info("received request for trs at url:" + httpServletRequest.getRequestURI() + " . "
                         + "Processing request");
        TrackedResourceSet result = new TrackedResourceSet();
        result.setAbout(buildURI("trs"));
        result.setBase(buildURI("trs/base"));//$NON-NLS-1$

        ChangeLog changeLog = getChangeHistories().getChangeLog("1", httpServletRequest);
        if (changeLog == null) {
            changeLog = new ChangeLog();
        }
        result.setChangeLog(changeLog);
        log.info("returning response for request for trs with url:"
                         + httpServletRequest.getRequestURI() + " .");
        log.info("the returned change log at url" + httpServletRequest.getRequestURI() + " "
                         + "contains :" + ((ChangeLog) changeLog).getChange().size() + " changes.");
        return result;
    }

    /**
     * manage http calls for the first page of the base. The call is redirected to the handler of
     * http calls for a specific base page as a call for the page 1 of the base
     *
     * @return the first page of the base
     */
    @Path(TRSConstants.TRS_TERM_BASE)
    @GET
    @Produces({OslcMediaType.TEXT_TURTLE, OslcMediaType.APPLICATION_RDF_XML,
                      OslcMediaType.APPLICATION_XML, OslcMediaType.APPLICATION_JSON})
    public Page getBase() {
        URI requestURI = uriInfo.getRequestUri();
        boolean endsWithSlash = requestURI.getPath().endsWith("/");
        String redirectLocation = requestURI.toString() + (endsWithSlash ? "1" : "/1");
        try {
            throw new WebApplicationException(Response.temporaryRedirect(new URI(redirectLocation))
                                                      .build());
        } catch (URISyntaxException e) {
            throw new IllegalStateException(e);
        }
    }

    /**
     * manage the calls for the change log and redirects to the handler of a specific page of the
     * change log with the call to the first page
     *
     * @return the first page of the change lof
     */
    @Path(TRSConstants.TRS_TERM_CHANGE_LOG)
    @GET
    @Produces({OslcMediaType.TEXT_TURTLE, OslcMediaType.APPLICATION_RDF_XML,
                      OslcMediaType.APPLICATION_XML, OslcMediaType.APPLICATION_JSON})
    public ChangeLog getChangeLog() {
        URI requestURI = uriInfo.getRequestUri();
        boolean endsWithSlash = requestURI.getPath().endsWith("/");
        String redirectLocation = requestURI.toString() + (endsWithSlash ? "1" : "/1");
        try {
            throw new WebApplicationException(Response.temporaryRedirect(new URI(redirectLocation))
                                                      .build());
        } catch (URISyntaxException e) {
            throw new IllegalStateException(e);
        }
    }

    private URI buildURI(String append) {
        return URI.create(getServiceBase()).resolve(append);
//        URI.create(getServiceBase()
    }

    /**
     * The value for the link header returned for each response for a base page
     *
     * @return the link header value
     */
    private String linkHeaderValue(Base base) {
        Page nextPage = base.getNextPage();
        URI pageOf = nextPage.getPageOf().getAbout();
        String headerValue = urize(pageOf.toString()) + "; rel=\"first\"," + newline + urize(
                nextPage.getNextPage().toString()) + "; rel=\"next\"," + newline + "<http://www"
                + "" + "" + "" + ".w3.org/ns/ldp#Page>; " + "rel=\"type\"" + newline;
        return headerValue;
    }

    private String urize(String uri) {
        return "<" + uri + ">";
    }

}
