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
import javax.inject.Inject;
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
import org.eclipse.lyo.oslc4j.core.model.Error;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.trs.server.IChangeHistories;
import org.eclipse.lyo.oslc4j.trs.server.TRSUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The service class for the TRS interface. This class needs to be implemented by an OSLC adapter
 * wishing to implement a TRS interface
 *
 * @version $version-stub$
 * @since 2.3.0
 */
@Path("/trs")
@OslcService(TRSConstants.TRS_NAMESPACE)
public class TrackedResourceSetService {
    private static final Logger log     = LoggerFactory.getLogger(TrackedResourceSetService.class);
    public static final String BASE_PATH = "base";
    public static final String CHANGELOG_PATH = "changeLog";

    /**
     * The instance of the change histories class used by a trs service class implementing this
     * class. The instance returned is expected to be a singleton of a class implementing the
     * ChangeHistories class
     *
     * @return the instance of the class implementing the change histories class
     */
    private IChangeHistories changeHistories;

    public TrackedResourceSetService() {
    }

    @Inject
    public TrackedResourceSetService(IChangeHistories _changeHistories) {
        changeHistories = _changeHistories;
    }

    /**
     * the method managing calls asking for the tracked resource set object.
     *
     * @return the tracked resource set representation
     */
    @GET
    @Produces({OslcMediaType.TEXT_TURTLE, OslcMediaType.APPLICATION_RDF_XML,
            OslcMediaType.APPLICATION_XML, OslcMediaType.APPLICATION_JSON})
    public TrackedResourceSet getTrackedResourceSet(@Context UriInfo uriInfo) {
        TrackedResourceSet result = new TrackedResourceSet();

        result.setAbout(uriInfo.getRequestUri());
        result.setBase(uriInfo.getAbsolutePathBuilder().path(BASE_PATH).build());

        ChangeLog changeLog = getChangeHistories().getChangeLog("1");
        if (changeLog == null) {
            changeLog = new ChangeLog();
        }
        try {
            result.setChangeLog(changeLog);
        } catch (URISyntaxException e) {
            // FIXME Andrew@2019-04-27: remove this exception from the signature
            throw new IllegalArgumentException("Can't set the change log", e);
        }
        return result;
    }

    /**
     * manage calls for a specific page of the base
     *
     * @param pageNo the requested page of the base
     * @return the requested page of the base
     */
    @GET
    @Path(BASE_PATH + "/{page}")
    @Produces({OslcMediaType.TEXT_TURTLE, OslcMediaType.APPLICATION_RDF_XML,
                      OslcMediaType.APPLICATION_XML, OslcMediaType.APPLICATION_JSON})
    public Response getBasePage(@PathParam("page") String pageNo) {
        Base base = getChangeHistories().getBaseResource(pageNo);
        if (base == null) {
            final Error entity = new Error();
            entity.setMessage("Wrong TRS Base page URI");
            entity.setStatusCode(String.valueOf(Status.NOT_FOUND.getStatusCode()));
            return Response.status(Status.NOT_FOUND).entity(entity).build();
        }
        // Due to OSLC4J limitation, not Base but NextPage will be returned.
        // See org.eclipse.lyo.rio.trs.resources.BaseResource.getBasePage(Long)
        // FIXME Andrew@2019-04-27: do the math right
        Page nextPage = base.getNextPage();
        if (nextPage == null) {
            throw new WebApplicationException(Status.NOT_FOUND);
        }
        log.debug("TRS Base page contains {} members", base.getMembers().size());
        return Response.ok(nextPage).header("Link", TRSUtil.linkHeaderValue(base)).build();
    }

    protected IChangeHistories getChangeHistories() {
        return changeHistories;
    }

    /**
     * Returns the requested page of the change log
     *
     * @param page the page number of the wanted page
     * @return the requested page of the change log
     */
    @GET
    @Path(CHANGELOG_PATH + "/{page}")
    @Produces({OslcMediaType.TEXT_TURTLE, OslcMediaType.APPLICATION_RDF_XML,
                      OslcMediaType.APPLICATION_XML, OslcMediaType.APPLICATION_JSON})
    public Response getChangeLogPage(@PathParam("page") String page) {
        log.trace("TRS Change Log page '{}' requested", page);

        ChangeLog changeLog = null;
        changeLog = getChangeHistories().getChangeLog(page);
        if (changeLog == null) {
            final Error entity = new Error();
            entity.setMessage("Wrong TRS Change Log page URI");
            entity.setStatusCode(String.valueOf(Status.NOT_FOUND.getStatusCode()));
            return Response.status(Status.NOT_FOUND).entity(entity).build();
        }
        log.debug("TRS Change Log page contains {} members", changeLog.getChange().size());
        return Response.ok(changeLog).build();
    }

    /**
     * manage the calls for the change log and redirects to the handler of a specific page of the
     * change log with the call to the first page
     *
     * @return the first page of the change lof
     */
    @GET
    @Path(CHANGELOG_PATH)
    public Response getChangeLog(@Context UriInfo uriInfo) {
        final URI newURI = uriInfo.getAbsolutePathBuilder().path("1").build();
        return Response.seeOther(newURI).build();
    }

    /**
     * manage http calls for the first page of the base. The call is redirected to the handler of
     * http calls for a specific base page as a call for the page 1 of the base
     *
     * @return the first page of the base
     */
    @GET
    @Path(BASE_PATH)
    public Response getBase(@Context UriInfo uriInfo) {
        final URI newURI = uriInfo.getAbsolutePathBuilder().path("1").build();
        return Response.seeOther(newURI).build();
    }
}
