package org.eclipse.lyo.oslc4j.provider.json4j;

import org.eclipse.lyo.oslc4j.core.OSLC4JConstants;
import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.core.annotation.OslcQueryCapability;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.provider.json4j.test.resources.TestResource;
import org.junit.Test;

import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.UriBuilder;
import javax.ws.rs.core.UriInfo;
import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

@Path("/test")
public class ProviderCollectionResource {
    @Context private HttpServletRequest httpServletRequest;
    @Context private HttpServletResponse httpServletResponse;
    @Context private UriInfo uriInfo;

    @Context
    private ServletConfig config;
    @Context
    private ServletContext context;

    @GET
    @Produces(MediaType.TEXT_PLAIN)
    public String testCall() {
        return "Hello world";
    }


    @OslcQueryCapability
        (
            title = "ProviderCollectionTestQC",
            label = "ProviderCollectionTestQC test endpoint (Array/RDF)",
//            resourceShape = OslcConstants.PATH_RESOURCE_SHAPES + "/" + Oslc_rmDomainConstants.REQUIREMENT_PATH,
            resourceTypes = {TestResource.TEST_RESOURCE_TYPE},
            usages = {}
        )
    @GET
    @Path("qc-array-rdf")
    @Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_JSON_LD, OslcMediaType.TEXT_TURTLE,
        OslcMediaType.APPLICATION_XML, OslcMediaType.APPLICATION_JSON})
    public TestResource[] queryRequirements(
        @QueryParam("oslc.where") final String where,
        @QueryParam("oslc.prefix") final String prefix,
        @QueryParam("oslc.paging") final String pagingString,
        @QueryParam("page") final String pageString,
        @QueryParam("oslc.pageSize") final String pageSizeString) throws IOException,
        ServletException {
        boolean paging = false;
        int page = 0;
        int pageSize = 20;
        if (null != pagingString) {
            paging = Boolean.parseBoolean(pagingString);
        }
        if (null != pageString) {
            page = Integer.parseInt(pageString);
        }
        if (null != pageSizeString) {
            pageSize = Integer.parseInt(pageSizeString);
        }

        List<TestResource> resources = generateTestResources();
        UriBuilder uriBuilder = UriBuilder.fromUri(uriInfo.getAbsolutePath())
            .queryParam("oslc.paging", "true")
            .queryParam("oslc.pageSize", pageSize)
            .queryParam("page", page);
        if (null != where) {
            uriBuilder.queryParam("oslc.where", where);
        }
        if (null != prefix) {
            uriBuilder.queryParam("oslc.prefix", prefix);
        }
        // TODO null in test, check if impacts the Provider logic
        httpServletRequest.setAttribute("queryUri", uriBuilder.build().toString());
        if ((OSLC4JUtils.hasLyoStorePagingPreciseLimit() && resources.size() >= pageSize)
            || (!OSLC4JUtils.hasLyoStorePagingPreciseLimit() && resources.size() > pageSize)) {
            resources = resources.subList(0, pageSize);
            uriBuilder.replaceQueryParam("page", page + 1);
            httpServletRequest.setAttribute(OSLC4JConstants.OSLC4J_NEXT_PAGE, uriBuilder.build().toString());
        }
        return resources.toArray(new TestResource[resources.size()]);
    }

    private List<TestResource> generateTestResources() {
        return IntStream.range(0, 10).mapToObj(i -> new TestResource()).collect(Collectors.toList());
    }
}
