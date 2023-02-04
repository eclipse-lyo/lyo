package org.eclipse.lyo.oslc4j.provider.jena;

import org.eclipse.lyo.oslc4j.core.OSLC4JConstants;
import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.core.annotation.OslcQueryCapability;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.provider.jena.test.resources.TestResource;
import org.eclipse.lyo.oslc4j.provider.jena.test.resources.TestResourceNonQ;

import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.GenericEntity;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import javax.ws.rs.core.UriInfo;
import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

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
    @Path("qc-array-method-rdf")
    @Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_JSON_LD, OslcMediaType.TEXT_TURTLE,
        OslcMediaType.APPLICATION_XML})
    public TestResource[] queryRequirementsMethodArray(
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

    @OslcQueryCapability
        (
            title = "ProviderCollectionTestQC",
            label = "ProviderCollectionTestQC test endpoint (Response/RDF)",
//            resourceShape = OslcConstants.PATH_RESOURCE_SHAPES + "/" + Oslc_rmDomainConstants.REQUIREMENT_PATH,
            resourceTypes = {TestResource.TEST_RESOURCE_TYPE},
            usages = {}
        )
    @GET
    @Path("qc-array-response-rdf")
    @Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_JSON_LD, OslcMediaType.TEXT_TURTLE,
        OslcMediaType.APPLICATION_XML})
    public Response queryRequirementsResponseArray(
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

        return Response.ok(resources.toArray(new TestResource[resources.size()]))
            .build();
    }

    @OslcQueryCapability
        (
            title = "ProviderCollectionTestQC",
            label = "ProviderCollectionTestQC test endpoint (Collection/RDF)",
//            resourceShape = OslcConstants.PATH_RESOURCE_SHAPES + "/" + Oslc_rmDomainConstants.REQUIREMENT_PATH,
            resourceTypes = {TestResource.TEST_RESOURCE_TYPE},
            usages = {}
        )
    @GET
    @Path("qc-coll-method-rdf")
    @Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_JSON_LD, OslcMediaType.TEXT_TURTLE,
        OslcMediaType.APPLICATION_XML})
    public Collection<TestResource> queryRequirementsMethodList(
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
        return resources;
    }

    @OslcQueryCapability
        (
            title = "ProviderCollectionTestQC",
            label = "ProviderCollectionTestQC test endpoint (CollectionResponse/RDF)",
//            resourceShape = OslcConstants.PATH_RESOURCE_SHAPES + "/" + Oslc_rmDomainConstants.REQUIREMENT_PATH,
            resourceTypes = {TestResource.TEST_RESOURCE_TYPE},
            usages = {}
        )
    @GET
    @Path("qc-coll-response-rdf")
    @Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_JSON_LD, OslcMediaType.TEXT_TURTLE,
        OslcMediaType.APPLICATION_XML, OslcMediaType.APPLICATION_JSON})
    public Response queryRequirementsResponseList(
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

        return Response.ok(new GenericEntity<Collection<TestResource>>(resources){})
            .build();
    }

    @OslcQueryCapability
        (
            title = "ProviderCollectionTestQC",
            label = "ProviderCollectionTestQC test endpoint (CollectionResponse/RDF)",
//            resourceShape = OslcConstants.PATH_RESOURCE_SHAPES + "/" + Oslc_rmDomainConstants.REQUIREMENT_PATH,
            resourceTypes = {TestResource.TEST_RESOURCE_TYPE},
            usages = {}
        )
    @GET
    @Path("qc-nonq-coll-response-rdf")
    @Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_JSON_LD, OslcMediaType.TEXT_TURTLE,
        OslcMediaType.APPLICATION_XML})
    public Response queryRequirementsResponseListNonQ(
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

        List<TestResourceNonQ> resources = generateTestResourcesNonQ();
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

        return Response.ok(new GenericEntity<Collection<TestResourceNonQ>>(resources){})
            .build();
    }

    @GET
    @Path("nonqc-nonq-coll-response-rdf")
    @Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_JSON_LD, OslcMediaType.TEXT_TURTLE,
        OslcMediaType.APPLICATION_XML})
    public Response serveRequirementsResponseListNonQ(
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

        List<TestResourceNonQ> resources = generateTestResourcesNonQ();
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
//        httpServletRequest.setAttribute("queryUri", uriBuilder.build().toString());
        if ((OSLC4JUtils.hasLyoStorePagingPreciseLimit() && resources.size() >= pageSize)
            || (!OSLC4JUtils.hasLyoStorePagingPreciseLimit() && resources.size() > pageSize)) {
            resources = resources.subList(0, pageSize);
            uriBuilder.replaceQueryParam("page", page + 1);
            // TODO decide if add to test matrix
//            httpServletRequest.setAttribute(OSLC4JConstants.OSLC4J_NEXT_PAGE, uriBuilder.build().toString());
        }

        return Response.ok(new GenericEntity<Collection<TestResourceNonQ>>(resources){})
            .build();
    }

    /**
     * Non-query response
     */
    @GET
    @Path("nonqc-array-response-rdf")
    @Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_JSON_LD, OslcMediaType.TEXT_TURTLE,
        OslcMediaType.APPLICATION_XML})
    public Response serveRequirementsResponseArrayNonQ(
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
//        httpServletRequest.setAttribute("queryUri", uriBuilder.build().toString());
        if ((OSLC4JUtils.hasLyoStorePagingPreciseLimit() && resources.size() >= pageSize)
            || (!OSLC4JUtils.hasLyoStorePagingPreciseLimit() && resources.size() > pageSize)) {
            resources = resources.subList(0, pageSize);
            uriBuilder.replaceQueryParam("page", page + 1);
//            httpServletRequest.setAttribute(OSLC4JConstants.OSLC4J_NEXT_PAGE, uriBuilder.build().toString());
        }

        return Response.ok(resources.toArray(new TestResource[resources.size()]))
            .build();
    }


    @OslcQueryCapability
        (
            title = "ProviderCollectionTestQC",
            label = "ProviderCollectionTestQC test endpoint (ArrayResponse/RDF)",
//            resourceShape = OslcConstants.PATH_RESOURCE_SHAPES + "/" + Oslc_rmDomainConstants.REQUIREMENT_PATH,
            resourceTypes = {TestResource.TEST_RESOURCE_TYPE},
            usages = {}
        )
    @GET
    @Path("qc-nonq-array-response-rdf")
    @Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_JSON_LD, OslcMediaType.TEXT_TURTLE,
        OslcMediaType.APPLICATION_XML})
    public Response queryRequirementsResponseArrayNonQ(
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
//        httpServletRequest.setAttribute("queryUri", uriBuilder.build().toString());
        if ((OSLC4JUtils.hasLyoStorePagingPreciseLimit() && resources.size() >= pageSize)
            || (!OSLC4JUtils.hasLyoStorePagingPreciseLimit() && resources.size() > pageSize)) {
            resources = resources.subList(0, pageSize);
            uriBuilder.replaceQueryParam("page", page + 1);
//            httpServletRequest.setAttribute(OSLC4JConstants.OSLC4J_NEXT_PAGE, uriBuilder.build().toString());
        }

        return Response.ok(resources.toArray(new TestResource[resources.size()]))
            .build();
    }

    private List<TestResource> generateTestResources() {
        List<TestResource> resources = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            TestResource testResource = new TestResource(URI.create("urn:test:TestResource" + i));
            testResource.setAproperty("value_" + i);
            resources.add(testResource);
        }
        return resources;
    }

    private List<TestResourceNonQ> generateTestResourcesNonQ() {
        List<TestResourceNonQ> resources = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            TestResourceNonQ testResource = new TestResourceNonQ(URI.create("urn:test:TestResource" + i));
            testResource.setAproperty("value_" + i);
            resources.add(testResource);
        }
        return resources;
    }
}
