/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
 *
 * See the NOTICE file(s) distributed with this work for additional
 * information regarding copyright ownership.
 *
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0, or the Eclipse Distribution License 1.0
 * which is available at http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause
 */
package org.eclipse.lyo.client.query;

import jakarta.ws.rs.core.Response;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.ResIterator;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.vocabulary.RDFS;
import org.eclipse.lyo.client.OSLCConstants;
import org.eclipse.lyo.oslc4j.core.exception.LyoModelException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.xml.datatype.DatatypeConfigurationException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;


/**
 * The results of an OSLC query. If the query was paged, subsequent pages can be retrieved using
 * the Iterator interface.
 * <p>
 * This class is not currently thread safe.
 */
public class OslcQueryResult implements Iterator<OslcQueryResult> {
    private static final Logger log = LoggerFactory.getLogger(OslcQueryResult.class);
    /**
     * The default member property to look for in OSLC query results
     * (rdfs:member). Can be changed using {@link #setMemberProperty(String)}.
     */
    public final static Property DEFAULT_MEMBER_PROPERTY = RDFS.member;

    /**
     * If system property {@value} is set to true, find any member in the
     */
    public final static String SELECT_ANY_MEMBER = "org.eclipse.lyo.client.oslc.query" +
        ".selectAnyMember";

    /**
     * Treat any resource in the members resource as a query result (except rdf:type).
     *
     * @see OslcQueryResult#SELECT_ANY_MEMBER
     */
    // FIXME: totally broken
    private final class AnyMemberSelector {
        private AnyMemberSelector(Resource subject) {
//            super(subject, null, (RDFNode) null);
        }

        public boolean selects(Statement s) {
            String fqPredicateName =
                s.getPredicate().getNameSpace() + s.getPredicate().getLocalName();
            if (OSLCConstants.RDF_TYPE_PROP.equals(fqPredicateName)) {
                return false;
            }

            return s.getObject().isResource();
        }
    }

    private final OslcQuery query;

    private final Response response;

    private final int pageNumber;

    private Property memberProperty = DEFAULT_MEMBER_PROPERTY;

    private Model rdfModel;

    private Resource infoResource, membersResource;

    private String nextPageUrl = "";

    private boolean rdfInitialized = false;

    public OslcQueryResult(OslcQuery query, Response response) {
        this.query = query;
        this.response = response;

        this.pageNumber = 1;


    }

    private OslcQueryResult(OslcQueryResult prev) {
        this.query = new OslcQuery(prev);
        this.response = this.query.getResponse();
        this.membersResource = prev.membersResource;
        this.memberProperty = prev.memberProperty;

        this.pageNumber = prev.pageNumber + 1;

    }

    private synchronized void initializeRdf() {
        if (!rdfInitialized) {
            rdfInitialized = true;
            rdfModel = ModelFactory.createDefaultModel();
            rdfModel.read(response.readEntity(InputStream.class), query.getCapabilityUrl());

            //Find a resource with rdf:type of oslc:ResourceInfo
            Property rdfType = rdfModel.createProperty(OslcConstants.RDF_NAMESPACE, "type");
            Property responseInfo = rdfModel.createProperty(OslcConstants.OSLC_CORE_NAMESPACE,
                "ResponseInfo");
            ResIterator iter = rdfModel.listResourcesWithProperty(rdfType, responseInfo);

            //The main ResponseInfo shall have the query URI or a page URI
            List<Resource> responseInfos = iter.toList();

            infoResource = null;
            membersResource = rdfModel.getResource(query.getCapabilityUrl());

            if (responseInfos.isEmpty()) {
                return;
            }

            infoResource = tryFindOnlyResponseInfo(responseInfos);
            if (infoResource == null) {
                log.trace("Cannot find exactly one ResponseInfo");
            } else {
                log.debug("Found exactly one ResponseInfo");
                return;
            }

            infoResource = tryFindOnlyWithNextPage(responseInfos);
            if (infoResource == null) {
                log.trace("Cannot find exactly one ResponseInfo with nextPage");
            } else {
                log.debug("Found exactly one ResponseInfo with nextPage");
                return;
            }

            infoResource = tryFindExactResponseInfoUri(responseInfos);
            if (infoResource == null) {
                log.trace("Cannot a ResponseInfo whose URI matches the query URI exactly");
            } else {
                log.debug("Found a ResponseInfo whose URI matches the query URI exactly");
                return;
            }

            infoResource = tryFindPrefixedResponseInfoUri(responseInfos);
            if (infoResource == null) {
                log.trace("Cannot find exactly one ResponseInfo whose URI starts with the query URI");
            } else {
                log.debug("Found exactly one ResponseInfo whose URI starts with the query URI");
                return;
            }

            if (infoResource == null) {
                throw new IllegalStateException("Failed to find an appropriate ResponseInfo object");
            }

        }
    }

    /**
     * Extracts a ResourceInfo resource if one and only one has a property with the nextPage
     * predicate.
     *
     * @param responseInfos from OSLC Query results
     * @return a ResourceInfo resource if one satisfies the conditions; null if none satisfy
     */
    private Resource tryFindOnlyWithNextPage(List<Resource> responseInfos) {
        Property nextPagePredicate = rdfModel.getProperty(OslcConstants.OSLC_CORE_NAMESPACE,
            "nextPage");
        var responsesWithNextPage =
            responseInfos.stream().filter(ri -> ri.getProperty(nextPagePredicate) != null).toList();
        if (responsesWithNextPage.size() == 1) {
            return responsesWithNextPage.get(0);
        }
        else if (responsesWithNextPage.size() > 1) {
            log.warn("Multiple ResponseInfo objects found with nextPage predicate");
        }
        return null;
    }

    /**
     * Extracts a ResourceInfo resource if one and only one has the same prefix as the query URI.
     *
     * @param responseInfos from OSLC Query results
     * @return a ResourceInfo resource if one satisfies the conditions; null if none satisfy
     */
    private Resource tryFindPrefixedResponseInfoUri(List<Resource> responseInfos) {
        List<Resource> filteredObjects =
            responseInfos.stream().filter(ri -> ri.getURI().startsWith(query.getQueryUrl())).toList();
        if (filteredObjects.size() == 1) {
            return filteredObjects.get(0);
        } else if (filteredObjects.size() > 1) {
            log.warn("Multiple ResponseInfo objects found starting with the same Query URI");
        }
        return null;
    }

    /**
     * Extracts a ResourceInfo resource if one and only one has exactly the same URI as the query
     * URI.
     *
     * @param responseInfos from OSLC Query results
     * @return a ResourceInfo resource if one satisfies the conditions; null if none satisfy
     * @throws IllegalStateException if multiple resources satisfy the same condition
     */
    private Resource tryFindExactResponseInfoUri(List<Resource> responseInfos) {
        List<Resource> filteredObjects =
            responseInfos.stream().filter(ri -> ri.getURI().equals(query.getQueryUrl())).toList();
        if (filteredObjects.size() == 1) {
            return filteredObjects.get(0);
        } else if (filteredObjects.size() > 1) {
            throw new IllegalStateException("Multiple ResponseInfo objects found with the same URI");
        }
        return null;
    }

    /**
     * Extracts a ResourceInfo resource if one and only one exists in the results.
     *
     * @param responseInfos from OSLC Query results
     * @return a ResourceInfo resource if one satisfies the conditions; null if none satisfy
     */
    private Resource tryFindOnlyResponseInfo(List<Resource> responseInfos) {
        if (responseInfos.size() == 1) {
            return responseInfos.get(0);
        }
        return null;
    }

    String getNextPageUrl() {
        initializeRdf();
        if ((nextPageUrl == null || nextPageUrl.isEmpty()) && infoResource != null) {
            Property predicate = rdfModel.getProperty(OslcConstants.OSLC_CORE_NAMESPACE,
                "nextPage");
//            Selector select = new SimpleSelector(infoResource, predicate, (RDFNode) null);
//            StmtIterator iter = rdfModel.listStatements(select);
            StmtIterator iter = rdfModel.listStatements(infoResource, predicate, (RDFNode) null);
            if (iter.hasNext()) {
                Statement nextPage = iter.next();
                nextPageUrl = nextPage.getResource().getURI();
            } else {
                nextPageUrl = "";
            }
        }
        return nextPageUrl;
    }

    /**
     * @return whether there is another page of results after this
     */
    public boolean hasNext() {
        return (!"".equals(getNextPageUrl()));
    }

    /**
     * @return the next page of results
     */
    public OslcQueryResult next() {
        return new OslcQueryResult(this);
    }

    /**
     * @throws UnsupportedOperationException always
     */
    public void remove() {
        throw new UnsupportedOperationException();
    }

    public OslcQuery getQuery() {
        return query;
    }

    /**
     * Returns the member property to find query result resources.
     *
     * @return the member property URI
     * @see #setMemberProperty(String)
     */
    public String getMemberProperty() {
        return this.memberProperty.getURI();
    }

    /**
     * Sets the predicate to use to find query result resources. If unset,
     * defaults to {@code http://www.w3.org/2000/01/rdf-schema#member}.
     *
     * @param memberPredicate the RDF predicate for member resources from the provider's
     *                        query shape
     * @see
     * <a href="http://open-services.net/bin/view/Main/OSLCCoreSpecRDFXMLExamples?sortcol=table;up=#Specifying_the_shape_of_a_query">Specifying the sahpe of a query</a>
     */
    public void setMemberProperty(String memberPredicate) {
        this.memberProperty = ModelFactory.createDefaultModel().createProperty(memberPredicate);
    }

    /**
     * Get the raw Wink client response to a query.
     * <p>
     * NOTE:  Using this method and consuming the response will make other methods
     * which examine the response unavailable (Examples:  getMemberUrls(), next() and hasNext()).
     * When this method is invoked, the consumer is responsible for OSLC page processing
     *
     * @return
     */
    public Response getRawResponse() {
        return response;
    }

    private Selector getMemberSelector() {
        if ("true".equalsIgnoreCase(System.getProperty(SELECT_ANY_MEMBER))) {
            return new AnyMemberSelector(membersResource);
        }

        return new SimpleSelector(membersResource, memberProperty, (RDFNode) null);
    }

    /**
     * Return the subject URLs of the query response.  The URLs are the location of all artifacts
     * which satisfy the query conditions.
     * <p>
     * NOTE:  Using this method consumes the query response and makes other methods
     * which examine the response unavailable (Example: getRawResponse().
     *
     * @return
     */
    public String[] getMembersUrls() {
        initializeRdf();
        ArrayList<String> membersUrls = new ArrayList<>();
        Selector select = getMemberSelector();
        StmtIterator iter = rdfModel.listStatements(select);
        while (iter.hasNext()) {
            Statement member = iter.next();
            membersUrls.add(member.getResource().getURI());
        }
        return membersUrls.toArray(new String[membersUrls.size()]);
    }

    /**
     * Return the enumeration of queried results from this page
     *
     * @return member statements from current page.
     */
    public <T> Iterable<T> getMembers(final Class<T> clazz) {
        initializeRdf();

        Selector select = getMemberSelector();
        final StmtIterator iter = rdfModel.listStatements(select);
        Iterable<T> result = () -> new Iterator<>() {
            public boolean hasNext() {
                return iter.hasNext();
            }

            @SuppressWarnings("unchecked")
            public T next() {
                Statement member = iter.next();

                try {
                    return (T) JenaModelHelper.fromJenaResource((Resource) member.getObject(),
                        clazz);
                } catch (DatatypeConfigurationException | IllegalAccessException |
                         InvocationTargetException | InstantiationException |
                         OslcCoreApplicationException | NoSuchMethodException |
                         URISyntaxException e) {
                    throw new LyoModelException(e);
                }
            }

            public void remove() {
                iter.remove();
            }
        };

        return result;
    }

    /**
     * Return the enumeration of queried results from this page
     *
     * @return member statements from current page.
     */
    public Iterable<Resource> getMembers() {
        initializeRdf();

        Selector select = getMemberSelector();
        final StmtIterator iter = rdfModel.listStatements(select);
        Iterable<Resource> result = () -> new Iterator<>() {
            public boolean hasNext() {
                return iter.hasNext();
            }

            @SuppressWarnings("unchecked")
            public Resource next() {
                Statement member = iter.next();

                try {
                    return (Resource) member.getObject();
                } catch (IllegalArgumentException | SecurityException e) {
                    throw new IllegalStateException(e);
                }
            }

            public void remove() {
                iter.remove();
            }
        };

        return result;
    }

}
