/*
 * Copyright (c) 2021 Contributors to the Eclipse Foundation
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
package org.eclipse.lyo.server.jaxrs.services;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Optional;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriBuilder;
import javax.ws.rs.core.UriInfo;

import org.apache.commons.lang3.reflect.TypeUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Compact;
import org.eclipse.lyo.oslc4j.core.model.Error;
import org.eclipse.lyo.oslc4j.core.model.Preview;
import org.eclipse.lyo.server.jaxrs.repository.RepositoryConnectionException;
import org.eclipse.lyo.server.jaxrs.repository.RepositoryOperationException;
import org.eclipse.lyo.server.jaxrs.repository.ResourceRepository;
import org.glassfish.hk2.api.ActiveDescriptor;
import org.glassfish.hk2.api.IterableProvider;
import org.glassfish.hk2.api.Self;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.google.common.base.Strings;

//@Service
public class DelegateImpl<RT extends AbstractResource, IDT extends ResourceId<RT>>
        implements Delegate<RT, IDT> {
    private final static Logger LOG = LoggerFactory.getLogger(DelegateImpl.class);

    //TODO: use Lyo core constants if possible
    private static final String OSLC_HEADER = "OSLC-Core-Version";
    private static final String OSLC_2_0 = "2.0";

    private static final String smallPreviewHintHeight = "10em";
    private static final String smallPreviewHintWidth = "45em";
    private static final String largePreviewHintHeight = "20em";
    private static final String largePreviewHintWidth = "45em";

    private final Logger log = LoggerFactory.getLogger(DelegateImpl.class);
    private final String iconUri = OSLC4JUtils.getPublicURI() + "/images/ui_preview_icon.gif";

    ResourceRepository<RT, IDT> repository;

    // Try to avoid using Servlet-specific classes and use JAX-RS classes instead!
//    @Context private HttpServletRequest httpServletRequest;
    @Context HttpHeaders headers;
    @Context UriInfo uriInfo;

    @Inject
    // see https://github.com/eclipse-ee4j/glassfish-hk2/issues/18
    public DelegateImpl(IterableProvider<ResourceRepository<?, ?>> repoProviderIter, @Self ActiveDescriptor<?> myOwnDescriptor) {
        Type[] contractTypes = myOwnDescriptor.getContractTypes().toArray(new Type[0]);
        if(contractTypes.length > 0 && contractTypes[0] instanceof ParameterizedType) {
            // OslcDelegateImpl<X,Y>
            ParameterizedType type0 = (ParameterizedType) contractTypes[0];
            ParameterizedType type1 = (ParameterizedType) contractTypes[0];
            // X
            Type rtType = type0.getActualTypeArguments()[0];
            Type idType = type1.getActualTypeArguments()[1];
            // ResourceRepository<X>
            ParameterizedType repoRtType = TypeUtils.parameterize(ResourceRepository.class, rtType, idType);

            @SuppressWarnings("unchecked")
            ResourceRepository<RT,IDT> _repository = (ResourceRepository<RT,IDT>)repoProviderIter.ofType(repoRtType).get();
            if(_repository == null) {
                throw new NullPointerException();
            }
            this.repository = _repository;
        } else {
            throw new IllegalStateException();
        }
    }

    public DelegateImpl(ResourceRepository<RT, IDT> repository) {
        this.repository = repository;
    }

    //TODO remove clazz
    @Override
    public ImmutablePair<ResponseBuilder, Optional<RT>> getResource(IDT id, Class<RT> clazz) {
        try {
            List<String> preferHeaders = headers.getRequestHeader("Prefer");
            if(preferHeaders != null && preferHeaders.size() == 1) {
                String prefer = preferHeaders.get(0);
                log.debug("Received a Prefer header, most likely the client wants a compact representation: {}", prefer);
            }
            Optional<RT> resource = repository.getResource(id);
            log.trace("Received a GET request for {}", id);

            if (resource.isPresent()) {
                ResponseBuilder response = Response.ok(resource.get())
                    .header("ETag", repository.getETag(resource.get()))
                    .header(OSLC_HEADER, OSLC_2_0);
                return ImmutablePair.of(response, resource);
            } else {
                ResponseBuilder response = Response.status(Status.NOT_FOUND).header(OSLC_HEADER, OSLC_2_0);
                return ImmutablePair.of(response, Optional.empty());
            }

        } catch (Exception e) {
            return ImmutablePair.of(Response.serverError(), Optional.empty());
        }
    }

    @Override
    public ImmutablePair<ResponseBuilder, Optional<Compact>> getResourceCompact(IDT id, Class<RT> clazz) {
        try {
            Optional<RT> resourceO = repository.getResource(id);
            if (resourceO.isPresent()) {
                final RT resource = resourceO.get();
                final Compact compact = buildCompact(resource);

                ResponseBuilder response = Response.ok(compact)
                    .header("ETag", repository.getETag(resourceO.get()))
                    .header(OSLC_HEADER, OSLC_2_0);
                return ImmutablePair.of(response, Optional.of(compact));
            } else {
                ResponseBuilder response = Response.status(Status.NOT_FOUND).header(OSLC_HEADER, OSLC_2_0);
                return ImmutablePair.of(response, Optional.empty());
            }

        } catch (Exception e) {
            return ImmutablePair.of(Response.serverError(), Optional.empty());
        }
    }

    @Override
    public ResponseBuilder deleteResource(IDT id) {
        try {
            boolean deleted = repository.deleteResource(id);
            if (deleted) {
                return Response.status(Status.NO_CONTENT).header(OSLC_HEADER, OSLC_2_0);
            } else {
                return Response.status(Response.Status.NOT_FOUND).header(OSLC_HEADER, OSLC_2_0);
            }

        } catch (Exception e) {
            return Response.serverError();
        }
    }

    @Override
    public ImmutablePair<ResponseBuilder, Optional<RT>> putResource(RT updatedResource, IDT id, String requestETag, Class<RT> klass) {
        if (Strings.isNullOrEmpty(requestETag)) {
            log.warn("Attempting resource update without checking the ETag");
        }
        try {
            Optional<RT> originalResourceO = repository.getResource(id);
            if (originalResourceO.isPresent()) {
                final RT originalResource = originalResourceO.get();
                final String resourceETag = repository.getETag(originalResource);
                if ((requestETag == null) || (resourceETag.equals(requestETag))) {
                    final RT resultResource = repository.update(updatedResource, id, klass);
                    ResponseBuilder response = Response.ok()
                        .header("ETag", repository.getETag(resultResource))
                        .header(OSLC_HEADER, OSLC_2_0);
                    return ImmutablePair.of(response, Optional.of(resultResource));
                } else {
                    // TODO add an ETag in the response if possible
                    ResponseBuilder response = Response.status(Status.PRECONDITION_FAILED)
                        .header(OSLC_HEADER, OSLC_2_0);
                    return ImmutablePair.of(response, Optional.empty());
                }
            } else {
                ResponseBuilder response = Response.status(Status.NOT_FOUND).header(OSLC_HEADER, OSLC_2_0);
                return ImmutablePair.of(response, Optional.empty());
            }
        } catch (RepositoryConnectionException e) {
            ResponseBuilder response = Response.status(Status.SERVICE_UNAVAILABLE).header(OSLC_HEADER, OSLC_2_0);
            return ImmutablePair.of(response, Optional.empty());
        } catch (RepositoryOperationException e) {
            ResponseBuilder response = Response.status(Status.INTERNAL_SERVER_ERROR).header(OSLC_HEADER, OSLC_2_0);
            return ImmutablePair.of(response, Optional.empty());
        }
    }

    private Compact buildCompact(final RT resource) throws URISyntaxException {
        final Compact compact = new Compact();

        compact.setAbout(resource.getAbout());
        compact.setTitle(resource.toString());

        compact.setIcon(new URI(iconUri));

        // Create and set attributes for OSLC preview resource
        final Preview smallPreview = new Preview();
        smallPreview.setHintHeight(smallPreviewHintHeight);
        smallPreview.setHintWidth(smallPreviewHintWidth);
        smallPreview
                .setDocument(UriBuilder.fromUri(resource.getAbout()).path("smallPreview").build());
        compact.setSmallPreview(smallPreview);

        final Preview largePreview = new Preview();
        largePreview.setHintHeight(largePreviewHintHeight);
        largePreview.setHintWidth(largePreviewHintWidth);
        largePreview
                .setDocument(UriBuilder.fromUri(resource.getAbout()).path("largePreview").build());
        compact.setLargePreview(largePreview);
        return compact;
    }

    @Override
    public ResponseBuilder queryResources(HttpServletRequest httpServletRequest, UriInfo uriInfo,
            String where, String prefix, int page, int pageSize) {
        // TODO think about a design pattern here, eg Strategy or Template method instead of a user code block
        // Start of user code queryChangeRequests
        // Here additional logic can be implemented that complements main action taken in CMManager
        // End of user code


        // TODO replace with a limit and offset
        // TODO request explicitly n+1 resources
        // Parse strings before handing off to the Repository
        //TODO extend Jena provider to cover all collections and don't assume above that we return a list
        List<RT> resources = repository.queryResources(where, prefix, page, pageSize);
        if(resources == null) {
            throw new IllegalStateException();
        }

        return Response.status(Status.OK)
                .header(OSLC_HEADER, OSLC_2_0)
                .entity(resources);
//                .entity(resources.toArray(new ChangeRequest [resources.size()]));

    }

    @Override
    public List<RT> find(String terms) throws RepositoryOperationException {
        return repository.queryResources(null, null, 0, 20);
    }

    @Override
    public Optional<RT> fetchResource(IDT id) throws RepositoryOperationException {
        return repository.getResource(id);
    }

    @Override
    public ImmutablePair<ResponseBuilder, Optional<RT>> createResource(RT aResource, IDT id, Class<RT> klass) {
        try {
            RT createdResource = repository.createResource(aResource, id, klass);
            if(createdResource.getAbout() == null) {
                throw new IllegalStateException("Created resource must have a URI");
            }
            ResponseBuilder response = Response.status(Status.CREATED)
                    .header(OSLC_HEADER, OSLC_2_0)
                    .location(createdResource.getAbout());
                return ImmutablePair.of(response, Optional.of(createdResource));
        } catch (RepositoryOperationException e) {
            ResponseBuilder response = Response.status(Status.INTERNAL_SERVER_ERROR)
                .header(OSLC_HEADER, OSLC_2_0)
                .entity(errorEntity("Resource creation failed"));
            return ImmutablePair.of(response, Optional.empty());
        }
    }

    private org.eclipse.lyo.oslc4j.core.model.Error errorEntity(String message) {
        Error error = new org.eclipse.lyo.oslc4j.core.model.Error();
        error.setMessage(message);
        return error;
    }

    @Override
    public ImmutablePair<ResponseBuilder, Optional<RT>> createResourceJson(RT aResource, IDT id, Class<RT> klass) {
        try {
            RT createdResource = repository.createResource(aResource, id, klass);
            if(createdResource == null) {
                throw new IllegalStateException();
            }
            String createdResourceInfo = jsonInfo(createdResource);
            ResponseBuilder response = Response.status(Status.CREATED)
                    .header(OSLC_HEADER, OSLC_2_0)
                    .location(createdResource.getAbout())
                    .entity(createdResourceInfo)
                    .type(MediaType.APPLICATION_JSON);
                return ImmutablePair.of(response, Optional.of(createdResource));
        } catch (RepositoryOperationException | JsonProcessingException e) {
            ResponseBuilder response = Response.status(Status.INTERNAL_SERVER_ERROR)
                .header(OSLC_HEADER, OSLC_2_0)
                .entity(errorEntity("Resource creation failed"));
            return ImmutablePair.of(response, Optional.empty());
        }
    }

    private String jsonInfo(RT createdResource) throws JsonProcessingException {
        ObjectMapper mapper = new ObjectMapper();
        ObjectNode rootNode = JsonNodeFactory.instance.objectNode();
        ArrayNode resultsNode = JsonNodeFactory.instance.arrayNode();
        ObjectNode result0Node = JsonNodeFactory.instance.objectNode();
        result0Node.set("rdf:resource", JsonNodeFactory.instance.textNode(createdResource.getAbout().toString()));
        result0Node.set("oslc:label", JsonNodeFactory.instance.textNode(createdResource.toString()));
        resultsNode.add(result0Node);
        rootNode.set("oslc:results", resultsNode);
        return mapper.writeValueAsString(rootNode);
    }
}
