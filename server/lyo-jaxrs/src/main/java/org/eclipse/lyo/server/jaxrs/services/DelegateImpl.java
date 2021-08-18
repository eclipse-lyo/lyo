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
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriBuilder;
import javax.ws.rs.core.UriInfo;

import org.apache.commons.lang3.reflect.TypeUtils;
import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Compact;
import org.eclipse.lyo.oslc4j.core.model.Preview;
import org.eclipse.lyo.server.jaxrs.repository.RepositoryConnectionException;
import org.eclipse.lyo.server.jaxrs.repository.RepositoryOperationException;
import org.eclipse.lyo.server.jaxrs.repository.ResourceRepository;
import org.glassfish.hk2.api.ActiveDescriptor;
import org.glassfish.hk2.api.IterableProvider;
import org.glassfish.hk2.api.Self;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.jsonldjava.shaded.com.google.common.base.Strings;

//@Service
public class DelegateImpl<RT extends AbstractResource, IBT extends ResourceId<RT>>
        implements Delegate<RT, IBT> {

    private static final String smallPreviewHintHeight = "10em";
    private static final String smallPreviewHintWidth = "45em";
    private static final String largePreviewHintHeight = "20em";
    private static final String largePreviewHintWidth = "45em";

    private final Logger log = LoggerFactory.getLogger(DelegateImpl.class);
    private final String iconUri = OSLC4JUtils.getPublicURI() + "/images/ui_preview_icon.gif";

    ResourceRepository<RT> repository;
    
    // Try to avoid using Servlet-specific classes and use JAX-RS classes instead!
//    @Context private HttpServletRequest httpServletRequest;
    @Context HttpHeaders headers;
    @Context UriInfo uriInfo;
    
    @Inject
    // see https://github.com/eclipse-ee4j/glassfish-hk2/issues/18
    public DelegateImpl(IterableProvider<ResourceRepository<?>> repoProviderIter, @Self ActiveDescriptor<?> myOwnDescriptor) {
        Type[] contractTypes = myOwnDescriptor.getContractTypes().toArray(new Type[0]);
        if(contractTypes.length > 0 && contractTypes[0] instanceof ParameterizedType) {
            // OslcDelegateImpl<X,Y>
            ParameterizedType type0 = (ParameterizedType) contractTypes[0];
            // X
            Type rtType = type0.getActualTypeArguments()[0];
            // ResourceRepository<X>
            ParameterizedType repoRtType = TypeUtils.parameterize(ResourceRepository.class, rtType);
            
            @SuppressWarnings("unchecked")
            ResourceRepository<RT> _repository = (ResourceRepository<RT>)repoProviderIter.ofType(repoRtType).get();
            this.repository = _repository;
        } else {
            throw new IllegalStateException();
        }
    }
    
    public DelegateImpl(ResourceRepository<RT> repository) { 
        this.repository = repository;
    }

    //TODO remove clazz
    @Override
    public Response.ResponseBuilder getResource(Class<RT> clazz, IBT id) {
        try {
            List<String> preferHeaders = headers.getRequestHeader("Prefer");
            if(preferHeaders != null && preferHeaders.size() == 1) {
                String prefer = preferHeaders.get(0);
                log.debug("Received a Prefer header, most likely the client wants a compact representation: {}", prefer);
            }
            Optional<RT> resource = repository.getResource(id.toUri());
            log.trace("Received a GET request for {}", id);
            
            if (resource.isPresent()) {
                return Response.ok(resource.get())
                        .header("ETag", repository.getETag(resource.get()))
                        .header("OSLC-Core-Version", "2.0");
            } else {
                return Response.status(Response.Status.NOT_FOUND).header("OSLC-Core-Version", "2.0");
            }

        } catch (Exception e) {
            return Response.serverError();
        }
    }

    //TODO remove clazz
    @Override
    public ResponseBuilder getResourceCompact(Class<RT> clazz, IBT id) {
        try {
            Optional<RT> resourceO = repository.getResource(id.toUri());
            if (resourceO.isPresent()) {
                final RT resource = resourceO.get();
                final Compact compact = buildCompact(resource);

                return Response.ok(compact)
                        .header("ETag", repository.getETag(resourceO.get()))
                        .header("OSLC-Core-Version", "2.0");
            } else {
                return Response.status(Response.Status.NOT_FOUND).header("OSLC-Core-Version", "2.0");
            }

        } catch (Exception e) {
            return Response.serverError();
        }
    }

    @Override
    public ResponseBuilder deleteResource(IBT id) {

        try {
            boolean deleted = repository.deleteResource(id.toUri());
            if (deleted) {
                return Response.status(Status.NO_CONTENT).header("OSLC-Core-Version", "2.0");
            } else {
                return Response.status(Response.Status.NOT_FOUND).header("OSLC-Core-Version", "2.0");
            }

        } catch (Exception e) {
            return Response.serverError();
        }
    }

    //TODO remove klass
    @Override
    public ResponseBuilder putResource(Class<RT> klass, IBT id, RT updatedResource,
            String requestETag) {
        if (Strings.isNullOrEmpty(requestETag)) {
            log.warn("Attempting resource update without checking the ETag");
        }
        try {
            Optional<RT> originalResourceO = repository.getResource(id.toUri());
            if (originalResourceO.isPresent()) {
                final RT originalResource = originalResourceO.get();
                final String resourceETag = repository.getETag(originalResource);
                if ((requestETag == null) || (resourceETag.equals(requestETag))) {
                    final RT resultResource = repository.update(id.toUri(), updatedResource, klass);
                    return Response.ok()
                            .header("ETag", repository.getETag(resultResource))
                            .header("OSLC-Core-Version", "2.0");
                } else {
                    // TODO add an ETag in the response if possible
                    return Response.status(Status.PRECONDITION_FAILED)
                            .header("OSLC-Core-Version", "2.0");
                }
            } else {
                return Response.status(Status.NOT_FOUND).header("OSLC-Core-Version", "2.0");
            }
        } catch (RepositoryConnectionException e) {
            return Response.status(Status.SERVICE_UNAVAILABLE).header("OSLC-Core-Version", "2.0");
        } catch (RepositoryOperationException e) {
            return Response.status(Status.INTERNAL_SERVER_ERROR).header("OSLC-Core-Version", "2.0");
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
            return Response.status(Status.NOT_FOUND);
        }
               
        return Response.status(Status.OK)
                .entity(resources);
//                .entity(resources.toArray(new ChangeRequest [resources.size()]));
        }

    @Override
    public List<RT> find(String terms) {
        return repository.queryResources(null, null, 0, 20);
    }

    @Override
    public Optional<RT> fetchResource(IBT id) throws RepositoryConnectionException {
        return repository.getResource(id.toUri());
    }
}
