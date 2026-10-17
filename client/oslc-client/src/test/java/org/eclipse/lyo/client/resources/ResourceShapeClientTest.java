/*
 * Copyright (c) 2026 Contributors to the Eclipse Foundation
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

package org.eclipse.lyo.client.resources;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.eclipse.lyo.oslc4j.core.model.OslcConstants.OSLC_CORE_NAMESPACE;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.ArgumentMatchers.nullable;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import jakarta.ws.rs.core.Response;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import org.eclipse.lyo.client.OSLCConstants;
import org.eclipse.lyo.client.OslcClient;
import org.eclipse.lyo.client.exception.ResourceNotFoundException;
import org.eclipse.lyo.oslc.domains.cm.Oslc_cmDomainConstants;
import org.eclipse.lyo.oslc.domains.rm.Oslc_rmDomainConstants;
import org.eclipse.lyo.oslc4j.core.model.CreationFactory;
import org.eclipse.lyo.oslc4j.core.model.ResourceShape;
import org.eclipse.lyo.oslc4j.core.model.Service;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.junit.jupiter.api.Test;

class ResourceShapeClientTest {

  private static final URI SERVICE_PROVIDER_URI =
      URI.create("https://example.test/providers/requirements");
  private static final URI SHAPE_URI = URI.create("https://example.test/shapes/system-requirement");
  private static final URI OTHER_SHAPE_URI = URI.create("https://example.test/shapes/other");

  @Test
  void lookupShapeMatchesShapeTitleAndFiltersServicesAndFactories() throws Exception {
    var client = mock(OslcClient.class);
    var responses = new HashMap<URI, Response>();

    var ignoredService =
        new Service(URI.create(Oslc_cmDomainConstants.CHANGE_MANAGEMENT_SHAPES_NAMSPACE));
    var ignoredFactory = new CreationFactory();
    ignoredFactory.setResourceTypes(
        new URI[] {URI.create(Oslc_rmDomainConstants.REQUIREMENT_TYPE)});
    ignoredFactory.setResourceShapes(new URI[] {SHAPE_URI});
    ignoredService.setCreationFactories(new CreationFactory[] {ignoredFactory});

    var matchingService =
        new Service(URI.create(Oslc_rmDomainConstants.REQUIREMENTS_MANAGEMENT_SHAPES_NAMSPACE));
    var wrongFactory = new CreationFactory();
    wrongFactory.setResourceTypes(new URI[] {URI.create(OSLCConstants.CM_CHANGE_REQUEST_TYPE)});
    wrongFactory.setResourceShapes(new URI[] {OTHER_SHAPE_URI});
    var matchingFactory = new CreationFactory();
    matchingFactory.setResourceTypes(
        new URI[] {URI.create(Oslc_rmDomainConstants.REQUIREMENT_TYPE)});
    matchingFactory.setResourceShapes(new URI[] {OTHER_SHAPE_URI, SHAPE_URI});
    matchingService.setCreationFactories(new CreationFactory[] {wrongFactory, matchingFactory});

    var serviceProvider = new ServiceProvider();
    serviceProvider.setServices(new Service[] {ignoredService, matchingService});
    responses.put(SERVICE_PROVIDER_URI, responseFor(ServiceProvider.class, serviceProvider));

    var otherShape = new ResourceShape(OTHER_SHAPE_URI);
    otherShape.setTitle("Other Shape");
    responses.put(OTHER_SHAPE_URI, responseFor(ResourceShape.class, otherShape));
    var matchingShape = new ResourceShape(SHAPE_URI);
    matchingShape.setTitle("System Requirement");
    responses.put(SHAPE_URI, responseFor(ResourceShape.class, matchingShape));
    stubResponses(client, responses);

    var result =
        new ResourceShapeClient(client)
            .lookupShape(
                SERVICE_PROVIDER_URI,
                URI.create(Oslc_rmDomainConstants.REQUIREMENTS_MANAGEMENT_SHAPES_NAMSPACE),
                URI.create(Oslc_rmDomainConstants.REQUIREMENT_TYPE),
                "system requirement");

    assertThat(result).isSameAs(matchingShape);
    verify(client).getResource(SERVICE_PROVIDER_URI.toString(), null, OSLCConstants.CT_RDF, null);
  }

  @Test
  void lookupShapeMatchesAdvertisedUriAndPassesConfigurationContext() throws Exception {
    var client = mock(OslcClient.class);
    var responses = new HashMap<URI, Response>();
    var factory = new CreationFactory();
    factory.setResourceTypes(new URI[] {URI.create(Oslc_rmDomainConstants.REQUIREMENT_TYPE)});
    factory.setResourceShapes(new URI[] {SHAPE_URI});
    var service =
        new Service(URI.create(Oslc_rmDomainConstants.REQUIREMENTS_MANAGEMENT_SHAPES_NAMSPACE));
    service.setCreationFactories(new CreationFactory[] {factory});
    var serviceProvider = new ServiceProvider();
    serviceProvider.setServices(new Service[] {service});
    responses.put(SERVICE_PROVIDER_URI, responseFor(ServiceProvider.class, serviceProvider));
    responses.put(SHAPE_URI, responseFor(ResourceShape.class, new ResourceShape()));
    stubResponses(client, responses);

    var result =
        new ResourceShapeClient(client, URI.create("https://example.test/configurations/main"))
            .lookupRequirementShape(SERVICE_PROVIDER_URI, SHAPE_URI.toString());

    assertThat(result).isNotNull();
    verify(client)
        .getResource(
            SERVICE_PROVIDER_URI.toString(),
            null,
            OSLCConstants.CT_RDF,
            "https://example.test/configurations/main");
    verify(client)
        .getResource(
            SHAPE_URI.toString(),
            null,
            OSLCConstants.CT_RDF,
            "https://example.test/configurations/main");
  }

  @Test
  void lookupShapeReportsMissingShape() throws Exception {
    var client = mock(OslcClient.class);
    var serviceProviderResponse = responseFor(ServiceProvider.class, new ServiceProvider());
    stubResponses(client, Map.of(SERVICE_PROVIDER_URI, serviceProviderResponse));

    assertThatThrownBy(
            () ->
                new ResourceShapeClient(client)
                    .lookupShape(
                        SERVICE_PROVIDER_URI,
                        URI.create(Oslc_rmDomainConstants.REQUIREMENTS_MANAGEMENT_SHAPES_NAMSPACE),
                        URI.create(Oslc_rmDomainConstants.REQUIREMENT_TYPE),
                        "missing"))
        .isInstanceOf(ResourceNotFoundException.class)
        .hasFieldOrPropertyWithValue("resource", SERVICE_PROVIDER_URI.toString())
        .hasFieldOrPropertyWithValue("value", "ResourceShape[missing]");
  }

  @Test
  void constructorAndLookupRejectInvalidArguments() {
    assertThatThrownBy(() -> new ResourceShapeClient(null))
        .isInstanceOf(NullPointerException.class);

    var client = new ResourceShapeClient(mock(OslcClient.class));
    var domain = URI.create(OSLC_CORE_NAMESPACE);
    var type = URI.create(OSLC_CORE_NAMESPACE + "Resource");

    assertThatThrownBy(() -> client.lookupShape(null, domain, type, "shape"))
        .isInstanceOf(NullPointerException.class);
    assertThatThrownBy(() -> client.lookupShape(SERVICE_PROVIDER_URI, null, type, "shape"))
        .isInstanceOf(NullPointerException.class);
    assertThatThrownBy(() -> client.lookupShape(SERVICE_PROVIDER_URI, domain, null, "shape"))
        .isInstanceOf(NullPointerException.class);
    assertThatThrownBy(() -> client.lookupShape(SERVICE_PROVIDER_URI, domain, type, " "))
        .isInstanceOf(IllegalArgumentException.class);
  }

  private static <T> Response responseFor(Class<T> entityType, T entity) {
    var response = mock(Response.class);
    when(response.readEntity(entityType)).thenReturn(entity);
    return response;
  }

  private static void stubResponses(OslcClient client, Map<URI, Response> responses) {
    when(client.getResource(
            anyString(), isNull(), eq(OSLCConstants.CT_RDF), nullable(String.class)))
        .thenAnswer(
            invocation -> responses.get(URI.create(invocation.getArgument(0, String.class))));
  }
}
