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

import static java.util.Objects.requireNonNull;

import jakarta.ws.rs.core.Response;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import net.oauth.OAuthException;
import org.eclipse.lyo.client.OSLCConstants;
import org.eclipse.lyo.client.OslcClient;
import org.eclipse.lyo.client.exception.ResourceNotFoundException;
import org.eclipse.lyo.oslc.domains.am.Oslc_amDomainConstants;
import org.eclipse.lyo.oslc.domains.cm.Oslc_cmDomainConstants;
import org.eclipse.lyo.oslc.domains.qm.Oslc_qmDomainConstants;
import org.eclipse.lyo.oslc.domains.rm.Oslc_rmDomainConstants;
import org.eclipse.lyo.oslc4j.core.model.CreationFactory;
import org.eclipse.lyo.oslc4j.core.model.ResourceShape;
import org.eclipse.lyo.oslc4j.core.model.Service;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;

/**
 * Discovers resource shapes advertised by OSLC creation factories.
 *
 * <p>The client can optionally be bound to an OSLC configuration context:
 *
 * <pre>{@code
 * var shapes = new ResourceShapeClient(oslcClient, configurationUri);
 *
 * var shape = shapes.lookupRequirementShape(
 *         serviceProviderUri,
 *         "System Requirement");
 * }</pre>
 */
public final class ResourceShapeClient {
  private final OslcClient client;
  private final URI configurationContext;

  public ResourceShapeClient(OslcClient client) {
    this(client, null);
  }

  public ResourceShapeClient(OslcClient client, URI configurationContext) {

    this.client = requireNonNull(client, "client");
    this.configurationContext = configurationContext;
  }

  /**
   * Looks up a resource shape advertised by a matching creation factory.
   *
   * <p>{@code shapeIdentifier} may be either:
   *
   * <ul>
   *   <li>the resource shape URI, or</li>
   *   <li>the resource shape title, matched case-insensitively.</li>
   * </ul>
   */
  public ResourceShape lookupShape(
      URI serviceProviderUri, URI domain, URI resourceType, String shapeIdentifier)
      throws IOException, URISyntaxException, ResourceNotFoundException, OAuthException {

    requireNonNull(serviceProviderUri, "serviceProviderUri");
    requireNonNull(domain, "domain");
    requireNonNull(resourceType, "resourceType");
    requireNonBlank(shapeIdentifier, "shapeIdentifier");

    var serviceProvider = readResource(serviceProviderUri, ServiceProvider.class);

    var services = serviceProvider.getServices();
    if (services != null) {
      for (var service : services) {
        var shape = lookupInService(service, domain, resourceType, shapeIdentifier);

        if (shape != null) {
          return shape;
        }
      }
    }

    throw new ResourceNotFoundException(
        serviceProviderUri.toString(), "ResourceShape[" + shapeIdentifier + "]");
  }

  public ResourceShape lookupRequirementShape(URI serviceProviderUri, String shapeIdentifier)
      throws IOException, URISyntaxException, ResourceNotFoundException, OAuthException {

    return lookupShape(
        serviceProviderUri,
        URI.create(Oslc_rmDomainConstants.REQUIREMENTS_MANAGEMENT_SHAPES_NAMSPACE),
        URI.create(Oslc_rmDomainConstants.REQUIREMENT_TYPE),
        shapeIdentifier);
  }

  public ResourceShape lookupRequirementCollectionShape(
      URI serviceProviderUri, String shapeIdentifier)
      throws IOException, URISyntaxException, ResourceNotFoundException, OAuthException {

    return lookupShape(
        serviceProviderUri,
        URI.create(Oslc_rmDomainConstants.REQUIREMENTS_MANAGEMENT_SHAPES_NAMSPACE),
        URI.create(Oslc_rmDomainConstants.REQUIREMENTCOLLECTION_TYPE),
        shapeIdentifier);
  }

  public ResourceShape lookupChangeRequestShape(URI serviceProviderUri, String shapeIdentifier)
      throws IOException, URISyntaxException, ResourceNotFoundException, OAuthException {

    return lookupShape(
        serviceProviderUri,
        URI.create(Oslc_cmDomainConstants.CHANGE_MANAGEMENT_SHAPES_NAMSPACE),
        URI.create(Oslc_cmDomainConstants.CHANGEREQUEST_TYPE),
        shapeIdentifier);
  }

  public ResourceShape lookupTestPlanShape(URI serviceProviderUri, String shapeIdentifier)
      throws IOException, URISyntaxException, ResourceNotFoundException, OAuthException {

    return lookupShape(
        serviceProviderUri,
        URI.create(Oslc_qmDomainConstants.QUALITY_MANAGEMENT_NAMSPACE),
        URI.create(Oslc_qmDomainConstants.TESTPLAN_TYPE),
        shapeIdentifier);
  }

  public ResourceShape lookupTestCaseShape(URI serviceProviderUri, String shapeIdentifier)
      throws IOException, URISyntaxException, ResourceNotFoundException, OAuthException {

    return lookupShape(
        serviceProviderUri,
        URI.create(Oslc_qmDomainConstants.QUALITY_MANAGEMENT_NAMSPACE),
        URI.create(Oslc_qmDomainConstants.TESTCASE_TYPE),
        shapeIdentifier);
  }

  public ResourceShape lookupTestScriptShape(URI serviceProviderUri, String shapeIdentifier)
      throws IOException, URISyntaxException, ResourceNotFoundException, OAuthException {

    return lookupShape(
        serviceProviderUri,
        URI.create(Oslc_qmDomainConstants.QUALITY_MANAGEMENT_NAMSPACE),
        URI.create(Oslc_qmDomainConstants.TESTSCRIPT_TYPE),
        shapeIdentifier);
  }

  public ResourceShape lookupTestExecutionRecordShape(
      URI serviceProviderUri, String shapeIdentifier)
      throws IOException, URISyntaxException, ResourceNotFoundException, OAuthException {

    return lookupShape(
        serviceProviderUri,
        URI.create(Oslc_qmDomainConstants.QUALITY_MANAGEMENT_NAMSPACE),
        URI.create(Oslc_qmDomainConstants.TESTEXECUTIONRECORD_TYPE),
        shapeIdentifier);
  }

  public ResourceShape lookupTestResultShape(URI serviceProviderUri, String shapeIdentifier)
      throws IOException, URISyntaxException, ResourceNotFoundException, OAuthException {

    return lookupShape(
        serviceProviderUri,
        URI.create(Oslc_qmDomainConstants.QUALITY_MANAGEMENT_NAMSPACE),
        URI.create(Oslc_qmDomainConstants.TESTRESULT_TYPE),
        shapeIdentifier);
  }

  public ResourceShape lookupArchitectureResourceShape(
      URI serviceProviderUri, String shapeIdentifier)
      throws IOException, URISyntaxException, ResourceNotFoundException, OAuthException {

    return lookupShape(
        serviceProviderUri,
        URI.create(Oslc_amDomainConstants.ARCHITECTURE_MANAGEMENT_NAMSPACE),
        URI.create(Oslc_amDomainConstants.RESOURCE_TYPE),
        shapeIdentifier);
  }

  private ResourceShape lookupInService(
      Service service, URI expectedDomain, URI expectedResourceType, String shapeIdentifier)
      throws IOException, URISyntaxException, ResourceNotFoundException, OAuthException {

    if (service == null || !expectedDomain.equals(service.getDomain())) {
      return null;
    }

    var creationFactories = service.getCreationFactories();
    if (creationFactories == null) {
      return null;
    }

    for (var creationFactory : creationFactories) {
      if (creationFactory == null || !supportsResourceType(creationFactory, expectedResourceType)) {
        continue;
      }

      var shape = lookupInCreationFactory(creationFactory, shapeIdentifier);

      if (shape != null) {
        return shape;
      }
    }

    return null;
  }

  private ResourceShape lookupInCreationFactory(
      CreationFactory creationFactory, String shapeIdentifier)
      throws IOException, URISyntaxException, ResourceNotFoundException, OAuthException {

    var shapeUris = creationFactory.getResourceShapes();
    if (shapeUris == null) {
      return null;
    }

    for (var shapeUri : shapeUris) {
      if (shapeUri == null) {
        continue;
      }

      var resourceShape = readResource(shapeUri, ResourceShape.class);

      if (matches(resourceShape, shapeUri, shapeIdentifier)) {
        return resourceShape;
      }
    }

    return null;
  }

  private static boolean supportsResourceType(
      CreationFactory creationFactory, URI expectedResourceType) {

    var resourceTypes = creationFactory.getResourceTypes();
    if (resourceTypes == null) {
      return false;
    }

    for (var resourceType : resourceTypes) {
      if (expectedResourceType.equals(resourceType)) {
        return true;
      }
    }

    return false;
  }

  private static boolean matches(
      ResourceShape resourceShape, URI advertisedShapeUri, String shapeIdentifier) {

    /*
     * The advertised shape URI normally identifies the fetched shape,
     * so check it before examining its representation.
     */
    if (shapeIdentifier.equals(advertisedShapeUri.toString())) {
      return true;
    }

    var about = resourceShape.getAbout();
    if (about != null && shapeIdentifier.equals(about.toString())) {
      return true;
    }

    var title = resourceShape.getTitle();
    return title != null && shapeIdentifier.equalsIgnoreCase(title);
  }

  private <T> T readResource(URI resourceUri, Class<T> entityType)
      throws IOException, URISyntaxException, ResourceNotFoundException, OAuthException {

    try (Response response =
        client.getResource(
            resourceUri.toString(), null, OSLCConstants.CT_RDF, configurationContextValue())) {

      var entity = response.readEntity(entityType);

      if (entity == null) {
        throw new ResourceNotFoundException(resourceUri.toString(), entityType.getSimpleName());
      }

      return entity;
    }
  }

  private String configurationContextValue() {
    return configurationContext == null ? null : configurationContext.toString();
  }

  private static String requireNonBlank(String value, String name) {

    requireNonNull(value, name);

    if (value.isBlank()) {
      throw new IllegalArgumentException(name + " must not be blank");
    }

    return value;
  }
}
