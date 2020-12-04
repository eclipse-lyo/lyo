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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Simple
 */
package org.eclipse.lyo.oslc4j.core.servlet;

import java.net.URI;
import java.net.URISyntaxException;

import java.util.HashMap;
import org.eclipse.lyo.oslc4j.client.ServiceProviderRegistryURIs;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.PrefixDefinition;
import org.eclipse.lyo.oslc4j.core.model.Publisher;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.core.resources.ServiceProviderCatalogResource;
import org.eclipse.lyo.oslc4j.core.resources.ServiceProviderResource;

class ServiceProviderFactory {
    private static final Class<?>[] RESOURCE_CLASSES = new Class<?>[]{
            ServiceProviderCatalogResource.class, ServiceProviderResource.class};

    private ServiceProviderFactory() {
        super();
    }

    public static ServiceProvider createServiceProvider(final String baseURI)
            throws OslcCoreApplicationException, URISyntaxException {
        final ServiceProvider serviceProvider = org.eclipse.lyo.oslc4j.core.model.ServiceProviderFactory
                .createServiceProvider(baseURI, ServiceProviderRegistryURIs.getUIURI(),
                                       "OSLC Service Provider Catalog",
                                       "Reference Implementation OSLC Service Provider Catalog",
                                       new Publisher("Project Lyo", "Eclipse"), RESOURCE_CLASSES, new HashMap<>());

        final PrefixDefinition[] prefixDefinitions = {
                new PrefixDefinition(OslcConstants.DCTERMS_NAMESPACE_PREFIX,
                                     new URI(OslcConstants.DCTERMS_NAMESPACE)
                ), new PrefixDefinition(OslcConstants.OSLC_CORE_NAMESPACE_PREFIX,
                                        new URI(OslcConstants.OSLC_CORE_NAMESPACE)
        ), new PrefixDefinition(OslcConstants.RDF_NAMESPACE_PREFIX,
                                new URI(OslcConstants.RDF_NAMESPACE)
        ), new PrefixDefinition(OslcConstants.RDFS_NAMESPACE_PREFIX,
                                new URI(OslcConstants.RDFS_NAMESPACE)
        )};

        serviceProvider.setPrefixDefinitions(prefixDefinitions);

        return serviceProvider;
    }
}
