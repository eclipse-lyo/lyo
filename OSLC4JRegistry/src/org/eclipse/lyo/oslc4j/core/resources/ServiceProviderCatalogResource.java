/*******************************************************************************
 * Copyright (c) 2012, 2013 IBM Corporation.
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
 *     Russell Boykin       - initial API and implementation
 *     Alberto Giammaria    - initial API and implementation
 *     Chris Peters         - initial API and implementation
 *     Gianluca Bernardini  - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.resources;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDialog;
import org.eclipse.lyo.oslc4j.core.annotation.OslcQueryCapability;
import org.eclipse.lyo.oslc4j.core.annotation.OslcService;
import org.eclipse.lyo.oslc4j.core.model.Compact;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.core.model.ServiceProviderCatalog;
import org.eclipse.lyo.oslc4j.core.servlet.ServiceProviderCatalogSingleton;

@OslcService(OslcConstants.OSLC_CORE_DOMAIN)
@Path("catalog")
public class ServiceProviderCatalogResource
{
    @OslcDialog
    (
         title = "Service Provider Catalog Selection Dialog",
         label = "Service Provider Catalog Selection Dialog",
         uri = "",
         hintWidth = "1000px",
         hintHeight = "600px",
         resourceTypes = {OslcConstants.TYPE_SERVICE_PROVIDER_CATALOG},
         usages = {OslcConstants.OSLC_USAGE_DEFAULT}
    )
    @OslcQueryCapability
    (
        title = "Service Provider Catalog Query Capability",
        label = "Service Provider Catalog Query",
        resourceShape = OslcConstants.PATH_RESOURCE_SHAPES + "/" + OslcConstants.PATH_SERVICE_PROVIDER_CATALOG,
        resourceTypes = {OslcConstants.TYPE_SERVICE_PROVIDER_CATALOG},
        usages = {OslcConstants.OSLC_USAGE_DEFAULT}
    )
    @GET
    @Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_XML, OslcMediaType.TEXT_XML, OslcMediaType.APPLICATION_JSON, OslcMediaType.TEXT_TURTLE})
    public ServiceProviderCatalog[] getServiceProviderCatalogs()
    {
        return new ServiceProviderCatalog[] {ServiceProviderCatalogSingleton.getServiceProviderCatalog()};
    }

    @GET
    @Path("{serviceProviderCatalogId}") // Required to distinguish from array result.  But, ignored.
    @Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_XML, OslcMediaType.TEXT_XML, OslcMediaType.APPLICATION_JSON, OslcMediaType.TEXT_TURTLE})
    public ServiceProviderCatalog getServiceProviderCatalog()
    {
        return ServiceProviderCatalogSingleton.getServiceProviderCatalog();
    }

    @GET
    @Path("{serviceProviderCatalogId}") // Required to distinguish from array result.  But, ignored.
    @Produces({OslcMediaType.APPLICATION_X_OSLC_COMPACT_XML, OslcMediaType.APPLICATION_X_OSLC_COMPACT_JSON})
    public Compact getCompact()
    {
        final ServiceProviderCatalog serviceProviderCatalog = ServiceProviderCatalogSingleton.getServiceProviderCatalog();

        final Compact compact = new Compact();

        compact.setAbout(serviceProviderCatalog.getAbout());
        compact.setShortTitle(serviceProviderCatalog.getTitle());
        compact.setTitle(serviceProviderCatalog.getTitle());

        // TODO - Need icon for ServiceProviderCatalog compact.

        return compact;
    }
}