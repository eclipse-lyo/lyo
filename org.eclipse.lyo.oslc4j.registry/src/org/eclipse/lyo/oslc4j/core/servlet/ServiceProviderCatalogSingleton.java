/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
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
package org.eclipse.lyo.oslc4j.core.servlet;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Date;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response.Status;

import org.eclipse.lyo.oslc4j.client.ServiceProviderRegistryURIs;
import org.eclipse.lyo.oslc4j.core.model.Publisher;
import org.eclipse.lyo.oslc4j.core.model.Service;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.core.model.ServiceProviderCatalog;

public class ServiceProviderCatalogSingleton
{
    private static final ServiceProviderCatalog             serviceProviderCatalog;
    private static final SortedMap<String, ServiceProvider> serviceProviders = new TreeMap<String, ServiceProvider>();

    static
    {
        try
        {
            serviceProviderCatalog = new ServiceProviderCatalog();

            serviceProviderCatalog.setAbout(new URI(ServiceProviderRegistryURIs.getServiceProviderRegistryURI()));
            serviceProviderCatalog.setTitle("OSLC Service Provider Catalog");
            serviceProviderCatalog.setDescription("Reference Implementation OSLC IBM Service Provider Catalog");
            serviceProviderCatalog.setPublisher(new Publisher("Project Lyo", "Eclipse"));
        }
        catch (final URISyntaxException exception)
        {
            // We should never get here.
            throw new ExceptionInInitializerError(exception);
        }
    }

    private static int serviceProviderId = 1;

    private ServiceProviderCatalogSingleton()
    {
        super();
    }

    public static ServiceProviderCatalog getServiceProviderCatalog()
    {
        return serviceProviderCatalog;
    }

    public static ServiceProvider[] getServiceProviders()
    {
        synchronized(serviceProviders)
        {
            return serviceProviders.values().toArray(new ServiceProvider[serviceProviders.size()]);
        }
    }

    public static ServiceProvider getServiceProvider(final String serviceProviderId)
    {
        final ServiceProvider serviceProvider;

        synchronized(serviceProviders)
        {
            serviceProvider = serviceProviders.get(serviceProviderId);
        }

        if (serviceProvider != null)
        {
            return serviceProvider;
        }

        throw new WebApplicationException(Status.NOT_FOUND);
    }

    public static ServiceProvider registerServiceProvider(final HttpServletRequest httpServletRequest,
                                                          final ServiceProvider    serviceProvider)
           throws URISyntaxException
    {
        synchronized(serviceProviders)
        {
            final URI serviceProviderURI = new URI(httpServletRequest.getScheme(),
                                                   null,
                                                   httpServletRequest.getServerName(),
                                                   httpServletRequest.getServerPort(),
                                                   httpServletRequest.getContextPath() + "/serviceProviders/" + serviceProviderId,
                                                   null,
                                                   null);

            return registerServiceProviderNoSync(serviceProviderURI,
                                                 serviceProvider);
        }
    }

    // This version is for self-registration and thus package-protected
    static ServiceProvider registerServiceProvider(final String          baseURI,
                                                   final ServiceProvider serviceProvider)
           throws URISyntaxException
    {
        synchronized(serviceProviders)
        {
            final URI serviceProviderURI = new URI(baseURI + "/serviceProviders/" + serviceProviderId);

            return registerServiceProviderNoSync(serviceProviderURI,
                                                 serviceProvider);
        }
    }

    private static ServiceProvider registerServiceProviderNoSync(final URI             serviceProviderURI,
                                                                 final ServiceProvider serviceProvider)
    {
        final SortedSet<URI> serviceProviderDomains = getServiceProviderDomains(serviceProvider);

        serviceProvider.setAbout(serviceProviderURI);
        serviceProvider.setIdentifier(String.valueOf(serviceProviderId));
        serviceProvider.setCreated(new Date());

        serviceProviderCatalog.addServiceProvider(serviceProvider);
        serviceProviderCatalog.addDomains(serviceProviderDomains);

        serviceProviders.put(String.valueOf(serviceProviderId),
                             serviceProvider);

        serviceProviderId++;

        return serviceProvider;
    }

    public static void deregisterServiceProvider(final String serviceProviderId)
    {
        synchronized(serviceProviders)
        {
            final ServiceProvider deregisteredServiceProvider = serviceProviders.remove(serviceProviderId);

            if (deregisteredServiceProvider != null)
            {
                final SortedSet<URI> remainingDomains = new TreeSet<URI>();

                for (final ServiceProvider remainingServiceProvider : serviceProviders.values())
                {
                    remainingDomains.addAll(getServiceProviderDomains(remainingServiceProvider));
                }

                final SortedSet<URI> removedServiceProviderDomains = getServiceProviderDomains(deregisteredServiceProvider);

                removedServiceProviderDomains.removeAll(remainingDomains);

                serviceProviderCatalog.removeDomains(removedServiceProviderDomains);

                serviceProviderCatalog.removeServiceProvider(deregisteredServiceProvider);
            }
            else
            {
                throw new WebApplicationException(Status.NOT_FOUND);
            }
        }
    }

    private static SortedSet<URI> getServiceProviderDomains(final ServiceProvider serviceProvider)
    {
        final SortedSet<URI> domains = new TreeSet<URI>();

        if (serviceProvider!=null) {
    		final Service[] services = serviceProvider.getServices();
	        for (final Service service : services)
	        {
	            final URI domain = service.getDomain();

	            domains.add(domain);
	        }
        }
        return domains;
    }
}
