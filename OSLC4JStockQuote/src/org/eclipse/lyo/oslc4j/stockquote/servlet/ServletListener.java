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
package org.eclipse.lyo.oslc4j.stockquote.servlet;

import java.net.InetAddress;
import java.net.URI;
import java.net.UnknownHostException;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

import org.eclipse.lyo.oslc4j.client.ServiceProviderRegistryClient;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.provider.jena.JenaProvidersRegistry;
import org.eclipse.lyo.oslc4j.stockquote.Persistence;

public class ServletListener
       implements ServletContextListener
{
    private static final String PROPERTY_SCHEME = ServletListener.class.getPackage().getName() + ".scheme";
    private static final String PROPERTY_PORT   = ServletListener.class.getPackage().getName() + ".port";

    private static final Logger logger = Logger.getLogger(ServletListener.class.getName());

    private static final String HOST = getHost();

    private ServiceProviderRegistryClient client;

    public ServletListener()
    {
        super();
    }

    @Override
    public void contextDestroyed(final ServletContextEvent servletContextEvent)
    {
        final String basePath = generateBasePath(servletContextEvent);

        if (client != null)
        {
            try
            {
                client.deregisterServiceProvider(ServiceProviderSingleton.getServiceProviderURI());
            }
            catch (final Exception exception)
            {
                logger.log(Level.SEVERE, "Unable to deregister with service provider catalog", exception);
            }
            finally
            {
                client = null;
                ServiceProviderSingleton.setServiceProviderURI(null);
            }
        }

        try
        {
            Persistence.save(basePath);
        }
        catch (final Exception exception)
        {
            logger.log(Level.SEVERE, "Unable to save", exception);
        }
    }

    @Override
    public void contextInitialized(final ServletContextEvent servletContextEvent)
    {
        final String basePath = generateBasePath(servletContextEvent);

        final URI serviceProviderURI;

        try
        {
            final ServiceProvider serviceProvider = ServiceProviderFactory.createServiceProvider(basePath);

            client = new ServiceProviderRegistryClient(JenaProvidersRegistry.getProviders());

            serviceProviderURI = client.registerServiceProvider(serviceProvider);

            ServiceProviderSingleton.setServiceProviderURI(serviceProviderURI);
        }
        catch (final Exception exception)
        {
            client = null;

            logger.log(Level.SEVERE, "Unable to register with service provider catalog", exception);

            return;
        }

        try
        {
            final Populate populate = new Populate(basePath,
                                                   serviceProviderURI);

            if (Persistence.load(basePath))
            {
                // References to ServiceProvider have to be updated
                populate.fixup();
            }
            else
            {
                populate.populate();
            }
        }
        catch (final Exception exception)
        {
            logger.log(Level.SEVERE, "Unable to load", exception);
        }
    }

    private static String generateBasePath(final ServletContextEvent servletContextEvent)
    {
        final ServletContext servletContext = servletContextEvent.getServletContext();

        String scheme = System.getProperty(PROPERTY_SCHEME);
        if (scheme == null)
        {
            scheme = servletContext.getInitParameter(PROPERTY_SCHEME);
        }

        String port = System.getProperty(PROPERTY_PORT);
        if (port == null)
        {
            port = servletContext.getInitParameter(PROPERTY_PORT);
        }

        return scheme + "://" + HOST + ":" + port + servletContext.getContextPath();
    }

    private static String getHost()
    {
        try
        {
            return InetAddress.getLocalHost().getCanonicalHostName();
        }
        catch (final UnknownHostException exception)
        {
            return "localhost";
        }
    }
}