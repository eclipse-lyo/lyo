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

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;

public class ServletListener
       implements ServletContextListener
{
    private static final String PROPERTY_SCHEME = ServletListener.class.getPackage().getName() + ".scheme";
    private static final String PROPERTY_PORT   = ServletListener.class.getPackage().getName() + ".port";

    private static final Logger logger = Logger.getLogger(ServletListener.class.getName());

    private static final String HOST = getHost();

    private String serviceProviderIdentifier;

    public ServletListener()
    {
        super();
    }

    @Override
    public void contextDestroyed(final ServletContextEvent servletContextEvent)
    {
        if (serviceProviderIdentifier != null)
        {
            try
            {
                ServiceProviderCatalogSingleton.deregisterServiceProvider(serviceProviderIdentifier);
            }
            catch (final Exception exception)
            {
                logger.log(Level.SEVERE, "Unable to deregister with service provider catalog", exception);
            }
            finally
            {
                serviceProviderIdentifier = null;
            }
        }
    }

    @Override
    public void contextInitialized(final ServletContextEvent servletContextEvent)
    {
        try
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

            final String baseURI = scheme + "://" + HOST + ":" + port + servletContext.getContextPath();

            final ServiceProvider serviceProvider = ServiceProviderFactory.createServiceProvider(baseURI);

            final ServiceProvider registeredServiceProvider = ServiceProviderCatalogSingleton.registerServiceProvider(baseURI,
                                                                                                                      serviceProvider);

            serviceProviderIdentifier = registeredServiceProvider.getIdentifier();
        }
        catch (final Exception exception)
        {
            logger.log(Level.SEVERE, "Unable to register with service provider catalog", exception);
        }
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