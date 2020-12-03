/*
 Copyright (c) 2012-2019 IBM Corporation and others

 All rights reserved. This program and the accompanying materials
 are made available under the terms of the Eclipse Public License v1.0
 and Eclipse Distribution License v. 1.0 which accompanies this distribution.

 The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 and the Eclipse Distribution License is available at
 http://www.eclipse.org/org/documents/edl-v10.php.
 */

package org.eclipse.lyo.server.oauth.webapp.services;

import org.eclipse.lyo.server.oauth.core.OAuthConfiguration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

public class OAuthServletListener implements ServletContextListener {

    private static final Logger log = LoggerFactory.getLogger(OAuthServletListener.class);

    @Override
    public void contextDestroyed(ServletContextEvent arg0) {
        log.info("Initialising Lyo OAuth webapp");
        OAuthConfiguration.getInstance().getConsumerStore().closeConsumerStore();
    }

    @Override
    public void contextInitialized(ServletContextEvent arg0) {

    }

}
