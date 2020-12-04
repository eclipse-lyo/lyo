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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause
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
