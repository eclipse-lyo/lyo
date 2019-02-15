/*
 * Copyright (c) 2016-2018 KTH Royal Institute of Technology and others.
 *
 * All rights reserved. This program and the accompanying materials are made available under the
 * terms of the Eclipse
 * Public License v1.0 and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html and the
 * Eclipse Distribution
 * License is available at http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 *     Andrew Berezovskyi  -  Initial implementation
 *     Omar Kacimi         -  Original code
 *     Xufei Ning          -  MQTT modification
 */

package org.eclipse.lyo.oslc4j.trs.client.util;

import com.google.common.base.Strings;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.BiFunction;
import org.eclipse.lyo.oslc4j.trs.client.config.TrsConsumerConfiguration;
import org.eclipse.lyo.oslc4j.trs.client.config.TrsProviderConfiguration;
import org.eclipse.lyo.oslc4j.trs.client.handlers.ConcurrentTrsProviderHandler;
import org.eclipse.lyo.oslc4j.trs.client.handlers.TrsProviderHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Created on 2018-02-27
 *
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.0.1
 */
public class TrsConsumerUtils {
    private final static Logger log = LoggerFactory.getLogger(TrsConsumerUtils.class);

    public static List<TrsProviderHandler> buildHandlersSequential(
            final TrsConsumerConfiguration consumerConfig,
            final Collection<TrsProviderConfiguration> providerConfigs) {
        return buildHandlers(consumerConfig, providerConfigs, TrsConsumerUtils::providerFor);
    }

    public static List<TrsProviderHandler> buildHandlersConcurrent(
            final TrsConsumerConfiguration consumerConfig,
            final Collection<TrsProviderConfiguration> providerConfigs) {
        return buildHandlers(consumerConfig,
                providerConfigs,
                TrsConsumerUtils::concurrentProviderFor
        );
    }

    public static List<TrsProviderHandler> buildHandlers(
            final TrsConsumerConfiguration consumerConfig,
            final Collection<TrsProviderConfiguration> providerConfigs,
            final BiFunction<TrsConsumerConfiguration, TrsProviderConfiguration,
                    TrsProviderHandler> function) {
        final List<TrsProviderHandler> providers = new ArrayList<>();

        for (TrsProviderConfiguration cfg : providerConfigs) {
            TrsProviderHandler trsProvider = function.apply(consumerConfig, cfg);
            providers.add(trsProvider);
            try {
                // we actually need to wait here, but not in the listener
                trsProvider.pollAndProcessChanges();
            } catch (Exception e) {
                log.error(
                        "Error polling the service provider {} before initialising MQTT " +
                                "subscription",
                        trsProvider
                );
            }
        }

        return providers;
    }

    private static TrsProviderHandler providerFor(final TrsConsumerConfiguration consumerConfig,
            final TrsProviderConfiguration cfg) {
        return new TrsProviderHandler(cfg.getTrsUri(),
                consumerConfig.getSparqlQueryUrl(),
                consumerConfig.getSparqlUpdateUrl(),
                consumerConfig.getHttpClient(),
                cfg.getBasicAuthUsername(),
                cfg.getBasicAuthPassword(),
                consumerConfig.getSparqlUsername(),
                consumerConfig.getSparqlPassword()
        );
    }

    private static TrsProviderHandler concurrentProviderFor(
            final TrsConsumerConfiguration consumerConfig, final TrsProviderConfiguration cfg) {
        return new ConcurrentTrsProviderHandler(cfg.getTrsUri(),
                                                consumerConfig.getSparqlQueryUrl(),
                                                consumerConfig.getSparqlUpdateUrl(),
                                                consumerConfig.getHttpClient(),
                                                cfg.getBasicAuthUsername(),
                                                cfg.getBasicAuthPassword(),
                                                consumerConfig.getSparqlUsername(),
                                                consumerConfig.getSparqlPassword()
        );
    }
}
