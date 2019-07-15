/*
 * Copyright (c) 2016 KTH Royal Institute of Technology and others
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */

package org.eclipse.lyo.trs.client.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.BiFunction;
import org.eclipse.lyo.oslc4j.client.OslcClient;
import org.eclipse.lyo.trs.client.config.TrsConsumerConfiguration;
import org.eclipse.lyo.trs.client.config.TrsProviderConfiguration;
import org.eclipse.lyo.trs.client.exceptions.ServerRollBackException;
import org.eclipse.lyo.trs.client.handlers.ConcurrentTrsProviderHandler;
import org.eclipse.lyo.trs.client.handlers.IProviderEventHandler;
import org.eclipse.lyo.trs.client.handlers.IProviderHandler;
import org.eclipse.lyo.trs.client.handlers.ITrsClient;
import org.eclipse.lyo.trs.client.handlers.SparqlBatchingHandler;
import org.eclipse.lyo.trs.client.handlers.SparqlDirectHandler;
import org.eclipse.lyo.trs.client.handlers.TrsClient;
import org.eclipse.lyo.trs.client.handlers.TrsProviderHandler;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class TrsConsumerUtils {
    private final static Logger log = LoggerFactory.getLogger(TrsConsumerUtils.class);

    public static List<IProviderHandler> buildHandlersSequential(
            final TrsConsumerConfiguration consumerConfig,
            final Collection<TrsProviderConfiguration> providerConfigs) {
        return buildHandlers(consumerConfig, providerConfigs, TrsConsumerUtils::providerFor);
    }

    public static List<IProviderHandler> buildHandlersConcurrent(
            final TrsConsumerConfiguration consumerConfig,
            final Collection<TrsProviderConfiguration> providerConfigs) {
        return buildHandlers(consumerConfig,
                providerConfigs,
                TrsConsumerUtils::concurrentProviderFor
        );
    }

    public static List<IProviderHandler> buildHandlers(
            final TrsConsumerConfiguration consumerConfig,
            final Collection<TrsProviderConfiguration> providerConfigs,
            final BiFunction<TrsConsumerConfiguration, TrsProviderConfiguration, IProviderHandler> function) {
        final List<IProviderHandler> providers = new ArrayList<>();

        for (TrsProviderConfiguration cfg : providerConfigs) {
            IProviderHandler trsProvider = function.apply(consumerConfig, cfg);
            providers.add(trsProvider);
//            try {
//                // we actually need to wait here, but not in the listener
//                trsProvider.pollAndProcessChanges();
//            } catch (ServerRollBackException e) {
//                log.warn(
//                        "Cannot find the internal cutoff event in the Change Logs; treating this as a rebase/rollback scenario.");
//            } catch (Exception e) {
//                log.error("Unknown error processing CEs from the TRS Server '{}'", trsProvider);
//            }
        }

        return providers;
    }

    private static IProviderHandler providerFor(final TrsConsumerConfiguration consumerConfig,
            final TrsProviderConfiguration cfg) {
//        return new TrsProviderHandler(cfg.getTrsUri(), ,
//                consumerConfig.getSparqlUpdateUrl(), consumerConfig.getSparqlQueryUrl(),
//                consumerConfig.getSparqlUsername(), consumerConfig.getSparqlPassword(), cfg.getBasicAuthUsername(),
//                cfg.getBasicAuthPassword(), trsClient);
        final ITrsClient trsClient = trsClientFactory(consumerConfig.getHttpClient());
        final IProviderEventHandler handler = new SparqlDirectHandler(
                consumerConfig.getSparqlUpdateUrl());
        IProviderHandler providerHandler = new TrsProviderHandler(cfg.getTrsUri(), trsClient,
                handler);
        return providerHandler;
    }

    private static IProviderHandler concurrentProviderFor(
            final TrsConsumerConfiguration consumerConfig, final TrsProviderConfiguration cfg) {
        final ITrsClient trsClient = trsClientFactory(consumerConfig.getHttpClient());
        final IProviderEventHandler handler = new SparqlBatchingHandler(
                consumerConfig.getSparqlUpdateUrl(), consumerConfig.getSparqlUsername(),
                consumerConfig.getSparqlPassword());
        final ConcurrentTrsProviderHandler providerHandler = new ConcurrentTrsProviderHandler(
                cfg.getTrsUri(), trsClient, handler);
        return providerHandler;
    }

    @NotNull
    private static ITrsClient trsClientFactory(final OslcClient httpClient) {
        return new TrsClient(httpClient);
    }
}
