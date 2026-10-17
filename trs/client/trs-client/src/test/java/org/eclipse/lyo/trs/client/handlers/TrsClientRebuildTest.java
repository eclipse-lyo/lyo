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
package org.eclipse.lyo.trs.client.handlers;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import java.net.URI;
import java.util.List;
import org.apache.jena.vocabulary.RDF;
import org.eclipse.lyo.core.trs.*;
import org.eclipse.lyo.trs.client.util.ITrackedResourceClient;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class TrsClientRebuildTest {
    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void serverRebaseKeepsSyncWhenLastEventIsRetained(boolean concurrent) throws Exception {
        exerciseServerRebase(concurrent, true);
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void missingLastEventRebuildsSyncFromNewBase(boolean concurrent) throws Exception {
        exerciseServerRebase(concurrent, false);
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void emptyBaseDoesNotRebuildWhileEventsAccumulate(boolean concurrent) throws Exception {
        URI endpoint = URI.create("https://example.com/trs");
        URI nil = URI.create(RDF.nil.getURI());
        ITrackedResourceClient client = mock(ITrackedResourceClient.class);
        IProviderEventHandler events = mock(IProviderEventHandler.class);
        IProviderHandler provider = concurrent
                ? new ConcurrentTrsProviderHandler(endpoint, client, events)
                : new TrsProviderHandler(endpoint, client, events);
        Base base = new Base();
        base.setCutoffEvent(nil);
        ChangeLog log = new ChangeLog();
        log.setPrevious(nil);
        TrackedResourceSet trs = new TrackedResourceSet();
        trs.setChangeLog(log);
        when(client.extractRemoteTrs(endpoint)).thenReturn(trs);
        when(client.updateBases(trs)).thenReturn(List.of(base));

        provider.update();
        // Both supported end-of-log representations must preserve the empty-base cursor.
        log.setPrevious(null);
        provider.update();
        log.getChange().add(deletion("first", 1));
        provider.update();
        log.getChange().add(deletion("second", 2));
        provider.update();

        verify(client).updateBases(trs);
        verify(events, never()).rebuild();
        verify(events, times(2)).handleChangeEvent(any());
        verify(events, times(4)).finishCycle();
    }

    private void exerciseServerRebase(boolean concurrent, boolean retainLastEvent) throws Exception {
        URI endpoint = URI.create("https://example.com/trs");
        URI nil = URI.create(RDF.nil.getURI());
        ITrackedResourceClient client = mock(ITrackedResourceClient.class);
        IProviderEventHandler events = mock(IProviderEventHandler.class);
        IProviderHandler provider = concurrent
                ? new ConcurrentTrsProviderHandler(endpoint, client, events)
                : new TrsProviderHandler(endpoint, client, events);
        Base base = new Base();
        base.setCutoffEvent(nil);
        ChangeLog log = new ChangeLog();
        log.setPrevious(nil);
        Deletion first = deletion("first", 1);
        log.getChange().add(first);
        TrackedResourceSet trs = new TrackedResourceSet();
        trs.setBase(URI.create("https://example.com/base/old"));
        trs.setChangeLog(log);
        when(client.extractRemoteTrs(endpoint)).thenReturn(trs);
        when(client.updateBases(trs)).thenReturn(List.of(base));

        provider.update();
        verify(events).handleChangeEvent(any());
        verify(client).updateBases(trs);

        // The server rebuilds its base, advancing the cutoff and changing its URI.
        trs.setBase(URI.create("https://example.com/base/new"));
        Deletion second = deletion("second", 2);
        base.setCutoffEvent(second.getAbout());
        ChangeLog older = new ChangeLog();
        older.setPrevious(nil);
        if (retainLastEvent) older.getChange().add(first);
        URI olderUri = URI.create("https://example.com/log/older");
        log.getChange().clear();
        log.getChange().add(second);
        log.setPrevious(olderUri);
        when(client.fetchRemoteChangeLog(olderUri)).thenReturn(older);

        provider.update();
        if (retainLastEvent) {
            verify(events, never()).rebuild();
            verify(events, times(2)).handleChangeEvent(any());
            verify(client).updateBases(trs);
        } else {
            verify(events).rebuild();
            verify(events).handleChangeEvent(any());
            provider.update();
            verify(client, times(2)).updateBases(trs);
        }
    }

    private Deletion deletion(String id, int order) {
        Deletion event = new Deletion();
        event.setAbout(URI.create("https://example.com/events/" + id));
        event.setChanged(URI.create("https://example.com/resources/" + id));
        event.setOrder(order);
        return event;
    }
}
