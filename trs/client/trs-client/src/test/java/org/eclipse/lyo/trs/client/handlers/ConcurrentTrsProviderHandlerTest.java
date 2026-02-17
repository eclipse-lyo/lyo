package org.eclipse.lyo.trs.client.handlers;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import org.apache.jena.sys.JenaSystem;
import org.apache.jena.vocabulary.RDF;
import org.eclipse.lyo.core.trs.ChangeLog;
import org.eclipse.lyo.trs.client.util.ITrackedResourceClient;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class ConcurrentTrsProviderHandlerTest {

    @Mock
    private ITrackedResourceClient trsClient;
    @Mock
    private IProviderEventHandler handler;

    @BeforeAll
    public static void initJena() {
        JenaSystem.init();
    }

    @Test
    public void testFetchRemoteChangeLogs_HitNil_Found() {
        URI trsUri = URI.create("http://example.com/trs");
        ConcurrentTrsProviderHandler provider = new ConcurrentTrsProviderHandler(trsUri, trsClient, handler);

        try {
            java.lang.reflect.Field field = ConcurrentTrsProviderHandler.class.getDeclaredField("lastProcessedChangeEventUri");
            field.setAccessible(true);
            field.set(provider, URI.create(RDF.nil.getURI()));
        } catch (Exception e) {
            Assertions.fail("Failed to set private field", e);
        }

        ChangeLog currentLog = new ChangeLog();
        currentLog.setPrevious(URI.create(RDF.nil.getURI()));
        
        List<ChangeLog> changeLogs = new ArrayList<>();
        boolean found = provider.fetchRemoteChangeLogs(currentLog, changeLogs);
        
        Assertions.assertTrue(found, "Should find sync event when hitting nil and last processed is nil");
        Assertions.assertEquals(1, changeLogs.size());
    }
    
    @Test
    public void testFetchRemoteChangeLogs_HitNil_NotFound() {
        URI trsUri = URI.create("http://example.com/trs");
        ConcurrentTrsProviderHandler provider = new ConcurrentTrsProviderHandler(trsUri, trsClient, handler);

        try {
            java.lang.reflect.Field field = ConcurrentTrsProviderHandler.class.getDeclaredField("lastProcessedChangeEventUri");
            field.setAccessible(true);
            field.set(provider, URI.create("http://example.com/event/1"));
        } catch (Exception e) {
            Assertions.fail("Failed to set private field", e);
        }

        ChangeLog currentLog = new ChangeLog();
        currentLog.setPrevious(URI.create(RDF.nil.getURI()));
        
        List<ChangeLog> changeLogs = new ArrayList<>();
        boolean found = provider.fetchRemoteChangeLogs(currentLog, changeLogs);
        
        Assertions.assertFalse(found, "Should NOT find sync event if we hit nil but were looking for specific event");
    }
}