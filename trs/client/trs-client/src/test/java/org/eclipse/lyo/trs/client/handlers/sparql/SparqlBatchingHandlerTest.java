/*
 * Copyright (c) 2025 Contributors to the Eclipse Foundation
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
package org.eclipse.lyo.trs.client.handlers.sparql;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Date;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.vocabulary.RDF;
import org.eclipse.lyo.core.trs.ChangeEvent;
import org.eclipse.lyo.core.trs.Creation;
import org.eclipse.lyo.core.trs.Deletion;
import org.eclipse.lyo.core.trs.Modification;
import org.eclipse.lyo.store.Store;
import org.eclipse.lyo.store.StoreFactory;
import org.eclipse.lyo.trs.client.model.BaseMember;
import org.eclipse.lyo.trs.client.model.ChangeEventMessageTR;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

@DisplayName("SparqlBatchingHandler Tests")
public class SparqlBatchingHandlerTest {

    private SparqlBatchingHandler handler;
    private final String sparqlUpdateEndpoint = "http://example.org/sparql/update";
    private final String username = "testuser";
    private final String password = "testpass";
    
    @BeforeEach
    public void setUp() {
        handler = new SparqlBatchingHandler(sparqlUpdateEndpoint, username, password);
    }

    @Test
    @DisplayName("Test SparqlBatchingHandler uses Lyo Store for finishCycle")
    public void testFinishCycleUsesLyoStore() {
        // Create mock Store
        Store mockStore = mock(Store.class);
        
        // Mock StoreFactory.sparql method
        try (MockedStatic<StoreFactory> mockedStoreFactory = mockStatic(StoreFactory.class)) {
            mockedStoreFactory.when(() -> StoreFactory.sparql(null, sparqlUpdateEndpoint, username, password))
                    .thenReturn(mockStore);
            
            // Add some test operations to create queries
            BaseMember baseMember = createTestBaseMember();
            handler.handleBaseMember(baseMember);
            
            // Call finishCycle which should use the Store
            handler.finishCycle();
            
            // Verify that StoreFactory.sparql was called with correct parameters
            mockedStoreFactory.verify(() -> StoreFactory.sparql(null, sparqlUpdateEndpoint, username, password));
            
            // Verify that rawUpdateQuery was called on the store
            verify(mockStore).rawUpdateQuery(anyString());
            
            // Verify that close was called on the store
            verify(mockStore).close();
        }
    }

    @Test
    @DisplayName("Test handleBaseMember creates proper queries")
    public void testHandleBaseMember() {
        BaseMember baseMember = createTestBaseMember();
        
        // This should add queries to the internal list
        handler.handleBaseMember(baseMember);
        
        // We can't directly test the internal queries list, but we can test
        // that finishCycle processes them by mocking the Store
        Store mockStore = mock(Store.class);
        
        try (MockedStatic<StoreFactory> mockedStoreFactory = mockStatic(StoreFactory.class)) {
            mockedStoreFactory.when(() -> StoreFactory.sparql(null, sparqlUpdateEndpoint, username, password))
                    .thenReturn(mockStore);
            
            handler.finishCycle();
            
            // Verify rawUpdateQuery was called with some query string
            verify(mockStore).rawUpdateQuery(anyString());
        }
    }

    @Test
    @DisplayName("Test handleChangeEvent with Creation")
    public void testHandleChangeEventCreation() throws URISyntaxException {
        Creation creation = new Creation();
        creation.setChanged(new URI("http://example.org/resource/1"));
        creation.setOrder(1);
        
        Model testModel = ModelFactory.createDefaultModel();
        testModel.add(testModel.createResource("http://example.org/resource/1"), 
                     RDF.type, 
                     testModel.createResource("http://example.org/TestType"));
        
        ChangeEventMessageTR eventMessage = new ChangeEventMessageTR(creation, testModel);
        
        handler.handleChangeEvent(eventMessage);
        
        // Verify by testing finishCycle
        Store mockStore = mock(Store.class);
        
        try (MockedStatic<StoreFactory> mockedStoreFactory = mockStatic(StoreFactory.class)) {
            mockedStoreFactory.when(() -> StoreFactory.sparql(null, sparqlUpdateEndpoint, username, password))
                    .thenReturn(mockStore);
            
            handler.finishCycle();
            
            verify(mockStore).rawUpdateQuery(anyString());
        }
    }

    @Test
    @DisplayName("Test handleChangeEvent with Deletion")
    public void testHandleChangeEventDeletion() throws URISyntaxException {
        Deletion deletion = new Deletion();
        deletion.setChanged(new URI("http://example.org/resource/1"));
        deletion.setOrder(2);
        
        ChangeEventMessageTR eventMessage = new ChangeEventMessageTR(deletion, null);
        
        handler.handleChangeEvent(eventMessage);
        
        // Verify by testing finishCycle
        Store mockStore = mock(Store.class);
        
        try (MockedStatic<StoreFactory> mockedStoreFactory = mockStatic(StoreFactory.class)) {
            mockedStoreFactory.when(() -> StoreFactory.sparql(null, sparqlUpdateEndpoint, username, password))
                    .thenReturn(mockStore);
            
            handler.finishCycle();
            
            verify(mockStore).rawUpdateQuery(anyString());
        }
    }

    @Test
    @DisplayName("Test handleChangeEvent with Modification")
    public void testHandleChangeEventModification() throws URISyntaxException {
        Modification modification = new Modification();
        modification.setChanged(new URI("http://example.org/resource/1"));
        modification.setOrder(3);
        
        Model testModel = ModelFactory.createDefaultModel();
        testModel.add(testModel.createResource("http://example.org/resource/1"), 
                     RDF.type, 
                     testModel.createResource("http://example.org/ModifiedType"));
        
        ChangeEventMessageTR eventMessage = new ChangeEventMessageTR(modification, testModel);
        
        handler.handleChangeEvent(eventMessage);
        
        // Verify by testing finishCycle
        Store mockStore = mock(Store.class);
        
        try (MockedStatic<StoreFactory> mockedStoreFactory = mockStatic(StoreFactory.class)) {
            mockedStoreFactory.when(() -> StoreFactory.sparql(null, sparqlUpdateEndpoint, username, password))
                    .thenReturn(mockStore);
            
            handler.finishCycle();
            
            verify(mockStore).rawUpdateQuery(anyString());
        }
    }

    private BaseMember createTestBaseMember() {
        try {
            URI memberUri = new URI("http://example.org/basemember/1");
            Model model = ModelFactory.createDefaultModel();
            model.add(model.createResource(memberUri.toString()), 
                     RDF.type, 
                     model.createResource("http://example.org/TestResource"));
            
            return new BaseMember(memberUri, model);
        } catch (URISyntaxException e) {
            throw new RuntimeException(e);
        }
    }
}
