package org.eclipse.lyo.client.query;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Collections;
import java.util.Map;

import org.eclipse.lyo.client.OslcClient;
import org.junit.Test;

public class OslcQueryTest {

    private static final String CAPABILITY_URL = "http://lyo.query.capability.url";
    OslcClient client = new OslcClient();

    @Test
    public void testConstructors_default() {
        OslcQuery query = new OslcQuery(client, CAPABILITY_URL);

        assertEquals(CAPABILITY_URL, query.getCapabilityUrl());
        assertEquals(0, query.getPageSize());
        assertEquals(CAPABILITY_URL, query.getQueryUrl());
        assertEquals(Collections.emptyMap(), query.getAdditionalHeaders());
    }

    @Test
    public void testConstructors_additionalHeaders() {
        OslcQuery query = new OslcQuery(client, CAPABILITY_URL, null, Map.of("key", "value"));

        assertEquals(CAPABILITY_URL, query.getCapabilityUrl());
        assertEquals(0, query.getPageSize());
        assertEquals(CAPABILITY_URL, query.getQueryUrl());
        assertEquals(Map.of("key", "value"), query.getAdditionalHeaders());
    }
    
}
