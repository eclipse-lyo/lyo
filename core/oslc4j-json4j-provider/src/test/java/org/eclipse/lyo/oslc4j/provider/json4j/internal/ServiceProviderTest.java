package org.eclipse.lyo.oslc4j.provider.json4j.internal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.InputStream;
import java.net.URI;
import java.util.Arrays;
import java.util.List;

import jakarta.json.Json;
import jakarta.json.JsonReader;
import org.eclipse.lyo.oslc4j.core.model.Service;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.provider.json4j.JsonHelper;
import org.junit.jupiter.api.Test;

public class ServiceProviderTest {

    @Test
    public void testUsage() throws Exception {
        InputStream is = ServiceProviderTest.class.getResourceAsStream("/provider.json");
        assertNotNull(is, "Could not read file: provider.json");

        JsonReader reader = Json.createReader(is);
        JSONObject jsonObject = new JSONObject(reader.readObject());

        Object[] objects = JsonHelper.fromJSON(jsonObject.build(), ServiceProvider.class);
        // Cast each element or create new array
        ServiceProvider[] providers = new ServiceProvider[objects.length];
        System.arraycopy(objects, 0, providers, 0, objects.length);

        assertEquals(1, providers.length, "Incorrect number of service providers");
        
        Service[] services = providers[0].getServices();
        assertEquals(1, services.length, "Incorrect number of services");
        
        URI[] usages = services[0].getUsages();
        assertEquals(2, usages.length, "Incorrect number of usages");

        List<URI> usageList = Arrays.asList(usages);
        assertTrue(usageList.contains(new URI("http://example.com/ns#usage1")), "Missing ex:usage1");
        assertTrue(usageList.contains(new URI("http://example.com/ns#usage2")), "Missing ex:usage2");
    }
}
