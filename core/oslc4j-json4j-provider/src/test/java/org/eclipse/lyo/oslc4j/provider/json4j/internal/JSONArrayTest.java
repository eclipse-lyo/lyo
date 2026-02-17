package org.eclipse.lyo.oslc4j.provider.json4j.internal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import jakarta.json.JsonValue;
import org.junit.jupiter.api.Test;

public class JSONArrayTest {

    @Test
    public void testAddAndGet() {
        JSONArray array = new JSONArray();
        array.add("one");
        array.add(2);
        
        assertEquals(2, array.size());
        assertFalse(array.isEmpty());
        
        // JsonString.toString() includes quotes, so "one" becomes ""one""
        // JsonNumber.toString() is "2"
        assertTrue(array.get(0).toString().contains("one")); 
        assertEquals("2", array.get(1).toString());
    }

    @Test
    public void testIterator() {
        JSONArray array = new JSONArray();
        array.add("A");
        array.add("B");
        
        int count = 0;
        for (Object v : array) {
            count++;
        }
        assertEquals(2, count);
    }
}
