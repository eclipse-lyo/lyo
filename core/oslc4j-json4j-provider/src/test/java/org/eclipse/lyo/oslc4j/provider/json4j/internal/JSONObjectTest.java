package org.eclipse.lyo.oslc4j.provider.json4j.internal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import jakarta.json.JsonException;
import org.junit.jupiter.api.Test;

public class JSONObjectTest {

    @Test
    public void testPutAndGet() {
        JSONObject json = new JSONObject();
        json.put("key", "value");
        assertEquals("value", json.get("key"));
        assertEquals("value", json.getString("key"));
        assertEquals("value", json.optString("key"));
    }

    @Test
    public void testOptStringDefaults() {
        JSONObject json = new JSONObject();
        assertEquals(null, json.optString("missing"));
    }

    @Test
    public void testGetMissing() {
        JSONObject json = new JSONObject();
        assertThrows(JsonException.class, () -> json.get("missing"));
    }

    @Test
    public void testNestedObject() {
        JSONObject parent = new JSONObject();
        JSONObject child = new JSONObject();
        child.put("childKey", "childValue");
        parent.put("parentKey", child);

        JSONObject retrievedChild = parent.getJSONObject("parentKey");
        assertNotNull(retrievedChild);
        assertEquals("childValue", retrievedChild.getString("childKey"));
    }

    @Test
    public void testJsonArray() {
        JSONObject json = new JSONObject();
        JSONArray array = new JSONArray();
        array.add("item1");
        array.add("item2");
        json.put("arrayKey", array);

        JSONArray retrievedArray = json.getJSONArray("arrayKey");
        assertNotNull(retrievedArray);
        assertEquals(2, retrievedArray.size());
        assertEquals("item1", retrievedArray.get(0).toString().replace(""", "")); // JsonString.toString() includes quotes
    }
    
    @Test
    public void testBuildAndToString() {
        JSONObject json = new JSONObject();
        json.put("k", "v");
        String s = json.toString();
        assertTrue(s.contains(""k":"v"") || s.contains(""k": "v""));
    }
}
