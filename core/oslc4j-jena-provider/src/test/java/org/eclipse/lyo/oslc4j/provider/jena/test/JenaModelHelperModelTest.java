package org.eclipse.lyo.oslc4j.provider.jena.test;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class JenaModelHelperModelTest {

    @Test
    public void testUnmarshalModel() throws Exception {
        Model m = ModelFactory.createDefaultModel();
        Resource r = m.createResource("http://example.com/r");
        r.addProperty(m.createProperty("http://example.com/p"), "v");

        Model[] models = JenaModelHelper.unmarshal(m, Model.class);
        assertNotNull(models);
        assertEquals(1, models.length);
        assertTrue(models[0].contains(r, m.createProperty("http://example.com/p"), "v"));
    }

    @Test
    public void testUnmarshalSingleModel() throws Exception {
        Model m = ModelFactory.createDefaultModel();
        Resource r = m.createResource("http://example.com/r");
        r.addProperty(m.createProperty("http://example.com/p"), "v");

        Model model = JenaModelHelper.unmarshalSingle(m, Model.class);
        assertNotNull(model);
        assertTrue(model.contains(r, m.createProperty("http://example.com/p"), "v"));
    }

    @Test
    public void testUnmarshalEmptyModel() throws Exception {
        Model m = ModelFactory.createDefaultModel();

        Model[] models = JenaModelHelper.unmarshal(m, Model.class);
        assertNotNull(models);
        assertEquals(1, models.length); // Assuming it returns the empty model itself
        assertTrue(models[0].isEmpty());
    }
}
