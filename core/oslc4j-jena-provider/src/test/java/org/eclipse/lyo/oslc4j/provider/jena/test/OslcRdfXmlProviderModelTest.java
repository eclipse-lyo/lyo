package org.eclipse.lyo.oslc4j.provider.jena.test;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.lang.annotation.Annotation;
import java.nio.charset.StandardCharsets;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.MultivaluedHashMap;

import org.apache.jena.rdf.model.Model;
import org.eclipse.lyo.oslc4j.provider.jena.OslcRdfXmlProvider;
import org.junit.Test;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class OslcRdfXmlProviderModelTest {

    @Test
    public void testReadModel() throws Exception {
        OslcRdfXmlProvider provider = new OslcRdfXmlProvider();

        // Check isReadable
        assertTrue("Should be readable for Model.class",
            provider.isReadable(Model.class, Model.class, new Annotation[]{}, MediaType.valueOf("application/rdf+xml")));

        // Check readFrom
        String rdfXml = "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:ex=\"http://example.com/\">" +
                        "<rdf:Description rdf:about=\"http://example.com/res\">" +
                        "<ex:p>value</ex:p>" +
                        "</rdf:Description>" +
                        "</rdf:RDF>";
        InputStream inputStream = new ByteArrayInputStream(rdfXml.getBytes(StandardCharsets.UTF_8));

        Object result = provider.readFrom(
            (Class) Model.class,
            Model.class,
            new Annotation[]{},
            MediaType.valueOf("application/rdf+xml"),
            new MultivaluedHashMap<>(),
            inputStream
        );

        assertNotNull("Result should not be null", result);
        assertTrue("Result should be instance of Model", result instanceof Model);
        Model model = (Model) result;
        assertTrue("Model should contain the resource", model.containsResource(model.createResource("http://example.com/res")));
    }
}
