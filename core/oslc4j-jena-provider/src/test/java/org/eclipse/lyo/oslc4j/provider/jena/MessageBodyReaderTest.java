package org.eclipse.lyo.oslc4j.provider.jena;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Collection;

import javax.ws.rs.core.MediaType;

import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.provider.jena.test.resources.ResourceWithoutShape;
import org.eclipse.lyo.oslc4j.provider.jena.test.resources.TestResource;
import org.junit.Test;

public class MessageBodyReaderTest {

    @Test
    public void testOslcXmlProvider() {
        OslcXmlProvider provider = new OslcXmlProvider();

        boolean isReadable = provider.isReadable(TestResource.class, null, TestResource.class.getAnnotations(),
                MediaType.APPLICATION_XML_TYPE);
        assertTrue(isReadable);

        isReadable = provider.isReadable(TestResource.class, null, TestResource.class.getAnnotations(),
                OslcMediaType.APPLICATION_RDF_XML_TYPE);
        assertTrue(isReadable);

        isReadable = provider.isReadable(ResourceWithoutShape.class, null, ResourceWithoutShape.class.getAnnotations(),
                MediaType.APPLICATION_XML_TYPE);
        assertFalse(isReadable);

        isReadable = provider.isReadable(TestResource.class, null, TestResource.class.getAnnotations(),
                MediaType.APPLICATION_JSON_TYPE);
        assertFalse(isReadable);

        isReadable = provider.isReadable(Collection.class, null, TestResource.class.getAnnotations(),
                MediaType.APPLICATION_JSON_TYPE);
        assertFalse(isReadable);
    }

    @Test
    public void testOslcArrayProvider() {

        OslcRdfXmlArrayProvider provider = new OslcRdfXmlArrayProvider();

        boolean isReadable = provider.isReadable(TestResource[].class, null, TestResource.class.getAnnotations(),
                MediaType.APPLICATION_XML_TYPE);
        assertTrue(isReadable);

        isReadable = provider.isReadable(TestResource[].class, null, TestResource.class.getAnnotations(),
                OslcMediaType.APPLICATION_RDF_XML_TYPE);
        assertTrue(isReadable);

        isReadable = provider.isReadable(ResourceWithoutShape[].class, null,
                ResourceWithoutShape.class.getAnnotations(), OslcMediaType.APPLICATION_RDF_XML_TYPE);
        assertFalse(isReadable);

        isReadable = provider.isReadable(TestResource[].class, null, TestResource.class.getAnnotations(),
                OslcMediaType.APPLICATION_JSON_TYPE);
        assertFalse(isReadable);

    }

}
