package org.eclipse.lyo.oslc4j.provider.jena.shapefactory.test;

import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Set;

import javax.xml.datatype.DatatypeConfigurationException;

import org.apache.jena.rdf.model.Model;
import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreInvalidRepresentationException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreInvalidValueTypeException;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.ResourceShape;
import org.eclipse.lyo.oslc4j.core.model.ResourceShapeFactory;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.oslc4j.provider.jena.shapefactory.resources.*;
import org.junit.Test;

import static org.junit.Assert.*;
import static org.assertj.core.api.Assertions.*;
import static org.eclipse.lyo.oslc4j.provider.jena.helpers.JenaAssert.assertThat;

public class ShapeFactoryTest {

    private static ShapeWithWorkingReferences createShapeInstanceWithWorkingReferences(String id) {
        ShapeWithWorkingReferences r = new ShapeWithWorkingReferences(URI.create("http://test.adaptor.net/r/" + id));
        r.setIdentifier(id);
        
        LocalShape local = new LocalShape();
        local.setIdentifier("local" + id);
        r.setReferencesAsLocal(local);
        
        ReferencedShape ref = new ReferencedShape();
        ref.setAbout(URI.create("http://test.adaptor.net/ref/" + id));
        r.setReferencesAsResource(new Link(ref.getAbout()));

        InlinedShape inline = new InlinedShape();
        inline.setAbout(URI.create("http://test.adaptor.net/inline" + id));
        inline.setIdentifier("inline" + id);
        r.setInlines(inline);

        
        InlinedShape i1 = new InlinedShape();
        i1.setAbout(URI.create("http://test.adaptor.net/inlineMany_1_" + id));
        i1.setIdentifier("inlineMany_1_" + id);
        r.addInlinesMany(i1);
        
        InlinedShape i2 = new InlinedShape();
        i2.setAbout(URI.create("http://test.adaptor.net/inlineMany_2_" + id));
        i2.setIdentifier("inlineMany_2_" + id);
        r.addInlinesMany(i2);
    
        return r;
    }

    @Test
    public void validShapeTest() throws OslcCoreApplicationException, URISyntaxException {
        ResourceShape shape = ResourceShapeFactory.createResourceShape("http://localhost:8080", 
                OslcConstants.PATH_RESOURCE_SHAPES, 
                "http://test.net/ns/test#", 
                ShapeWithWorkingReferences.class);
        assertNotNull(shape);
    }


    @Test(expected = OslcCoreInvalidRepresentationException.class)
    public void invalidShapeWithInlinedResourceTest() throws OslcCoreApplicationException, URISyntaxException {
        ResourceShape shape = ResourceShapeFactory.createResourceShape("http://localhost:8080", 
                OslcConstants.PATH_RESOURCE_SHAPES, 
                "http://test.net/ns/test#", 
                ShapeWithWrongReferenceToInline.class);
        assertNotNull(shape);
    }

    @Test(expected = OslcCoreInvalidValueTypeException.class)
    public void invalidShapeWithReferencedResourceTest() throws OslcCoreApplicationException, URISyntaxException {
        ResourceShape shape = ResourceShapeFactory.createResourceShape("http://localhost:8080", 
                OslcConstants.PATH_RESOURCE_SHAPES, 
                "http://test.net/ns/test#", 
                ShapeWithWrongReferenceToResource.class);
        assertNotNull(shape);
    }

    @Test
    public void validInstanceMarshalsAndUnmarshalsAsModelsTest() throws NoSuchMethodException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, DatatypeConfigurationException, OslcCoreApplicationException, InstantiationException, SecurityException, URISyntaxException {
         ShapeWithWorkingReferences sourceResource = createShapeInstanceWithWorkingReferences("1");
         final Model sourceModel = JenaModelHelper.createJenaModel(new Object[]{sourceResource});
         ShapeWithWorkingReferences resultingResource = JenaModelHelper.unmarshalSingle(sourceModel, ShapeWithWorkingReferences.class);
         final Model resultingModel = JenaModelHelper.createJenaModel(new Object[]{resultingResource});
         
         assertThat(sourceModel).isomorphicWith(resultingModel);
    }

    @Test
    public void validInstanceMarshalsAndUnmarshalsAsResourcesTest2() throws NoSuchMethodException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, DatatypeConfigurationException, OslcCoreApplicationException, InstantiationException, SecurityException, URISyntaxException {
         ShapeWithWorkingReferences sourceResource = createShapeInstanceWithWorkingReferences("1");
         final Model sourceModel = JenaModelHelper.createJenaModel(new Object[]{sourceResource});
         ShapeWithWorkingReferences resultingResource = JenaModelHelper.unmarshalSingle(sourceModel, ShapeWithWorkingReferences.class);
         final Model resultingModel = JenaModelHelper.createJenaModel(new Object[]{resultingResource});
         
         //Assert that the identifier of the inlined resource remains intact.
         assertTrue(sourceResource.getInlines().getIdentifier().equals(resultingResource.getInlines().getIdentifier()));

         //Assert that the URL of the inlined resource remains intact.
         assertTrue(sourceResource.getInlines().getAbout().equals(resultingResource.getInlines().getAbout()));

         
         //Assert that inlined resources, with multiple cardinality, remains intact.
         InlinedShape s1 = (InlinedShape) sourceResource.getInlinesMany().toArray()[0];
         InlinedShape r1 = (InlinedShape) resultingResource.getInlinesMany().toArray()[0];
         assertTrue(s1.getIdentifier().equals(r1.getIdentifier()));
         
         //Assert that the identifier of the Local resource remains intact.
         assertTrue(sourceResource.getReferencesAsLocal().getIdentifier().equals(resultingResource.getReferencesAsLocal().getIdentifier()));

         //Assert that the URL of the local resource is null.
         assertNull(sourceResource.getReferencesAsLocal().getAbout());
         assertNull(resultingResource.getReferencesAsLocal().getAbout());

         //Assert that the URL of the referenced resource remains intact.
         assertNotNull(resultingResource.getReferencesAsResource().getValue());
         assertTrue(sourceResource.getReferencesAsResource().getValue().equals(resultingResource.getReferencesAsResource().getValue()));
    }
    
}
