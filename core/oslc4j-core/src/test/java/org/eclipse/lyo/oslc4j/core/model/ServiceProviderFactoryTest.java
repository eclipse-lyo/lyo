package org.eclipse.lyo.oslc4j.core.model;

import java.util.HashMap;
import javax.ws.rs.Path;
import org.eclipse.lyo.oslc4j.core.annotation.OslcCreationFactory;
import static org.junit.Assert.*;
import org.junit.Test;
import org.mockito.Mockito;

/**
 * Unit test for verifying {@link ServiceProviderFactory} URI-related code.
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 2.2.0
 */
public class ServiceProviderFactoryTest {

    static final String FAKE_SERVICE = "fakeService";
    private static final String FAKE_CREATION_FACTORY = "fakeCreationFactory";
    private static final String FAKE_CREATION_FACTORY_WITH_ID = "fakeCreationFactory/{id}";
    private static final String ID_VALUE = "23";
    private static final String FAKE_BASE = "http://fake.base";

    @Test
    public void createCreationFactory() throws Exception {
        HashMap<String, Object> pathParameterValues = new HashMap<>();
        CreationFactory creationFactory = ServiceProviderFactory.createCreationFactory(FAKE_BASE,
                pathParameterValues, FakeService.class.getMethod("fakeCreationFactory"));

        assertEquals(getCreationFactoryUri(FAKE_SERVICE, FAKE_CREATION_FACTORY),
                creationFactory.getCreation().toString());
    }

    @Test
    public void createCreationFactoryOnParametrisedMethod() throws Exception {
        HashMap<String, Object> pathParameterValues = new HashMap<>();
        pathParameterValues.put("id", ID_VALUE);
        CreationFactory creationFactory = ServiceProviderFactory.createCreationFactory(FAKE_BASE,
                pathParameterValues, FakeService.class.getMethod("fakeCreationFactoryWithId"));

        String substitutedURI = FAKE_CREATION_FACTORY_WITH_ID.replace("{id}", ID_VALUE);
        assertEquals(getCreationFactoryUri(FAKE_SERVICE, substitutedURI),
                creationFactory.getCreation().toString());
    }

    @Test
    public void createCreationFactoryOnParametrisedMethodWithClassParam() throws Exception {
        Path classPath = Mockito.mock(Path.class);
        Path methodPath = Mockito.mock(Path.class);
        OslcCreationFactory creationFactoryAnnotation = Mockito.mock(OslcCreationFactory.class);

        Mockito.when(classPath.value()).thenReturn("mockService/{id}");
        Mockito.when(methodPath.value()).thenReturn("mockFactory/{id}");
        Mockito.when(creationFactoryAnnotation.resourceShapes()).thenReturn(new String[0]);
        Mockito.when(creationFactoryAnnotation.resourceTypes()).thenReturn(new String[0]);
        Mockito.when(creationFactoryAnnotation.usages()).thenReturn(new String[0]);

        HashMap<String, Object> pathParameterValues = new HashMap<>();
        pathParameterValues.put("id", ID_VALUE);
        CreationFactory creationFactory = ServiceProviderFactory.createCreationFactory(FAKE_BASE,
                pathParameterValues, classPath, creationFactoryAnnotation, methodPath);

        assertEquals(getUri("mockService/23/mockFactory/23"),
                creationFactory.getCreation().toString());
    }

    @Test(expected = IllegalArgumentException.class)
    public void createCreationFactoryOnParametrisedMethodWithNullParam() throws Exception {
        HashMap<String, Object> pathParameterValues = new HashMap<>();
        pathParameterValues.put("id", null);
        ServiceProviderFactory.createCreationFactory(FAKE_BASE, pathParameterValues,
                FakeService.class.getMethod("fakeCreationFactoryWithId"));
    }

    private String getCreationFactoryUri(final String fakeService,
            final String fakeCreationFactory) {
        String s = fakeService + "/" + fakeCreationFactory;
        return getUri(s);
    }

    private String getUri(final String s) {
        return FAKE_BASE + "/" + s;
    }

    @Path(FAKE_SERVICE)
    class FakeService {
        @OslcCreationFactory(label = "fake creation factory", title = "fake")
        @Path(FAKE_CREATION_FACTORY)
        public void fakeCreationFactory() {}

        @OslcCreationFactory(label = "fake creation factory", title = "fake")
        @Path(FAKE_CREATION_FACTORY_WITH_ID)
        public void fakeCreationFactoryWithId() {}
    }

}
