package org.eclipse.lyo.oslc4j.provider.jena;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assumptions.assumeFalse;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import jakarta.ws.rs.core.GenericType;
import jakarta.ws.rs.core.Response;
import java.io.InputStream;
import java.lang.reflect.Array;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFLanguages;
import org.apache.jena.vocabulary.RDF;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.provider.jena.test.resources.TestResource;
import org.eclipse.lyo.oslc4j.provider.jena.test.resources.TestResourceNonQ;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.server.ResourceConfig;
import org.glassfish.jersey.servlet.ServletContainer;
import org.glassfish.jersey.test.DeploymentContext;
import org.glassfish.jersey.test.JerseyTest;
import org.glassfish.jersey.test.ServletDeploymentContext;
import org.glassfish.jersey.test.TestProperties;
import org.glassfish.jersey.test.grizzly.GrizzlyWebTestContainerFactory;
import org.glassfish.jersey.test.spi.TestContainerException;
import org.glassfish.jersey.test.spi.TestContainerFactory;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.ArgumentsProvider;
import org.junit.jupiter.params.provider.ArgumentsSource;

public class RdfJenaProviderCollectionTest extends JerseyTest {

    @Override
    protected TestContainerFactory getTestContainerFactory() throws TestContainerException {
        // With Servlet initialization
        return new GrizzlyWebTestContainerFactory();
        // Jetty has no servlet-enabled factory

        // Only JAX-RS init, no Servlet
        //        return new GrizzlyTestContainerFactory();
        //        return new JettyTestContainerFactory();
    }

    @Override
    protected DeploymentContext configureDeployment() {
        return ServletDeploymentContext.builder(configure())
                // either the same configure() value needs to be passed to builder() and servlet()
                // or PROVIDER_PACKAGES needs to be supplied
                .servlet(new ServletContainer(configure()))
                //            .initParam(ServerProperties.PROVIDER_PACKAGES,
                // this.getClass().getPackage().getName())
                .build();
        // see
        // https://stackoverflow.com/questions/28436040/hk2-is-not-injecting-the-httpservletrequest-with-jersey
        // for session support
    }

    @Override
    protected void configureClient(final ClientConfig config) {
        configureProviders().forEach(config::register);
    }

    @Override
    protected ResourceConfig configure() {
        enable(TestProperties.LOG_TRAFFIC);
        enable(TestProperties.DUMP_ENTITY);

        return new ResourceConfig(ProviderCollectionResource.class)
                .registerClasses(configureProviders());
    }

    private static Set<Class<?>> configureProviders() {
        return JenaProvidersRegistry.getProviders();
        // FIXME: tests fail on "simple" or "updated" sets of providers
        //        return JenaSimpleProvidersRegistry.getProviders();
        //        return JenaUpdatedProvidersRegistry.getProviders();
    }

    @Test
    public void testSetEndpoint() {
        Response response = target("/test").request().get();

        assertThat(response.getStatusInfo().getFamily())
                .isEqualTo(Response.Status.Family.SUCCESSFUL);
    }

    @ParameterizedTest
    @ArgumentsSource(ProviderCollectionTestArguments.class)
    public void testListQuery(String path, boolean isExpectedQuery) {
        assumeTrue(isExpectedQuery);
        Response response = target(path).request(OslcMediaType.TEXT_TURTLE_TYPE).get();
        response.bufferEntity();

        assertThat(response.getStatusInfo().getFamily())
                .isEqualTo(Response.Status.Family.SUCCESSFUL);
        List<?> rdfResponse = response.readEntity(new GenericType<List<TestResource>>() {});
        String rdfResponseString = response.readEntity(String.class);
        InputStream rdfResponseStream = response.readEntity(InputStream.class);
        Model model = ModelFactory.createDefaultModel();
        RDFDataMgr.read(model, rdfResponseStream, RDFLanguages.TURTLE);
        Resource responseInfoType = model.createResource(OslcConstants.TYPE_RESPONSE_INFO);

        boolean isQueryResponse = model.contains(null, RDF.type, responseInfoType);
        assertThat(isQueryResponse).isEqualTo(isExpectedQuery);

        assertThat(rdfResponse).hasSize(10);
    }

    @ParameterizedTest
    @ArgumentsSource(ProviderCollectionTestArguments.class)
    public void testListNonQuery(String path, boolean isExpectedQuery) {
        assumeFalse(isExpectedQuery);
        Response response = target(path).request(OslcMediaType.TEXT_TURTLE_TYPE).get();
        response.bufferEntity();

        assertThat(response.getStatusInfo().getFamily())
                .isEqualTo(Response.Status.Family.SUCCESSFUL);
        List<?> rdfResponse = response.readEntity(new GenericType<List<TestResource>>() {});
        InputStream rdfResponseStream = response.readEntity(InputStream.class);
        Model model = ModelFactory.createDefaultModel();
        RDFDataMgr.read(model, rdfResponseStream, RDFLanguages.TURTLE);
        Resource responseInfoType = model.createResource(OslcConstants.TYPE_RESPONSE_INFO);

        boolean isQueryResponse = model.contains(null, RDF.type, responseInfoType);
        assertThat(isQueryResponse).isEqualTo(isExpectedQuery);

        assertThat(rdfResponse).hasSize(10);
    }

    @ParameterizedTest
    @ArgumentsSource(ProviderCollectionTestArguments.class)
    public void testArrayQuery(String path, boolean isExpectedQuery) {
        assumeTrue(isExpectedQuery);
        Response response = target(path).request(OslcMediaType.APPLICATION_RDF_XML).get();
        response.bufferEntity();

        assertThat(response.getStatusInfo().getFamily())
                .isEqualTo(Response.Status.Family.SUCCESSFUL);
        TestResource[] rdfResponse =
                response.readEntity(
                        ((TestResource[]) Array.newInstance(TestResource.class, 0)).getClass());
        InputStream rdfResponseStream = response.readEntity(InputStream.class);
        Model model = ModelFactory.createDefaultModel();
        RDFDataMgr.read(model, rdfResponseStream, RDFLanguages.RDFXML);
        Resource responseInfoType = model.createResource(OslcConstants.TYPE_RESPONSE_INFO);

        boolean isQueryResponse = model.contains(null, RDF.type, responseInfoType);
        assertThat(isQueryResponse).isEqualTo(isExpectedQuery);

        if (isExpectedQuery) {
            assertThat(rdfResponse).hasSize(10);
        }
    }

    @ParameterizedTest
    @ArgumentsSource(ProviderCollectionTestArguments.class)
    public void testArrayNonQuery(
            String path, boolean isExpectedQuery, Class<?> _ignore, String mimeType) {
        assumeFalse(isExpectedQuery);
        Response response = target(path).request(mimeType).get();
        response.bufferEntity();

        assertThat(response.getStatusInfo().getFamily())
                .isEqualTo(Response.Status.Family.SUCCESSFUL);
        //        Object[] rdfResponse = response.readEntity(((Object[])
        // Array.newInstance(Object.class, 0)).getClass());
        TestResourceNonQ[] rdfResponse =
                response.readEntity(
                        ((TestResourceNonQ[]) Array.newInstance(TestResourceNonQ.class, 0))
                                .getClass());

        InputStream rdfResponseStream = response.readEntity(InputStream.class);
        Model model = ModelFactory.createDefaultModel();
        RDFDataMgr.read(model, rdfResponseStream, mapMimeType(mimeType));
        Resource responseInfoType = model.createResource(OslcConstants.TYPE_RESPONSE_INFO);

        boolean isQueryResponse = model.contains(null, RDF.type, responseInfoType);
        assertThat(isQueryResponse).isEqualTo(isExpectedQuery);

        assertThat(rdfResponse).hasSize(10);
    }

    private Lang mapMimeType(String mimeType) {
        if (OslcMediaType.APPLICATION_RDF_XML.equals(mimeType)) {
            return RDFLanguages.RDFXML;
        } else if (OslcMediaType.TEXT_TURTLE.equals(mimeType)) {
            return RDFLanguages.TURTLE;
        } else if (OslcMediaType.APPLICATION_JSON_LD.equals(mimeType)) {
            return RDFLanguages.JSONLD10;
        } else if (OslcMediaType.APPLICATION_XML.equals(mimeType)) {
            return RDFLanguages.RDFXML;
        }
        return null;
    }

    @ParameterizedTest
    @ArgumentsSource(ProviderCollectionTestArguments.class)
    public void testJMH(String path, boolean isExpectedQuery, Class<?> klass) {
        Response response = target(path).request(OslcMediaType.TEXT_TURTLE).get();
        response.bufferEntity();

        assertThat(response.getStatusInfo().getFamily())
                .isEqualTo(Response.Status.Family.SUCCESSFUL);
        //        String rdfResponseString = response.readEntity(String.class);
        InputStream rdfResponseStream = response.readEntity(InputStream.class);
        Model model = ModelFactory.createDefaultModel();
        RDFDataMgr.read(model, rdfResponseStream, RDFLanguages.TURTLE);
        Resource responseInfoType = model.createResource(OslcConstants.TYPE_RESPONSE_INFO);

        Object[] testResources = JenaModelHelper.unmarshal(model, klass);

        boolean isQueryResponse = model.contains(null, RDF.type, responseInfoType);
        assertThat(isQueryResponse)
                .withFailMessage(
                        () ->
                                "The server is expected to return a ResponseInfo type of response: "
                                        + isExpectedQuery)
                .isEqualTo(isExpectedQuery);

        assertThat(testResources).hasSize(10);
    }

    private static class ProviderCollectionTestArguments implements ArgumentsProvider {
        @Override
        public Stream<? extends Arguments> provideArguments(ExtensionContext extensionContext)
                throws Exception {
            Stream.Builder<Arguments> builder = Stream.builder();
            for (String mimeType :
                    List.of(
                            OslcMediaType.APPLICATION_RDF_XML,
                            OslcMediaType.TEXT_TURTLE,
                            OslcMediaType.APPLICATION_JSON_LD,
                            OslcMediaType.APPLICATION_XML)) {
                builder.accept(
                        Arguments.of(
                                "/test/qc-array-method-rdf", true, TestResource.class, mimeType));
                builder.accept(
                        Arguments.of(
                                "/test/qc-array-response-rdf", true, TestResource.class, mimeType));
                builder.accept(
                        Arguments.of(
                                "/test/qc-coll-method-rdf", true, TestResource.class, mimeType));
                builder.accept(
                        Arguments.of(
                                "/test/qc-coll-response-rdf", true, TestResource.class, mimeType));
                builder.accept(
                        Arguments.of(
                                "/test/nonqc-array-response-rdf",
                                false,
                                TestResourceNonQ.class,
                                mimeType));
                builder.accept(
                        Arguments.of(
                                "/test/nonqc-nonq-coll-response-rdf",
                                false,
                                TestResourceNonQ.class,
                                mimeType));
                builder.accept(
                        Arguments.of(
                                "/test/qc-nonq-coll-response-rdf",
                                false,
                                TestResourceNonQ.class,
                                mimeType));
                builder.accept(
                        Arguments.of(
                                "/test/qc-nonq-array-response-rdf",
                                false,
                                TestResourceNonQ.class,
                                mimeType));
            }

            return builder.build();
        }
    }
}
