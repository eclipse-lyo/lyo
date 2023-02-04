package org.eclipse.lyo.oslc4j.provider.json4j;

import org.apache.wink.json4j.JSONException;
import org.apache.wink.json4j.JSONObject;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.provider.json4j.test.resources.TestResource;
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

import javax.ws.rs.core.GenericType;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.xml.datatype.DatatypeConfigurationException;
import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.*;

public class OslcJsonProviderCollectionTest extends JerseyTest {

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
//            .initParam(ServerProperties.PROVIDER_PACKAGES, this.getClass().getPackage().getName())
            .build();
        // see https://stackoverflow.com/questions/28436040/hk2-is-not-injecting-the-httpservletrequest-with-jersey
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
        return Json4JProvidersRegistry.getProviders();
        //FIXME: tests fail on "simple" or "updated" sets of providers
//        return Json4JSimpleProvidersRegistry.getProviders();
//        return Json4JUpdatedProvidersRegistry.getProviders();
    }

    @Test
    public void testSetEndpoint() {
        Response response = target("/test").request().get();

        assertThat(response.getStatusInfo().getFamily()).isEqualTo(
            Response.Status.Family.SUCCESSFUL);
    }


    @ParameterizedTest
    @ArgumentsSource(ProviderCollectionTestArguments.class)
    public void testQueryList(String path, boolean isExpectedQuery)
        throws JSONException, DatatypeConfigurationException, URISyntaxException, OslcCoreApplicationException,
        InvocationTargetException, IllegalAccessException, InstantiationException {
        Response response = target(path).request(MediaType.APPLICATION_JSON).get();
        response.bufferEntity();

        assertThat(response.getStatusInfo().getFamily()).isEqualTo(
            Response.Status.Family.SUCCESSFUL);
        List<TestResource> rdfResponse = response.readEntity(new GenericType<List<TestResource>>() {});
        String rdfResponseString = response.readEntity(String.class);
        JSONObject responseJSON = new JSONObject(rdfResponseString);
        Object[] objects = JsonHelper.fromJSON(responseJSON, TestResource.class);


        boolean isQueryResponse = responseJSON.get("oslc:responseInfo") != null;
        assertThat(isQueryResponse).isEqualTo(isExpectedQuery);

        assertThat(rdfResponse).hasSize(10);
    }

    /**
     * Want to test Array UNmarshalling at least a few times too
     */
    @Test
    public void testQueryArray() {
        Response response = target("/test/qc-array-method-json").request(MediaType.APPLICATION_JSON).get();

        TestResource[] rdfResponse =
            response.readEntity(((TestResource[]) Array.newInstance(TestResource.class, 0)).getClass());

        assertThat(response.getStatusInfo().getFamily()).isEqualTo(
            Response.Status.Family.SUCCESSFUL);

        assertThat(rdfResponse).hasSize(10);
    }

    private static class ProviderCollectionTestArguments implements ArgumentsProvider {
        @Override
        public Stream<? extends Arguments> provideArguments(ExtensionContext extensionContext) throws Exception {
            return  Stream.of(
                // path, isQueryResponse
                Arguments.of("/test/qc-array-method-json", true)
                ,Arguments.of("/test/qc-array-response-json", true)
                ,Arguments.of("/test/qc-coll-method-json", true)
                ,Arguments.of("/test/qc-coll-response-json", true)

                /*
                OSLC JSON provider does not provide marshaling of non-Query resource collections.
                @OslcQueryCapability and @OslcNotQueryResult are ignored.
                 */
                ,Arguments.of("/test/qc-nonq-coll-response-json", true)
                ,Arguments.of("/test/nonqc-nonq-coll-response-json", true)
                ,Arguments.of("/test/nonqc-array-response-json", true)
            );
        }
    }
}
