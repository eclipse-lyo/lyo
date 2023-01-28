package org.eclipse.lyo.oslc4j.provider.json4j;

import org.apache.wink.json4j.JSONException;
import org.apache.wink.json4j.JSONObject;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.ResponseInfoArray;
import org.eclipse.lyo.oslc4j.provider.json4j.test.resources.TestResource;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.server.ResourceConfig;
import org.glassfish.jersey.server.ServerProperties;
import org.glassfish.jersey.servlet.ServletContainer;
import org.glassfish.jersey.test.DeploymentContext;
import org.glassfish.jersey.test.JerseyTest;
import org.glassfish.jersey.test.ServletDeploymentContext;
import org.glassfish.jersey.test.TestProperties;
import org.glassfish.jersey.test.grizzly.GrizzlyTestContainerFactory;
import org.glassfish.jersey.test.grizzly.GrizzlyWebTestContainerFactory;
import org.glassfish.jersey.test.spi.TestContainerException;
import org.glassfish.jersey.test.spi.TestContainerFactory;
import org.junit.Test;

import javax.ws.rs.core.Application;
import javax.ws.rs.core.GenericType;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.xml.datatype.DatatypeConfigurationException;

import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Set;

import static org.assertj.core.api.Assertions.*;

public class ProviderCollectionTest extends JerseyTest {

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


    @Test
    public void testQueryOk()
        throws JSONException, DatatypeConfigurationException, URISyntaxException, OslcCoreApplicationException,
        InvocationTargetException, IllegalAccessException, InstantiationException {
        Response response = target("/test/qc-array-rdf").request(MediaType.APPLICATION_JSON).get();

        String responseStr = response.readEntity(String.class);
        JSONObject responseJSON = new JSONObject(responseStr);
        Object[] objects = JsonHelper.fromJSON(responseJSON, TestResource.class);

        assertThat(response.getStatusInfo().getFamily()).isEqualTo(
            Response.Status.Family.SUCCESSFUL);

        assertThat(responseStr).isNotEmpty();
        // OSLC Query response
        assertThat(responseJSON.get("oslc:responseInfo")).isNotNull();
        // Everything is in place
        // TODO check about URI and aproperty
        assertThat(objects).hasSize(10);
    }

    @Test
    public void testQueryArray() {
        Response response = target("/test/qc-array-rdf").request(MediaType.APPLICATION_JSON).get();

        TestResource[] rdfResponse =
            response.readEntity(((TestResource[]) Array.newInstance(TestResource.class, 0)).getClass());

        assertThat(response.getStatusInfo().getFamily()).isEqualTo(
            Response.Status.Family.SUCCESSFUL);

        assertThat(rdfResponse).hasSize(10);
    }

    @Test
    public void testQueryList() {
        Response response = target("/test/qc-array-rdf").request(MediaType.APPLICATION_JSON).get();
        List<TestResource> rdfResponse = response.readEntity(new GenericType<List<TestResource>>() {});

        assertThat(response.getStatusInfo().getFamily()).isEqualTo(
            Response.Status.Family.SUCCESSFUL);

        assertThat(rdfResponse).hasSize(10);
    }
}
