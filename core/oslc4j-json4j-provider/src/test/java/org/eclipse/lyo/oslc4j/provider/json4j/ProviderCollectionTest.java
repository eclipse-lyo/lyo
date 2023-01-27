package org.eclipse.lyo.oslc4j.provider.json4j;

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
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import static org.assertj.core.api.Assertions.*;

public class ProviderCollectionTest extends JerseyTest {

    @Override
    protected TestContainerFactory getTestContainerFactory() throws TestContainerException {
        return new GrizzlyWebTestContainerFactory();
//        return new JettyTestContainerFactory();
    }

    @Override
    protected DeploymentContext configureDeployment() {
        // w/o Servlet injection support
        return ServletDeploymentContext.builder(configure())
            .initParam(ServerProperties.PROVIDER_PACKAGES, this.getClass().getPackage().getName())
            .build();

        // with Servlet injection support

//        return ServletDeploymentContext.
////            forServlet(new ServletContainer(configure()))
//            builder(configure())
//            .initParam(ServerProperties.PROVIDER_PACKAGES, this.getClass().getPackage().getName())
//            .build();
        // see https://stackoverflow.com/questions/28436040/hk2-is-not-injecting-the-httpservletrequest-with-jersey
        // for session support

//        return ServletDeploymentContext.builder(new ResourceConfig())
//            .initParam(ServerProperties.PROVIDER_PACKAGES, this.getClass().getPackage().getName())
//            .build();
    }


    @Override
    protected void configureClient(final ClientConfig config) {
        Json4JProvidersRegistry.getProviders().forEach(config::register);
    }


    @Override
    protected ResourceConfig configure() {
        enable(TestProperties.LOG_TRAFFIC);
        enable(TestProperties.DUMP_ENTITY);

        return new ResourceConfig(ProviderCollectionResource.class)
            .registerClasses(Json4JUpdatedProvidersRegistry.getProviders());
    }

    @Test
    public void testSetEndpoint() {
        Response response = target("/test").request().get();

        assertThat(response.getStatusInfo().getFamily()).isEqualTo(
                Response.Status.Family.SUCCESSFUL);
    }


    @Test
    public void testQueryArrayRdf() {
        Response response = target("/test/qc-array-rdf").request(MediaType.APPLICATION_JSON).get();

        assertThat(response.getStatusInfo().getFamily()).isEqualTo(
            Response.Status.Family.SUCCESSFUL);
    }

}
