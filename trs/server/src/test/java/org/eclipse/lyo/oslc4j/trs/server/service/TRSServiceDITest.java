package org.eclipse.lyo.oslc4j.trs.server.service;

import java.net.MalformedURLException;
import java.util.ArrayList;
import javax.inject.Singleton;
import javax.ws.rs.core.Application;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import org.apache.http.client.utils.URIBuilder;
import org.eclipse.lyo.core.trs.Base;
import org.eclipse.lyo.core.trs.TrackedResourceSet;
import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.server.provider.jena.JenaProvidersRegistry;
import org.eclipse.lyo.oslc4j.trs.server.InmemPagedTrs;
import org.eclipse.lyo.oslc4j.trs.server.InmemPagedTrsTest;
import org.eclipse.lyo.oslc4j.trs.server.PagedTrs;
import org.glassfish.hk2.utilities.binding.AbstractBinder;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.server.ResourceConfig;
import org.glassfish.jersey.test.DeploymentContext;
import org.glassfish.jersey.test.JerseyTest;
import org.glassfish.jersey.test.ServletDeploymentContext;
import org.glassfish.jersey.test.TestProperties;
import org.glassfish.jersey.test.grizzly.GrizzlyTestContainerFactory;
import org.glassfish.jersey.test.spi.TestContainerException;
import org.glassfish.jersey.test.spi.TestContainerFactory;
import org.junit.Before;
import org.junit.Test;

import static org.assertj.core.api.Assertions.*;

public class TRSServiceDITest extends JerseyTest {

    @Override
    protected TestContainerFactory getTestContainerFactory() throws TestContainerException {
        return new GrizzlyTestContainerFactory();
    }

    @Override
    protected DeploymentContext configureDeployment() {
        return ServletDeploymentContext.builder(configure()).build();
    }

    @Override
    protected void configureClient(final ClientConfig config) {
        JenaProvidersRegistry.getProviders().forEach(config::register);
    }

    @Override
    protected Application configure() {
        enable(TestProperties.LOG_TRAFFIC);
        enable(TestProperties.DUMP_ENTITY);

        try {
            OSLC4JUtils.setPublicURI(getBaseUri().toString());
        } catch (MalformedURLException e) {
            System.err.println("Can't set the OSLC4J public URI");
        }
        OSLC4JUtils.setServletPath("/");

        return new ResourceConfig(TrackedResourceSetService.class)
                .register(new AbstractBinder() {
                    @Override
                    protected void configure() {
                        bind(new InmemPagedTrs(5, 5,
                                UriBuilder.fromUri(OSLC4JUtils.getServletURI()).path("trs").build(),
                                new ArrayList<>(0))).to(PagedTrs.class);
                    }
                })
                .registerClasses(JenaProvidersRegistry.getProviders());
    }

    @Test
    public void testSetEndpoint() {
        Response response = target("/trs").request("text/turtle").get();

        assertThat(response.getStatusInfo().getFamily()).isEqualTo(
                Response.Status.Family.SUCCESSFUL);

        final TrackedResourceSet trs = response.readEntity(TrackedResourceSet.class);
        assertThat(trs.getBase()).hasPath("/trs/base");
    }

    @Test
    public void testBaseDoubleSlashEndpoint() {
        Response response = target("/trs/base").request("text/turtle").get();

        assertThat(response.getStatusInfo().getFamily()).isEqualTo(
                Response.Status.Family.SUCCESSFUL);

        final Base trsBase = response.readEntity(Base.class);
        final String uriAfterHttps = trsBase.getAbout().toString().split(":", 2)[1].substring(2);
        assertThat(uriAfterHttps).doesNotContain("//trs");
    }

    @Test
    public void testBaseEndpoint() {
        Response response = target("/trs/base").request("text/turtle").get();

        assertThat(response.getStatusInfo().getFamily()).isEqualTo(
                Response.Status.Family.SUCCESSFUL);

        final Base trsBase = response.readEntity(Base.class);
        assertThat(trsBase.getMembers()).hasSize(0);
    }

}
