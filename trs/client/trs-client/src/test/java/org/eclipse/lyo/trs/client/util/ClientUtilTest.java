package org.eclipse.lyo.trs.client.util;

import static com.diffplug.selfie.Selfie.expectSelfie;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockserver.integration.ClientAndServer.startClientAndServer;
import static org.mockserver.model.HttpRequest.request;
import static org.mockserver.model.HttpResponse.response;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.sys.JenaSystem;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
import org.eclipse.lyo.core.trs.Base;
import org.eclipse.lyo.core.trs.ChangeEvent;
import org.eclipse.lyo.core.trs.ChangeLog;
import org.eclipse.lyo.core.trs.TrackedResourceSet;
import org.eclipse.lyo.trs.client.exceptions.TrsEndpointConfigException;
import org.eclipse.lyo.trs.client.exceptions.TrsEndpointErrorException;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockserver.client.MockServerClient;
import org.mockserver.integration.ClientAndServer;
import org.mockserver.model.MediaType;

import jakarta.ws.rs.client.Client;
import jakarta.ws.rs.client.ClientBuilder;
import jakarta.ws.rs.core.Response;

class ClientUtilTest {

    private static ClientAndServer mockServer;
    private static MockServerClient mockServerClient;
    private static String baseUri;
    private static Client jaxrsClient;

    static {
        JenaSystem.init();
    }

    @BeforeAll
    static void startServer() {
        mockServer = startClientAndServer(0);
        mockServerClient = new MockServerClient("localhost", mockServer.getPort());
        baseUri = "http://localhost:" + mockServer.getPort();
        jaxrsClient = ClientBuilder.newClient();
    }

    @AfterAll
    static void stopServer() {
        mockServer.stop();
        jaxrsClient.close();
    }

    @Test
    void testExtractTrsFromRdfModel() throws Exception {
        Model model = loadModel("01-trs-main.rdf");
        TrackedResourceSet trs = ClientUtil.extractTrsFromRdfModel(model);
        
        StringBuilder sb = new StringBuilder();
        sb.append("TRS About: ").append(trs.getAbout()).append("\n");
        sb.append("TRS Base: ").append(trs.getBase()).append("\n");
        sb.append("TRS ChangeLog About: ").append(trs.getChangeLog().getAbout()).append("\n");
        sb.append("TRS ChangeLog Changes count: ").append(trs.getChangeLog().getChange().size()).append("\n");
        
        expectSelfie(stabilize(sb.toString())).toMatchDisk();
    }

    @Test
    void testExtractBaseFromRdfModelDirectContainer() throws Exception {
        Model model = loadModel("02-trs-base.rdf");
        Base base = ClientUtil.extractBaseFromRdfModel(model);
        
        StringBuilder sb = new StringBuilder();
        sb.append("Base About: ").append(base.getAbout()).append("\n");
        sb.append("Base CutoffEvent: ").append(base.getCutoffEvent()).append("\n");
        sb.append("Base Members count: ").append(base.getMembers().size()).append("\n");
        for (URI member : base.getMembers()) {
            sb.append("  Member: ").append(member).append("\n");
        }
        
        expectSelfie(stabilize(sb.toString())).toMatchDisk();
    }

    @Test
    void testExtractBaseFromRdfModelStandardContainer() throws Exception {
        Model model = ModelFactory.createDefaultModel();
        String trsNs = "http://open-services.net/ns/core/trs#";
        String ldpNs = "http://www.w3.org/ns/ldp#";
        
        Resource baseRes = model.createResource(baseUri + "/base");
        baseRes.addProperty(RDF.type, model.createResource(ldpNs + "Container"));
        baseRes.addProperty(model.createProperty(trsNs + "cutoffEvent"), model.createResource(baseUri + "/event1"));
        baseRes.addProperty(RDFS.member, model.createResource(baseUri + "/res1"));
        baseRes.addProperty(RDFS.member, model.createResource(baseUri + "/res2"));

        Base base = ClientUtil.extractBaseFromRdfModel(model);
        
        StringBuilder sb = new StringBuilder();
        sb.append("Base About: ").append(base.getAbout()).append("\n");
        sb.append("Base CutoffEvent: ").append(base.getCutoffEvent()).append("\n");
        sb.append("Base Members count: ").append(base.getMembers().size()).append("\n");
        for (URI member : base.getMembers()) {
            sb.append("  Member: ").append(member).append("\n");
        }
        
        expectSelfie(stabilize(sb.toString())).toMatchDisk();
    }

    @Test
    void testExtractBaseFromRdfModelDirectContainerRdfsMember() throws Exception {
        Model model = ModelFactory.createDefaultModel();
        String ldpNs = "http://www.w3.org/ns/ldp#";
        
        Resource baseRes = model.createResource(baseUri + "/base");
        baseRes.addProperty(RDF.type, model.createResource(ldpNs + "DirectContainer"));
        baseRes.addProperty(RDFS.member, model.createResource(baseUri + "/res1"));

        Base base = ClientUtil.extractBaseFromRdfModel(model);
        
        StringBuilder sb = new StringBuilder();
        sb.append("Base About: ").append(base.getAbout()).append("\n");
        sb.append("Base Members count: ").append(base.getMembers().size()).append("\n");
        sb.append("  Member: ").append(base.getMembers().get(0)).append("\n");
        
        expectSelfie(stabilize(sb.toString())).toMatchDisk();
    }

    @Test
    void testExtractChangeLogFromRdfModel() throws Exception {
        Model model = loadModel("01-trs-main.rdf");
        ChangeLog cl = ClientUtil.extractChangeLogFromRdfModel(model);
        
        expectSelfie("Change count: " + cl.getChange().size()).toMatchDisk();
        List<ChangeEvent> changes = cl.getChange();
        if (!changes.isEmpty()) {
            expectSelfie(stabilize("First change about: " + changes.get(0).getAbout())).toMatchDisk("firstChange");
        }
    }

    @Test
    void testExtractResourceFromResponse() throws Exception {
        String rdfBody = getFixtureBody("01-trs-main.rdf");
        mockServerClient
                .when(request().withPath("/trs"))
                .respond(response()
                        .withStatusCode(200)
                        .withContentType(MediaType.parse("application/rdf+xml"))
                        .withBody(rdfBody));

        try (Response response = jaxrsClient.target(baseUri + "/trs").request().get()) {
            Model model = (Model) ClientUtil.extractResourceFromResponse(response, Model.class);
            expectSelfie(String.valueOf(model.size())).toMatchDisk("modelSize");
        }
    }

    @Test
    void testExtractResourceFromResponseClientError() {
        mockServerClient
                .when(request().withPath("/404"))
                .respond(response().withStatusCode(404));

        try (Response response = jaxrsClient.target(baseUri + "/404").request().get()) {
            assertThrows(TrsEndpointConfigException.class, () -> 
                ClientUtil.extractResourceFromResponse(response, Model.class)
            );
        }
    }

    @Test
    void testExtractResourceFromResponseServerError() {
        mockServerClient
                .when(request().withPath("/500"))
                .respond(response().withStatusCode(500));

        try (Response response = jaxrsClient.target(baseUri + "/500").request().get()) {
            assertThrows(TrsEndpointErrorException.class, () -> 
                ClientUtil.extractResourceFromResponse(response, Model.class)
            );
        }
    }

    private Model loadModel(String fixtureName) throws IOException {
        String body = getFixtureBody(fixtureName);
        Model model = ModelFactory.createDefaultModel();
        model.read(new ByteArrayInputStream(body.getBytes(StandardCharsets.UTF_8)), null, "RDF/XML");
        return model;
    }

    private String getFixtureBody(String fixtureName) throws IOException {
        Path path = Path.of("src/test/resources/fixtures", fixtureName);
        return Files.readString(path).replace("http://localhost:8080", baseUri);
    }

    private String stabilize(String content) {
        return content.replace(baseUri, "http://localhost:PORT");
    }
}