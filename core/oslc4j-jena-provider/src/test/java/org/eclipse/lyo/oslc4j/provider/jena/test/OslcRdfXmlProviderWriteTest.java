/*
 * Copyright (c) 2026 Contributors to the Eclipse Foundation
 *
 * See the NOTICE file(s) distributed with this work for additional
 * information regarding copyright ownership.
 *
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0, or the Eclipse Distribution License 1.0
 * which is available at http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause
 */
package org.eclipse.lyo.oslc4j.provider.jena.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.MultivaluedHashMap;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.lang.annotation.Annotation;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.eclipse.lyo.oslc4j.core.OSLC4JConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.core.model.ServiceProviderCatalog;
import org.eclipse.lyo.oslc4j.provider.jena.OslcRdfXmlProvider;
import org.eclipse.lyo.oslc4j.provider.jena.test.resources.TestResource;
import org.junit.Test;

/**
 * Regression tests for {@code AbstractOslcRdfXmlProvider#getRdfWriter(String, Model)}: on
 * Jena 6+, {@code model.getWriter("RDF/XML")} is unreliable and can silently return abbreviated
 * (typed-node) output instead of the plain writer. These tests verify the actual RDF/XML text
 * produced for each media type, since both writers emit triple-equivalent models - only the
 * syntactic form (which these tests check) differs.
 */
public class OslcRdfXmlProviderWriteTest {

  @Test
  public void testRdfXmlSettingsAndDeclaration() throws Exception {
    String oldPretty = System.getProperty(OSLC4JConstants.LYO_PRETTY_PRINT);
    String oldJazz = System.getProperty(OSLC4JConstants.LYO_JAZZ_COMPATIBILITY);
    try {
      // Include unset values to verify both defaults as well as all explicit combinations.
      for (String pretty : new String[] {null, "true", "false"}) {
        for (String jazz : new String[] {null, "true", "false"}) {
          restoreProperty(OSLC4JConstants.LYO_PRETTY_PRINT, pretty);
          restoreProperty(OSLC4JConstants.LYO_JAZZ_COMPATIBILITY, jazz);
          for (String mediaType :
              new String[] {"application/rdf+xml", "text/xml", "application/xml"}) {
            TestResource resource = new TestResource(URI.create("http://example.com/resource1"));
            resource.setAproperty("Räksmörgås");
            ByteArrayOutputStream output = new ByteArrayOutputStream();
            new OslcRdfXmlProvider()
                .writeTo(
                    resource,
                    TestResource.class,
                    TestResource.class,
                    new Annotation[] {},
                    MediaType.valueOf(mediaType),
                    new MultivaluedHashMap<>(),
                    output);
            String xml = output.toString(StandardCharsets.UTF_8);
            assertTrue(
                "Missing explicit UTF-8 declaration: " + xml,
                xml.startsWith("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"));
            assertEquals(
                "Exactly one XML declaration", xml.indexOf("<?xml"), xml.lastIndexOf("<?xml"));
            boolean plain =
                !mediaType.equals("application/xml")
                    && (!"false".equals(jazz) || "false".equals(pretty));
            assertEquals(
                "Unexpected RDF/XML form for pretty=" + pretty + ", jazz=" + jazz,
                plain,
                xml.contains("<rdf:Description"));
            Model model = ModelFactory.createDefaultModel();
            RDFDataMgr.read(model, new ByteArrayInputStream(output.toByteArray()), Lang.RDFXML);
            assertTrue(
                "UTF-8 literal must survive serialization",
                model.contains(null, null, "Räksmörgås"));
          }
        }
      }
    } finally {
      restoreProperty(OSLC4JConstants.LYO_PRETTY_PRINT, oldPretty);
      restoreProperty(OSLC4JConstants.LYO_JAZZ_COMPATIBILITY, oldJazz);
    }
  }

  @Test
  public void testCompactTurtleAndJsonLdRoundTrips() throws Exception {
    String oldPretty = System.getProperty(OSLC4JConstants.LYO_PRETTY_PRINT);
    try {
      System.setProperty(OSLC4JConstants.LYO_PRETTY_PRINT, "false");
      testTurtleRoundTrip();
      testJsonLdRoundTrip();
    } finally {
      restoreProperty(OSLC4JConstants.LYO_PRETTY_PRINT, oldPretty);
    }
  }

  @Test
  public void testServiceProviderAbbreviationOverridesJenaPrettyWriter() throws Exception {
    String oldPretty = System.getProperty(OSLC4JConstants.LYO_PRETTY_PRINT);
    String oldJazz = System.getProperty(OSLC4JConstants.LYO_JAZZ_COMPATIBILITY);
    try {
      System.setProperty(OSLC4JConstants.LYO_PRETTY_PRINT, "true");
      System.setProperty(OSLC4JConstants.LYO_JAZZ_COMPATIBILITY, "false");
      testServiceProviderCatalogUsesAbbreviatedWriterWhenForced();
    } finally {
      restoreProperty(OSLC4JConstants.LYO_PRETTY_PRINT, oldPretty);
      restoreProperty(OSLC4JConstants.LYO_JAZZ_COMPATIBILITY, oldJazz);
    }
  }

  private static void restoreProperty(String key, String value) {
    if (value == null) {
      System.clearProperty(key);
    } else {
      System.setProperty(key, value);
    }
  }

  @Test
  public void testApplicationRdfXmlUsesPlainNonAbbreviatedWriter() throws Exception {
    OslcRdfXmlProvider provider = new OslcRdfXmlProvider();

    TestResource resource = new TestResource(URI.create("http://example.com/resource1"));
    resource.setAproperty("value1");

    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
    provider.writeTo(
        resource,
        TestResource.class,
        TestResource.class,
        new Annotation[] {},
        MediaType.valueOf("application/rdf+xml"),
        new MultivaluedHashMap<>(),
        outputStream);

    String rdfXml = outputStream.toString(StandardCharsets.UTF_8);

    // The plain ("RDF/XML") writer always wraps every subject in a literal
    // <rdf:Description> element with an explicit <rdf:type>, never a typed-node
    // shorthand element (e.g. <ex:Test rdf:about="...">) or nested resources.
    assertTrue(
        "Plain RDF/XML writer should use <rdf:Description>, got: " + rdfXml,
        rdfXml.contains("<rdf:Description"));
    assertTrue(
        "Plain RDF/XML writer should express the type as an explicit rdf:type statement",
        rdfXml.contains(TestResource.TEST_RESOURCE_TYPE));
  }

  @Test
  public void testXmlLiteralWithAmpersandIsWellFormedRdfXml() throws Exception {
    // ServiceProvider.title is an XMLLiteral; plain text containing '&' is not well-formed XML.
    assertEquals("Washer & Wiper", roundTripTitle("Washer & Wiper"));
  }

  @Test
  public void testXmlLiteralWithEscapedAmpersandIsKeptAsIs() throws Exception {
    // Already well-formed XML: must not be unescaped (or escaped twice) on the way back.
    assertEquals("Washer &amp; Wiper", roundTripTitle("Washer &amp; Wiper"));
  }

  @Test
  public void testXmlLiteralWithMarkupIsKeptAsIs() throws Exception {
    assertEquals(
        "<b>Washer</b> &amp; <i>Wiper</i>", roundTripTitle("<b>Washer</b> &amp; <i>Wiper</i>"));
  }

  @Test
  public void testExternalXmlLiteralsAreReadAndPassedOnUnchanged() throws Exception {
    // XMLLiterals as other servers may send them: as rdf:parseType="Literal" (embedded XML) or
    // escaped with rdf:datatype. Both encodings of the same XML must give the same title, and
    // writing that title out again must not alter it.
    String[][] cases = {
      {"<dcterms:title rdf:parseType=\"Literal\">Washer &amp; Wiper</dcterms:title>",
        "Washer &amp; Wiper"},
      {"<dcterms:title rdf:parseType=\"Literal\">Washer &amp; <b>Wiper</b></dcterms:title>",
        "Washer &amp; <b>Wiper</b>"},
      {"<dcterms:title rdf:parseType=\"Literal\">"
          + "<b xmlns=\"http://www.w3.org/1999/xhtml\">Washer</b> &amp; Wiper</dcterms:title>",
        "<b xmlns=\"http://www.w3.org/1999/xhtml\">Washer</b> &amp; Wiper"},
      {"<dcterms:title rdf:datatype=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral\">"
          + "Washer &amp;amp; &lt;b&gt;Wiper&lt;/b&gt;</dcterms:title>",
        "Washer &amp; <b>Wiper</b>"},
    };
    for (String[] c : cases) {
      String rdfXml =
          "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\""
              + " xmlns:dcterms=\"http://purl.org/dc/terms/\""
              + " xmlns:oslc=\"http://open-services.net/ns/core#\">"
              + "<oslc:ServiceProvider rdf:about=\"http://example.com/sp1\">"
              + c[0]
              + "</oslc:ServiceProvider></rdf:RDF>";
      String title = readServiceProvider(rdfXml.getBytes(StandardCharsets.UTF_8)).getTitle();
      assertEquals("Read " + c[0], c[1], title);
      assertEquals("Pass on " + c[0], c[1], roundTripTitle(title));
    }
  }

  @SuppressWarnings({"unchecked", "rawtypes"})
  private static ServiceProvider readServiceProvider(byte[] rdfXml) throws Exception {
    return (ServiceProvider)
        new OslcRdfXmlProvider()
            .readFrom(
                (Class) ServiceProvider.class,
                ServiceProvider.class,
                new Annotation[] {},
                MediaType.valueOf("application/rdf+xml"),
                new MultivaluedHashMap<>(),
                new ByteArrayInputStream(rdfXml));
  }

  /** Writes a ServiceProvider with the given title as application/rdf+xml and reads it back. */
  private static String roundTripTitle(String title) throws Exception {
    ServiceProvider serviceProvider = new ServiceProvider();
    serviceProvider.setAbout(URI.create("http://example.com/sp1"));
    serviceProvider.setTitle(title);

    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
    new OslcRdfXmlProvider()
        .writeTo(
            serviceProvider,
            ServiceProvider.class,
            ServiceProvider.class,
            new Annotation[] {},
            MediaType.valueOf("application/rdf+xml"),
            new MultivaluedHashMap<>(),
            outputStream);

    return readServiceProvider(outputStream.toByteArray()).getTitle();
  }

  @Test
  public void testApplicationXmlUsesAbbreviatedWriter() throws Exception {
    OslcRdfXmlProvider provider = new OslcRdfXmlProvider();

    TestResource resource = new TestResource(URI.create("http://example.com/resource1"));
    resource.setAproperty("value1");

    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
    provider.writeTo(
        resource,
        TestResource.class,
        TestResource.class,
        new Annotation[] {},
        MediaType.valueOf("application/xml"),
        new MultivaluedHashMap<>(),
        outputStream);

    String rdfXml = outputStream.toString(StandardCharsets.UTF_8);

    // application/xml is intentionally always abbreviated (unaffected by this fix);
    // asserting this documents the existing contrast and guards against a regression
    // in the other direction.
    assertFalse(
        "Abbreviated writer should not use the plain <rdf:Description> form",
        rdfXml.contains("<rdf:Description"));
  }

  @Test
  public void testServiceProviderCatalogUsesAbbreviatedWriterWhenForced() throws Exception {
    String oldAbbrev = System.getProperty(OSLC4JConstants.LYO_ALWAYS_XML_ABBREV_ONLY_PROVIDERS);
    System.setProperty(OSLC4JConstants.LYO_ALWAYS_XML_ABBREV_ONLY_PROVIDERS, "true");
    try {
      OslcRdfXmlProvider provider = new OslcRdfXmlProvider();
      ServiceProviderCatalog catalog = new ServiceProviderCatalog();
      catalog.setAbout(URI.create("http://example.com/catalog"));

      ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
      provider.writeTo(
          catalog,
          ServiceProviderCatalog.class,
          ServiceProviderCatalog.class,
          new Annotation[] {},
          MediaType.valueOf("application/rdf+xml"),
          new MultivaluedHashMap<>(),
          outputStream);

      String rdfXml = outputStream.toString(StandardCharsets.UTF_8);

      // RDNG workaround: even with Accept=application/rdf+xml, the forced-abbrev system
      // property should route ServiceProviderCatalog through the abbreviated writer.
      assertFalse(
          "ServiceProviderCatalog should use the abbreviated writer when "
              + "alwaysXMLAbbrevOnlyProviders is set, got: "
              + rdfXml,
          rdfXml.contains("<rdf:Description"));
      ByteArrayOutputStream abbreviated = new ByteArrayOutputStream();
      provider.writeTo(
          catalog,
          ServiceProviderCatalog.class,
          ServiceProviderCatalog.class,
          new Annotation[] {},
          MediaType.APPLICATION_XML_TYPE,
          new MultivaluedHashMap<>(),
          abbreviated);
      assertEquals(
          "Provider override must use the custom application/xml writer",
          rdfXml,
          abbreviated.toString(StandardCharsets.UTF_8));
    } finally {
      restoreProperty(OSLC4JConstants.LYO_ALWAYS_XML_ABBREV_ONLY_PROVIDERS, oldAbbrev);
    }
  }

  @Test
  @SuppressWarnings({"unchecked", "rawtypes"})
  public void testTurtleRoundTrip() throws Exception {
    OslcRdfXmlProvider provider = new OslcRdfXmlProvider();

    TestResource resource = new TestResource(URI.create("http://example.com/resource1"));
    resource.setAproperty("value1");

    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
    provider.writeTo(
        resource,
        TestResource.class,
        TestResource.class,
        new Annotation[] {},
        OslcMediaType.TEXT_TURTLE_TYPE,
        new MultivaluedHashMap<>(),
        outputStream);

    TestResource roundTripped =
        (TestResource)
            provider.readFrom(
                (Class) TestResource.class,
                TestResource.class,
                new Annotation[] {},
                OslcMediaType.TEXT_TURTLE_TYPE,
                new MultivaluedHashMap<>(),
                new ByteArrayInputStream(outputStream.toByteArray()));

    assertEquals("value1", roundTripped.getAproperty());
  }

  @Test
  @SuppressWarnings({"unchecked", "rawtypes"})
  public void testJsonLdRoundTrip() throws Exception {
    OslcRdfXmlProvider provider = new OslcRdfXmlProvider();

    TestResource resource = new TestResource(URI.create("http://example.com/resource1"));
    resource.setAproperty("value1");

    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
    provider.writeTo(
        resource,
        TestResource.class,
        TestResource.class,
        new Annotation[] {},
        OslcMediaType.APPLICATION_JSON_LD_TYPE,
        new MultivaluedHashMap<>(),
        outputStream);

    TestResource roundTripped =
        (TestResource)
            provider.readFrom(
                (Class) TestResource.class,
                TestResource.class,
                new Annotation[] {},
                OslcMediaType.APPLICATION_JSON_LD_TYPE,
                new MultivaluedHashMap<>(),
                new ByteArrayInputStream(outputStream.toByteArray()));

    assertEquals("value1", roundTripped.getAproperty());
  }
}
