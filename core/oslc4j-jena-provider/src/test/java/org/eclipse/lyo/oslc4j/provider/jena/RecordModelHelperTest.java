/* SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause */
package org.eclipse.lyo.oslc4j.provider.jena;

import static org.junit.jupiter.api.Assertions.*;

import java.io.StringReader;
import java.net.URI;
import java.util.List;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.eclipse.lyo.oslc4j.core.annotation.*;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.junit.jupiter.api.Test;

class RecordModelHelperTest {
  private static final String NS = "urn:record-test:";

  @OslcName("Element") @OslcNamespace(NS)
  public record Element(
      @OslcAbout URI about,
      @OslcPropertyDefinition(NS + "name") String name,
      @OslcPropertyDefinition(NS + "active") Boolean active,
      @OslcPropertyDefinition(NS + "links") List<URI> links,
      @OslcPropertyDefinition(NS + "tags") List<String> tags) {}

  @OslcName("Catalog") @OslcNamespace(NS)
  public record Catalog(
      @OslcPropertyDefinition(NS + "links") List<Link> links,
      @OslcPropertyDefinition(NS + "providers") List<ServiceProvider> providers,
      @OslcPropertyDefinition(NS + "elements") List<Element> elements) {}

  @Test void recordsCanContainLinksExistingPojosAndOtherRecords() throws Exception {
    Model model = ModelFactory.createDefaultModel().read(new StringReader("""
        <urn:catalog> a <urn:record-test:Catalog> ;
          <urn:record-test:links> <urn:external> ;
          <urn:record-test:providers> <urn:provider> ;
          <urn:record-test:elements> <urn:item> .
        <urn:provider> a <http://open-services.net/ns/core#ServiceProvider> ;
          <http://purl.org/dc/terms/title> "Provider" .
        <urn:item> a <urn:record-test:Element> ; <urn:record-test:name> "Nested" .
        """), null, "TURTLE");
    Catalog result = JenaModelHelper.unmarshalSingle(model, Catalog.class);
    assertEquals(URI.create("urn:external"), result.links().getFirst().getValue());
    assertEquals("Provider", result.providers().getFirst().getTitle());
    assertEquals(URI.create("urn:provider"), result.providers().getFirst().getAbout());
    assertEquals("Nested", result.elements().getFirst().name());
  }

  private Model turtle(String properties) {
    return ModelFactory.createDefaultModel().read(new StringReader(
        "<urn:item> a <urn:record-test:Element> " + properties + " ."), null, "TURTLE");
  }

  @Test void canonicalConstructorReadsScalarsAndRepeatedValues() throws Exception {
    Model model = turtle("; <urn:record-test:name> \"item\" ; <urn:record-test:active> true"
        + " ; <urn:record-test:links> <urn:b>, <urn:a> ; <urn:record-test:tags> \"z\", \"a\"");
    Element value = JenaModelHelper.unmarshalSingle(model, Element.class);
    assertEquals(URI.create("urn:item"), value.about());
    assertEquals("item", value.name());
    assertEquals(true, value.active());
    assertEquals(List.of(URI.create("urn:a"), URI.create("urn:b")), value.links());
    assertEquals(List.of("a", "z"), value.tags());
    assertThrows(UnsupportedOperationException.class, () -> value.tags().add("mutated"));
    assertEquals(value, JenaModelHelper.unmarshal(model.getResource("urn:item"), Element.class));
  }

  @Test void missingValuesRemainNullOrEmpty() throws Exception {
    Element value = JenaModelHelper.unmarshalSingle(turtle(""), Element.class);
    assertNull(value.name());
    assertNull(value.active());
    assertEquals(List.of(), value.links());
  }

  @Test void rdfListOrderIsPreserved() throws Exception {
    Element value = JenaModelHelper.unmarshalSingle(turtle("; <urn:record-test:tags> (\"z\" \"a\")"), Element.class);
    assertEquals(List.of("z", "a"), value.tags());
  }

  @Test void scalarMultiplicityIsRejected() {
    assertThrows(IllegalArgumentException.class, () -> JenaModelHelper.unmarshalSingle(
        turtle("; <urn:record-test:name> \"one\", \"two\""), Element.class));
  }

  @Test void literalCannotBecomeUri() {
    assertThrows(IllegalArgumentException.class, () -> JenaModelHelper.unmarshalSingle(
        turtle("; <urn:record-test:links> \"not-a-link\""), Element.class));
  }

  @Test void typeSelectionDoesNotUseUnrelatedSubjects() throws Exception {
    Model model = turtle("");
    model.read(new StringReader("<urn:other> a <urn:Other> ."), null, "TURTLE");
    assertEquals(1, JenaModelHelper.unmarshal(model, Element.class).length);
    assertThrows(IllegalArgumentException.class, () -> JenaModelHelper.unmarshalSingle(
        ModelFactory.createDefaultModel(), Element.class));
  }

  @Test void jsonLdContextSelectsTheSourceType() throws Exception {
    Model model = ModelFactory.createDefaultModel().read(new StringReader("""
        {"@context":{"@vocab":"urn:record-test:","active":{"@type":"http://www.w3.org/2001/XMLSchema#boolean"},
        "links":{"@type":"@id"}},"@id":"urn:item","@type":"Element","name":"API element",
        "active":false,"links":[{"@id":"urn:target"}]}
        """), null, "JSON-LD");
    Element value = JenaModelHelper.unmarshalSingle(model, Element.class);
    assertEquals("API element", value.name());
    assertEquals(false, value.active());
    assertEquals(List.of(URI.create("urn:target")), value.links());
  }
}
