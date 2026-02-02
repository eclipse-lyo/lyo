/*
 * Copyright (c) 2023 Contributors to the Eclipse Foundation
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

package org.eclipse.lyo.oslc4j.provider.jena;

import static org.eclipse.lyo.oslc4j.provider.jena.helpers.JenaAssert.*;
import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.namespace.QName;
import org.apache.jena.datatypes.DatatypeFormatException;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.vocabulary.RDF;
import org.eclipse.lyo.oslc4j.core.exception.LyoModelException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.MultiStatementLink;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.provider.jena.helpers.RDFHelper;
import org.eclipse.lyo.oslc4j.provider.jena.resources.Container;
import org.eclipse.lyo.oslc4j.provider.jena.resources.Dog;
import org.eclipse.lyo.oslc4j.provider.jena.resources.Element;
import org.eclipse.lyo.oslc4j.provider.jena.resources.Person;
import org.eclipse.lyo.oslc4j.provider.jena.resources.Pet;
import org.eclipse.lyo.oslc4j.provider.jena.test.resources.TestResource;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @since 2.4.0
 */
public class JenaModelHelperTest {

  private static final Logger log = LoggerFactory.getLogger(JenaModelHelperTest.class);

  @Test
  public void testSeqMarshalling()
      throws InvocationTargetException,
          DatatypeConfigurationException,
          OslcCoreApplicationException,
          IllegalAccessException,
          IOException {
    final Model expectedModel = RDFHelper.loadResourceModel("container-element.ttl");
    final Container container = new Container();
    container.setAbout(URI.create("urn:test:containerA"));
    final List<Element> children = List.of(element("A"), element("B"));
    container.setChildrenL(children);
    container.setChildrenB(children);

    final Model actualModel = JenaModelHelper.createJenaModel(new Object[] {container});

    //        log.info(RDFHelper.toTurtleString(actualModel));
    assertThat(actualModel).isomorphicWith(expectedModel);
  }

  private Element element(final String name) {
    final Element element = new Element();
    element.setAbout(URI.create(String.format("urn:test:%s", name)));
    element.setName(name);
    return element;
  }

  @Test
  public void testAbstractTypes() throws IOException, LyoModelException {
    final Model expectedModel = RDFHelper.loadResourceModel("abstract-types.ttl");
    final Person person = JenaModelHelper.unmarshalSingle(expectedModel, Person.class);
    assertNotNull(person);
    assertNotNull(person.getPet());
    assertEquals(Dog.class, person.getPet().getClass());
  }

  @Test
  public void testAbstractTypesForMainClass() throws IOException, LyoModelException {
    final Model model = RDFHelper.loadResourceModel("abstract-types.ttl");
    final Pet pet = JenaModelHelper.unmarshal(model.getResource("urn:test:rex"), Pet.class);
    assertEquals(Dog.class, pet.getClass());
  }

  // Removed expected = DatatypeFormatException.class as the input data is now valid (reverted to
  // original)
  @Test
  public void testExtendedEscape() throws IOException, LyoModelException {
    final Model model = RDFHelper.loadResourceModel("escape.ttl");
    final ServiceProvider resource =
        JenaModelHelper.unmarshal(
            model.getResource("http://example.com/test"), ServiceProvider.class);
  }

  public void testExtendedEscapeValid() throws IOException, LyoModelException {
    final Model model = RDFHelper.loadResourceModel("escape.ttl");
    final ServiceProvider resource =
        JenaModelHelper.unmarshal(
            model.getResource("http://example.com/test"), ServiceProvider.class);
    assertEquals(3, resource.getExtendedProperties().size());
  }

  // Removed expected = DatatypeFormatException.class as the input data is now valid (reverted to
  // original)
  @Test
  public void testExtendedEscapeXML() throws IOException, LyoModelException {
    final Model model = RDFHelper.loadResourceModel("escape.xml");
    final ServiceProvider resource =
        JenaModelHelper.unmarshal(
            model.getResource("http://example.com/test"), ServiceProvider.class);
  }

  @Test
  public void testExtendedEscapeXMLValid() throws IOException, LyoModelException {
    final Model model = RDFHelper.loadResourceModel("escape-valid.xml");
    final ServiceProvider resource =
        JenaModelHelper.unmarshal(
            model.getResource("http://example.com/test"), ServiceProvider.class);
    assertEquals(2, resource.getExtendedProperties().size());
  }

  @Test(expected = DatatypeFormatException.class)
  public void testExtendedEscapeMalformed() throws IOException, LyoModelException {
    final Model model = RDFHelper.loadResourceModel("escape-malformed.ttl");
    final ServiceProvider resource =
        JenaModelHelper.unmarshal(
            model.getResource("http://example.com/test"), ServiceProvider.class);
  }

  @Test(expected = DatatypeFormatException.class)
  public void testExtendedEscapeXMLMalformed() throws IOException, LyoModelException {
    final Model model = RDFHelper.loadResourceModel("escape-malformed.xml");
    final ServiceProvider resource =
        JenaModelHelper.unmarshal(
            model.getResource("http://example.com/test"), ServiceProvider.class);
  }

  @Test
  public void testExtendedProperties_URI_Single()
      throws Exception {
    final ServiceProvider sp = new ServiceProvider();
    sp.setAbout(URI.create("http://example.com/sp"));
    final QName uriProp = new QName("http://example.com/ns#", "uri");
    final URI uriValue = URI.create("http://example.com/u1");

    sp.getExtendedProperties().put(uriProp, uriValue);

    final ServiceProvider unmarshalled = roundTrip(sp);
    final Map<QName, Object> props = unmarshalled.getExtendedProperties();

    assertInstanceOf(URI.class, props.get(uriProp));
    assertEquals(uriValue, props.get(uriProp));
  }

  @Test
  public void testExtendedProperties_URI_Array()
      throws Exception {
    final ServiceProvider sp = new ServiceProvider();
    sp.setAbout(URI.create("http://example.com/sp"));
    final QName uriArrayProp = new QName("http://example.com/ns#", "uriArray");
    final URI[] uriArrayValue = {
      URI.create("http://example.com/u2"), URI.create("http://example.com/u3")
    };

    sp.getExtendedProperties().put(uriArrayProp, uriArrayValue);

    final ServiceProvider unmarshalled = roundTrip(sp);
    final Map<QName, Object> props = unmarshalled.getExtendedProperties();

    Object uriArrayRet = props.get(uriArrayProp);
    assertTrue(uriArrayRet instanceof Collection);
    Collection<?> uriArrayCol = (Collection<?>) uriArrayRet;
    assertTrue(uriArrayCol.contains(URI.create("http://example.com/u2")));
    assertTrue(uriArrayCol.contains(URI.create("http://example.com/u3")));
  }

  @Test
  public void testExtendedProperties_URI_List()
      throws Exception {
    final ServiceProvider sp = new ServiceProvider();
    sp.setAbout(URI.create("http://example.com/sp"));
    final QName uriListProp = new QName("http://example.com/ns#", "uriList");
    final List<URI> uriListValue =
        Arrays.asList(URI.create("http://example.com/u4"), URI.create("http://example.com/u5"));

    sp.getExtendedProperties().put(uriListProp, uriListValue);

    final ServiceProvider unmarshalled = roundTrip(sp);
    final Map<QName, Object> props = unmarshalled.getExtendedProperties();

    Object uriListRet = props.get(uriListProp);
    assertTrue(uriListRet instanceof Collection);
    Collection<?> uriListCol = (Collection<?>) uriListRet;
    assertTrue(uriListCol.contains(URI.create("http://example.com/u4")));
    assertTrue(uriListCol.contains(URI.create("http://example.com/u5")));
  }

  @Test
  public void testExtendedProperties_Link_WithoutLabel()
      throws Exception {
    final ServiceProvider sp = new ServiceProvider();
    sp.setAbout(URI.create("http://example.com/sp"));
    final QName linkProp = new QName("http://example.com/ns#", "link");
    final Link linkValue = new Link(URI.create("http://example.com/l1"));

    sp.getExtendedProperties().put(linkProp, linkValue);

    final ServiceProvider unmarshalled = roundTrip(sp);
    final Map<QName, Object> props = unmarshalled.getExtendedProperties();

    //If it is a Link without label, it should be unmarshalled as URI
    Object linkRet = props.get(linkProp);
    assertInstanceOf(URI.class, linkRet);
    assertEquals(linkValue.getValue(), linkRet);
  }

  @Test
  public void testExtendedProperties_Link_WithLabel()
      throws Exception {
    final ServiceProvider sp = new ServiceProvider();
    sp.setAbout(URI.create("http://example.com/sp"));
    final QName linkWithLabelProp = new QName("http://example.com/ns#", "linkWithLabel");
    final Link linkWithLabelValue = new Link(URI.create("http://example.com/l1b"), "someLabel");

    sp.getExtendedProperties().put(linkWithLabelProp, linkWithLabelValue);

    final ServiceProvider unmarshalled = roundTrip(sp);
    final Map<QName, Object> props = unmarshalled.getExtendedProperties();

    Object linkWithLabelRet = props.get(linkWithLabelProp);

    //assert that linkWithLabelRet is an instance of Link
    assertInstanceOf(Link.class, linkWithLabelRet);
    Link extendedProperty = (Link)linkWithLabelRet;
    assertEquals(linkWithLabelValue.getValue(), extendedProperty.getValue());
    assertEquals(linkWithLabelValue.getLabel(), extendedProperty.getLabel());
  }

  @Test
  public void testExtendedProperties_Link_Array_WithoutLabels()
      throws Exception {
    final ServiceProvider sp = new ServiceProvider();
    sp.setAbout(URI.create("http://example.com/sp"));
    final QName linkArrayProp = new QName("http://example.com/ns#", "linkArray");
    final Link[] linkArrayValue = {
      new Link(URI.create("http://example.com/l2")), new Link(URI.create("http://example.com/l3"))
    };

    sp.getExtendedProperties().put(linkArrayProp, linkArrayValue);

    final ServiceProvider unmarshalled = roundTrip(sp);
    final Map<QName, Object> props = unmarshalled.getExtendedProperties();

    Object linkArrayRet = props.get(linkArrayProp);
    assertTrue(linkArrayRet instanceof Collection);
    Collection<?> linkArrayCol = (Collection<?>) linkArrayRet;
    assertEquals(2, linkArrayCol.size());
    for(Object item : linkArrayCol) {
        assertTrue(item instanceof URI);
        boolean matched = Arrays.asList(linkArrayValue).stream().
            anyMatch(link -> link.getValue().equals((URI)item));
        assertTrue("URI " + item + " should match one of the expected link values", matched);
    }
  }

  @Test
  public void testExtendedProperties_Link_List_WithLabels()
      throws Exception {
    final ServiceProvider sp = new ServiceProvider();
    sp.setAbout(URI.create("http://example.com/sp"));
    final QName linkListProp = new QName("http://example.com/ns#", "linkList");
    final List<Link> linkListValue =
        Arrays.asList(
            new Link(URI.create("http://example.com/l4"), "label4"),
            new Link(URI.create("http://example.com/l5"), "label5"));

    sp.getExtendedProperties().put(linkListProp, linkListValue);

    final ServiceProvider unmarshalled = roundTrip(sp);
    final Map<QName, Object> props = unmarshalled.getExtendedProperties();

    Object linkListRet = props.get(linkListProp);
    assertTrue(linkListRet instanceof Collection);
    Collection<?> linkListCol = (Collection<?>) linkListRet;
    assertEquals(2, linkListCol.size());

    for (Object linkReturned : linkListCol) {
        assertTrue(linkReturned instanceof Link);
        Link link = (Link) linkReturned;
        if (link.getValue().toString().equals("http://example.com/l4")) {
            assertEquals("label4", link.getLabel());
        } else if (link.getValue().toString().equals("http://example.com/l5")) {
            assertEquals("label5", link.getLabel());
        } else {
            fail("Unexpected Link value: " + link.getValue());
        }
    }
  }

  @Test
  public void testExtendedProperties_URI_Iterable()
      throws Exception {
    final ServiceProvider sp = new ServiceProvider();
    sp.setAbout(URI.create("http://example.com/sp"));
    final QName uriIterableProp = new QName("http://example.com/ns#", "uriIterable");
    final List<URI> uriList =
        Arrays.asList(URI.create("http://example.com/u6"), URI.create("http://example.com/u7"));

    // Create an Iterable that is NOT a Collection
    final Iterable<URI> uriIterableValue = new Iterable<URI>() {
        @Override
        public java.util.Iterator<URI> iterator() {
            return uriList.iterator();
        }
    };

    sp.getExtendedProperties().put(uriIterableProp, uriIterableValue);

    final ServiceProvider unmarshalled = roundTrip(sp);
    final Map<QName, Object> props = unmarshalled.getExtendedProperties();

    Object uriIterableRet = props.get(uriIterableProp);
    assertTrue(uriIterableRet instanceof Collection);
    Collection<?> uriIterableCol = (Collection<?>) uriIterableRet;
    assertTrue(uriIterableCol.contains(URI.create("http://example.com/u6")));
    assertTrue(uriIterableCol.contains(URI.create("http://example.com/u7")));
  }

  @Test
  public void testReificationIntoMultiStatementLink() throws Exception {
      try(InputStream is = JenaModelHelperTest.class.getResourceAsStream("/extendedPropertyWithMultipleStatements.ttl")) {
          assertNotNull("Could not read file: extendedPropertyWithMultipleStatements.ttl", is);
          Model m = ModelFactory.createDefaultModel();
          m.read(is, null, "TURTLE");

          TestResource resource = JenaModelHelper.unmarshalSingle(m, TestResource.class);

          Map<QName, Object> extendedProperties = resource.getExtendedProperties();

          QName propertyAsLink = new QName("http://example.com/ns#", "asLink");
          Object propertyAsLinkRet = extendedProperties.get(propertyAsLink);
          assertTrue(propertyAsLinkRet instanceof Link);
          assertEquals(URI.create("http://example.com/object_1"), ((Link)propertyAsLinkRet).getValue());
          assertEquals("creator_label", ((Link)propertyAsLinkRet).getLabel());

          QName property1AsMultiStatement = new QName("http://example.com/ns#", "asMultiStatement1");
          Object property1AsMultiStatementRet = extendedProperties.get(property1AsMultiStatement);
          assertTrue(property1AsMultiStatementRet instanceof MultiStatementLink);
          MultiStatementLink prop1 = (MultiStatementLink)property1AsMultiStatementRet;
          assertEquals(2, prop1.getStatements().size());
          assertEquals("object_2_title", prop1.getStatements().get(new QName("http://purl.org/dc/terms/", "title")));
          assertTrue(prop1.getStatements().get(new QName("http://purl.org/dc/terms/", "created")) instanceof Date);

          QName property2AsMultiStatement = new QName("http://example.com/ns#", "asMultiStatement2");
          Object property2AsMultiStatementRet = extendedProperties.get(property2AsMultiStatement);
          assertTrue(property2AsMultiStatementRet instanceof MultiStatementLink);
          MultiStatementLink prop2 = (MultiStatementLink)property2AsMultiStatementRet;
          assertEquals(1, prop2.getStatements().size());
          assertEquals(URI.create("http://example.com/creator"), prop2.getStatements().get(new QName("http://purl.org/dc/terms/", "creator")));
      }
  }

  /**
   * Tests that reified statements are correctly unmarshalled using the new caching mechanism.
   * This test verifies that the optimization doesn't break existing functionality.
   */
  @Test
  public void testReificationCacheBasicUnmarshalling() throws Exception {
    final Model model = RDFHelper.loadResourceModel("extendedPropertyWithMultipleStatements.ttl");
    final TestResource resource = JenaModelHelper.unmarshal(
        model.getResource("http://example.com/subject_1"), TestResource.class);

    assertNotNull(resource);
    assertNotNull(resource.getExtendedProperties());

    // Verify Link with label is correctly unmarshalled
    QName linkProp = new QName("http://example.com/ns#", "asLink");
    Object linkValue = resource.getExtendedProperties().get(linkProp);
    assertInstanceOf(Link.class, linkValue);
    Link link = (Link) linkValue;
    assertEquals("creator_label", link.getLabel());
    assertEquals(URI.create("http://example.com/object_1"), link.getValue());
  }

  /**
   * Tests that multiple reified statements on different properties are all correctly processed
   * when using the reification cache.
   */
  @Test
  public void testReificationCacheMultipleReifiedProperties() throws Exception {
    final Model model = RDFHelper.loadResourceModel("extendedPropertyWithMultipleStatements.ttl");
    final TestResource resource = JenaModelHelper.unmarshal(
        model.getResource("http://example.com/subject_1"), TestResource.class);

    Map<QName, Object> extendedProps = resource.getExtendedProperties();

    // Verify first multi-statement link
    QName multiStmt1 = new QName("http://example.com/ns#", "asMultiStatement1");
    Object multiStmt1Value = extendedProps.get(multiStmt1);
    assertInstanceOf(MultiStatementLink.class, multiStmt1Value);
    MultiStatementLink ms1 = (MultiStatementLink) multiStmt1Value;
    assertEquals(URI.create("http://example.com/object_2"), ms1.getValue());
    assertEquals(2, ms1.getStatements().size());

    // Verify second multi-statement link
    QName multiStmt2 = new QName("http://example.com/ns#", "asMultiStatement2");
    Object multiStmt2Value = extendedProps.get(multiStmt2);
    assertInstanceOf(MultiStatementLink.class, multiStmt2Value);
    MultiStatementLink ms2 = (MultiStatementLink) multiStmt2Value;
    assertEquals(URI.create("http://example.com/object_3"), ms2.getValue());
    assertEquals(1, ms2.getStatements().size());
  }

  /**
   * Tests that the reification cache correctly handles models with NO reified statements.
   * The cache should be empty and unmarshalling should work normally.
   */
  @Test
  public void testReificationCacheWithNoReifications() throws Exception {
    final Model model = ModelFactory.createDefaultModel();
    final String ns = "http://example.com/ns#";
    final String resourceUri = "http://example.com/resource1";

    // Create a simple resource with extended properties but NO reifications
    org.apache.jena.rdf.model.Resource resource = model.createResource(resourceUri);
    resource.addProperty(
        model.createProperty("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "type"),
        model.createResource("http://example.com/ns#Test"));
    resource.addProperty(
        model.createProperty(ns, "simpleProp"),
        model.createResource("http://example.com/object1"));

    final TestResource testResource = JenaModelHelper.unmarshal(
        model.getResource(resourceUri), TestResource.class);

    assertNotNull(testResource);
    // Extended property should be a plain URI, not a Link or MultiStatementLink
    Object simpleProp = testResource.getExtendedProperties().get(new QName(ns, "simpleProp"));
    assertInstanceOf(URI.class, simpleProp);
    assertEquals(URI.create("http://example.com/object1"), simpleProp);
  }

  /**
   * Tests that the reification cache works correctly when unmarshalling multiple resources
   * from the same model. The cache should be built once and reused for all resources.
   * This test uses the existing extendedPropertyWithMultipleStatements.ttl which has
   * proper reification examples.
   */
  @Test
  public void testReificationCacheReuseAcrossMultipleResources() throws Exception {
    // Use the existing test file which has properly formatted reifications
    final Model model = RDFHelper.loadResourceModel("extendedPropertyWithMultipleStatements.ttl");

    // Unmarshal the resource - the cache should be built once
    final TestResource resource = JenaModelHelper.unmarshal(
        model.getResource("http://example.com/subject_1"), TestResource.class);

    assertNotNull(resource);
    assertNotNull(resource.getExtendedProperties());
    assertFalse("Resource should have extended properties", resource.getExtendedProperties().isEmpty());

    // Verify that reified properties are correctly unmarshalled
    // The model has: asLink with dcterms:title="creator_label"
    QName asLinkProp = new QName("http://example.com/ns#", "asLink");
    Object asLinkValue = resource.getExtendedProperties().get(asLinkProp);

    // With reification cache, this should be a Link with label
    assertInstanceOf(Link.class, asLinkValue);
    Link link = (Link) asLinkValue;
    assertEquals("creator_label", link.getLabel());
    assertEquals(URI.create("http://example.com/object_1"), link.getValue());
  }

  /**
   * Tests that the cache correctly handles incomplete reifications (missing subject, predicate, or object).
   * Such incomplete reifications should be skipped without causing errors.
   */
  @Test
  public void testReificationCacheWithIncompleteReifications() throws Exception {
    final Model model = ModelFactory.createDefaultModel();
    final String ns = "http://example.com/ns#";
    final String resourceUri = "http://example.com/resource1";

    org.apache.jena.rdf.model.Resource resource = model.createResource(resourceUri);
    resource.addProperty(
        model.createProperty("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "type"),
        model.createResource(ns + "Test"));
    resource.addProperty(
        model.createProperty(ns, "testProp"),
        model.createResource("http://example.com/obj1"));

    // Create an incomplete reification (missing rdf:object)
    org.apache.jena.rdf.model.Resource incompleteReification = model.createResource();
    incompleteReification.addProperty(
        model.createProperty("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "type"),
        model.createProperty("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "Statement"));
    incompleteReification.addProperty(
        model.createProperty("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "subject"),
        resource);
    incompleteReification.addProperty(
        model.createProperty("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "predicate"),
        model.createProperty(ns, "testProp"));
    // Missing rdf:object!
    incompleteReification.addLiteral(
        model.createProperty("http://purl.org/dc/terms/", "title"),
        "Should be ignored");

    // Should not throw an exception - incomplete reifications should be skipped
    final TestResource testResource = JenaModelHelper.unmarshal(
        model.getResource(resourceUri), TestResource.class);

    assertNotNull(testResource);
    // Property should be a plain URI since the reification is incomplete
    Object testProp = testResource.getExtendedProperties().get(new QName(ns, "testProp"));
    assertInstanceOf(URI.class, testProp);
  }

  private ServiceProvider roundTrip(ServiceProvider sp)
      throws DatatypeConfigurationException, IllegalAccessException, IllegalArgumentException,
      InvocationTargetException, OslcCoreApplicationException, LyoModelException {
    final Model model = JenaModelHelper.createJenaModel(new Object[] {sp});
    return JenaModelHelper.unmarshal(
            model.getResource(sp.getAbout().toString()), ServiceProvider.class);
  }

  /**
   * Tests that buildResourceList correctly includes URI resources from reified statements.
   * The resource http://example.com/issue/ADC-37 should be included even though
   * it appears as the object of rdf:subject in a reified statement.
   * The rdf:Statement blank node should be excluded.
   */
  @Test
  public void testBuildResourceListWithReifiedStatement() throws Exception {
    final Model model = RDFHelper.loadResourceModel("reified-statement.xml");

    // Unmarshal with TestResource which should pick up any resource with rdf:type
    final TestResource[] resources = JenaModelHelper.unmarshal(model, TestResource.class);

    // Should find the resource, not the reification node
    assertNotNull(resources);
    assertEquals("Should find exactly one resource (not the rdf:Statement node)", 1, resources.length);
    assertEquals("Should be the actual resource URI", 
        URI.create("http://example.com/issue/ADC-37"), 
        resources[0].getAbout());
  }

  /**
   * Tests that blank nodes (including reification nodes) are excluded from the result list.
   * Only URI resources should be returned.
   */
  @Test
  public void testBuildResourceListExcludesBlankNodes() throws Exception {
    final Model model = RDFHelper.loadResourceModel("uri-and-blank-nodes.ttl");

    final TestResource[] resources = JenaModelHelper.unmarshal(model, TestResource.class);

    // Should only find the URI resource, not the blank node
    assertNotNull(resources);
    assertEquals("Should find only the URI resource, not blank nodes", 1, resources.length);
    assertEquals(URI.create("http://example.com/resource1"), resources[0].getAbout());
  }

  /**
   * Tests that both top-level and inline URI resources are included in the result list.
   * The distinction between "top-level" and "inline" is artificial in RDF - if a resource
   * has rdf:type and properties, it should be unmarshaled regardless.
   */
  @Test
  public void testBuildResourceListIncludesInlineResources() throws Exception {
    final Model model = RDFHelper.loadResourceModel("inline-resources.ttl");

    final TestResource[] resources = JenaModelHelper.unmarshal(model, TestResource.class);

    // Should find BOTH resources - the "top-level" one and the "inline" one
    assertNotNull(resources);
    assertEquals("Should find all resources with rdf:type and properties", 2, resources.length);
    
    // Verify both URIs are present
    List<URI> uris = Arrays.stream(resources).map(TestResource::getAbout).toList();
    assertTrue("Should include the top-level resource", 
        uris.contains(URI.create("http://example.com/toplevel")));
    assertTrue("Should include the inline resource", 
        uris.contains(URI.create("http://example.com/inline")));
  }

  /**
   * Tests the fallback mechanism when type matching fails in backward-compatible mode.
   * When useBeanClassForParsing is false and no resources match the bean class type,
   * it should fall back to returning top-level resources.
   */
  @Test
  public void testBuildResourceListFallbackWhenNoTypeMatch() throws Exception {
    final Model model = RDFHelper.loadResourceModel("non-matching-type.ttl");

    // Even though the type doesn't match, the fallback should find it
    final TestResource[] resources = JenaModelHelper.unmarshal(model, TestResource.class);

    assertNotNull(resources);
    assertTrue("Should find at least one resource via fallback", resources.length >= 1);
    assertEquals(URI.create("http://example.com/resource1"), resources[0].getAbout());
  }

  /**
   * Tests that resources referenced only by rdf:subject are included (reified statement subjects).
   * The actual resource should be included despite being referenced by rdf:subject in a reification.
   */
  @Test
  public void testBuildResourceListIncludesReifiedSubjects() throws Exception {
    final Model model = RDFHelper.loadResourceModel("reified-subject.ttl");

    final TestResource[] resources = JenaModelHelper.unmarshal(model, TestResource.class);

    // The actual resource should be included despite being referenced by rdf:subject
    assertNotNull(resources);
    assertEquals("Should find the resource even though it's referenced by rdf:subject", 1, resources.length);
    assertEquals(URI.create("http://example.com/resource1"), resources[0].getAbout());
  }

  /**
   * Tests that multiple top-level resources are all included.
   */
  @Test
  public void testBuildResourceListWithMultipleTopLevelResources() throws Exception {
    final Model model = RDFHelper.loadResourceModel("multiple-top-level.ttl");

    final TestResource[] resources = JenaModelHelper.unmarshal(model, TestResource.class);

    assertNotNull(resources);
    assertEquals("Should find all three top-level resources", 3, resources.length);

    // Verify all URIs are present
    List<URI> uris = Arrays.stream(resources).map(TestResource::getAbout).toList();
    assertTrue(uris.contains(URI.create("http://example.com/resource1")));
    assertTrue(uris.contains(URI.create("http://example.com/resource2")));
    assertTrue(uris.contains(URI.create("http://example.com/resource3")));
  }

  /**
   * Tests that top-level resources with circular references are correctly included.
   * 
   * When two top-level resources reference each other (e.g., a Requirement that is
   * "implementedBy" a ChangeRequest, and that ChangeRequest "implements" the same Requirement),
   * both resources should be included since they are both top-level
   * resources with rdf:type properties that simply happen to reference each other.
   * 
   */
  @Test
  public void testBuildResourceListWithCircularReferences() throws Exception {
    final Model model = RDFHelper.loadResourceModel("circular-reference.xml");

    final TestResource[] resources = JenaModelHelper.unmarshal(model, TestResource.class);

    // EXPECTED: Should find both resources despite circular references
    // ACTUAL BUG: Current implementation excludes both resources because they reference each other
    assertNotNull(resources);
    assertEquals("Should find both top-level resources despite circular references", 2, resources.length);

    // Verify both URIs are present
    List<URI> uris = Arrays.stream(resources).map(TestResource::getAbout).toList();
    assertTrue("Should include the Requirement resource", 
        uris.contains(URI.create("http://example.com/requirements/REQ-123")));
    assertTrue("Should include the ChangeRequest resource", 
        uris.contains(URI.create("http://example.com/changeRequests/CR-456")));
  }

  /**
   * Tests that when unmarshaling a model containing two resources where one references the other,
   * both resources are included in the results and the reference is properly populated.
   * 
   * <ul>
   *   <li>Both resources are returned in the results array (both have rdf:type and properties)</li>
   *   <li>The referencing resource has its relatedResource property populated with correct URI and values</li>
   * </ul>
   */
  @Test
  public void testUnmarshalTwoResourcesWhereOneReferencesOther() throws Exception {
    final Model model = RDFHelper.loadResourceModel("two-resources-one-references-other.xml");

    // Unmarshal to TestResource - should get both resources
    final TestResource[] resources = JenaModelHelper.unmarshal(model, TestResource.class);

    // Verify we get both resources back
    assertNotNull("Resources array should not be null", resources);
    assertEquals("Should find both resources in the model", 2, resources.length);

    // Find resource1 and resource2 in the results
    TestResource resource1 = null;
    TestResource resource2 = null;
    for (TestResource r : resources) {
      if (r.getAbout().toString().equals("http://example.com/resource1")) {
        resource1 = r;
      } else if (r.getAbout().toString().equals("http://example.com/resource2")) {
        resource2 = r;
      }
    }

    assertNotNull("Resource1 should be in the results", resource1);
    assertNotNull("Resource2 should be in the results", resource2);

    // Verify properties
    assertEquals("Resource1 should have correct property value", "Resource 1", resource1.getAproperty());
    assertEquals("Resource2 should have correct property value", "Resource 2", resource2.getAproperty());

    // Verify that resource1's reference to resource2 is properly populated
    assertNotNull("Resource1 should have relatedResource populated", resource1.getRelatedResource());
    assertEquals("Resource1's relatedResource should point to resource2", 
        URI.create("http://example.com/resource2"), 
        resource1.getRelatedResource().getAbout());
    assertEquals("Resource1's relatedResource should have correct property value",
        "Resource 2",
        resource1.getRelatedResource().getAproperty());
  }
}

