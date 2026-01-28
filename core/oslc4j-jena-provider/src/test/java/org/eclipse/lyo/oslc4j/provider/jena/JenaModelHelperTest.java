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
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.namespace.QName;
import org.apache.jena.datatypes.DatatypeFormatException;
import org.apache.jena.rdf.model.Model;
import org.eclipse.lyo.oslc4j.core.exception.LyoModelException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.MultiStatementLink;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.provider.jena.helpers.RDFHelper;
import org.eclipse.lyo.oslc4j.provider.jena.resources.Container;
import org.eclipse.lyo.oslc4j.provider.jena.resources.Dog;
import org.eclipse.lyo.oslc4j.provider.jena.resources.Element;
import org.eclipse.lyo.oslc4j.provider.jena.resources.Person;
import org.eclipse.lyo.oslc4j.provider.jena.resources.Pet;
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
    
    for(Object linkReturned : linkListCol) {
        assertTrue(linkReturned instanceof Link);
        Link msl = (Link)linkReturned;
        if (msl.getValue().toString().equals("http://example.com/l4")) {
            assertEquals("label4", msl.getLabel());
        } else if (msl.getValue().toString().equals("http://example.com/l5")) {
            assertEquals("label5", msl.getLabel());
        } else {
            fail("Unexpected Link value: " + msl.getValue());
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

 
  private ServiceProvider roundTrip(ServiceProvider sp)
      throws DatatypeConfigurationException, IllegalAccessException, IllegalArgumentException,
      InvocationTargetException, OslcCoreApplicationException, LyoModelException {
    final Model model = JenaModelHelper.createJenaModel(new Object[] {sp});
    return JenaModelHelper.unmarshal(
            model.getResource(sp.getAbout().toString()), ServiceProvider.class);
  }
}
