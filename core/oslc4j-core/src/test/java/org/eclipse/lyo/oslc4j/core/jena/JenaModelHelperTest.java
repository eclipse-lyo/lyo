/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
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

package org.eclipse.lyo.oslc4j.core.jena;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URISyntaxException;
import javax.xml.datatype.DatatypeConfigurationException;

import org.apache.jena.datatypes.DatatypeFormatException;
import org.apache.jena.ext.com.google.common.collect.ImmutableList;
import org.apache.jena.rdf.model.Model;
import org.eclipse.lyo.oslc4j.core.OSLC4JConstants;
import org.eclipse.lyo.oslc4j.core.exception.LyoModelException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.core.jena.helpers.RDFHelper;
import org.eclipse.lyo.oslc4j.core.jena.resources.Container;
import org.eclipse.lyo.oslc4j.core.jena.resources.Element;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.junit.Assert.*;
import static org.eclipse.lyo.oslc4j.core.jena.helpers.JenaAssert.*;
import org.eclipse.lyo.oslc4j.core.jena.resources.Animal;
import org.eclipse.lyo.oslc4j.core.jena.resources.Dog;
import org.eclipse.lyo.oslc4j.core.jena.resources.Person;
import org.eclipse.lyo.oslc4j.core.jena.resources.Pet;

/**
 *
 * @version $version-stub$
 * @since 2.4.0
 */
public class JenaModelHelperTest {

    private final static Logger log = LoggerFactory.getLogger(JenaModelHelperTest.class);

    @Test
    public void testSeqMarshalling()
            throws InvocationTargetException, DatatypeConfigurationException,
            OslcCoreApplicationException, IllegalAccessException, IOException {
        final Model expectedModel = RDFHelper.loadResourceModel("container-element.ttl");
        final Container container = new Container();
        container.setAbout(URI.create("urn:containerA"));
        final ImmutableList<Element> children = ImmutableList.of(element("A"), element("B"));
        container.setChildrenL(children);
        container.setChildrenB(children);

        final Model actualModel = JenaModelHelper.createJenaModel(new Object[]{container});

//        log.info(RDFHelper.toTurtleString(actualModel));
        assertThat(actualModel).isomorphicWith(expectedModel);
    }

    private Element element(final String name) {
        final Element element = new Element();
        element.setAbout(URI.create(String.format("urn:%s", name)));
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
        final Pet pet = JenaModelHelper.unmarshal(model.getResource("urn:rex"), Pet.class);
        assertEquals(Dog.class, pet.getClass());
    }

    @Test(expected = DatatypeFormatException.class)
    public void testExtendedEscape() throws IOException, LyoModelException {
        final Model model = RDFHelper.loadResourceModel("escape.ttl");
        final ServiceProvider resource = JenaModelHelper.unmarshal(model.getResource("http://example.com/test"), ServiceProvider.class);
    }

    public void testExtendedEscapeValid() throws IOException, LyoModelException {
        final Model model = RDFHelper.loadResourceModel("escape.ttl");
        final ServiceProvider resource = JenaModelHelper.unmarshal(model.getResource("http://example.com/test"), ServiceProvider.class);
        assertEquals(3, resource.getExtendedProperties().size());
    }

    @Test(expected = DatatypeFormatException.class)
    public void testExtendedEscapeXML() throws IOException, LyoModelException {
        final Model model = RDFHelper.loadResourceModel("escape.xml");
        final ServiceProvider resource = JenaModelHelper.unmarshal(model.getResource("http://example.com/test"), ServiceProvider.class);
    }

    @Test
    public void testExtendedEscapeXMLValid() throws IOException, LyoModelException {
        final Model model = RDFHelper.loadResourceModel("escape-valid.xml");
        final ServiceProvider resource = JenaModelHelper.unmarshal(model.getResource("http://example.com/test"), ServiceProvider.class);
//        assertEquals(56, resource.getExtendedProperties().size());
    }
}
