/*
 * Copyright (c) 2018 Andrew Berezovskyi and others
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */

package org.eclipse.lyo.oslc4j.provider.jena;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import javax.xml.datatype.DatatypeConfigurationException;
import org.apache.jena.ext.com.google.common.collect.ImmutableList;
import org.apache.jena.rdf.model.Model;
import org.eclipse.lyo.oslc4j.core.exception.LyoModelException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.provider.jena.helpers.RDFHelper;
import org.eclipse.lyo.oslc4j.provider.jena.resources.Container;
import org.eclipse.lyo.oslc4j.provider.jena.resources.Element;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.junit.Assert.*;
import static org.eclipse.lyo.oslc4j.provider.jena.helpers.JenaAssert.*;
import org.eclipse.lyo.oslc4j.provider.jena.resources.Dog;
import org.eclipse.lyo.oslc4j.provider.jena.resources.Person;

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

}
